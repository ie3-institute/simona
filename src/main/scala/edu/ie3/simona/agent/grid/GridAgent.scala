/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.actor.SimonaActorNaming
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.{
  GridAgentBaseData,
  GridAgentConstantData,
  GridAgentInitData,
}
import edu.ie3.simona.agent.grid.GridAgentMessages._
import edu.ie3.simona.agent.participant.ParticipantAgent.ParticipantMessage
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.model.grid.GridModel
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.{Failure, Success}

object GridAgent extends DBFSAlgorithm with DCMAlgorithm {

  /** Trait for requests made to the [[GridAgent]] */
  sealed trait Request

  /** Necessary because we want to extend messages in other classes, but we do
    * want to keep the messages only available inside this package.
    */
  private[grid] trait InternalRequest extends Request
  private[grid] trait InternalReply extends Request

  def apply(
      environmentRefs: EnvironmentRefs,
      simonaConfig: SimonaConfig,
      listener: Iterable[ActorRef[ResultEvent]],
  ): Behavior[Request] = Behaviors.withStash(100) { buffer =>
    Behaviors.setup[Request] { context =>
      val activationAdapter: ActorRef[Activation] =
        context.messageAdapter[Activation](msg => WrappedActivation(msg))

      // val initialization
      val resolution: Long = simonaConfig.simona.powerflow.resolution.get(
        ChronoUnit.SECONDS
      ) // this determines the agents regular time bin it wants to be triggered e.g one hour

      val simStartTime: ZonedDateTime = TimeUtil.withDefaults
        .toZonedDateTime(simonaConfig.simona.time.startDateTime)

      val agentValues = GridAgentConstantData(
        environmentRefs,
        simonaConfig,
        listener,
        resolution,
        simStartTime,
        activationAdapter,
      )

      uninitialized(agentValues, buffer, simonaConfig)
    }
  }

  private def uninitialized(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Request],
      simonaConfig: SimonaConfig,
  ): Behavior[Request] =
    Behaviors.receiveMessagePartial {
      case CreateGridAgent(gridAgentInitData, unlockKey) =>
        constantData.environmentRefs.scheduler ! ScheduleActivation(
          constantData.activationAdapter,
          INIT_SIM_TICK,
          Some(unlockKey),
        )
        initializing(gridAgentInitData, simonaConfig)
    }

  private def initializing(
      gridAgentInitData: GridAgentInitData,
      simonaConfig: SimonaConfig,
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Request],
  ): Behavior[Request] = Behaviors.receivePartial {
    case (ctx, WrappedActivation(Activation(INIT_SIM_TICK))) =>
      // fail fast sanity checks
      failFast(gridAgentInitData, SimonaActorNaming.actorName(ctx.self))

      ctx.log.debug(
        s"Inferior Subnets: {}; Inferior Subnet Nodes: {}",
        gridAgentInitData.inferiorGridIds,
        gridAgentInitData.inferiorGridNodeUuids,
      )

      ctx.log.debug(
        s"Superior Subnets: {}; Superior Subnet Nodes: {}",
        gridAgentInitData.superiorGridIds,
        gridAgentInitData.superiorGridNodeUuids,
      )

      ctx.log.debug("Received InitializeTrigger.")

      val cfg = constantData.simonaConfig.simona

      // build the assets concurrently
      val subGridContainer = gridAgentInitData.subGridContainer
      val refSystem = gridAgentInitData.refSystem
      val thermalGridsByBusId = gridAgentInitData.thermalIslandGrids.map {
        thermalGrid => thermalGrid.bus().getUuid -> thermalGrid
      }.toMap
      ctx.log.debug(s"Thermal island grids: ${thermalGridsByBusId.size}")

      // get the [[GridModel]]
      val gridModel = GridModel(
        subGridContainer,
        refSystem,
        gridAgentInitData.voltageLimits,
        TimeUtil.withDefaults.toZonedDateTime(
          cfg.time.startDateTime
        ),
        TimeUtil.withDefaults.toZonedDateTime(
          cfg.time.endDateTime
        ),
        simonaConfig,
      )

      val gridAgentController =
        new GridAgentController(
          ctx,
          constantData.environmentRefs,
          constantData.simStartTime,
          TimeUtil.withDefaults
            .toZonedDateTime(cfg.time.endDateTime),
          cfg.runtime.participant,
          cfg.output.participant,
          constantData.resolution,
          constantData.listener,
          ctx.log,
        )

      /* Reassure, that there are also calculation models for the given uuids */
      val nodeToAssetAgentsMap: Map[UUID, Set[ActorRef[ParticipantMessage]]] =
        gridAgentController
          .buildSystemParticipants(subGridContainer, thermalGridsByBusId)
          .map { case (uuid: UUID, actorSet) =>
            val nodeUuid = gridModel.gridComponents.nodes
              .find(_.uuid == uuid)
              .getOrElse(
                throw new RuntimeException(
                  s"Unable to find node with uuid $uuid"
                )
              )
              .uuid
            nodeUuid -> actorSet
          }

      // create the GridAgentBaseData
      val gridAgentBaseData = GridAgentBaseData(
        gridModel,
        gridAgentInitData.subGridGateToActorRef,
        nodeToAssetAgentsMap,
        gridAgentInitData.superiorGridNodeUuids,
        gridAgentInitData.inferiorGridGates,
        PowerFlowParams(
          cfg.powerflow.maxSweepPowerDeviation,
          cfg.powerflow.newtonraphson.epsilon.toVector.sorted,
          cfg.powerflow.newtonraphson.iterations,
          cfg.powerflow.sweepTimeout,
          cfg.powerflow.stopOnFailure,
        ),
        CongestionManagementParams(
          cfg.congestionManagement.enable,
          cfg.congestionManagement.enableTransformerTapping,
          cfg.congestionManagement.enableTopologyChanges,
          cfg.congestionManagement.enableUsingFlexOptions,
          cfg.congestionManagement.maxOptimizationIterations,
          cfg.congestionManagement.timeout,
        ),
        SimonaActorNaming.actorName(ctx.self),
      )

      ctx.log.debug("Je suis initialized")

      constantData.environmentRefs.scheduler ! Completion(
        constantData.activationAdapter,
        Some(constantData.resolution),
      )

      idle(gridAgentBaseData)
  }

  /** Method that defines the idle [[Behavior]] of the agent.
    *
    * @param gridAgentBaseData
    *   state data of the actor
    * @param constantData
    *   immutable [[GridAgent]] values
    * @param buffer
    *   for [[GridAgent.Request]]s
    * @return
    *   a [[Behavior]]
    */
  private[grid] def idle(
      gridAgentBaseData: GridAgentBaseData
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Request],
  ): Behavior[Request] = Behaviors.receivePartial {
    case (_, WrappedActivation(activation: Activation)) =>
      constantData.environmentRefs.scheduler ! Completion(
        constantData.activationAdapter,
        Some(activation.tick),
      )
      buffer.unstashAll(simulateGrid(gridAgentBaseData, activation.tick))

    case (_, msg: Request) =>
      // needs to be set here to handle if the messages arrive too early
      // before a transition to GridAgentBehaviour took place
      buffer.stash(msg)
      Behaviors.same
  }

  private[grid] def gotoIdle(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      results: Option[PowerFlowResultEvent],
      ctx: ActorContext[Request],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[Request] = {

    // notify listener about the results
    results.foreach(constantData.notifyListeners)

    // do my cleanup stuff
    ctx.log.debug("Doing my cleanup stuff")

    // / clean copy of the gridAgentBaseData
    val cleanedGridAgentBaseData = GridAgentBaseData.clean(
      gridAgentBaseData,
      gridAgentBaseData.superiorGridNodeUuids,
      gridAgentBaseData.inferiorGridGates,
    )

    // / inform scheduler that we are done with the whole simulation and request new trigger for next time step
    constantData.environmentRefs.scheduler ! Completion(
      constantData.activationAdapter,
      Some(currentTick + constantData.resolution),
    )

    // return to Idle
    buffer.unstashAll(idle(cleanedGridAgentBaseData))
  }

  /** This method uses [[ActorContext.pipeToSelf()]] to send a future message to
    * itself. If the future is a [[Success]] the message is send, else a
    * [[WrappedFailure]] with the thrown error is send.
    *
    * @param future
    *   future message that should be send to the agent after it was processed
    * @param ctx
    *   [[ActorContext]] of the receiving actor
    */
  private[grid] def pipeToSelf(
      future: Future[GridAgent.Request],
      ctx: ActorContext[GridAgent.Request],
  ): Unit = {
    ctx.pipeToSelf[GridAgent.Request](future) {
      case Success(value)     => value
      case Failure(exception) => WrappedFailure(exception)
    }
  }

  private def failFast(
      gridAgentInitData: GridAgentInitData,
      actorName: String,
  ): Unit = {
    if (
      gridAgentInitData.superiorGridGates.isEmpty && gridAgentInitData.inferiorGridGates.isEmpty
    )
      throw new GridAgentInitializationException(
        s"$actorName has neither superior nor inferior grids! This can either " +
          s"be cause by wrong subnetGate information or invalid parametrization of the simulation!"
      )
  }
}
