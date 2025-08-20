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
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  CreateGridAgent,
  WrappedFailure,
}
import edu.ie3.simona.agent.grid.congestion.{
  CongestionManagementParams,
  DCMAlgorithm,
}
import edu.ie3.simona.agent.participant.ParticipantAgent
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
import edu.ie3.simona.util.TickUtil.{RichZonedDateTime, TickLong}
import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior, Scheduler}
import org.apache.pekko.util.Timeout
import org.slf4j.Logger

import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, postfixOps}
import scala.util.{Failure, Success}

object GridAgent extends DBFSAlgorithm with DCMAlgorithm {

  /** All messages, that can be received by a [[GridAgent]]. */
  final type Message = InternalRequest | InternalReply | Activation

  /** Necessary because we want to extend messages in other classes, but we do
    * want to keep the messages only available inside this package.
    */
  private[grid] trait InternalRequest
  private[grid] trait InternalReply
  private[grid] trait InternalReplyWithSender[T] extends InternalReply {
    def sender: ActorRef[GridAgent.Message]
    def value: T
  }

  def apply(
      environmentRefs: EnvironmentRefs,
      simonaConfig: SimonaConfig,
      listener: Iterable[ActorRef[ResultEvent]],
  ): Behavior[Message] = Behaviors.withStash(100) { buffer =>
    val cfg = simonaConfig.simona

    given simStartTime: ZonedDateTime = cfg.time.simStartTime
    val simEndTime = cfg.time.simEndTime

    // this determines the agents regular time bin it wants to be triggered e.g. one hour
    // if no resolution is given, we use the maximal long value as a placeholder, as no future activation should take place
    val resolution =
      cfg.powerflow.map(_.resolution.toSeconds).getOrElse(Long.MaxValue)

    val agentValues = GridAgentConstantData(
      environmentRefs,
      simonaConfig,
      listener,
      resolution,
      simStartTime,
      simEndTime,
    )

    uninitialized(using agentValues, buffer, simonaConfig)
  }

  private def uninitialized(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
      simonaConfig: SimonaConfig,
  ): Behavior[Message] = Behaviors.receivePartial {
    case (
          ctx,
          CreateGridAgent(gridAgentInitData, unlockKey, onlyOneSubGrid),
        ) =>
      // fail fast sanity checks
      failFast(
        gridAgentInitData,
        SimonaActorNaming.actorName(ctx.self),
        onlyOneSubGrid,
      )

      ctx.log.debug(
        s"Inferior sub grids: {}; Inferior sub grid nodes: {}",
        gridAgentInitData.inferiorGridIds,
        gridAgentInitData.inferiorGridNodeUuids,
      )

      ctx.log.debug(
        s"Superior sub grids: {}; Superior sub grid nodes: {}",
        gridAgentInitData.superiorGridIds,
        gridAgentInitData.superiorGridNodeUuids,
      )

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
        constantData.simStartTime,
        constantData.simEndTime,
        simonaConfig,
      )

      /* Reassure, that there are also calculation models for the given uuids */
      val nodeToAssetAgentsMap
          : Map[UUID, Set[ActorRef[ParticipantAgent.Request]]] =
        GridAgentBuilder
          .buildSystemParticipants(subGridContainer, thermalGridsByBusId)(using
            constantData,
            ctx,
            ctx.log,
          )
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

      // create powerflow parameters
      val pfParams = cfg.powerflow match {
        case Some(value) =>
          PowerFlowParams(
            value.maxSweepPowerDeviation,
            value.newtonraphson.epsilon.toVector.sorted,
            value.newtonraphson.iterations,
            value.sweepTimeout,
            value.stopOnFailure,
          )
        case None =>
          // empty powerflow params, since we need an object
          PowerFlowParams(0, Vector.empty, 0, 0.seconds, false)
      }

      // create the GridAgentBaseData
      val gridAgentBaseData = GridAgentBaseData(
        gridModel,
        gridAgentInitData.subGridGateToActorRef,
        nodeToAssetAgentsMap,
        gridAgentInitData.superiorGridNodeUuids,
        gridAgentInitData.inferiorGridGates,
        gridAgentInitData.superiorGridGates,
        pfParams,
        CongestionManagementParams(
          cfg.congestionManagement.enableDetection,
          cfg.congestionManagement.timeout,
        ),
        SimonaActorNaming.actorName(ctx.self),
      )

      val resolution = constantData.resolution

      if resolution == Long.MaxValue then {
        unlockKey.unlock()
      } else {
        constantData.environmentRefs.scheduler ! ScheduleActivation(
          ctx.self,
          resolution,
          Some(unlockKey),
        )
      }

      idle(gridAgentBaseData)
  }

  /** Method that defines the idle [[Behavior]] of the agent.
    *
    * @param gridAgentBaseData
    *   state data of the actor
    * @param constantData
    *   immutable [[GridAgent]] values
    * @param buffer
    *   for [[GridAgent.Message]]s
    * @return
    *   a [[Behavior]]
    */
  private[grid] def idle(
      gridAgentBaseData: GridAgentBaseData
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
  ): Behavior[Message] = Behaviors.receivePartial {
    case (ctx, activation: Activation) =>
      constantData.environmentRefs.scheduler ! Completion(
        ctx.self,
        Some(activation.tick),
      )
      buffer.unstashAll(simulateGrid(gridAgentBaseData, activation.tick))

    case (_, msg: Message) =>
      // needs to be set here to handle if the messages arrive too early
      // before a transition to GridAgentBehaviour took place
      buffer.stash(msg)
      Behaviors.same
  }

  /** Behavior of the [[GridAgent]] after the powerflow is finished.
    *
    * @param gridAgentBaseData
    *   state data of the actor
    * @param currentTick
    *   the current tick in the simulation
    * @param ctx
    *   actor context
    * @param constantData
    *   immutable [[GridAgent]] values
    * @param buffer
    *   for [[GridAgent.Message]]s
    * @return
    *   a [[Behavior]]
    */
  private[grid] def afterPowerFlow(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      nextTick: Long,
      ctx: ActorContext[Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
  ): Behavior[Message] = {
    ctx.log.debug(
      "Calculate results ..."
    )
    val results: Option[PowerFlowResultEvent] =
      gridAgentBaseData.sweepValueStores.lastOption.map {
        case (_, valueStore) =>
          createResultModels(
            gridAgentBaseData.gridEnv.gridModel,
            valueStore,
          )(using
            currentTick.toDateTime(using constantData.simStartTime),
            ctx.log,
          )
      }

    // check if congestion management is enabled
    if gridAgentBaseData.congestionManagementParams.detectionEnabled then {
      startCongestionManagement(gridAgentBaseData, currentTick, results, ctx)
    } else {
      // clean up agent and go back to idle
      gotoIdle(gridAgentBaseData, nextTick, results, ctx)
    }
  }

  /** Method that will clean up the [[GridAgentBaseData]] and go to the
    * [[idle()]] state.
    *
    * @param gridAgentBaseData
    *   state data of the actor
    * @param nextTick
    *   the next tick in the simulation
    * @param results
    *   option for the last power flow, that should be written
    * @param ctx
    *   actor context
    * @param constantData
    *   immutable [[GridAgent]] values
    * @param buffer
    *   for [[GridAgent.Message]]s
    * @return
    *   a [[Behavior]]
    */
  private[grid] def gotoIdle(
      gridAgentBaseData: GridAgentBaseData,
      nextTick: Long,
      results: Option[PowerFlowResultEvent],
      ctx: ActorContext[Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[Message] = {

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
      ctx.self,
      Some(nextTick),
    )

    // return to Idle
    buffer.unstashAll(idle(cleanedGridAgentBaseData))
  }

  /** Method to ask all inferior grids.
    *
    * @param inferiorGridRefs
    *   a map containing a mapping from [[ActorRef]]s to corresponding [[UUID]]s
    *   of inferior nodes
    * @param askMsgBuilder
    *   function to build the asked message
    * @param resMsgBuilder
    *   function to build the returned message
    * @param ctx
    *   actor context to use
    * @tparam T
    *   type of data
    */
  private[grid] def askInferior[T](
      inferiorGridRefs: Map[ActorRef[GridAgent.Message], Seq[UUID]],
      askMsgBuilder: ActorRef[GridAgent.Message] => Message,
      resMsgBuilder: Vector[(ActorRef[GridAgent.Message], T)] => InternalReply,
      ctx: ActorContext[GridAgent.Message],
  )(using timeout: FiniteDuration): Unit = {
    if inferiorGridRefs.nonEmpty then {
      // creating implicit vals
      given ec: ExecutionContext = ctx.executionContext
      given scheduler: Scheduler = ctx.system.scheduler
      given askTimeout: Timeout = Timeout(timeout)

      // asking process
      val future = Future
        .sequence(
          inferiorGridRefs.map { case (inferiorGridAgentRef, _) =>
            inferiorGridAgentRef
              .ask(askMsgBuilder)
              .map { case response: InternalReplyWithSender[T] =>
                (response.sender, response.value)
              }
          }.toVector
        )
        .map(resMsgBuilder)
      pipeToSelf(future, ctx)
    }
  }

  /** This method uses [[ActorContext.pipeToSelf()]] to send a future message to
    * itself. If the future is a [[Success]] the message is sent, else a
    * [[WrappedFailure]] with the thrown error is sent.
    *
    * @param future
    *   future message that should be sent to the agent after it was processed
    * @param ctx
    *   [[ActorContext]] of the receiving actor
    */
  private[grid] def pipeToSelf(
      future: Future[GridAgent.Message],
      ctx: ActorContext[GridAgent.Message],
  ): Unit = {
    ctx.pipeToSelf[GridAgent.Message](future) {
      case Success(value)     => value
      case Failure(exception) => WrappedFailure(exception)
    }
  }

  private def failFast(
      gridAgentInitData: GridAgentInitData,
      actorName: String,
      onlyOneSubGrid: Boolean,
  ): Unit = {
    if gridAgentInitData.superiorGridGates.isEmpty && gridAgentInitData.inferiorGridGates.isEmpty && !onlyOneSubGrid
    then
      throw new GridAgentInitializationException(
        s"$actorName has neither superior nor inferior grids! This can either " +
          s"be cause by wrong subnetGate information or invalid parametrization of the simulation!"
      )
  }

  private[grid] def unsupported(msg: Message, log: Logger)(using
      buffer: StashBuffer[GridAgent.Message]
  ): Unit = {
    log.debug(s"Received unsupported msg: $msg. Stash away!")
    buffer.stash(msg)
  }
}
