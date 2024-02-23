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
  GridAgentInitData,
  GridAgentValues,
}
import edu.ie3.simona.agent.grid.GridAgentMessage._
import edu.ie3.simona.agent.participant.ParticipantAgent.ParticipantMessage
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.model.grid.GridModel
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.typed.scaladsl.adapter.{
  ClassicActorRefOps,
  TypedActorContextOps,
}
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => ClassicRef}

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.language.postfixOps

object GridAgent extends DBFSAlgorithm {

  def apply(
      environmentRefs: EnvironmentRefs,
      simonaConfig: SimonaConfig,
      listener: Iterable[ClassicRef],
  ): Behavior[GridAgentMessage] = Behaviors.withStash(100) { buffer =>
    Behaviors.setup[GridAgentMessage] { context =>
      context.messageAdapter(msg => WrappedPowerMessage(msg))
      val activationAdapter: ActorRef[Activation] =
        context.messageAdapter[Activation](msg => WrappedActivation(msg))

      // val initialization
      val resolution: Long = simonaConfig.simona.powerflow.resolution.get(
        ChronoUnit.SECONDS
      ) // this determines the agents regular time bin it wants to be triggered e.g one hour

      val simStartTime: ZonedDateTime = TimeUtil.withDefaults
        .toZonedDateTime(simonaConfig.simona.time.startDateTime)

      val agentValues = GridAgentValues(
        environmentRefs,
        simonaConfig,
        listener,
        resolution,
        simStartTime,
        activationAdapter,
      )

      uninitialized(agentValues, buffer)
    }
  }

  private def uninitialized(implicit
      values: GridAgentValues,
      buffer: StashBuffer[GridAgentMessage],
  ): Behavior[GridAgentMessage] =
    Behaviors.receiveMessage[GridAgentMessage] {
      case CreateGridAgent(gridAgentInitData, unlockKey) =>
        values.environmentRefs.scheduler ! ScheduleActivation(
          values.activationAdapter,
          INIT_SIM_TICK,
          Some(unlockKey),
        )

        initializing(gridAgentInitData)

      case GridAgentMessage.StopGridAgent =>
        Behaviors.stopped

      case _ =>
        Behaviors.unhandled
    }

  private def initializing(
      gridAgentInitData: GridAgentInitData
  )(implicit
      values: GridAgentValues,
      buffer: StashBuffer[GridAgentMessage],
  ): Behavior[GridAgentMessage] = Behaviors.receive[GridAgentMessage] {
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
        TimeUtil.withDefaults.toZonedDateTime(
          values.simonaConfig.simona.time.startDateTime
        ),
        TimeUtil.withDefaults.toZonedDateTime(
          values.simonaConfig.simona.time.endDateTime
        ),
      )

      val gridAgentController =
        new GridAgentController(
          ctx.toClassic,
          values.environmentRefs,
          values.simStartTime,
          TimeUtil.withDefaults
            .toZonedDateTime(values.simonaConfig.simona.time.endDateTime),
          values.simonaConfig.simona.runtime.participant,
          values.simonaConfig.simona.output.participant,
          values.resolution,
          values.listener.map(_.toTyped[ResultEvent]),
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
          values.simonaConfig.simona.powerflow.maxSweepPowerDeviation,
          values.simonaConfig.simona.powerflow.newtonraphson.epsilon.toVector.sorted,
          values.simonaConfig.simona.powerflow.newtonraphson.iterations,
          values.simonaConfig.simona.powerflow.sweepTimeout,
          values.simonaConfig.simona.powerflow.stopOnFailure,
        ),
        SimonaActorNaming.actorName(ctx.self),
      )

      ctx.log.debug("Je suis initialized")

      values.environmentRefs.scheduler ! Completion(
        values.activationAdapter,
        Some(values.resolution),
      )

      idle(gridAgentBaseData)
    case (_, StopGridAgent) =>
      Behaviors.stopped

    case (_, _) =>
      Behaviors.unhandled
  }

  /** Method that defines the idle [[Behavior]] of the agent.
    *
    * @param gridAgentBaseData
    *   state data of the actor
    * @param values
    *   immutable [[GridAgent]] values
    * @param buffer
    *   for [[GridAgentMessage]]s
    * @return
    *   a [[Behavior]]
    */
  private[grid] def idle(
      gridAgentBaseData: GridAgentBaseData
  )(implicit
      values: GridAgentValues,
      buffer: StashBuffer[GridAgentMessage],
  ): Behavior[GridAgentMessage] = Behaviors.receive[GridAgentMessage] {
    case (_, pm: WrappedPowerMessage) =>
      // needs to be set here to handle if the messages arrive too early
      // before a transition to GridAgentBehaviour took place
      buffer.stash(pm)
      Behaviors.same

    case (_, WrappedActivation(activation: Activation)) =>
      values.environmentRefs.scheduler ! Completion(
        values.activationAdapter,
        Some(activation.tick),
      )
      buffer.unstashAll(simulateGrid(gridAgentBaseData, activation.tick))

    case _ =>
      Behaviors.unhandled
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
