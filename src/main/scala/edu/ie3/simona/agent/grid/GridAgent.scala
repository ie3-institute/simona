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
}
import edu.ie3.simona.agent.grid.GridAgentMessage._
import edu.ie3.simona.agent.participant.ParticipantAgent.ParticipantMessage
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.notifier.Notifier
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.model.grid.GridModel
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, StopMessage}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.typed.scaladsl.adapter.{
  ClassicActorRefOps,
  TypedActorContextOps,
}
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => classicRef}

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.language.postfixOps

object GridAgent {
  def apply(
      environmentRefs: EnvironmentRefs,
      simonaConfig: SimonaConfig,
      listener: Iterable[classicRef],
  ): Behavior[GridAgentMessage] = Behaviors.withStash(100) { buffer =>
    Behaviors.setup[GridAgentMessage] { context =>
      context.messageAdapter(values => ValuesAdapter(values))
      context.messageAdapter(msg => PMAdapter(msg))
      context.messageAdapter(msg => VMAdapter(msg))
      val activationAdapter: ActorRef[Activation] =
        context.messageAdapter[Activation](msg => ActivationAdapter(msg))

      // val initialization
      val resolution: Long = simonaConfig.simona.powerflow.resolution.get(
        ChronoUnit.SECONDS
      ) // this determines the agents regular time bin it wants to be triggered e.g one hour

      val simStartTime: ZonedDateTime = TimeUtil.withDefaults
        .toZonedDateTime(simonaConfig.simona.time.startDateTime)

      val gridAgentController =
        new GridAgentController(
          context.toClassic,
          environmentRefs,
          simStartTime,
          TimeUtil.withDefaults
            .toZonedDateTime(simonaConfig.simona.time.endDateTime),
          simonaConfig.simona.runtime.participant,
          simonaConfig.simona.output.participant,
          resolution,
          listener.map(_.toTyped[ResultEvent]),
          context.log,
        )

      val agent = GridAgent(
        environmentRefs,
        simonaConfig,
        listener,
        resolution,
        simStartTime,
        gridAgentController,
        buffer,
        activationAdapter,
        SimonaActorNaming.actorName(context.self),
      )

      agent.uninitialized
    }
  }
}

final case class GridAgent(
    environmentRefs: EnvironmentRefs,
    simonaConfig: SimonaConfig,
    override val listener: Iterable[classicRef],
    resolution: Long,
    simStartTime: ZonedDateTime,
    gridAgentController: GridAgentController,
    buffer: StashBuffer[GridAgentMessage],
    activationAdapter: ActorRef[Activation],
    actorName: String,
) extends DBFSAlgorithm
    with Notifier {

  protected def uninitialized: Behavior[GridAgentMessage] =
    Behaviors.receiveMessage[GridAgentMessage] {
      case CreateGridAgent(gridAgentInitData, unlockKey) =>
        environmentRefs.scheduler ! ScheduleActivation(
          activationAdapter,
          INIT_SIM_TICK,
          Some(unlockKey),
        )

        initializing(gridAgentInitData)

      case GridAgentMessage.StopGridAgent =>
        Behaviors.stopped

      case _ =>
        Behaviors.unhandled
    }

  protected def initializing(
      gridAgentInitData: GridAgentInitData
  ): Behavior[GridAgentMessage] = Behaviors.receive[GridAgentMessage] {
    case (ctx, ActivationAdapter(Activation(INIT_SIM_TICK))) =>
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
          simonaConfig.simona.time.startDateTime
        ),
        TimeUtil.withDefaults.toZonedDateTime(
          simonaConfig.simona.time.endDateTime
        ),
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
          simonaConfig.simona.powerflow.maxSweepPowerDeviation,
          simonaConfig.simona.powerflow.newtonraphson.epsilon.toVector.sorted,
          simonaConfig.simona.powerflow.newtonraphson.iterations,
          simonaConfig.simona.powerflow.sweepTimeout,
          simonaConfig.simona.powerflow.stopOnFailure,
        ),
        ctx.log,
        ctx.self.toString,
      )

      ctx.log.debug("Je suis initialized")

      environmentRefs.scheduler ! Completion(
        activationAdapter,
        Some(resolution),
      )

      idle(gridAgentBaseData)

    case (_, StopGridAgent) =>
      Behaviors.stopped

    case (_, _) =>
      Behaviors.unhandled
  }

  protected def idle(
      gridAgentBaseData: GridAgentBaseData
  ): Behavior[GridAgentMessage] = Behaviors.receive[GridAgentMessage] {
    case (_, pm: PMAdapter) =>
      // needs to be set here to handle if the messages arrive too early
      // before a transition to GridAgentBehaviour took place
      buffer.stash(pm)
      Behaviors.same

    case (_, ActivationAdapter(activation: Activation)) =>
      environmentRefs.scheduler ! Completion(
        activationAdapter,
        Some(activation.tick),
      )
      buffer.unstashAll(simulateGrid(gridAgentBaseData, activation.tick))

    case (ctx, ResultMessageAdapter(StopMessage(_))) =>
      // shutdown children
      gridAgentBaseData.gridEnv.nodeToAssetAgents.foreach { case (_, actors) =>
        actors.foreach(a => ctx.stop(a))
      }

      // we are done
      Behaviors.stopped

    case (_, StopGridAgent) =>
      Behaviors.stopped

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
