/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.GridAgent.Create
import edu.ie3.simona.agent.grid.GridAgentData.{
  GridAgentBaseData,
  GridAgentInitData,
  GridAgentUninitializedData,
}
import edu.ie3.simona.model.grid.GridModel
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.GridAgentState.{Initializing, SimulateGrid}
import edu.ie3.simona.agent.{EnvironmentRefs, SimonaAgent}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException

import edu.ie3.simona.ontology.messages.PowerMessage.RequestGridPowerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, StopMessage}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.{ActorRef, Props, Stash}
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.language.postfixOps

object GridAgent {
  def props(
      environmentRefs: EnvironmentRefs,
      simonaConfig: SimonaConfig,
      listener: Iterable[ActorRef],
  ): Props =
    Props(
      new GridAgent(
        environmentRefs,
        simonaConfig,
        listener,
      )
    )

  /** GridAgent initialization data can only be constructed once all GridAgent
    * actors are created. Thus, we need an extra initialization message.
    * @param gridAgentInitData
    *   The initialization data
    */
  final case class Create(
      gridAgentInitData: GridAgentInitData,
      unlockKey: ScheduleKey,
  )

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * execute a power flow calculation
    *
    * @param tick
    *   current tick
    */
  final case class DoPowerFlowTrigger(tick: Long, currentSweepNo: Int)

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * activate the superior grid agent to check for deviation after two sweeps
    * and see if the power flow converges
    *
    * @param tick
    *   current tick
    */
  final case class CheckPowerDifferencesTrigger(tick: Long)

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * trigger the [[edu.ie3.simona.agent.grid.GridAgent]] s to prepare
    * themselves for a new sweep
    *
    * @param tick
    *   current tick
    */
  final case class PrepareNextSweepTrigger(tick: Long)

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * indicate that a result has been found and each
    * [[edu.ie3.simona.agent.grid.GridAgent]] should do it's cleanup work
    *
    * @param tick
    *   current tick
    */
  final case class FinishGridSimulationTrigger(tick: Long)
}

class GridAgent(
    val environmentRefs: EnvironmentRefs,
    simonaConfig: SimonaConfig,
    val listener: Iterable[ActorRef],
) extends SimonaAgent[GridAgentData]
    with DBFSAlgorithm
    with Stash {

  // val initialization
  protected val resolution: Long = simonaConfig.simona.powerflow.resolution.get(
    ChronoUnit.SECONDS
  ) // this determines the agents regular time bin it wants to be triggered e.g one hour

  protected val simStartTime: ZonedDateTime = TimeUtil.withDefaults
    .toZonedDateTime(simonaConfig.simona.time.startDateTime)

  private val gridAgentController =
    new GridAgentController(
      context,
      environmentRefs,
      simStartTime,
      TimeUtil.withDefaults
        .toZonedDateTime(simonaConfig.simona.time.endDateTime),
      simonaConfig.simona.runtime.participant,
      simonaConfig.simona.output.participant,
      resolution,
      listener,
      log,
    )

  override def postStop(): Unit = {
    log.debug("{} shutdown", self)
  }

  override def preStart(): Unit = {
    log.debug("{} started!", self)
  }

  // general agent states
  // first fsm state of the agent
  startWith(Uninitialized, GridAgentUninitializedData)

  when(Uninitialized) {
    case Event(
          Create(gridAgentInitData, unlockKey),
          _,
        ) =>
      environmentRefs.scheduler ! ScheduleActivation(
        self.toTyped,
        INIT_SIM_TICK,
        Some(unlockKey),
      )

      goto(Initializing) using gridAgentInitData
  }

  when(Initializing) {
    case Event(
          Activation(INIT_SIM_TICK),
          gridAgentInitData: GridAgentInitData,
        ) =>
      // fail fast sanity checks
      failFast(gridAgentInitData)

      log.debug(
        s"Inferior Subnets: {}; Inferior Subnet Nodes: {}",
        gridAgentInitData.inferiorGridIds,
        gridAgentInitData.inferiorGridNodeUuids,
      )

      log.debug(
        s"Superior Subnets: {}; Superior Subnet Nodes: {}",
        gridAgentInitData.superiorGridIds,
        gridAgentInitData.superiorGridNodeUuids,
      )

      log.debug("Received InitializeTrigger.")

      // build the assets concurrently
      val subGridContainer = gridAgentInitData.subGridContainer
      val refSystem = gridAgentInitData.refSystem
      val thermalGridsByBusId = gridAgentInitData.thermalIslandGrids.map {
        thermalGrid => thermalGrid.bus().getUuid -> thermalGrid
      }.toMap
      log.debug(s"Thermal island grids: ${thermalGridsByBusId.size}")

      // get the [[GridModel]]
      val gridModel = GridModel(
        subGridContainer,
        refSystem,
        TimeUtil.withDefaults
          .toZonedDateTime(simonaConfig.simona.time.startDateTime),
        TimeUtil.withDefaults.toZonedDateTime(
          simonaConfig.simona.time.endDateTime
        ),
        simonaConfig,
      )

      /* Reassure, that there are also calculation models for the given uuids */
      val nodeToAssetAgentsMap: Map[UUID, Set[ActorRef]] =
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
        log,
        actorName,
      )

      log.debug("Je suis initialized")

      environmentRefs.scheduler ! Completion(
        self.toTyped,
        Some(resolution),
      )

      goto(Idle) using gridAgentBaseData
  }

  when(Idle) {

    // needs to be set here to handle if the messages arrive too early
    // before a transition to GridAgentBehaviour took place
    case Event(RequestGridPowerMessage(_, _), _: GridAgentBaseData) =>
      stash()
      stay()

    case Event(
          Activation(tick),
          gridAgentBaseData: GridAgentBaseData,
        ) =>
      unstashAll()

      environmentRefs.scheduler ! Completion(
        self.toTyped,
        Some(tick),
      )

      goto(SimulateGrid) using gridAgentBaseData

    case Event(StopMessage(_), data: GridAgentBaseData) =>
      // shutdown children
      data.gridEnv.nodeToAssetAgents.foreach { case (_, actors) =>
        actors.foreach(context.stop)
      }

      // we are done
      stop()
  }

  // everything else
  whenUnhandled(myUnhandled())

  private def failFast(gridAgentInitData: GridAgentInitData): Unit = {
    if (
      gridAgentInitData.superiorGridGates.isEmpty && gridAgentInitData.inferiorGridGates.isEmpty
    )
      throw new GridAgentInitializationException(
        s"$actorName has neither superior nor inferior grids! This can either " +
          s"be cause by wrong subnetGate information or invalid parametrization of the simulation!"
      )
  }
}
