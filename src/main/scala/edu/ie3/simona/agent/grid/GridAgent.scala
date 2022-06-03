/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.{ActorRef, Props, Stash}
import edu.ie3.simona.agent.grid.GridAgentData.{
  GridAgentBaseData,
  GridAgentInitData,
  GridAgentUninitializedData
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.GridAgentState.SimulateGrid
import edu.ie3.simona.agent.{EnvironmentRefs, SimonaAgent}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.TransformerControlGroup
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.model.grid.GridModel
import edu.ie3.simona.ontology.messages.PowerMessage.RequestGridPowerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.StopMessage
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeGridAgentTrigger,
  StartGridSimulationTrigger
}
import edu.ie3.util.TimeUtil

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.language.postfixOps

object GridAgent {
  def props(
      environmentRefs: EnvironmentRefs,
      simonaConfig: SimonaConfig,
      listener: Iterable[ActorRef]
  ): Props =
    Props(
      new GridAgent(
        environmentRefs,
        simonaConfig,
        listener
      )
    )

  private def failFast(
      simonaConfig: SimonaConfig,
      gridAgentInitData: GridAgentInitData
  ): Unit = {

    /** Check if there is InitData for superior or inferior GridGates
      */
    if (
      gridAgentInitData.superiorGridGates.isEmpty && gridAgentInitData.inferiorGridGates.isEmpty
    )
      throw new GridAgentInitializationException(
        s"${gridAgentInitData.subGridContainer.getGridName} has neither superior nor inferior grids! This can either " +
          s"be cause by wrong subnetGate information or invalid parametrization of the simulation!"
      )

    /** Check of Control Boundaries
      */
    simonaConfig.simona.control.foreach(control =>
      checkBoundariesOfControlGroup(control.transformer, gridAgentInitData)
    )
  }

  private def checkBoundariesOfControlGroup(
      transformerControlGroup: Seq[TransformerControlGroup],
      gridAgentInitData: GridAgentInitData
  ): Unit = {
    val vNom = gridAgentInitData.refSystem.nominalVoltage
    // TODO transfer lowerBoundary Voltage from RefSystem to per Unit
    //    val lowerBoundary = vNom.multiply(0.8).getValue.doubleValue()
    val lowerBoundary = 0.8
    // TODO transfer lowerBoundary Voltage from RefSystem to per Unit
    //    val upperBoundary = vNom.multiply(1.2).getValue.doubleValue()
    val upperBoundary = 1.2

    transformerControlGroup.foreach {
      case TransformerControlGroup(_, _, vMax, vMin) =>
        if (vMin < lowerBoundary)
          throw new GridAgentInitializationException(
            s"${gridAgentInitData.subGridContainer.getGridName} has a control group which control boundaries exceed the limit of +- 20% of nominal voltage! This may be caused " +
              s"by invalid parametrization of one control groups where vMin is lower than the lower boundary (0.8 of nominal Voltage)!"
          )
        if (vMax > upperBoundary)
          throw new GridAgentInitializationException(
            s"${gridAgentInitData.subGridContainer.getGridName} has a control group which control boundaries exceed the limit of +- 20% of nominal voltage! This may be caused " +
              s"by invalid parametrization of one control groups where vMax is higher than the upper boundary (1.2 of nominal Voltage)!"
          )
    }
  }
}

class GridAgent(
    val environmentRefs: EnvironmentRefs,
    simonaConfig: SimonaConfig,
    val listener: Iterable[ActorRef]
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
      log
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
          TriggerWithIdMessage(
            InitializeGridAgentTrigger(
              gridAgentInitData: GridAgentInitData
            ),
            triggerId,
            _
          ),
          _
        ) =>
      // fail fast sanity checks
      GridAgent.failFast(simonaConfig, gridAgentInitData)

      log.debug(
        s"Inferior Subnets: {}; Inferior Subnet Nodes: {}",
        gridAgentInitData.inferiorGridIds,
        gridAgentInitData.inferiorGridNodeUuids
      )

      log.debug(
        s"Superior Subnets: {}; Superior Subnet Nodes: {}",
        gridAgentInitData.superiorGridIds,
        gridAgentInitData.superiorGridNodeUuids
      )

      log.debug("Received InitializeTrigger.")

      // build the assets concurrently
      val subGridContainer = gridAgentInitData.subGridContainer
      val refSystem = gridAgentInitData.refSystem

      // get the [[GridModel]]
      val gridModel = GridModel(
        subGridContainer,
        refSystem,
        TimeUtil.withDefaults
          .toZonedDateTime(simonaConfig.simona.time.startDateTime),
        TimeUtil.withDefaults.toZonedDateTime(
          simonaConfig.simona.time.endDateTime
        ),
        simonaConfig.simona.control
      )

      /* Reassure, that there are also calculation models for the given uuids */
      val nodeToAssetAgentsMap: Map[UUID, Set[ActorRef]] =
        gridAgentController.buildSystemParticipants(subGridContainer).map {
          case (uuid: UUID, actorSet) =>
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
          simonaConfig.simona.powerflow.sweepTimeout
        ),
        log,
        actorName
      )

      log.debug("Je suis initialized")

      goto(Idle) using gridAgentBaseData replying CompletionMessage(
        triggerId,
        Some(
          Vector(ScheduleTriggerMessage(ActivityStartTrigger(resolution), self))
        )
      )
  }

  when(Idle) {

    // needs to be set here to handle if the messages arrive too early
    // before a transition to GridAgentBehaviour took place
    case Event(RequestGridPowerMessage(_, _), _: GridAgentBaseData) =>
      stash()
      stay()

    case Event(
          TriggerWithIdMessage(ActivityStartTrigger(currentTick), triggerId, _),
          gridAgentBaseData: GridAgentBaseData
        ) =>
      log.debug("received activity start trigger {}", triggerId)

      unstashAll()

      goto(SimulateGrid) using gridAgentBaseData replying CompletionMessage(
        triggerId,
        Some(
          Vector(
            ScheduleTriggerMessage(
              StartGridSimulationTrigger(currentTick),
              self
            )
          )
        )
      )

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

}
