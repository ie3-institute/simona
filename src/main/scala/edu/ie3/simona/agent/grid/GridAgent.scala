/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.{Props, Stash}
import com.google.common.util.concurrent.ThreadFactoryBuilder
import edu.ie3.simona.agent.grid.GridAgentData.{
  GridAgentBaseData,
  GridAgentInitData,
  GridAgentUninitializedData
}
import edu.ie3.simona.agent.state.AgentState.{Finish, Idle, Uninitialized}
import edu.ie3.simona.agent.state.GridAgentState.SimulateGrid
import edu.ie3.simona.agent.{EnvironmentRefs, SimonaAgent}
import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.akka.SimonaActorRef.selfSharded
import edu.ie3.simona.akka.SimonaActorRefUtils.RichActorContext
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.model.grid.GridModel
import edu.ie3.simona.ontology.messages.PowerMessage.RequestGridPowerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.VoltageMessage.RequestSlackVoltageMessage
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeGridAgentTrigger,
  StartGridSimulationTrigger
}
import edu.ie3.util.TimeUtil

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps

object GridAgent {
  def props(
      environmentRefs: EnvironmentRefs,
      simonaConfig: SimonaConfig,
      listener: Iterable[SimonaActorRef]
  ): Props =
    Props(
      new GridAgent(
        environmentRefs,
        simonaConfig,
        listener
      )
    )
}

class GridAgent(
    val environmentRefs: EnvironmentRefs,
    simonaConfig: SimonaConfig,
    val listener: Iterable[SimonaActorRef]
) extends SimonaAgent[GridAgentData]
    with DBFSAlgorithm
    with Stash {

  // setup future & concurrency stuff
  // details see
  // https://stackoverflow.com/questions/48963300/which-executioncontext-to-choose-for-future-in-akka-applications
  private val numOfAvailableProcessors =
    Runtime.getRuntime.availableProcessors()
  protected val numOfThreads: Int =
    if (numOfAvailableProcessors <= 2) 1
    else numOfAvailableProcessors - 2
  protected val execSvc: ExecutorService = Executors.newFixedThreadPool(
    numOfThreads,
    new ThreadFactoryBuilder()
      .setDaemon(true)
      .setNameFormat(actorName)
      .build()
  )
  protected implicit val executionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(execSvc)

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
      failFast(gridAgentInitData)

      log.info(
        s"Found {} available processors. Will use {}.",
        numOfThreads + 2,
        numOfThreads
      )

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
      val nodeToAssetAgentMapFuture = Future {
        gridAgentController.buildSystemParticipants(subGridContainer)
      }

      // get the [[GridModel]]
      val gridModel = GridModel(
        subGridContainer,
        refSystem,
        TimeUtil.withDefaults
          .toZonedDateTime(simonaConfig.simona.time.startDateTime),
        TimeUtil.withDefaults.toZonedDateTime(
          simonaConfig.simona.time.endDateTime
        )
      )

      // we have to wait until the assets are ready
      // and add the inferiorGridNodeIds -> ActorRef to the nodeToAgentMap
      // this is the map with all agents that contribute to p/q value provision
      val nodeInputToParticipantRefMap: Map[UUID, Set[SimonaActorRef]] =
        Await.result(nodeToAssetAgentMapFuture, 60 seconds)

      /* Reassure, that there are also calculation models for the given uuids */
      val nodeToAssetAgentsMap: Map[UUID, Set[SimonaActorRef]] =
        nodeInputToParticipantRefMap.map { case (uuid: UUID, actorSet) =>
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
        selfSharded(gridModel.subnetNo),
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(resolution),
              selfSharded(gridModel.subnetNo)
            )
          )
        )
      )
  }

  when(Idle) {

    // needs to be set here to handle if the messages arrive too early
    // before a transition to GridAgentBehaviour took place
    case Event(RequestGridPowerMessage(_, _), _: GridAgentBaseData) |
        Event(RequestSlackVoltageMessage(_, _), _: GridAgentBaseData) =>
      stash()
      stay()

    case Event(
          TriggerWithIdMessage(ActivityStartTrigger(currentTick), triggerId, _),
          gridAgentBaseData: GridAgentBaseData
        ) =>
      log.debug("received activity start trigger {}", triggerId)

      // unstashing RequestGridPowerMessages and RequestSlackVoltageMessages, which can be handled now
      unstashAll()

      val selfRef = selfSharded(gridAgentBaseData.gridEnv.gridModel.subnetNo)

      goto(SimulateGrid) using gridAgentBaseData replying CompletionMessage(
        triggerId,
        selfRef,
        Some(
          Vector(
            ScheduleTriggerMessage(
              StartGridSimulationTrigger(currentTick),
              selfRef
            )
          )
        )
      )

    case Event(Finish, data: GridAgentBaseData) =>
      // shutdown children
      data.gridEnv.nodeToAssetAgents.foreach { case (_, actors) =>
        actors.foreach(ref => context.stop(ref))
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
