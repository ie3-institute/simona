/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.input.container.SubGridContainer
import edu.ie3.datamodel.utils.ContainerUtils
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentMessages.*
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.{
  DoCongestionManagement,
  GotoIdle,
  NextStep,
}
import edu.ie3.simona.agent.grid.congestion.mitigations.MitigationSteps.NoMeasure
import edu.ie3.simona.agent.grid.congestion.{
  CongestionManagementParams,
  Congestions,
}
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  AwaitingData,
  GridAgentConstantData,
  GridAgentInitData,
  GridAgentRef,
}
import edu.ie3.simona.agent.grid.powerflow.PowerFlowParams
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.config.GridConfigParser.{
  ConfigRefSystems,
  ConfigVoltageLimits,
}
import edu.ie3.simona.config.{GridConfigParser, SimonaConfig}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.model.grid.{GridModel, RefSystem, VoltageLimits}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.simona.util.SimonaConstants.FIRST_TICK_IN_SIMULATION
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.collection.immutable.RichMultiMap.MultiMap
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.slf4j.Logger
import squants.electro.Kilovolts

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters.SetHasAsScala

/** Agent that sits between the scheduler and the grid agents and coordinates
  * all grid agents.
  */
object GridAgentCoordinator {

  type Message = Activation | Request

  /** A request that is sent to the [[GridAgentCoordinator]].
    */
  sealed trait Request

  /** Message send to the coordinator to register assets.
    * @param nodeToAssets
    *   A map: node uuid to set of participant references.
    */
  final case class RegisterAssets(
      nodeToAssets: MultiMap[UUID, ActorRef[ParticipantAgent.Request]]
  ) extends Request

  /** Message to inform the coordinator that a subgrid has been initialized.
    * @param gridRef
    *   Reference of the initialized grid.
    */
  final case class FinishedInitialization(gridRef: GridAgentRef) extends Request

  /** Message to provide power flow results to the coordinator.
    * @param gridAgent
    *   Reference of the grid.
    * @param results
    *   An option for the power flow results. In case of a failed power flow the
    *   results will be empty.
    */
  final case class PowerFlowResults(
      gridAgent: GridAgentRef,
      results: Option[PowerFlowResultEvent],
  ) extends Request

  /** Message to provide congestions information to the coordinator.
    * @param gridAgent
    *   Reference of the grid.
    * @param congestions
    *   Information regarding congestions in the subgrid.
    */
  final case class CongestionResult(
      gridAgent: GridAgentRef,
      congestions: Congestions,
  ) extends Request

  final case class StepFinished(sender: GridAgentRef) extends Request

  /** @param scheduler
    *   Reference of the scheduler.
    * @param congestionManagementParams
    *   Parameters for the congestion management.
    * @param resultProxy
    *   Reference of the result service proxy.
    * @param simStartTime
    *   The start time of the simulation.
    * @param currentTick
    *   The current tick in the simulation.
    * @param resolution
    *   That is used for the power flow. If no power flow should be carried out,
    *   this value is set to [[None]].
    * @param gridAgentsRef
    *   A set of all grid agent references.
    * @param superiorGrids
    *   A set of references of all slack grid agents.
    * @param nodeToSubgrid
    *   A map: node uuid to subgrid number.
    * @param hasRunCongestionManagement
    *   A boolean flag to determine if the congestion management was run.
    */
  final case class StateData(
      scheduler: ActorRef[SchedulerMessage],
      congestionManagementParams: CongestionManagementParams,
      resultProxy: ActorRef[ResultEvent],
      simStartTime: ZonedDateTime,
      currentTick: Long = FIRST_TICK_IN_SIMULATION,
      resolution: Option[Long] = None,
      gridAgentsRef: Set[GridAgentRef] = Set.empty,
      superiorGrids: Set[GridAgentRef] = Set.empty,
      nodeToSubgrid: Map[UUID, Int] = Map.empty,
      hasRunCongestionManagement: Boolean = false,
  ) {

    def runCongestionManagement: Boolean =
      congestionManagementParams.detectionEnabled && !hasRunCongestionManagement

    /** Returns an option for the next tick.
      */
    def maybeNextTick: Option[Long] = resolution.map(_ + currentTick)

    /** Method to create [[AwaitingData]] that waits for all references.
      * @tparam T
      *   Type of the awaited data.
      * @return
      *   A [[ReceiveDataMap]].
      */
    def createAwaitingData[T]: AwaitingData[T] = ReceiveDataMap(gridAgentsRef)

    /** Method to inform all grid agents.
      *
      * @param msg
      *   Message to send to the agents.
      */
    def informGridAgents(msg: GridAgent.Message): Unit =
      gridAgentsRef.foreach(_ ! msg)

    /** Method to inform only superior grid agents.
      *
      * @param msg
      *   Message to send to the agents.
      */
    def informSuperiorGridAgents(msg: GridAgent.Message): Unit =
      superiorGrids.foreach(_ ! msg)

    /** Method to inform all grid agents.
      * @param msgBuilder
      *   Builder that uses a reference to build the message that should be
      *   sent.
      */
    def informGridAgents(msgBuilder: GridAgentRef => GridAgent.Message): Unit =
      gridAgentsRef.foreach(ref => ref ! msgBuilder(ref))
  }

  def apply(
      config: SimonaConfig,
      subgrids: Seq[SubGridContainer],
  )(using environmentRefs: EnvironmentRefs): Behavior[Message] =
    Behaviors.setup { ctx =>
      val scheduler = environmentRefs.scheduler

      val congestionManagementParams =
        CongestionManagementParams(config.congestionManagement)

      val stateData = StateData(
        scheduler,
        congestionManagementParams,
        environmentRefs.resultProxy,
        config.time.simStartTime,
      )

      config.powerflow match {
        case Some(pfConfig) =>
          // we need to perform powerflow calculations
          // -> creating grid agents
          val resolution = pfConfig.resolution.toSeconds

          val (subgridToRef, nodeToSubgrid, superiorGrids) = createGridAgents(
            subgrids,
            resolution,
            PowerFlowParams(pfConfig),
            config,
          )(using ctx, environmentRefs)

          initializing(
            stateData.copy(
              resolution = Some(resolution),
              gridAgentsRef = subgridToRef.values.toSet,
              superiorGrids = superiorGrids,
              nodeToSubgrid = nodeToSubgrid,
            ),
            subgridToRef.values.toSet,
            subgridToRef,
          )

        case None =>
          // no power flow configured
          // -> creating no agents
          initializing(stateData)
      }
    }

  /** Initialization behavior of the [[GridAgentCoordinator]].
    * @param stateData
    *   The state data of the coordinator.
    * @param toInitialize
    *   The agents that are not initialized yet.
    * @param subgridToRef
    *   A map: subgrid number to grid agent reference.
    * @return
    *   A new behavior.
    */
  private[grid] def initializing(
      stateData: StateData,
      toInitialize: Set[GridAgentRef] = Set.empty,
      subgridToRef: Map[Int, GridAgentRef] = Map.empty,
  ): Behavior[Message] = Behaviors.receive {
    case (_, RegisterAssets(nodeToAssets)) if subgridToRef.nonEmpty =>
      val nodeToSubgrid = stateData.nodeToSubgrid
      val onlyOneSubgrid = subgridToRef.size == 1

      nodeToAssets
        .foldLeft(
          Map.empty[Int, MultiMap[UUID, ActorRef[ParticipantAgent.Request]]]
        ) { case (res, (node, assets)) =>
          val subgrid = nodeToSubgrid(node)

          res.get(subgrid) match {
            case Some(value) =>
              res.updated(subgrid, value.updated(node, assets))
            case None =>
              res.updated(subgrid, Map(node -> assets))
          }
        }
        .foreach { case (subgrid, nodeToAssets) =>
          subgridToRef
            .get(subgrid)
            .foreach(_ ! RegisterParticipants(nodeToAssets))
        }

      // complete grid agent initialization
      stateData.informGridAgents(CompleteInitialization(onlyOneSubgrid))

      Behaviors.same

    case (ctx, FinishedInitialization(gridRef)) =>
      val updated = toInitialize.excl(gridRef)

      if updated.isEmpty then {
        // inform scheduler to schedule the next power flow calculation
        stateData.resolution.foreach { nextTick =>
          stateData.scheduler ! ScheduleActivation(ctx.self, nextTick)
        }

        idle(stateData)

      } else {
        initializing(stateData, updated, subgridToRef)
      }

    case (_, _) =>
      // we sent no activation request to the scheduler
      // -> no power flow calculations
      idle(stateData)
  }

  /** Idle behavior of the [[GridAgentCoordinator]].
    * @param stateData
    *   The state data of the coordinator.
    * @return
    *   A new behavior.
    */
  private[grid] def idle(stateData: StateData): Behavior[Message] = {
    Behaviors.receivePartial { case (_, Activation(tick)) =>
      initPowerFlow(stateData, tick)
    }
  }

  private def initPowerFlow(
      stateData: StateData,
      tick: Long,
      sameTick: Boolean = false,
  ): Behavior[Message] = {
    // informing all grid agents
    stateData.informGridAgents(DoPowerFlowTrigger(tick, sameTick))

    awaitGridSimulation(
      stateData.copy(currentTick = tick),
      stateData.createAwaitingData,
    )
  }

  /** Behavior that waits for finished grid simulations.
    * @param stateData
    *   The state data of the coordinator.
    * @param awaitingData
    *   Map that stores the already received data.
    * @return
    *   A new behavior.
    */
  private def awaitGridSimulation(
      stateData: StateData,
      awaitingData: AwaitingData[Option[PowerFlowResultEvent]],
  ): Behavior[Message] = Behaviors.receivePartial {
    case (ctx, PowerFlowResults(gridAgent, results)) =>
      val updated = awaitingData.addData(gridAgent, results)

      if updated.nonComplete then {
        // still waiting for results
        awaitGridSimulation(stateData, updated)

      } else if stateData.runCongestionManagement then {
        // handle congestion management
        val currentTick = stateData.currentTick
        val results = updated.receivedData

        // inform the grid agents to start the congestion management
        stateData.informGridAgents(ref =>
          DoCongestionManagement(currentTick, results(ref))
        )

        // waiting for results
        awaitCongestionResults(
          stateData.copy(hasRunCongestionManagement = true),
          ReceiveDataMap(stateData.superiorGrids),
        )

      } else {
        finishTick(stateData, updated.values.flatten, ctx)
      }
  }

  /** Behavior that waits for finished congestion detection.
    *
    * @param stateData
    *   The state data of the coordinator.
    * @param awaitingData
    *   Map that stores the already received data.
    * @return
    *   A new behavior.
    */
  private[grid] def awaitCongestionResults(
      stateData: StateData,
      awaitingData: AwaitingData[Congestions],
  ): Behavior[Message] = Behaviors.receivePartial {
    case (ctx, CongestionResult(gridAgent, congestions)) =>
      val updated = awaitingData.addData(gridAgent, congestions)

      if updated.nonComplete then {
        // we are still waiting for responses
        awaitCongestionResults(stateData, updated)

      } else {
        // all congestion received
        val allCongestions = Congestions.none.combine(updated.values)

        // checking for any congestion in the complete grid
        if !allCongestions.hasCongestion then {
          ctx.log.info(
            s"No congestions found. Finishing the congestion management."
          )

          stateData.superiorGrids.foreach(_ ! GotoIdle)
          awaitGridSimulation(stateData, stateData.createAwaitingData)

        } else {

          val (nextStep, updatedParams) =
            stateData.congestionManagementParams.getNextStepsAndUpdate

          nextStep match {
            case NoMeasure =>
              ctx.log.debug(
                s"Congestion overall: $allCongestions"
              )

              val timestamp =
                stateData.simStartTime.plusSeconds(stateData.currentTick)

              ctx.log.info(
                s"There were some congestions that could not be resolved for timestamp: $timestamp."
              )

              stateData.superiorGrids.foreach(_ ! GotoIdle)
              awaitGridSimulation(stateData, stateData.createAwaitingData)

            case _ =>
              // informs the grid agent about the next mitigation step
              stateData.informSuperiorGridAgents(NextStep(nextStep))

              awaitMitigationStepCompletion(
                stateData.copy(congestionManagementParams = updatedParams),
                stateData.createAwaitingData,
              )
          }
        }
      }
  }

  private def awaitMitigationStepCompletion(
      stateData: StateData,
      awaitingData: AwaitingData[StepFinished],
  ): Behavior[Message] = Behaviors.receivePartial {
    case (_, stepFinished: StepFinished) =>
      val updated = awaitingData.addData(stepFinished.sender, stepFinished)

      if updated.nonComplete then {
        awaitMitigationStepCompletion(stateData, updated)

      } else {
        initPowerFlow(stateData, stateData.currentTick, true)

      }
  }

  /** Method to finish the current tick.
    * @param stateData
    *   The state data of the coordinator.
    * @return
    *   A new behavior.
    */
  private def finishTick(
      stateData: StateData,
      results: Iterable[PowerFlowResultEvent],
      ctx: ActorContext[Message],
  ): Behavior[Message] = {

    // no congestion management is configured
    val resultProxy = stateData.resultProxy
    results.foreach(res => resultProxy ! res)

    stateData.scheduler ! Completion(ctx.self, stateData.maybeNextTick)

    idle(stateData.copy(hasRunCongestionManagement = false))
  }

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  // setup methods

  /** Method to create the grid agents.
    * @param subgrids
    *   A sequence of subgrid containers.
    * @param resolution
    *   The power flow resolution.
    * @param pfParams
    *   The power flow parameters.
    * @param cfg
    *   The simona config.
    * @param context
    *   The actor context for spawning grid agents.
    * @param environmentRefs
    *   The environment references.
    * @return
    *   A map: subgrid number to reference, a map: node uuid to subgrid and a
    *   set of references of slack grid agents.
    */
  private def createGridAgents(
      subgrids: Seq[SubGridContainer],
      resolution: Long,
      pfParams: PowerFlowParams,
      cfg: SimonaConfig,
  )(using
      context: ActorContext[Message],
      environmentRefs: EnvironmentRefs,
  ): (
      Map[Int, GridAgentRef],
      Map[UUID, Int],
      Set[GridAgentRef],
  ) = {
    val nodeToSubgrid = subgrids.flatMap {
      _.getRawGrid.getNodes.asScala.map(node => node.getUuid -> node.getSubnet)
    }.toMap

    given GridAgentConstantData = GridAgentConstantData(
      context.self,
      environmentRefs,
      cfg,
      resolution,
      cfg.time.simStartTime,
      cfg.time.simEndTime,
    )

    /* Create all agents and map the sub grid id to their actor references */
    val (subGridToActorRefMap, actorRefToNodes) = createGridAgents(
      subgrids,
      context,
      pfParams,
    )

    // register inferior grids
    val superiorGrids = actorRefToNodes.flatMap {
      case (actorRef, couplingNodes) =>
        // register inferior grid by providing the superior grid with the coupling nodes
        couplingNodes.groupBy(nodeToSubgrid).foreach {
          case (subgridNo, nodes) =>
            val superiorGrid = subGridToActorRefMap(subgridNo)

            superiorGrid ! RegisterInferiorGrid(actorRef, nodes, subgridNo)
            actorRef ! RegisterSuperiorGrid(superiorGrid, nodes, subgridNo)
        }

        Option.when(couplingNodes.isEmpty)(actorRef)
    }.toSet

    (subGridToActorRefMap, nodeToSubgrid, superiorGrids)
  }

  /** Method to create a map from subgrid number to grid agent reference and a
    * map of grid agent reference to coupling nodes with the superior grid.
    * @param subgrids
    *   A sequence of subgrid container.
    * @param context
    *   The actor context for spawning grid agents.
    * @param pfParams
    *   The parameter for the power flow.
    * @param constantData
    *   The grid agent constant data.
    * @return
    *   A map: subgrid number to reference and a map: reference to coupling
    *   nodes.
    */
  private[grid] def createGridAgents(
      subgrids: Seq[SubGridContainer],
      context: ActorContext[Message],
      pfParams: PowerFlowParams,
  )(using
      constantData: GridAgentConstantData
  ): (
      Map[Int, GridAgentRef],
      Map[GridAgentRef, Set[UUID]],
  ) = {
    given Logger = context.log

    val cfg = constantData.simonaConfig

    /* extract and prepare refSystem information from config */
    val (configRefSystems, configVoltageLimits) =
      GridConfigParser.parse(cfg.gridConfig)

    val (numberToRef, refToCouplingNodes) = subgrids.map { subGridContainer =>
      /* Prepare the subgrid container for the agents by adapting the transformer high voltage nodes to be slacks */
      val updatedSubGridContainer =
        ContainerUtils.withTrafoNodeAsSlack(subGridContainer)

      val rawGrid = updatedSubGridContainer.getRawGrid
      val slackNodes = rawGrid.getNodes.asScala.filter(_.isSlack)

      val couplingNodes =
        (rawGrid.getTransformer2Ws.asScala ++ rawGrid.getTransformer3Ws.asScala)
          .filter(t => slackNodes.contains(t.getNodeA))
          .map(_.getNodeA.getUuid)
          .toSet

      val subgridNumber = subGridContainer.getSubnet

      // get the [[GridModel]]
      val gridModel = GridModel(
        updatedSubGridContainer,
        getRefSystem(configRefSystems, subGridContainer),
        getVoltageLimits(configVoltageLimits, subGridContainer),
        constantData.simStartTime,
        constantData.simEndTime,
        cfg,
      )

      // create the GridAgentInitData
      val gridAgentInitData = GridAgentInitData(gridModel, pfParams)

      val gridAgentRef =
        context.spawn(
          GridAgent(gridAgentInitData),
          s"GridAgent-$subgridNumber",
        )

      context.watch(gridAgentRef)

      (subgridNumber -> gridAgentRef, gridAgentRef -> couplingNodes)
    }.unzip

    (numberToRef.toMap, refToCouplingNodes.toMap)
  }

  /** Searches for the reference system to be used with the given
    * [[SubGridContainer]] within the information provided by config.
    *
    * @param configRefSystems
    *   Collection of reference systems definitions from config.
    * @param subGridContainer
    *   Container model for the respective sub grid.
    * @return
    *   The reference system to use.
    */
  private def getRefSystem(
      configRefSystems: ConfigRefSystems,
      subGridContainer: SubGridContainer,
  )(using log: Logger): RefSystem = {
    val refSystem = configRefSystems
      .find(
        subGridContainer.getSubnet,
        Some(subGridContainer.getPredominantVoltageLevel),
      )
      .getOrElse(
        throw new InitializationException(
          s"Unable to determine refSystem for grid with id ${subGridContainer.getSubnet} @ " +
            s"volt level ${subGridContainer.getPredominantVoltageLevel}. Please either provide a refSystem for the grid id or the whole volt level!"
        )
      )

    val containerPotential = Kilovolts(
      subGridContainer.getPredominantVoltageLevel.getNominalVoltage
        .to(PowerSystemUnits.KILOVOLT)
        .getValue
        .doubleValue
    )

    if refSystem.nominalVoltage != containerPotential then
      log.warn(
        s"The configured RefSystem for subGrid ${subGridContainer.getSubnet} differs in its nominal voltage (${refSystem.nominalVoltage}) from the grids" +
          s"predominant voltage level nominal voltage ($containerPotential). If this is by intention and still valid, this warning can be just ignored!"
      )

    refSystem
  }

  /** Searches for the voltage limits to be used with the given
    * [[SubGridContainer]] within the information provided by config.
    *
    * @param configVoltageLimits
    *   Collection of voltage limits definitions from config.
    * @param subGridContainer
    *   Container model for the respective sub grid.
    * @return
    *   The voltage limits to use.
    */
  private def getVoltageLimits(
      configVoltageLimits: ConfigVoltageLimits,
      subGridContainer: SubGridContainer,
  ): VoltageLimits = configVoltageLimits
    .find(
      subGridContainer.getSubnet,
      Some(subGridContainer.getPredominantVoltageLevel),
    )
    .getOrElse(
      throw new InitializationException(
        s"Unable to determine voltage limits for grid with id ${subGridContainer.getSubnet} @ " +
          s"volt level ${subGridContainer.getPredominantVoltageLevel}. Please either provide voltage limits for the grid id or the whole volt level!"
      )
    )
}
