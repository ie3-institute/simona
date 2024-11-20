/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.datamodel.graph.{SubGridGate, SubGridTopologyGraph}
import edu.ie3.datamodel.models.input.connector.Transformer3WInput
import edu.ie3.datamodel.models.input.container.{GridContainer, ThermalGrid}
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.simona.actor.SimonaActorNaming.RichActorRefFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgentMessages.CreateGridAgent
import edu.ie3.simona.config.{ArgsParser, RefSystemParser, SimonaConfig}
import edu.ie3.simona.event.listener.{ResultEventListener, RuntimeEventListener}
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.io.grid.GridProvider
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.scheduler.core.Core.CoreFactory
import edu.ie3.simona.scheduler.core.RegularSchedulerCore
import edu.ie3.simona.scheduler.{ScheduleLock, Scheduler, TimeAdvancer}
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.sim.setup.ExtSimSetup.setupExtSim
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.typed.scaladsl.adapter.{
  ClassicActorRefOps,
  TypedActorContextOps,
  TypedActorRefOps,
}
import org.apache.pekko.actor.{ActorRef => ClassicRef}

import java.util.UUID
import java.util.concurrent.LinkedBlockingQueue
import scala.jdk.CollectionConverters.SetHasAsScala

/** Trait that can be used to setup a customized simona simulation by providing
  * implementations for all setup information required by a
  * [[edu.ie3.simona.sim.SimonaSim]]. Most of the time, using or extending
  * [[SimonaStandaloneSetup]] might be considered instead of providing a your
  * own implementation for all methods.
  *
  * @version 0.1
  * @since 01.07.20
  */
trait SimonaSetup {

  val simonaConfig: SimonaConfig
  val resultFileHierarchy: ResultFileHierarchy
  val runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]]

  /** Main arguments of the executable. May be used to pass additional
    * configuration parameters to the setup e.g. for external simulation
    * configuration
    */
  val args: Array[String]

  /** Directory of the log output.
    */
  val logOutputDir: String = resultFileHierarchy.logOutputDir

  /** Creates the runtime event listener
    *
    * @param context
    *   Actor context to use
    * @return
    *   An actor reference to the runtime event listener
    */
  def runtimeEventListener(
      context: ActorContext[_]
  ): ActorRef[RuntimeEventListener.Request] =
    context
      .spawn(
        RuntimeEventListener(
          simonaConfig.simona.runtime.listener,
          runtimeEventQueue,
          startDateTimeString = simonaConfig.simona.time.startDateTime,
        ),
        RuntimeEventListener.getClass.getSimpleName,
      )

  /** Creates a sequence of result event listeners
    *
    * @param context
    *   Actor context to use
    * @return
    *   A sequence of actor references to result event listeners
    */
  def resultEventListener(
      context: ActorContext[_],
      extSimulationData: ExtSimSetupData,
  ): Seq[ActorRef[ResultEventListener.Request]] =
    // append ResultEventListener as well to write raw output files
    ArgsParser
      .parseListenerConfigOption(simonaConfig.simona.event.listener)
      .zipWithIndex
      .map { case ((listenerCompanion, events), index) =>
        context.toClassic
          .simonaActorOf(
            listenerCompanion.props(events),
            index.toString,
          )
          .toTyped
      }
      .toSeq :+ context
      .spawn(
        ResultEventListener(
          resultFileHierarchy,
          extSimulationData.extResultListeners,
        ),
        ResultEventListener.getClass.getSimpleName,
      )

  /** Creates a primary service proxy. The proxy is the first instance to ask
    * for primary data. If necessary, it delegates the registration request to
    * it's subordinate workers.
    *
    * @param context
    *   Actor context to use
    * @param scheduler
    *   Actor reference to it's according scheduler to use
    * @return
    *   An actor reference to the service
    */
  def primaryServiceProxy(
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimSetupData: ExtSimSetupData,
  ): ClassicRef = {
    val simulationStart = TimeUtil.withDefaults.toZonedDateTime(
      simonaConfig.simona.time.startDateTime
    )
    val primaryServiceProxy = context.toClassic.simonaActorOf(
      PrimaryServiceProxy.props(
        scheduler.toClassic,
        InitPrimaryServiceProxyStateData(
          simonaConfig.simona.input.primary,
          simulationStart,
          extSimSetupData.extPrimaryDataServices,
        ),
        simulationStart,
      ),
      "primaryServiceProxyAgent",
    )

    scheduler ! ScheduleActivation(primaryServiceProxy.toTyped, INIT_SIM_TICK)
    primaryServiceProxy
  }

  /** Creates a weather service
    *
    * @param context
    *   Actor context to use
    * @param scheduler
    *   Actor reference to it's according scheduler to use
    * @return
    *   An actor reference to the service as well as matching data to initialize
    *   the service
    */
  def weatherService(
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
  ): ClassicRef = {
    val weatherService = context.toClassic.simonaActorOf(
      WeatherService.props(
        scheduler.toClassic,
        TimeUtil.withDefaults
          .toZonedDateTime(simonaConfig.simona.time.startDateTime),
        TimeUtil.withDefaults
          .toZonedDateTime(simonaConfig.simona.time.endDateTime),
      ),
      "weatherAgent",
    )
    weatherService ! SimonaService.Create(
      InitWeatherServiceStateData(
        simonaConfig.simona.input.weather.datasource
      ),
      ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
    )

    weatherService
  }

  /** Loads external simulations and provides corresponding actors and init data
    *
    * @param context
    *   Actor context to use
    * @param scheduler
    *   Actor reference to the scheduler to use
    * @return
    *   External simulations and their init data
    */
  def extSimulations(
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
  ): ExtSimSetupData = {
    val jars = ExtSimLoader.scanInputFolder()
    val extLinks = jars.flatMap(ExtSimLoader.loadExtLink).toList

    setupExtSim(extLinks, args)(
      context,
      scheduler,
      simonaConfig,
    )
  }

  /** Creates the time advancer
    *
    * @param context
    *   Actor context to use
    * @param simulation
    *   The simulation root actor ([[edu.ie3.simona.sim.SimonaSim]])
    * @param runtimeEventListener
    *   Runtime event listener
    * @return
    *   An actor reference to the time advancer
    */
  def timeAdvancer(
      context: ActorContext[_],
      simulation: ActorRef[SimonaSim.SimulationEnded.type],
      runtimeEventListener: ActorRef[RuntimeEvent],
  ): ActorRef[TimeAdvancer.Request] = {
    val startDateTime = TimeUtil.withDefaults.toZonedDateTime(
      simonaConfig.simona.time.startDateTime
    )
    val endDateTime = TimeUtil.withDefaults.toZonedDateTime(
      simonaConfig.simona.time.endDateTime
    )

    context.spawn(
      TimeAdvancer(
        simulation,
        Some(runtimeEventListener),
        simonaConfig.simona.time.schedulerReadyCheckWindow,
        endDateTime.toTick(startDateTime),
      ),
      TimeAdvancer.getClass.getSimpleName,
    )
  }

  /** Creates a scheduler service
    *
    * @param context
    *   Actor context to use
    * @param parent
    *   The parent scheduler, which could be a time advancer
    * @param coreFactory
    *   The factory creating a scheduler core that determines the scheduler's
    *   behavior, defaulting to a regular scheduler
    * @return
    *   An actor reference to the scheduler
    */
  def scheduler(
      context: ActorContext[_],
      parent: ActorRef[SchedulerMessage],
      coreFactory: CoreFactory = RegularSchedulerCore,
  ): ActorRef[SchedulerMessage] =
    context
      .spawn(
        Scheduler(parent, coreFactory),
        s"${Scheduler.getClass.getSimpleName}_${coreFactory}_${UUID.randomUUID()}",
      )

  /** Creates all the needed grid agents
    *
    * @param context
    *   Actor context to use
    * @param environmentRefs
    *   EnvironmentRefs to use
    * @param resultEventListeners
    *   Listeners that await events from system participants
    * @return
    *   A mapping from actor reference to it's according initialization data to
    *   be used when setting up the agents
    */
  def gridAgents(
      context: ActorContext[_],
      environmentRefs: EnvironmentRefs,
      resultEventListeners: Seq[ActorRef[ResultEvent]],
  ): Iterable[ActorRef[GridAgent.Request]] = {

    /* get the grid */
    val subGridTopologyGraph = GridProvider
      .gridFromConfig(
        simonaConfig.simona.simulationName,
        simonaConfig.simona.input.grid.datasource,
      )
      .getSubGridTopologyGraph
    val thermalGridsByThermalBus = GridProvider.getThermalGridsFromConfig(
      simonaConfig.simona.input.grid.datasource
    )

    /* extract and prepare refSystem information from config */
    val configRefSystems =
      RefSystemParser.parse(simonaConfig.simona.gridConfig.refSystems)

    /* Create all agents and map the sub grid id to their actor references */
    val subGridToActorRefMap = buildSubGridToActorRefMap(
      subGridTopologyGraph,
      context,
      environmentRefs,
      resultEventListeners,
    )

    val keys = ScheduleLock.multiKey(
      context,
      environmentRefs.scheduler,
      INIT_SIM_TICK,
      subGridTopologyGraph.vertexSet().size,
    )

    /* build the initialization data */
    subGridTopologyGraph
      .vertexSet()
      .asScala
      .zip(keys)
      .map { case (subGridContainer, key) =>
        /* Get all connections to superior and inferior sub grids */
        val subGridGates =
          Set.from(
            subGridTopologyGraph
              .edgesOf(subGridContainer)
              .asScala
              .map(modifySubGridGateForThreeWindingSupport)
          )
        val currentSubGrid = subGridContainer.getSubnet
        val currentActorRef = subGridToActorRefMap.getOrElse(
          currentSubGrid,
          throw new GridAgentInitializationException(
            "Was asked to setup agent for sub grid " + currentSubGrid + ", but did not found it's actor reference."
          ),
        )
        val thermalGrids =
          getThermalGrids(subGridContainer, thermalGridsByThermalBus)

        /* build the grid agent data and check for its validity */
        val gridAgentInitData = SimonaStandaloneSetup.buildGridAgentInitData(
          subGridContainer,
          subGridToActorRefMap,
          subGridGates,
          configRefSystems,
          thermalGrids,
        )

        currentActorRef ! CreateGridAgent(gridAgentInitData, key)

        currentActorRef
      }
  }

  def buildSubGridToActorRefMap(
      subGridTopologyGraph: SubGridTopologyGraph,
      context: ActorContext[_],
      environmentRefs: EnvironmentRefs,
      resultEventListeners: Seq[ActorRef[ResultEvent]],
  ): Map[Int, ActorRef[GridAgent.Request]] =
    subGridTopologyGraph
      .vertexSet()
      .asScala
      .map(subGridContainer => {
        val gridAgentRef =
          context.spawn(
            GridAgent(
              environmentRefs,
              simonaConfig,
              resultEventListeners,
            ),
            subGridContainer.getSubnet.toString,
          )
        subGridContainer.getSubnet -> gridAgentRef
      })
      .toMap

  /** Get all thermal grids, that apply for the given grid container
    *
    * @param grid
    *   The grid container to assess
    * @param thermalGridByBus
    *   Mapping from thermal bus to thermal grid
    * @return
    *   A sequence of applicable thermal grids
    */
  private[setup] def getThermalGrids(
      grid: GridContainer,
      thermalGridByBus: Map[ThermalBusInput, ThermalGrid],
  ): Seq[ThermalGrid] = {
    grid.getSystemParticipants.getHeatPumps.asScala
      .flatten(hpInput => thermalGridByBus.get(hpInput.getThermalBus))
      .toSeq
  }

  /** SIMONA links sub grids connected by a three winding transformer a bit
    * different. Therefore, the internal node has to be set as superior node.
    * All other gates are left unchanged
    */
  protected val modifySubGridGateForThreeWindingSupport
      : SubGridGate => SubGridGate =
    (gate: SubGridGate) =>
      gate.link match {
        case transformer: Transformer3WInput =>
          new SubGridGate(
            transformer,
            transformer.getNodeInternal,
            gate.inferiorNode,
          )
        case _ => gate
      }
}
