/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import com.typesafe.config.Config
import edu.ie3.datamodel.models.input.container.{
  JointGridContainer,
  ThermalGrid,
}
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentCoordinator
import edu.ie3.simona.agent.grid.GridAgentCoordinator.RegisterAssets
import edu.ie3.simona.agent.participant.{
  ParticipantAgent,
  ParticipantAgentFactory,
}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.listener.{ResultListener, RuntimeEventListener}
import edu.ie3.simona.io.grid.GridProvider
import edu.ie3.simona.ontology.messages.ResultMessage.ResultResponse
import edu.ie3.simona.ontology.messages.{SchedulerMessage, ServiceMessage}
import edu.ie3.simona.scheduler.core.Core.CoreFactory
import edu.ie3.simona.scheduler.core.RegularSchedulerCore
import edu.ie3.simona.scheduler.{ScheduleLock, Scheduler, TimeAdvancer}
import edu.ie3.simona.service.load.LoadProfileService
import edu.ie3.simona.service.load.LoadProfileService.InitLoadProfileServiceStateData
import edu.ie3.simona.service.price.EnergyPriceService
import edu.ie3.simona.service.price.EnergyPriceService.InitPriceServiceStateData
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.results.ResultServiceProxy
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.sim.setup.ExtSimSetup.setupExtSim
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.toTick
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext

import java.nio.file.Path
import java.time.ZonedDateTime
import java.util.UUID
import java.util.concurrent.LinkedBlockingQueue
import scala.jdk.CollectionConverters.SetHasAsScala

/** Class that contains all methods to set up a simulation.
  *
  * @param typeSafeConfig
  *   The parsed config.
  * @param simonaConfig
  *   The build SIMONA config.
  * @param args
  *   Main arguments of the executable. May be used to pass additional
  *   configuration parameters to the setup e.g. for external simulation
  *   configuration.
  * @param runtimeEventQueue
  *   An option for a runtime event queue.
  */
class SimonaSetup(
    val typeSafeConfig: Config,
    val simonaConfig: SimonaConfig,
    val args: Array[String] = Array.empty[String],
    val runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] = None,
) {

  /** The electrical grid.
    */
  lazy val grid: JointGridContainer = GridProvider.gridFromConfig(
    simonaConfig.simulationName,
    simonaConfig.input.grid.datasource,
  )

  /** Map: thermal bus to thermal grid.
    */
  lazy val thermalGridsByThermalBus: Map[ThermalBusInput, ThermalGrid] =
    GridProvider.getThermalGridsFromConfig(simonaConfig.input.grid.datasource)

  lazy val baseInputPath: Path = Path.of(simonaConfig.input.baseInputDir)

  val resultFileHierarchy: ResultFileHierarchy =
    ResultFileHierarchy(typeSafeConfig, simonaConfig)

  /** Directory of the log output.
    */
  lazy val logOutputDir: Path = resultFileHierarchy.logOutputDir

  /** Creates the runtime event listener.
    *
    * @param context
    *   Actor context to use.
    * @return
    *   An actor reference to the runtime event listener.
    */
  def runtimeEventListener(
      context: ActorContext[?]
  ): ActorRef[RuntimeEventListener.Request] =
    context
      .spawn(
        RuntimeEventListener(
          simonaConfig.runtime.listener,
          runtimeEventQueue,
          startDateTimeString = simonaConfig.time.startDateTime,
        ),
        RuntimeEventListener.getClass.getSimpleName,
      )

  /** Creates a sequence of result event listeners.
    *
    * @param context
    *   Actor context to use.
    * @return
    *   A sequence of actor references to result event listeners.
    */
  def resultEventListener(
      context: ActorContext[?]
  ): Seq[ActorRef[ResultListener.Message]] = {
    // creates a sequence of ResultEventListener to write raw output files
    Seq(
      context
        .spawn(
          ResultListener(resultFileHierarchy),
          ResultListener.getClass.getSimpleName,
        )
    )
  }

  /** Creates a primary service proxy. The proxy is the first instance to ask
    * for primary data. If necessary, it delegates the registration request to
    * it's subordinate workers.
    *
    * @param context
    *   Actor context to use.
    * @param scheduler
    *   Actor reference to it's according scheduler to use.
    * @param extSimSetupData
    *   that can contain external
    *   [[edu.ie3.simona.api.data.connection.ExtPrimaryDataConnection]].
    * @return
    *   An actor reference to the service.
    */
  def primaryServiceProxy(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      extSimSetupData: ExtSimSetupData,
  ): ActorRef[ServiceMessage] = {
    val simulationStart = simonaConfig.time.simStartTime

    val primaryServiceProxy = context.spawn(
      PrimaryServiceProxy(
        scheduler,
        InitPrimaryServiceProxyStateData(
          simonaConfig.input.primary,
          simulationStart,
          extSimSetupData.primaryDataServices,
        ),
      ),
      "primaryServiceProxyAgent",
    )

    primaryServiceProxy
  }

  /** Creates a result service proxy. The proxy will receive information about
    * the result that should be expected for the current tick and all result
    * events that are send by the agents. The proxy is responsible for
    * processing the result events and passing the processed data to the
    * different result listeners and providers.
    *
    * @param context
    *   Actor context to use.
    * @param listeners
    *   The internal result event listeners.
    * @param simStartTime
    *   The start time of the simulation.
    * @return
    *   An actor reference to the service.
    */
  def resultServiceProxy(
      context: ActorContext[?],
      listeners: Seq[ActorRef[ResultResponse]],
      simStartTime: ZonedDateTime,
  ): ActorRef[ResultServiceProxy.Message] =
    context.spawn(
      ResultServiceProxy(listeners, simStartTime),
      "resultServiceProxyAgent",
    )

  /** Creates a weather service.
    *
    * @param context
    *   Actor context to use.
    * @param scheduler
    *   Actor reference to it's according scheduler to use.
    * @return
    *   An actor reference to the service.
    */
  def weatherService(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
  ): ActorRef[ServiceMessage] =
    context.spawn(
      WeatherService(
        scheduler,
        InitWeatherServiceStateData(
          simonaConfig.input.weather.datasource,
          simonaConfig.time.simStartTime,
          simonaConfig.time.simEndTime,
        ),
        ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
      ),
      "weatherService",
    )

  /** Creates an energy price service, if such service is configured.
    *
    * @param context
    *   Actor context to use.
    * @param scheduler
    *   Actor reference to it's according scheduler to use.
    * @return
    *   An actor reference to the service.
    */
  def priceService(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
  ): Option[ActorRef[ServiceMessage]] =
    simonaConfig.input.prices.datasource.map { dataSource =>
      context.spawn(
        EnergyPriceService(
          scheduler,
          InitPriceServiceStateData(
            dataSource,
            simonaConfig.time.simStartTime,
          ),
          ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
        ),
        "priceService",
      )
    }

  /** Creates a load profile service.
    *
    * @param context
    *   Actor context to use.
    * @param scheduler
    *   Actor reference to it's according scheduler to use.
    * @return
    *   An actor reference to the service as well as matching data to initialize
    *   the service.
    */
  def loadProfileService(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
  ): ActorRef[ServiceMessage] =
    context.spawn(
      LoadProfileService(
        scheduler,
        InitLoadProfileServiceStateData(
          simonaConfig.input.loadProfile.datasource,
          simonaConfig.time.simStartTime,
        ),
        ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
      ),
      "loadProfileService",
    )

  /** Loads external simulations and provides corresponding actors and init
    * data.
    *
    * @param context
    *   Actor context to use.
    * @param scheduler
    *   Actor reference to the scheduler to use.
    * @param resultProxy
    *   Actor reference to the result provider.
    * @param extSimPath
    *   Option for a directory with external simulations.
    * @return
    *   External simulations and their init data.
    */
  def extSimulations(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      resultProxy: ActorRef[ResultServiceProxy.Message],
      extSimPath: Option[Path],
  ): ExtSimSetupData = {
    val jars = ExtSimLoader.scanInputFolder(extSimPath)
    val extLinks = jars.flatMap(ExtSimLoader.loadExtLink).toList

    setupExtSim(
      extLinks,
      args,
      typeSafeConfig,
      grid,
      baseInputPath,
      resultFileHierarchy.runOutputDir,
    )(using
      context,
      scheduler,
      resultProxy,
      simonaConfig.time.simStartTime,
    )
  }

  /** Creates the time advancer.
    *
    * @param context
    *   Actor context to use.
    * @param simulation
    *   The simulation root actor ([[edu.ie3.simona.sim.SimonaSim]]).
    * @param runtimeEventListener
    *   Runtime event listener.
    * @return
    *   An actor reference to the time advancer.
    */
  def timeAdvancer(
      context: ActorContext[?],
      simulation: ActorRef[SimonaSim.SimulationEnded.type],
      runtimeEventListener: ActorRef[RuntimeEvent],
  ): ActorRef[TimeAdvancer.Request] = {
    val startDateTime = simonaConfig.time.simStartTime
    val endDateTime = simonaConfig.time.simEndTime

    context.spawn(
      TimeAdvancer(
        simulation,
        Some(runtimeEventListener),
        simonaConfig.time.schedulerReadyCheckWindow,
        endDateTime.toTick(using startDateTime),
      ),
      TimeAdvancer.getClass.getSimpleName,
    )
  }

  /** Creates a scheduler service.
    *
    * @param context
    *   Actor context to use.
    * @param parent
    *   The parent scheduler, which could be a time advancer.
    * @param coreFactory
    *   The factory creating a scheduler core that determines the scheduler's
    *   behavior, defaulting to a regular scheduler.
    * @return
    *   An actor reference to the scheduler.
    */
  def scheduler(
      context: ActorContext[?],
      parent: ActorRef[SchedulerMessage],
      coreFactory: CoreFactory = RegularSchedulerCore,
  ): ActorRef[SchedulerMessage] =
    context
      .spawn(
        Scheduler(parent, coreFactory),
        s"${Scheduler.getClass.getSimpleName}_${coreFactory}_${UUID.randomUUID()}",
      )

  /** Creates the grid agent coordinator which will create and coordinate all
    * grid agents.
    *
    * @param participantRefs
    *   A map of node uuid to set of participant references.
    * @param context
    *   Actor context to use.
    * @param environmentRefs
    *   EnvironmentRefs to use.
    * @return
    *   The reference to the [[GridAgentCoordinator]].
    */
  def gridAgentCoordinator(
      participantRefs: Map[UUID, Set[ActorRef[ParticipantAgent.Request]]]
  )(using
      context: ActorContext[?],
      environmentRefs: EnvironmentRefs,
  ): ActorRef[GridAgentCoordinator.Message] = {
    // get the subgrids
    val subgrids = grid.getSubGridTopologyGraph.vertexSet.asScala.toSeq

    /* spawn the grid agent coordinator */
    val coordinator = context.spawn(
      GridAgentCoordinator(simonaConfig, subgrids),
      "GridAgentCoordinator",
    )

    coordinator ! RegisterAssets(participantRefs)

    coordinator
  }

  /** Creates the participant agents of the simulation.
    * @param context
    *   Actor context to use.
    * @param environmentRefs
    *   EnvironmentRefs to use.
    * @return
    *   A map of node uuid to set of participant references.
    */
  def participantAgents(using
      context: ActorContext[?],
      environmentRefs: EnvironmentRefs,
  ): Map[UUID, Set[ActorRef[ParticipantAgent.Request]]] = {
    // build participants
    val thermalIslandGridsByBusId = thermalGridsByThermalBus.map {
      case (_, thermalGrid) => thermalGrid.bus().getUuid -> thermalGrid
    }

    ParticipantAgentFactory.buildSystemParticipants(
      grid.getSystemParticipants,
      thermalIslandGridsByBusId,
      simonaConfig,
    )
  }
}
