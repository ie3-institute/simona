/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentCoordinator
import edu.ie3.simona.agent.grid.GridAgentCoordinator.RegisterAssets
import edu.ie3.simona.agent.participant.ParticipantAgentFactory
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.listener.{ResultListener, RuntimeEventListener}
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
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext

import java.nio.file.Path
import java.time.ZonedDateTime
import java.util.UUID
import java.util.concurrent.LinkedBlockingQueue
import scala.jdk.CollectionConverters.*

/** Sample implementation to run a standalone simulation of simona configured
  * with the provided [[SimonaConfig]] and [[ResultFileHierarchy]]
  *
  * @version 0.1
  * @since 01.07.20
  */
class SimonaStandaloneSetup(
    val typeSafeConfig: Config,
    override val simonaConfig: SimonaConfig,
    resultFileHierarchy: ResultFileHierarchy,
    runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] = None,
    override val args: Array[String],
) extends SimonaSetup {

  override def logOutputDir: Path = resultFileHierarchy.logOutputDir

  override def gridAgentCoordinator(using
      context: ActorContext[?],
      environmentRefs: EnvironmentRefs,
  ): ActorRef[GridAgentCoordinator.Message] = {
    // build participants
    val thermalIslandGridsByBusId = thermalGridsByThermalBus.map {
      case (_, thermalGrid) => thermalGrid.bus().getUuid -> thermalGrid
    }

    val nodeToParticipants = ParticipantAgentFactory.buildSystemParticipants(
      grid.getSystemParticipants,
      thermalIslandGridsByBusId,
      simonaConfig,
    )

    // get the subgrids
    val subgrids = grid.getSubGridTopologyGraph.vertexSet.asScala.toSeq

    /* spawn the grid agent coordinator */
    val coordinator = context.spawn(
      GridAgentCoordinator(simonaConfig, subgrids),
      "GridAgentCoordinator",
    )

    coordinator ! RegisterAssets(nodeToParticipants)

    coordinator
  }

  override def primaryServiceProxy(
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
        ),
      ),
      "primaryServiceProxyAgent",
    )

    primaryServiceProxy
  }

  override def resultServiceProxy(
      context: ActorContext[?],
      listeners: Seq[ActorRef[ResultResponse]],
      simStartTime: ZonedDateTime,
  ): ActorRef[ResultServiceProxy.Message] =
    context.spawn(
      ResultServiceProxy(listeners, simStartTime),
      "resultServiceProxyAgent",
    )

  override def weatherService(
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

  override def priceService(
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

  override def loadProfileService(
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

  override def extSimulations(
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

  override def timeAdvancer(
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

  override def scheduler(
      context: ActorContext[?],
      parent: ActorRef[SchedulerMessage],
      coreFactory: CoreFactory = RegularSchedulerCore,
  ): ActorRef[SchedulerMessage] =
    context
      .spawn(
        Scheduler(parent, coreFactory),
        s"${Scheduler.getClass.getSimpleName}_${coreFactory}_${UUID.randomUUID()}",
      )

  override def runtimeEventListener(
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

  override def resultEventListener(
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
}

/** Companion object for [[SimonaStandaloneSetup]]
  */
object SimonaStandaloneSetup extends LazyLogging {

  def apply(
      typeSafeConfig: Config,
      simonaConfig: SimonaConfig,
      resultFileHierarchy: ResultFileHierarchy,
      runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] = None,
      mainArgs: Array[String] = Array.empty[String],
  ): SimonaStandaloneSetup =
    new SimonaStandaloneSetup(
      typeSafeConfig,
      simonaConfig,
      resultFileHierarchy,
      runtimeEventQueue,
      mainArgs,
    )
}
