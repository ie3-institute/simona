/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.graph.SubGridTopologyGraph
import edu.ie3.datamodel.models.input.container.{GridContainer, ThermalGrid}
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  CompleteInitialization,
  CreateGridAgent,
  RegisterParticipants,
}
import edu.ie3.simona.agent.grid.data.GridAgentData.GridAgentRef
import edu.ie3.simona.agent.grid.powerflow.PowerFlowParams
import edu.ie3.simona.agent.grid.{
  GridAgent,
  GridAgentCoordinator,
  ParticipantAgentBuilder,
}
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.config.{GridConfigParser, SimonaConfig}
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.listener.{ResultListener, RuntimeEventListener}
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
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
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.util.TimeUtil
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

  override def gridAgents(using
      context: ActorContext[?],
      environmentRefs: EnvironmentRefs,
  ): Iterable[ActorRef[GridAgent.Message]] = {

    val cfg = simonaConfig.simona

    /* extract and prepare refSystem information from config */
    val (configRefSystems, configVoltageLimits) =
      GridConfigParser.parse(cfg.gridConfig)

    val nodeToAssets = ParticipantAgentBuilder.buildSystemParticipants(
      grid.getSystemParticipants,
      thermalGridsByThermalBus.map { case (bus, thermalGrid) =>
        bus.getUuid -> thermalGrid
      },
      cfg,
    )

    cfg.powerflow
      .map { pfConfig =>
        val (subgridToRef, nodeToSubgrid, _) =
          GridAgentCoordinator.createGridAgents(
            grid,
            pfConfig.resolution.toSeconds,
            PowerFlowParams(pfConfig),
            cfg,
          )

        nodeToAssets
          .foldLeft(
            Map.empty[Int, Map[UUID, Set[ActorRef[ParticipantAgent.Request]]]]
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
            subgridToRef(subgrid) ! RegisterParticipants(nodeToAssets)
          }

        val refs = subgridToRef.values

        // finish initialization of grid agents
        val onlyOneSubgrid = refs.size == 1
        refs.foreach(_ ! CompleteInitialization(onlyOneSubgrid))

        refs
      }
      .getOrElse(Iterable.empty[GridAgentRef])
  }

  override def primaryServiceProxy(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      extSimSetupData: ExtSimSetupData,
  ): ActorRef[ServiceMessage] = {
    val simulationStart = simonaConfig.simona.time.simStartTime

    val primaryServiceProxy = context.spawn(
      PrimaryServiceProxy(
        scheduler,
        InitPrimaryServiceProxyStateData(
          simonaConfig.simona.input.primary,
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
  ): ActorRef[ServiceMessage] = {
    val weatherService = context.spawn(
      WeatherService(scheduler),
      "weatherAgent",
    )
    weatherService ! ServiceMessage.Create(
      InitWeatherServiceStateData(
        simonaConfig.simona.input.weather.datasource,
        TimeUtil.withDefaults
          .toZonedDateTime(simonaConfig.simona.time.startDateTime),
        TimeUtil.withDefaults
          .toZonedDateTime(simonaConfig.simona.time.endDateTime),
      ),
      ScheduleLock.singleKey(context, scheduler, PRE_INIT_TICK),
    )

    weatherService
  }

  override def priceService(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
  ): Option[ActorRef[ServiceMessage]] =
    simonaConfig.simona.input.prices.datasource.map { dataSource =>
      val priceService = context.spawn(
        EnergyPriceService(scheduler),
        "priceAgent",
      )
      priceService ! ServiceMessage.Create(
        InitPriceServiceStateData(
          dataSource,
          TimeUtil.withDefaults
            .toZonedDateTime(simonaConfig.simona.time.startDateTime),
        ),
        ScheduleLock.singleKey(context, scheduler, PRE_INIT_TICK),
      )

      priceService
    }

  override def loadProfileService(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
  ): ActorRef[ServiceMessage] = {
    val loadProfileService = context.spawn(
      LoadProfileService(scheduler),
      "loadProfileService",
    )

    val cfg = simonaConfig.simona

    loadProfileService ! ServiceMessage.Create(
      InitLoadProfileServiceStateData(
        cfg.input.loadProfile.datasource,
        cfg.time.simStartTime,
      ),
      ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
    )

    loadProfileService
  }

  override def extSimulations(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      resultProxy: ActorRef[ResultServiceProxy.Message],
      extSimPath: Option[Path],
  ): ExtSimSetupData = {
    val jars = ExtSimLoader.scanInputFolder(extSimPath)
    val extLinks = jars.flatMap(ExtSimLoader.loadExtLink).toList

    setupExtSim(extLinks, args, typeSafeConfig, grid)(using
      context,
      scheduler,
      resultProxy,
      simonaConfig.simona.time.simStartTime,
    )
  }

  override def timeAdvancer(
      context: ActorContext[?],
      simulation: ActorRef[SimonaSim.SimulationEnded.type],
      runtimeEventListener: ActorRef[RuntimeEvent],
  ): ActorRef[TimeAdvancer.Request] = {
    val startDateTime = simonaConfig.simona.time.simStartTime
    val endDateTime = simonaConfig.simona.time.simEndTime

    context.spawn(
      TimeAdvancer(
        simulation,
        Some(runtimeEventListener),
        simonaConfig.simona.time.schedulerReadyCheckWindow,
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
          simonaConfig.simona.runtime.listener,
          runtimeEventQueue,
          startDateTimeString = simonaConfig.simona.time.startDateTime,
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

/** Companion object to provide [[SetupHelper]] methods for
  * [[SimonaStandaloneSetup]]
  */
object SimonaStandaloneSetup extends LazyLogging with SetupHelper {

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
