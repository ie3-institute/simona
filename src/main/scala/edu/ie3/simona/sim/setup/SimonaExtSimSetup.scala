/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import com.typesafe.config.Config
import edu.ie3.datamodel.graph.SubGridTopologyGraph
import edu.ie3.datamodel.models.input.container.{GridContainer, ThermalGrid}
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.simona.actor.SimonaActorNaming.RichActorRefFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgentMessages.CreateGridAgent
import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.api.data.em.ExtEmData
import edu.ie3.simona.api.data.primarydata.ExtPrimaryData
import edu.ie3.simona.api.data.results.ExtResultData
import edu.ie3.simona.api.data.results.ontology.ResultDataMessageFromExt
import edu.ie3.simona.api.simulation.{ExtSimAdapterData, ExtSimulation}
import edu.ie3.simona.config.{ArgsParser, RefSystemParser, SimonaConfig}
import edu.ie3.simona.event.listener.{ResultEventListener, RuntimeEventListener}
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.io.grid.GridProvider
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.ScheduleServiceActivation
import edu.ie3.simona.scheduler.core.Core.CoreFactory
import edu.ie3.simona.scheduler.core.RegularSchedulerCore
import edu.ie3.simona.scheduler.{ScheduleLock, Scheduler, TimeAdvancer}
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.em.ExtEmDataService
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.service.primary.ExtPrimaryDataService.InitExtPrimaryData
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.primary.{ExtPrimaryDataService, PrimaryServiceProxy}
import edu.ie3.simona.service.results.ExtResultDataProvider
import edu.ie3.simona.service.results.ExtResultDataProvider.{InitExtResultData, RequestDataMessageAdapter, RequestScheduleActivationAdapter}
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.typed.scaladsl.AskPattern._
import org.apache.pekko.actor.typed.scaladsl.adapter.{ClassicActorRefOps, TypedActorContextOps, TypedActorRefOps}
import org.apache.pekko.actor.typed.{ActorRef, Scheduler}
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import org.apache.pekko.util.{Timeout => PekkoTimeout}

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.jdk.DurationConverters._

/** Sample implementation to run a standalone simulation of simona configured
  * with the provided [[SimonaConfig]] and [[ResultFileHierarchy]]
  *
  * @version 0.1
  * @since 01.07.20
  */
abstract class SimonaExtSimSetup(
    val typeSafeConfig: Config,
    val simonaConfig: SimonaConfig,
    val resultFileHierarchy: ResultFileHierarchy,
    val runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] = None,
    override val args: Array[String]
) extends SimonaSetup {
  override def logOutputDir: String = resultFileHierarchy.logOutputDir

  override def gridAgents(
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

  override def primaryServiceProxy(
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
          extSimSetupData.extPrimaryDataService,
          extSimSetupData.extPrimaryData
        ),
        simulationStart,
      ),
      "primaryServiceProxyAgent",
    )

    scheduler ! ScheduleActivation(primaryServiceProxy.toTyped, INIT_SIM_TICK)
    primaryServiceProxy
  }

  override def weatherService(
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

  override def timeAdvancer(
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

  override def scheduler(
      context: ActorContext[_],
      parent: ActorRef[SchedulerMessage],
      coreFactory: CoreFactory = RegularSchedulerCore,
  ): ActorRef[SchedulerMessage] =
    context
      .spawn(
        Scheduler(parent, coreFactory),
        s"${Scheduler.getClass.getSimpleName}_${coreFactory}_${UUID.randomUUID()}",
      )

  override def runtimeEventListener(
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

  override def resultEventListener(
      context: ActorContext[_],
      extSimulationData: ExtSimSetupData,
  ): Seq[ActorRef[ResultEventListener.Request]] = {
    val extResultDataService: Option[ActorRef[ExtResultDataProvider.Request]] =
      extSimulationData.extResultDataService
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
          extResultDataService,
        ),
        ResultEventListener.getClass.getSimpleName,
      )
  }

  def buildSubGridToActorRefMap(
      subGridTopologyGraph: SubGridTopologyGraph,
      context: ActorContext[_],
      environmentRefs: EnvironmentRefs,
      resultEventListeners: Seq[ActorRef[ResultEvent]],
  ): Map[Int, ActorRef[GridAgent.Request]] = {
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
  }

  /** Get all thermal grids, that apply for the given grid container
    * @param grid
    *   The grid container to assess
    * @param thermalGridByBus
    *   Mapping from thermal bus to thermal grid
    * @return
    *   A sequence of applicable thermal grids
    */
  private def getThermalGrids(
      grid: GridContainer,
      thermalGridByBus: Map[ThermalBusInput, ThermalGrid],
  ): Seq[ThermalGrid] = {
    grid.getSystemParticipants.getHeatPumps.asScala
      .flatten(hpInput => thermalGridByBus.get(hpInput.getThermalBus))
      .toSeq
  }


  def extSimulationSetup(
                          context: ActorContext[_],
                          scheduler: ActorRef[SchedulerMessage],
                          extSim: ExtSimulation
                        ): ExtSimSetupData = {
    // ExtSimAdapter
    val extSimAdapter = context.toClassic.simonaActorOf(
      ExtSimAdapter.props(scheduler.toClassic),
      s"ExtSimAdapter",
    )

    val extSimAdapterData = new ExtSimAdapterData(extSimAdapter, args)

    // send init data right away, init activation is scheduled
    extSimAdapter ! ExtSimAdapter.Create(
      extSimAdapterData,
      ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
    )

    val extDataServicesMap: mutable.Map[Class[_], ClassicRef] = mutable.Map.empty
    val extDataListenerMap: mutable.Map[Class[_], ActorRef[ExtResultDataProvider.Request]] = mutable.Map.empty

    val dataConnections = extSim.getDataConnections

    dataConnections.asScala.foreach {
      case extPrimaryData: ExtPrimaryData =>
        val extPrimaryDataService = extPrimaryDataSimulationSetup(
          context,
          scheduler,
          extSimAdapterData,
          extPrimaryData
        )
        extDataServicesMap += (classOf[ExtPrimaryDataService] -> extPrimaryDataService)


      case extResultData: ExtResultData =>
        val extResultDataProvider = extResultDataSimulationSetup(
          context,
          scheduler,
          extSimAdapterData,
          extResultData,
          TimeUtil.withDefaults.toZonedDateTime(
            simonaConfig.simona.time.startDateTime
          ),
          simonaConfig.simona.powerflow.resolution.get(
            ChronoUnit.SECONDS
          )
        )
        extDataListenerMap += (ExtResultDataProvider.getClass -> extResultDataProvider)


      case extEmData: ExtEmData =>
        val extEmDataService = extEmDataSimulationSetup(
          context,
          scheduler,
          extSimAdapterData,
          extEmData
        )
        extDataServicesMap += (classOf[ExtEmDataService] -> extEmDataService)

    }
    context.log.info("Set up all data connections!")
    extSim.setup(
      extSimAdapterData,
      dataConnections
    )
    new Thread(extSim, s"External simulation").start()

    val extSimAdapters = Iterable(extSimAdapter)

    ExtSimSetupData(
      extSimAdapters,
      extDataServicesMap.toMap,
      extDataListenerMap.toMap,
      dataConnections.asScala.toSet
    )
  }

  private def extPrimaryDataSimulationSetup(
                                     context: ActorContext[_],
                                     scheduler: ActorRef[SchedulerMessage],
                                     extSimAdapterData: ExtSimAdapterData,
                                     extPrimaryData: ExtPrimaryData
                                   ): ClassicRef = {
    val extSimAdapter = extSimAdapterData.getAdapter

    val extPrimaryDataService = context.toClassic.simonaActorOf(
      ExtPrimaryDataService.props(scheduler.toClassic),
      s"0-0",
    )

    extPrimaryData.setActorRefs(
      extPrimaryDataService,
      extSimAdapter
    )

    extPrimaryDataService ! SimonaService.Create(
      InitExtPrimaryData(extPrimaryData),
      ScheduleLock.singleKey(
        context,
        scheduler,
        INIT_SIM_TICK,
      ),
    )
    extPrimaryDataService
  }


  private def extEmDataSimulationSetup(
                                context: ActorContext[_],
                                scheduler: ActorRef[SchedulerMessage],
                                extSimAdapterData: ExtSimAdapterData,
                                extEmData: ExtEmData
                              ): ClassicRef = {
    val extSimAdapter = extSimAdapterData.getAdapter

    val extEmDataService = context.toClassic.simonaActorOf(
      ExtEmDataService.props(scheduler.toClassic),
      s"0-0",
    )

    extEmData.setActorRefs(
      extEmDataService,
      extSimAdapter
    )

    extEmDataService ! SimonaService.Create(
      InitExtEmData(extEmData),
      ScheduleLock.singleKey(
        context,
        scheduler,
        INIT_SIM_TICK,
      ),
    )
    extEmDataService
  }


  private def extResultDataSimulationSetup(
                                    context: ActorContext[_],
                                    scheduler: ActorRef[SchedulerMessage],
                                    extSimAdapterData: ExtSimAdapterData,
                                    extResultData: ExtResultData,
                                    simulationStart: ZonedDateTime,
                                    powerFlowResolution: Long
                                  ): ActorRef[ExtResultDataProvider.Request] = {
    val extResultDataProvider = {
      context.spawn(
        ExtResultDataProvider(scheduler),
        s"ExtResultDataProvider",
      )
    }

    val timeout: PekkoTimeout = PekkoTimeout.create(5.seconds.toJava)
    val scheduler2: Scheduler = context.system.scheduler

    val adapterRef = Await.result(
      extResultDataProvider.ask[ActorRef[ResultDataMessageFromExt]](ref => RequestDataMessageAdapter(ref))(timeout, scheduler2), timeout.duration)
    val adapterScheduleRef = Await.result(
      extResultDataProvider.ask[ActorRef[ScheduleServiceActivation]](ref => RequestScheduleActivationAdapter(ref))(timeout, scheduler2), timeout.duration)

    val extSimAdapter = extSimAdapterData.getAdapter

    extResultData.setActorRefs(
      adapterRef.toClassic,
      adapterScheduleRef.toClassic,
      extSimAdapter
    )

    extResultDataProvider ! ExtResultDataProvider.Create(
      InitExtResultData(extResultData, powerFlowResolution),
      ScheduleLock.singleKey(
        context,
        scheduler,
        INIT_SIM_TICK,
      ),
    )
    extResultDataProvider
  }

}
