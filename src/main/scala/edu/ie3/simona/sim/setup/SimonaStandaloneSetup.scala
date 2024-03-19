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
import edu.ie3.simona.actor.SimonaActorNaming.RichActorRefFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentMessage.CreateGridAgent
import edu.ie3.simona.agent.grid.{GridAgent, GridAgentMessage}
import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.api.data.ExtData
import edu.ie3.simona.api.data.ev.{ExtEvData, ExtEvSimulation}
import edu.ie3.simona.api.data.primarydata.{ExtPrimaryData, ExtPrimaryDataSimulation}
import edu.ie3.simona.api.data.results.{ExtResultData, ExtResultDataSimulation}
import edu.ie3.simona.api.data.results.ontology.ResultDataMessageFromExt
import edu.ie3.simona.api.simulation.ExtSimAdapterData
import edu.ie3.simona.config.{ArgsParser, RefSystemParser, SimonaConfig}
import edu.ie3.simona.event.listener.{ResultEventListener, RuntimeEventListener}
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.io.grid.GridProvider
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.ScheduleServiceActivation
import edu.ie3.simona.scheduler.{ScheduleLock, Scheduler, TimeAdvancer}
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
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
//import edu.ie3.simosaik.simpleextsim.SimpleExtSimulation
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.typed.scaladsl.AskPattern._
import org.apache.pekko.actor.typed.scaladsl.adapter.{ClassicActorRefOps, TypedActorContextOps, TypedActorRefOps}
import org.apache.pekko.actor.typed.{ActorRef, Scheduler}
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import org.apache.pekko.util.{Timeout => PekkoTimeout}

import java.util.concurrent.LinkedBlockingQueue
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
class SimonaStandaloneSetup(
    val typeSafeConfig: Config,
    simonaConfig: SimonaConfig,
    resultFileHierarchy: ResultFileHierarchy,
    runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] = None,
    override val args: Array[String],
) extends SimonaSetup {

  override def gridAgents(
      context: ActorContext[_],
      environmentRefs: EnvironmentRefs,
      resultEventListeners: Seq[ActorRef[ResultEvent]],
  ): Iterable[ActorRef[GridAgentMessage]] = {

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
      )
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
      )
    )
    weatherService ! SimonaService.Create(
      InitWeatherServiceStateData(
        simonaConfig.simona.input.weather.datasource
      ),
      ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
    )

    weatherService
  }


  override def extSimulations(
                               context: ActorContext[_],
                               theScheduler: ActorRef[SchedulerMessage],
                             ): ExtSimSetupData = {
    val jars = ExtSimLoader.scanInputFolder()

    val extLinks = jars.flatMap(ExtSimLoader.loadExtLink)

    context.log.info(s"extLinks = $extLinks")

    val (extSimAdapters, extDatasAndServices) =
      extLinks.zipWithIndex.map { case (extLink, index) =>
        // external simulation always needs at least an ExtSimAdapter
        val extSimAdapter = context.toClassic.simonaActorOf(
          ExtSimAdapter.props(theScheduler.toClassic),
          s"$index",
        )
        val extSimAdapterData = new ExtSimAdapterData(extSimAdapter, args)

        // send init data right away, init activation is scheduled
        extSimAdapter ! ExtSimAdapter.Create(
          extSimAdapterData,
          ScheduleLock.singleKey(context, theScheduler, INIT_SIM_TICK),
        )

        // setup data services that belong to this external simulation
        val (extData, extDataServiceToRef): (
            Iterable[ExtData],
            Iterable[(Class[_], ClassicRef)],
          ) =
          extLink.getExtDataSimulations.asScala.zipWithIndex.map {
            case (_: ExtEvSimulation, dIndex) =>
              val extEvDataService = context.toClassic.simonaActorOf(
                ExtEvDataService.props(theScheduler.toClassic),
                s"$index-$dIndex",
              )
              val extEvData = new ExtEvData(extEvDataService, extSimAdapter)

              extEvDataService ! SimonaService.Create(
                InitExtEvData(extEvData),
                ScheduleLock.singleKey(
                  context,
                  theScheduler,
                  INIT_SIM_TICK,
                ),
              )

              (extEvData, (classOf[ExtEvDataService], extEvDataService))

            case (extPrimaryDataSimulation: ExtPrimaryDataSimulation, dIndex) =>
              val extPrimaryDataService = context.toClassic.simonaActorOf(
                ExtPrimaryDataService.props(theScheduler.toClassic),
                s"$index-$dIndex",
              )
              val extPrimaryData = new ExtPrimaryData(
                extPrimaryDataService,
                extSimAdapter,
                extPrimaryDataSimulation.getPrimaryDataFactory,
                extPrimaryDataSimulation.getPrimaryDataAssets
              )

              extPrimaryDataService ! SimonaService.Create(
                InitExtPrimaryData(extPrimaryData),
                ScheduleLock.singleKey(
                  context,
                  theScheduler,
                  INIT_SIM_TICK,
                ),
              )

              (extPrimaryData, (classOf[ExtPrimaryDataService], extPrimaryDataService))

            case (extResultDataSimulation: ExtResultDataSimulation, dIndex) =>

              val extResultDataProvider = {
                context.spawn(
                  ExtResultDataProvider(theScheduler),
                  s"$index-$dIndex",
                )
              }

              implicit val timeout: PekkoTimeout = PekkoTimeout.create(5.seconds.toJava)
              implicit val scheduler: Scheduler = context.system.scheduler

              val adapterRef = Await.result(
                extResultDataProvider.ask[ActorRef[ResultDataMessageFromExt]] (ref => RequestDataMessageAdapter(ref)), timeout.duration)
              val adapterScheduleRef = Await.result(
                extResultDataProvider.ask[ActorRef[ScheduleServiceActivation]] (ref => RequestScheduleActivationAdapter(ref)), timeout.duration)

              val extResultData =
                new ExtResultData(
                  adapterRef.toClassic,
                  adapterScheduleRef.toClassic,
                  extSimAdapter,
                  extResultDataSimulation.getResultDataFactory,
                  extResultDataSimulation.getResultDataAssets
                )

              context.log.info(s"extResultData $extResultData")

              extResultDataProvider ! ExtResultDataProvider.Create(
                InitExtResultData(extResultData),
                ScheduleLock.singleKey(
                  context,
                  theScheduler,
                  INIT_SIM_TICK,
                ),
              )
              /*
              val extResultDataService = context.toClassic.simonaActorOf(
                ExtResultDataProvider.props(scheduler.toClassic),
                s"$index-$dIndex",
              )
              val extResultsData =
                new ExtResultsData(extResultDataService, extSimAdapter, null)

              extResultDataService ! SimonaService.Create(
                InitExtResultsData(extResultsData),
                ScheduleLock.singleKey(
                  context,
                  scheduler,
                  INIT_SIM_TICK,
                ),
              )
               */

              (
                extResultData,
                (ExtResultDataProvider.getClass, extResultDataProvider.toClassic),
              )
          }.unzip

        extLink.getExtSimulation.setup(
          extSimAdapterData,
          extData.toList.asJava,
        )

        // starting external simulation
        new Thread(extLink.getExtSimulation, s"External simulation $index").start()

        (extSimAdapter, (extDataServiceToRef, extData))
      }.unzip

    val extDataServices = extDatasAndServices.map(_._1)
    val extDatas = extDatasAndServices.flatMap(_._2).toSet
    context.log.info(s"extDatasAndServices = $extDatasAndServices")
    ExtSimSetupData(extSimAdapters, extDataServices.flatten.toMap, extDatas)
  }

  /*

  def extTestSimulations(
      context: ActorContext[_],
      theScheduler: ActorRef[SchedulerMessage],
  ): ExtSimSetupData = {
    val simpleExtSim = new SimpleExtSimulation()

    val extSimAdapter = context.toClassic.simonaActorOf(
      ExtSimAdapter.props(theScheduler.toClassic),
      s"0",
    )
    val extSimAdapterData = new ExtSimAdapterData(extSimAdapter, args)

    // send init data right away, init activation is scheduled
    extSimAdapter ! ExtSimAdapter.Create(
      extSimAdapterData,
      ScheduleLock.singleKey(context, theScheduler, INIT_SIM_TICK),
    )

    val extPrimaryDataService = context.toClassic.simonaActorOf(
      ExtPrimaryDataService.props(theScheduler.toClassic),
      s"0-0",
    )
    val extPrimaryData = new ExtPrimaryData(
      extPrimaryDataService,
      extSimAdapter,
      simpleExtSim.getPrimaryDataFactory,
      simpleExtSim.getPrimaryDataAssets
    )

    extPrimaryDataService ! SimonaService.Create(
      InitExtPrimaryData(extPrimaryData),
      ScheduleLock.singleKey(
        context,
        theScheduler,
        INIT_SIM_TICK,
      ),
    )


    val extResultDataProvider = {
      context.spawn(
        ExtResultDataProvider(theScheduler),
        s"ExtResultDataProvider",
      )
    }

    implicit val timeout: PekkoTimeout = PekkoTimeout.create(5.seconds.toJava)
    implicit val scheduler: Scheduler = context.system.scheduler

    val adapterRef = Await.result(
      extResultDataProvider.ask[ActorRef[ResultDataMessageFromExt]] (ref => RequestDataMessageAdapter(ref)), timeout.duration)
    val adapterScheduleRef = Await.result(
      extResultDataProvider.ask[ActorRef[ScheduleServiceActivation]] (ref => RequestScheduleActivationAdapter(ref)), timeout.duration)

    val extResultData =
      new ExtResultData(
        adapterRef.toClassic,
        adapterScheduleRef.toClassic,
        extSimAdapter,
        simpleExtSim.getResultDataFactory,
        simpleExtSim.getResultDataAssets
      )

    context.log.info(s"extResultData $extResultData")

    extResultDataProvider ! ExtResultDataProvider.Create(
      InitExtResultData(extResultData),
      ScheduleLock.singleKey(
        context,
        theScheduler,
        INIT_SIM_TICK,
      ),
    )

    val simpleExtSimDatas: List[ExtData] = List(extResultData, extPrimaryData)

    simpleExtSim.setup(
      extSimAdapterData,
      simpleExtSimDatas.asJava,
    )
    // starting external simulation
    context.log.info("Starting External simulation")
    new Thread(simpleExtSim, s"External simulation 0")
      .start()

    val extDataServicesMap: Map[Class[_], ClassicRef] = Map(
      classOf[ExtPrimaryDataService] -> extPrimaryDataService,
      ExtResultDataProvider.getClass -> extResultDataProvider.toClassic
    )

    val extSimAdapters = Iterable(extSimAdapter)

    val extDatas = Set(
      extPrimaryData,
      extResultData
    )

    ExtSimSetupData(extSimAdapters, extDataServicesMap, extDatas)
  }

   */

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
      timeAdvancer: ActorRef[TimeAdvancer.Request],
  ): ActorRef[SchedulerMessage] =
    context
      .spawn(
        Scheduler(timeAdvancer),
        Scheduler.getClass.getSimpleName,
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
    val extResultDataService: Option[ClassicRef] =
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
  ): Map[Int, ActorRef[GridAgentMessage]] = {
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
}

/** Companion object to provide [[SetupHelper]] methods for
  * [[SimonaStandaloneSetup]]
  */
object SimonaStandaloneSetup extends LazyLogging with SetupHelper {

  def apply(
      typeSafeConfig: Config,
      resultFileHierarchy: ResultFileHierarchy,
      runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] = None,
      mainArgs: Array[String] = Array.empty[String],
  ): SimonaStandaloneSetup =
    new SimonaStandaloneSetup(
      typeSafeConfig,
      SimonaConfig(typeSafeConfig),
      resultFileHierarchy,
      runtimeEventQueue,
      mainArgs,
    )
}
