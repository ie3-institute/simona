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
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgentMessages.CreateGridAgent
import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.simulation.ExtSimAdapterData
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
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.SimonaSim
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

import java.nio.file.Path
import java.util.UUID
import java.util.concurrent.LinkedBlockingQueue
import scala.jdk.CollectionConverters._

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

  override def logOutputDir: Path = resultFileHierarchy.logOutputDir

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

  override def extSimulations(
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
  ): ExtSimSetupData = {
    val jars = ExtSimLoader.scanInputFolder()

    val extLinks = jars.flatMap(ExtSimLoader.loadExtLink).toSeq

    if (extLinks.nonEmpty) {

      val (extSimAdapters, extDataServices) =
        extLinks.zipWithIndex.map { case (extLink, index) =>
          // external simulation always needs at least an ExtSimAdapter
          val extSimAdapter = context.spawn(
            ExtSimAdapter(scheduler),
            s"$index",
          )

          val extSimAdapterMsgAdapter = context.spawn(
            ExtSimAdapter.adapter(extSimAdapter),
            s"$index-message-adapter",
          )

          val extSimAdapterData =
            new ExtSimAdapterData(extSimAdapterMsgAdapter.toClassic, args)

          // send init data right away, init activation is scheduled
          extSimAdapter ! ExtSimAdapter.Create(
            extSimAdapterData,
            ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
          )

          // setup data services that belong to this external simulation
          extLink.setup(extSimAdapterData)
          val extSim = extLink.getExtSimulation

          val extDataInit
              : Iterable[(Class[_ <: SimonaService[_]], ClassicRef)] =
            extSim.getDataConnections.asScala.zipWithIndex.map {
              case (evConnection: ExtEvDataConnection, dIndex) =>
                val extEvDataService = context.toClassic.simonaActorOf(
                  ExtEvDataService.props(scheduler.toClassic),
                  s"$index-$dIndex",
                )
                evConnection.setActorRefs(
                  extEvDataService,
                  extSimAdapterMsgAdapter.toClassic,
                )

                extEvDataService ! SimonaService.Create(
                  InitExtEvData(evConnection),
                  ScheduleLock.singleKey(
                    context,
                    scheduler,
                    INIT_SIM_TICK,
                  ),
                )

                (classOf[ExtEvDataService], extEvDataService)
            }

          // starting external simulation
          new Thread(extLink.getExtSimulation, s"External simulation $index")
            .start()

          (extSimAdapter, extDataInit)
        }.unzip

      ExtSimSetupData(
        extSimAdapters,
        extDataServices.flatten.toMap,
      )
    } else {
      ExtSimSetupData(Iterable.empty, Map.empty)
    }
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
      context: ActorContext[_]
  ): Seq[ActorRef[ResultEventListener.Request]] = {
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
          resultFileHierarchy
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
