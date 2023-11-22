/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import akka.actor.typed.scaladsl.adapter.{
  ClassicActorContextOps,
  TypedActorRefOps
}
import akka.actor.{ActorContext, ActorRef, ActorSystem}
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.graph.SubGridTopologyGraph
import edu.ie3.datamodel.models.input.container.{GridContainer, ThermalGrid}
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.simona.actor.SimonaActorNaming._
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.{GridAgent, GridAgentData}
import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.api.ExtSimAdapter.InitExtSimAdapter
import edu.ie3.simona.api.data.ExtData
import edu.ie3.simona.api.data.ev.{ExtEvData, ExtEvSimulation}
import edu.ie3.simona.api.simulation.ExtSimAdapterData
import edu.ie3.simona.config.{ArgsParser, RefSystemParser, SimonaConfig}
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.listener.{ResultEventListener, RuntimeEventListener}
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.io.grid.GridProvider
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.trigger.Trigger.{
  InitializeExtSimAdapterTrigger,
  InitializeServiceTrigger
}
import edu.ie3.simona.scheduler.{Scheduler, TimeAdvancer}
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.util.TimeUtil
import edu.ie3.simona.util.TickUtil.RichZonedDateTime

import java.util.concurrent.LinkedBlockingQueue
import scala.jdk.CollectionConverters._

/** Sample implementation to run a standalone simulation of simona configured
  * with the provided [[SimonaConfig]] and [[ResultFileHierarchy]]
  *
  * @version 0.1
  * @since 01.07.20
  */
class SimonaStandaloneSetup(
    override val buildActorSystem: () => ActorSystem,
    simonaConfig: SimonaConfig,
    resultFileHierarchy: ResultFileHierarchy,
    runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] = None,
    override val args: Array[String]
) extends SimonaSetup {

  override def gridAgents(
      context: ActorContext,
      environmentRefs: EnvironmentRefs,
      systemParticipantListener: Seq[ActorRef]
  ): Map[ActorRef, GridAgentData.GridAgentInitData] = {

    /* get the grid */
    val subGridTopologyGraph = GridProvider
      .gridFromConfig(
        simonaConfig.simona.simulationName,
        simonaConfig.simona.input.grid.datasource
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
      systemParticipantListener
    )

    /* build the initialization data */
    subGridTopologyGraph
      .vertexSet()
      .asScala
      .map(subGridContainer => {
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
          )
        )
        val thermalGrids =
          getThermalGrids(subGridContainer, thermalGridsByThermalBus)

        /* build the grid agent data and check for its validity */
        val gridAgentInitData = SimonaStandaloneSetup.buildGridAgentInitData(
          subGridContainer,
          subGridToActorRefMap,
          subGridGates,
          configRefSystems,
          thermalGrids
        )

        currentActorRef -> gridAgentInitData
      })
      .toMap
  }

  override def primaryServiceProxy(
      context: ActorContext,
      scheduler: ActorRef
  ): (ActorRef, PrimaryServiceProxy.InitPrimaryServiceProxyStateData) = {
    val simulationStart = TimeUtil.withDefaults.toZonedDateTime(
      simonaConfig.simona.time.startDateTime
    )
    (
      context.simonaActorOf(
        PrimaryServiceProxy.props(
          scheduler,
          simulationStart
        )
      ),
      InitPrimaryServiceProxyStateData(
        simonaConfig.simona.input.primary,
        simulationStart
      )
    )
  }

  override def weatherService(
      context: ActorContext,
      scheduler: ActorRef
  ): (ActorRef, InitWeatherServiceStateData) =
    (
      context.simonaActorOf(
        WeatherService.props(
          scheduler,
          TimeUtil.withDefaults
            .toZonedDateTime(simonaConfig.simona.time.startDateTime),
          TimeUtil.withDefaults
            .toZonedDateTime(simonaConfig.simona.time.endDateTime)
        )
      ),
      InitWeatherServiceStateData(
        simonaConfig.simona.input.weather.datasource
      )
    )

  override def extSimulations(
      context: ActorContext,
      scheduler: ActorRef
  ): ExtSimSetupData = {
    val jars = ExtSimLoader.scanInputFolder()

    val extLinks = jars.flatMap(ExtSimLoader.loadExtLink)

    val (extSimAdapters, extDataServices) =
      extLinks.zipWithIndex.map { case (extLink, index) =>
        // external simulation always needs at least an ExtSimAdapter
        val extSimAdapter = context.simonaActorOf(
          ExtSimAdapter.props(scheduler),
          s"$index"
        )
        val extSimAdapterData = new ExtSimAdapterData(extSimAdapter, args)
        val initExtSimAdapter = InitializeExtSimAdapterTrigger(
          InitExtSimAdapter(extSimAdapterData)
        )

        // setup data services that belong to this external simulation
        val (extData, extDataInit): (
            Iterable[ExtData],
            Iterable[(ActorRef, InitializeServiceTrigger[_])]
        ) =
          extLink.getExtDataSimulations.asScala.zipWithIndex.map {
            case (_: ExtEvSimulation, dIndex) =>
              val extEvDataService = context.simonaActorOf(
                ExtEvDataService.props(scheduler),
                s"$index-$dIndex"
              )
              val extEvData = new ExtEvData(extEvDataService, extSimAdapter)

              val initExtEvData = InitializeServiceTrigger(
                InitExtEvData(extEvData)
              )

              (extEvData, (extEvDataService, initExtEvData))
          }.unzip

        extLink.getExtSimulation.setup(
          extSimAdapterData,
          extData.toList.asJava
        )

        // starting external simulation
        new Thread(extLink.getExtSimulation, s"External simulation $index")
          .start()

        ((extSimAdapter, initExtSimAdapter), extDataInit)
      }.unzip

    ExtSimSetupData(extSimAdapters, extDataServices.flatten)
  }

  override def timeAdvancer(
      context: ActorContext,
      simulation: ActorRef,
      runtimeEventListener: akka.actor.typed.ActorRef[RuntimeEvent]
  ): akka.actor.typed.ActorRef[SchedulerMessage] = {
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
        endDateTime.toTick(startDateTime)
      ),
      TimeAdvancer.getClass.getSimpleName
    )
  }

  override def scheduler(
      context: ActorContext,
      timeAdvancer: akka.actor.typed.ActorRef[SchedulerMessage]
  ): ActorRef =
    context
      .spawn(
        Scheduler(
          timeAdvancer
        ),
        Scheduler.getClass.getSimpleName
      )
      .toClassic

  override def runtimeEventListener(
      context: ActorContext
  ): akka.actor.typed.ActorRef[RuntimeEvent] =
    context
      .spawn(
        RuntimeEventListener(
          simonaConfig.simona.runtime.listener,
          runtimeEventQueue,
          startDateTimeString = simonaConfig.simona.time.startDateTime
        ),
        RuntimeEventListener.getClass.getSimpleName
      )

  override def systemParticipantsListener(
      context: ActorContext
  ): Seq[ActorRef] = {
    // append ResultEventListener as well to write raw output files
    ArgsParser
      .parseListenerConfigOption(simonaConfig.simona.event.listener)
      .zipWithIndex
      .map { case ((listenerCompanion, events), index) =>
        context.simonaActorOf(
          listenerCompanion.props(events),
          index.toString
        )
      }
      .toSeq :+ context
      .spawn(
        ResultEventListener(
          resultFileHierarchy
        ),
        ResultEventListener.getClass.getSimpleName
      )
      .toClassic
  }

  def buildSubGridToActorRefMap(
      subGridTopologyGraph: SubGridTopologyGraph,
      context: ActorContext,
      environmentRefs: EnvironmentRefs,
      systemParticipantListener: Seq[ActorRef]
  ): Map[Int, ActorRef] = {
    subGridTopologyGraph
      .vertexSet()
      .asScala
      .map(subGridContainer => {
        val gridAgentRef =
          context.simonaActorOf(
            GridAgent.props(
              environmentRefs,
              simonaConfig,
              systemParticipantListener
            ),
            subGridContainer.getSubnet.toString
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
      thermalGridByBus: Map[ThermalBusInput, ThermalGrid]
  ): Seq[ThermalGrid] = {
    grid
      .allEntitiesAsList()
      .asScala
      .flatMap {
        case hpInput: HpInput =>
          thermalGridByBus.get(hpInput.getThermalBus)
        case _ =>
          None
      }
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
      mainArgs: Array[String] = Array.empty[String]
  ): SimonaStandaloneSetup =
    new SimonaStandaloneSetup(
      () => ActorSystem("simona", typeSafeConfig),
      SimonaConfig(typeSafeConfig),
      resultFileHierarchy,
      runtimeEventQueue,
      mainArgs
    )
}
