/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import akka.actor.ActorRefFactory
import edu.ie3.datamodel.graph.SubGridTopologyGraph
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.{GridAgent, GridAgentData}
import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.akka.SimonaActorRef.{
  LocalActorRef,
  RichActorRefFactory,
  SingletonRef
}
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
import edu.ie3.simona.ontology.trigger.Trigger.{
  InitializeExtSimAdapterTrigger,
  InitializeServiceTrigger
}
import edu.ie3.simona.scheduler.main.SimScheduler
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.util.TimeUtil

import java.util.concurrent.LinkedBlockingQueue
import scala.jdk.CollectionConverters._

/** Sample implementation to run a standalone simulation of simona configured
  * with the provided [[SimonaConfig]] and [[ResultFileHierarchy]]
  *
  * @version 0.1
  * @since 01.07.20
  */
final case class SimonaStandaloneSetup(
    simonaConfig: SimonaConfig,
    resultFileHierarchy: ResultFileHierarchy,
    runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] = None,
    override val args: Array[String] = Array.empty
) extends SimonaSetup {

  override def gridAgents(
      refFactory: ActorRefFactory,
      environmentRefs: EnvironmentRefs,
      systemParticipantListener: Seq[SimonaActorRef]
  ): Map[SimonaActorRef, GridAgentData.GridAgentInitData] = {

    /* get the grid */
    val subGridTopologyGraph = GridProvider
      .gridFromConfig(
        simonaConfig.simona.simulationName,
        simonaConfig.simona.input.grid.datasource
      )
      .getSubGridTopologyGraph

    /* extract and prepare refSystem information from config */
    val configRefSystems =
      RefSystemParser.parse(simonaConfig.simona.gridConfig.refSystems)

    /* Create all agents and map the sub grid id to their actor references */
    val subGridToActorRefMap = buildSubGridToActorRefMap(
      subGridTopologyGraph,
      refFactory,
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
        val subnet = subGridContainer.getSubnet
        val gridAgentActorRef = subGridToActorRefMap.getOrElse(
          subnet,
          throw new GridAgentInitializationException(
            s"Was asked to setup agent for sub grid $subnet, but did not found it's actor reference."
          )
        )

        /* build the grid agent data and check for its validity */
        val gridAgentInitData = SimonaStandaloneSetup.buildGridAgentInitData(
          subGridContainer,
          subGridToActorRefMap,
          subGridGates,
          configRefSystems
        )

        gridAgentActorRef -> gridAgentInitData
      })
      .toMap
  }

  override def primaryServiceProxy(
      refFactory: ActorRefFactory,
      scheduler: SimonaActorRef
  ): (SimonaActorRef, PrimaryServiceProxy.InitPrimaryServiceProxyStateData) = {
    val simulationStart = TimeUtil.withDefaults.toZonedDateTime(
      simonaConfig.simona.time.startDateTime
    )
    (
      refFactory.createSingletonOf(
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
      refFactory: ActorRefFactory,
      scheduler: SimonaActorRef
  ): (SimonaActorRef, InitWeatherServiceStateData) =
    (
      refFactory.createSingletonOf(
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
      refFactory: ActorRefFactory,
      scheduler: SimonaActorRef
  ): ExtSimSetupData = {
    val jars = ExtSimLoader.scanInputFolder()

    val extLinks = jars.map { jar =>
      ExtSimLoader.loadExtLink(jar)
    }

    val (extSimAdapters, extDataServices) =
      extLinks.zipWithIndex.map { case (extLink, index) =>
        // external simulation always needs at least an ExtSimAdapter
        val extSimAdapter = refFactory.createSingletonWithIdOf(
          ExtSimAdapter.props(scheduler),
          s"$index"
        )

        val actorRef = extSimAdapter match {
          case LocalActorRef(ref)   => ref
          case SingletonRef(ref, _) => ref
        }

        val extSimAdapterData = new ExtSimAdapterData(actorRef, args)
        val initExtSimAdapter = InitializeExtSimAdapterTrigger(
          InitExtSimAdapter(extSimAdapterData)
        )

        // setup data services that belong to this external simulation
        val (extData, extDataInit): (
            Iterable[ExtData],
            Iterable[(SimonaActorRef, InitializeServiceTrigger[_])]
        ) =
          extLink.getExtDataSimulations.asScala.zipWithIndex.map {
            case (_: ExtEvSimulation, dIndex) =>
              val extEvDataService = refFactory.createSingletonWithIdOf(
                ExtEvDataService.props(scheduler),
                s"$index-$dIndex"
              )

              val actorRef = extEvDataService match {
                case LocalActorRef(actorRef)   => actorRef
                case SingletonRef(actorRef, _) => actorRef
              }
              val extEvData = new ExtEvData(actorRef, actorRef)

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

  override def scheduler(
      refFactory: ActorRefFactory,
      runtimeEventListener: Seq[SimonaActorRef]
  ): SimonaActorRef =
    refFactory.createSingletonOf(
      SimScheduler.props(
        simonaConfig.simona.time,
        runtimeEventListener,
        simonaConfig.simona.time.stopOnFailedPowerFlow
      )
    )

  override def runtimeEventListener(
      refFactory: ActorRefFactory
  ): Seq[SimonaActorRef] =
    Seq(
      refFactory.createSingletonOf(
        RuntimeEventListener.props(
          None,
          runtimeEventQueue,
          startDateTimeString = simonaConfig.simona.time.startDateTime
        )
      )
    )

  override def systemParticipantsListener(
      refFactory: ActorRefFactory,
      simonaSim: SimonaActorRef
  ): Seq[SimonaActorRef] = {
    // append ResultEventListener as well to write raw output files
    ArgsParser
      .parseListenerConfigOption(simonaConfig.simona.event.listener)
      .zipWithIndex
      .map { case ((listenerCompanion, events), index) =>
        refFactory.createSingletonWithIdOf(
          props = listenerCompanion.props(events),
          actorId = index.toString
        )
      }
      .toSeq :+
      refFactory.createSingletonOf(
        ResultEventListener.props(
          SetupHelper.allResultEntitiesToWrite(simonaConfig.simona.output),
          resultFileHierarchy,
          simonaSim
        )
      )
  }

  def buildSubGridToActorRefMap(
      subGridTopologyGraph: SubGridTopologyGraph,
      refFactory: ActorRefFactory,
      environmentRefs: EnvironmentRefs,
      systemParticipantListener: Seq[SimonaActorRef]
  ): Map[Int, SimonaActorRef] = {
    subGridTopologyGraph
      .vertexSet()
      .asScala
      .map(subGridContainer => {
        val subnetNo = subGridContainer.getSubnet

        val gridAgentRef =
          refFactory.createShardedEntityOf(
            GridAgent.props(
              environmentRefs,
              simonaConfig,
              systemParticipantListener
            ),
            subnetNo
          )
        subnetNo -> gridAgentRef
      })
      .toMap
  }
}

/** Companion object to provide [[SetupHelper]] methods for
  * [[SimonaStandaloneSetup]]
  */
object SimonaStandaloneSetup extends SetupHelper
