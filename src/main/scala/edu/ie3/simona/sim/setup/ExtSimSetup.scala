/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.actor.SimonaActorNaming.RichActorRefFactory
import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.api.data.em.ExtEmDataConnection
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.api.data.results.ExtResultDataConnection
import edu.ie3.simona.api.data.results.ontology.ResultDataMessageFromExt
import edu.ie3.simona.api.data.{ExtDataConnection, ExtInputDataConnection}
import edu.ie3.simona.api.simulation.{ExtSimAdapterData, ExtSimulation}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.ScheduleServiceActivation
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.em.ExtEmDataService
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.service.primary.ExtPrimaryDataService
import edu.ie3.simona.service.primary.ExtPrimaryDataService.InitExtPrimaryData
import edu.ie3.simona.service.results.ExtResultDataProvider
import edu.ie3.simona.service.results.ExtResultDataProvider.{
  InitExtResultData,
  RequestDataMessageAdapter,
  RequestScheduleActivationAdapter,
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.typed.scaladsl.AskPattern._
import org.apache.pekko.actor.typed.scaladsl.adapter.{
  TypedActorContextOps,
  TypedActorRefOps,
}
import org.apache.pekko.actor.typed.{ActorRef, Scheduler}
import org.apache.pekko.actor.{Props, ActorRef => ClassicRef}
import org.apache.pekko.util.{Timeout => PekkoTimeout}

import java.time.temporal.ChronoUnit
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.jdk.DurationConverters._

object ExtSimSetup {

  // sets up the external simulations
  def setupExtSim(
      extSimulations: List[ExtSimulation],
      args: Array[String],
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      simonaConfig: SimonaConfig,
  ): ExtSimSetupData = {

    val extInputDataConnections: List[ExtInputDataConnection] = extSimulations
      .flatMap(_.getDataConnections.asScala)
      .flatMap {
        case connection: ExtInputDataConnection => Some(connection)
        case _                                  => None
      }

    // checking the given data connections
    checkExtInputDataConnections(extInputDataConnections)

    val emptyExtSimSetupData = ExtSimSetupData(
      Iterable.empty,
      Map.empty,
      Map.empty,
      Map.empty,
      Map.empty,
      Set.empty,
    )

    extSimulations.zipWithIndex.foldLeft(emptyExtSimSetupData) {
      case (extSimSetupData, (extSimulation, index)) =>
        // external simulation always needs at least an ExtSimAdapter
        val extSimAdapter = context.toClassic.simonaActorOf(
          ExtSimAdapter.props(scheduler.toClassic),
          s"$index",
        )

        implicit val extSimAdapterData: ExtSimAdapterData =
          new ExtSimAdapterData(extSimAdapter, args)

        extSimulation.setup(extSimAdapterData)

        // send init data right away, init activation is scheduled
        extSimAdapter ! ExtSimAdapter.Create(
          extSimAdapterData,
          ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
        )

        // setup data services that belong to this external simulation
        val (updatedSetupData, dataConnections) =
          connect(extSimulation, extSimSetupData)

        // starting external simulation
        new Thread(extSimulation, s"External simulation $index")
          .start()

        val adapters = updatedSetupData.extSimAdapters
        val extData = updatedSetupData.extDatas

        // updating the data with newly connected external simulation
        updatedSetupData.copy(
          extSimAdapters = adapters ++ Set(extSimAdapter),
          extDatas = extData ++ dataConnections,
        )
    }
  }

  // connects the external simulation
  private[setup] def connect(
      extLink: ExtSimulation,
      extSimSetupData: ExtSimSetupData,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapterData: ExtSimAdapterData,
      simonaConfig: SimonaConfig,
  ): (ExtSimSetupData, Set[ExtDataConnection]) = {
    implicit val extSimAdapter: ClassicRef = extSimAdapterData.getAdapter

    // the data connections this external simulation provides
    val connections = extLink.getDataConnections.asScala.toSet

    val updatedSetupData = connections.foldLeft(extSimSetupData) {
      case (setupData, connection) =>
        connection match {
          case extPrimaryDataConnection: ExtPrimaryDataConnection =>
            extPrimaryDataSetup(setupData, extPrimaryDataConnection)

          case extEmDataConnection: ExtEmDataConnection =>
            setupInputService(
              extSimSetupData,
              extEmDataConnection,
              ExtEmDataService.props,
              "ExtEmDataService",
              InitExtEmData,
            )

          case extEvDataConnection: ExtEvDataConnection =>
            setupInputService(
              extSimSetupData,
              extEvDataConnection,
              ExtEvDataService.props,
              "ExtEvDataService",
              InitExtEvData,
            )

          case extResultDataConnection: ExtResultDataConnection =>
            extResultDataSetup(setupData, extResultDataConnection)
        }
    }

    (updatedSetupData, connections)
  }

  private[setup] def setupInputService[T <: ExtInputDataConnection](
      extSimSetupData: ExtSimSetupData,
      extInputData: T,
      props: ClassicRef => Props,
      name: String,
      initData: T => InitializeServiceStateData,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapterData: ExtSimAdapterData,
  ): ExtSimSetupData = {
    val extDataServices = extSimSetupData.extDataServices
    val serviceClass = extInputData.getClass

    val extDataService = context.toClassic.simonaActorOf(
      props(scheduler.toClassic),
      name,
    )

    extDataService ! SimonaService.Create(
      initData(extInputData),
      ScheduleLock.singleKey(
        context,
        scheduler,
        INIT_SIM_TICK,
      ),
    )

    extInputData.setActorRefs(extDataService, extSimAdapterData.getAdapter)

    extSimSetupData.copy(extDataServices =
      extDataServices ++ Map(serviceClass -> extDataService)
    )
  }

  private[setup] def extPrimaryDataSetup(
      extSimSetupData: ExtSimSetupData,
      extPrimaryDataConnection: ExtPrimaryDataConnection,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapter: ClassicRef,
  ): ExtSimSetupData = {
    val extPrimaryDataService = context.toClassic.simonaActorOf(
      ExtPrimaryDataService.props(scheduler.toClassic),
      "ExtPrimaryDataService",
    )

    extPrimaryDataService ! SimonaService.Create(
      InitExtPrimaryData(extPrimaryDataConnection),
      ScheduleLock.singleKey(
        context,
        scheduler,
        INIT_SIM_TICK,
      ),
    )

    extPrimaryDataConnection.setActorRefs(extPrimaryDataService, extSimAdapter)

    val extPrimaryData = extSimSetupData.extPrimaryData ++ Map(
      extPrimaryDataConnection -> extPrimaryDataService
    )

    val extDatas = extSimSetupData.extDatas ++ Set(extPrimaryDataConnection)

    extSimSetupData.copy(
      extPrimaryData = extPrimaryData,
      extDatas = extDatas,
    )
  }

  private[setup] def extResultDataSetup(
      extSimSetupData: ExtSimSetupData,
      extResultDataConnection: ExtResultDataConnection,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapter: ClassicRef,
      simonaConfig: SimonaConfig,
  ): ExtSimSetupData = {
    val extResultDataProvider =
      context.spawn(
        ExtResultDataProvider(scheduler),
        s"ExtResultDataProvider",
      )

    val timeout: PekkoTimeout = PekkoTimeout.create(5.seconds.toJava)
    val scheduler2: Scheduler = context.system.scheduler

    val adapterRef = Await.result(
      extResultDataProvider.ask[ActorRef[ResultDataMessageFromExt]](ref =>
        RequestDataMessageAdapter(ref)
      )(timeout, scheduler2),
      timeout.duration,
    )
    val adapterScheduleRef = Await.result(
      extResultDataProvider.ask[ActorRef[ScheduleServiceActivation]](ref =>
        RequestScheduleActivationAdapter(ref)
      )(timeout, scheduler2),
      timeout.duration,
    )

    extResultDataConnection.setActorRefs(
      adapterRef.toClassic,
      adapterScheduleRef.toClassic,
      extSimAdapter,
    )

    val powerFlowResolution =
      simonaConfig.simona.powerflow.resolution.get(ChronoUnit.SECONDS)

    extResultDataProvider ! ExtResultDataProvider.Create(
      InitExtResultData(extResultDataConnection, powerFlowResolution),
      ScheduleLock.singleKey(
        context,
        scheduler,
        INIT_SIM_TICK,
      ),
    )

    extSimSetupData.copy(extResultListeners =
      extSimSetupData.extResultListeners ++ Map(
        extResultDataConnection -> extResultDataProvider
      )
    )
  }

  /** Checks the external input data connections.
    * @param extInputDataConnections
    *   given external input data connections
    */
  private[setup] def checkExtInputDataConnections(
      extInputDataConnections: Seq[ExtInputDataConnection]
  ): Unit = {

    val dataConnectionSizes: Map[Class[_ <: ExtInputDataConnection], Int] =
      extInputDataConnections
        .map(_.getClass)
        .groupBy(identity)
        .map { case (value, list) => value -> list.size }

    // check for connections that should one be present once
    val allowOnlySingleConnection: Set[Class[_ <: ExtInputDataConnection]] =
      Set(
        classOf[ExtEvDataConnection],
        classOf[ExtEmDataConnection],
      )

    val notAllowedConfiguration = allowOnlySingleConnection.flatMap {
      connectionType =>
        dataConnectionSizes.get(connectionType) match {
          case Some(value) if value > 1 => Some(connectionType)
          case _                        => None
        }
    }

    if (notAllowedConfiguration.nonEmpty) {
      throw ServiceException(
        s"Found multiple data connections, that only allows one occurrence ($allowOnlySingleConnection)."
      )
    }

    // check primary data connections
    dataConnectionSizes.get(classOf[ExtPrimaryDataConnection]) match {
      case Some(size) if size > 1 =>
        val assetUuids = extInputDataConnections.map {
          case extPrimaryDataConnection: ExtPrimaryDataConnection =>
            extPrimaryDataConnection.getPrimaryDataAssets.asScala
        }

        if (assetUuids.size != assetUuids.toSet.size) {
          val duplicateAssets =
            assetUuids.groupBy(identity).filter(_._2.size > 1)
          throw ServiceException(
            s"Multiple data connections provide primary data for assets: $duplicateAssets"
          )
        }

      case _ =>
      // do nothing
    }

  }
}
