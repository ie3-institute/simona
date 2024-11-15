/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.datamodel.models.input.AssetInput
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
import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
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
    // checking the given data connections
    checkExtDataConnections(
      extSimulations.flatMap(_.getDataConnections.asScala)
    )

    val emptyExtSimSetupData = ExtSimSetupData(
      Iterable.empty,
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
        val extSimAdapterData = new ExtSimAdapterData(extSimAdapter, args)

        // send init data right away, init activation is scheduled
        extSimAdapter ! ExtSimAdapter.Create(
          extSimAdapterData,
          ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
        )

        // setup data services that belong to this external simulation
        val (updatedSetupData, dataConnections) =
          connect(extSimulation, extSimAdapterData, extSimSetupData)

        extSimulation.setup(extSimAdapterData)

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
      extSimAdapterData: ExtSimAdapterData,
      extSimSetupData: ExtSimSetupData,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      simonaConfig: SimonaConfig,
  ): (ExtSimSetupData, Set[ExtDataConnection]) = {
    implicit val extSimAdapter: ClassicRef = extSimAdapterData.getAdapter

    // the data connections this external simulation provides
    val connections = extLink.getDataConnections.asScala.toSet

    val updatedSetupData = connections.foldLeft(extSimSetupData) {
      case (setupData, connection) =>
        connection match {
          case extPrimaryData: ExtPrimaryDataConnection =>
            inputDataConnectionSetup(
              setupData,
              extPrimaryData,
              classOf[ExtPrimaryDataService],
              extPrimaryDataSimulationSetup,
            )

          case extEmData: ExtEmDataConnection =>
            inputDataConnectionSetup(
              setupData,
              extEmData,
              classOf[ExtEmDataService],
              extEmDataSimulationSetup,
            )

          case extEvData: ExtEvDataConnection =>
            inputDataConnectionSetup(
              setupData,
              extEvData,
              classOf[ExtEmDataService],
              extEvDataSimulationSetup,
            )

          case extResultData: ExtResultDataConnection =>
            extResultDataSetup(setupData, extResultData)
        }
    }

    (updatedSetupData, connections)
  }

  /** Checks the external data connection, if there are multiple connection that
    * provide data to the same asset.
    * @param extDataConnections
    *   given external data connections
    */
  private def checkExtDataConnections(
      extDataConnections: Seq[ExtDataConnection]
  ): Unit = {
    val extDataProvision: Seq[Class[_ <: AssetInput]] =
      extDataConnections.flatMap {
        case extInputDataConnection: ExtInputDataConnection =>
          extInputDataConnection.getTargetClasses.asScala
      }

    val extDataProvisionSet = extDataProvision.toSet

    if (extDataProvisionSet.size != extDataProvision.size) {
      val duplicateDataProvision: Set[Class[_ <: AssetInput]] = extDataProvision
        .groupBy(identity)
        .map { case (value, list) => value -> list.size }
        .filter(_._2 > 1)
        .keySet

      throw ServiceException(
        s"Duplicate data provision for inputs: $duplicateDataProvision"
      )
    }
  }

  private[setup] def inputDataConnectionSetup[T <: ExtInputDataConnection](
      extSimSetupData: ExtSimSetupData,
      extInputData: T,
      serviceClass: Class[_],
      setupMethod: T => ClassicRef,
  )(implicit extSimAdapter: ClassicRef): ExtSimSetupData = {
    val extDataServices = extSimSetupData.extDataServices

    val (updatedSetupData, extPrimaryDataService) =
      extDataServices.get(serviceClass) match {
        case Some(service) =>
          (extSimSetupData, service)

        case None =>
          val service = setupMethod(extInputData)

          (
            extSimSetupData.copy(extDataServices =
              extDataServices ++ Map(serviceClass -> service)
            ),
            service,
          )
      }

    extInputData.setActorRefs(extPrimaryDataService, extSimAdapter)

    updatedSetupData
  }

  private[setup] def extPrimaryDataSimulationSetup(
      extPrimaryData: ExtPrimaryDataConnection
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
  ): ClassicRef =
    setupService(
      ExtPrimaryDataService.props,
      "ExtPrimaryDataService",
      InitExtPrimaryData(extPrimaryData),
    )

  private[setup] def extEmDataSimulationSetup(extEmData: ExtEmDataConnection)(
      implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
  ): ClassicRef =
    setupService(
      ExtEmDataService.props,
      "ExtEmDataService",
      InitExtEmData(extEmData),
    )

  private[setup] def extEvDataSimulationSetup(extEvData: ExtEvDataConnection)(
      implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
  ): ClassicRef = setupService(
    ExtEvDataService.props,
    "ExtEvDataService",
    InitExtEvData(extEvData),
  )

  private[setup] def setupService(
      props: ClassicRef => Props,
      name: String,
      initData: InitializeServiceStateData,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
  ): ClassicRef = {
    val extEvDataService = context.toClassic.simonaActorOf(
      props(scheduler.toClassic),
      name,
    )

    extEvDataService ! SimonaService.Create(
      initData,
      ScheduleLock.singleKey(
        context,
        scheduler,
        INIT_SIM_TICK,
      ),
    )
    extEvDataService
  }

  private[setup] def extResultDataSetup(
      extSimSetupData: ExtSimSetupData,
      extResultData: ExtResultDataConnection,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapter: ClassicRef,
      simonaConfig: SimonaConfig,
  ): ExtSimSetupData = {
    val extDataListener = extSimSetupData.extDataListener
    val serviceClass = ExtResultDataProvider.getClass

    val (updatedSetupData, extResultDataProvider) =
      extDataListener.get(serviceClass) match {
        case Some(service) =>
          (extSimSetupData, service)

        case None =>
          val extResultDataProvider = {
            context.spawn(
              ExtResultDataProvider(scheduler),
              s"ExtResultDataProvider",
            )
          }

          val powerFlowResolution =
            simonaConfig.simona.powerflow.resolution.get(
              ChronoUnit.SECONDS
            )

          extResultDataProvider ! ExtResultDataProvider.Create(
            InitExtResultData(extResultData, powerFlowResolution),
            ScheduleLock.singleKey(
              context,
              scheduler,
              INIT_SIM_TICK,
            ),
          )

          (
            extSimSetupData.copy(extDataListener =
              extDataListener ++ Map(serviceClass -> extResultDataProvider)
            ),
            extResultDataProvider,
          )
      }

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

    extResultData.setActorRefs(
      adapterRef.toClassic,
      adapterScheduleRef.toClassic,
      extSimAdapter,
    )

    updatedSetupData
  }
}
