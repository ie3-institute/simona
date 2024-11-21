/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.actor.SimonaActorNaming.RichActorRefFactory
import edu.ie3.simona.api.data.ExtInputDataConnection
import edu.ie3.simona.api.data.em.ExtEmDataConnection
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.api.data.results.ExtResultDataConnection
import edu.ie3.simona.api.data.results.ontology.ResultDataMessageFromExt
import edu.ie3.simona.api.simulation.{ExtSimAdapterData, ExtSimulation}
import edu.ie3.simona.api.{ExtLinkInterface, ExtSimAdapter}
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
import org.slf4j.{Logger, LoggerFactory}

import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.jdk.DurationConverters._

object ExtSimSetup {

  private val log: Logger = LoggerFactory.getLogger(ExtSimSetup.getClass)

  /** Method to set up all external simulations defined via the given
    * [[ExtLinkInterface]]s.
    * @param extLinks
    *   interfaces that hold information regarding external simulations
    * @param args
    *   the main args the simulation is started with
    * @param context
    *   the actor context of this actor system
    * @param scheduler
    *   the scheduler of simona
    * @param simonaConfig
    *   the config
    * @return
    *   an [[ExtSimSetupData]] that holds information regarding the external
    *   data connections as well as the actor references of the created services
    */
  def setupExtSim(
      extLinks: List[ExtLinkInterface],
      args: Array[String],
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      simonaConfig: SimonaConfig,
  ): ExtSimSetupData = extLinks.zipWithIndex.foldLeft(ExtSimSetupData()) {
    case (extSimSetupData, (extLink, index)) =>
      // external simulation always needs at least an ExtSimAdapter
      val extSimAdapter = context.toClassic.simonaActorOf(
        ExtSimAdapter.props(scheduler.toClassic),
        s"$index",
      )

      // creating the adapter data
      implicit val extSimAdapterData: ExtSimAdapterData =
        new ExtSimAdapterData(extSimAdapter, args)

      // sets up the external simulation
      extLink.setup(extSimAdapterData)
      val extSimulation = extLink.getExtSimulation

      // send init data right away, init activation is scheduled
      extSimAdapter ! ExtSimAdapter.Create(
        extSimAdapterData,
        ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
      )

      // setup data services that belong to this external simulation
      val updatedSetupData = connect(extSimulation, extSimSetupData)

      // starting external simulation
      new Thread(extSimulation, s"External simulation $index")
        .start()

      // updating the data with newly connected external simulation
      updatedSetupData.copy(extSimAdapters =
        updatedSetupData.extSimAdapters ++ Set(extSimAdapter)
      )
  }

  // connects the external simulation
  private[setup] def connect(
      extSimulation: ExtSimulation,
      extSimSetupData: ExtSimSetupData,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapterData: ExtSimAdapterData,
      simonaConfig: SimonaConfig,
  ): ExtSimSetupData = {
    implicit val extSimAdapter: ClassicRef = extSimAdapterData.getAdapter

    // the data connections this external simulation provides
    val connections = extSimulation.getDataConnections.asScala.toSet

    log.info(
      s"Setting up external simulation `${extSimulation.getSimulationName}` with the following data connections: ${connections.map(_.getClass).mkString(",")}."
    )

    val updatedSetupData = connections.foldLeft(extSimSetupData) {
      case (setupData, connection) =>
        connection match {
          case extPrimaryDataConnection: ExtPrimaryDataConnection =>
            extPrimaryDataSetup(setupData, extPrimaryDataConnection)

          case extEmDataConnection: ExtEmDataConnection =>
            if (setupData.extEmDataConnection.nonEmpty) {
              throw ServiceException(
                s"Trying to connect another EmDataConnection. Currently only one is allowed."
              )
            }

            setupInputService(
              extSimSetupData,
              extEmDataConnection,
              ExtEmDataService.props,
              "ExtEmDataService",
              InitExtEmData,
            )

          case extEvDataConnection: ExtEvDataConnection =>
            if (setupData.extEvDataConnection.nonEmpty) {
              throw ServiceException(
                s"Trying to connect another EvDataConnection. Currently only one is allowed."
              )
            }

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

    // validate data
    validatePrimaryData(updatedSetupData.extPrimaryDataServices.keySet)

    updatedSetupData
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

    extSimSetupData + (extInputData, extDataService)
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

    extSimSetupData + (extPrimaryDataConnection, extPrimaryDataService)
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

    extSimSetupData + (extResultDataConnection, extResultDataProvider)
  }

  // check primary data for duplicate assets
  private[setup] def validatePrimaryData(
      extPrimaryDataConnection: Set[ExtPrimaryDataConnection]
  ): Unit = {
    val assetUuids: List[UUID] =
      extPrimaryDataConnection.toList.flatMap(_.getPrimaryDataAssets.asScala)

    if (assetUuids.size != assetUuids.toSet.size) {
      val duplicateAssets =
        assetUuids.groupBy(identity).filter(_._2.size > 1).mkString(",")
      throw ServiceException(
        s"Multiple data connections provide primary data for assets: $duplicateAssets"
      )
    }
  }
}
