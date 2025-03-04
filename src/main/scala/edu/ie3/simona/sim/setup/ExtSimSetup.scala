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
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ScheduleServiceActivation
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
import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
import org.apache.pekko.actor.typed.scaladsl.adapter.{
  TypedActorContextOps,
  TypedActorRefOps,
}
import org.apache.pekko.actor.typed.{ActorRef, Scheduler}
import org.apache.pekko.actor.{Props, ActorRef => ClassicRef}
import org.apache.pekko.util.{Timeout => PekkoTimeout}
import org.slf4j.{Logger, LoggerFactory}

import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}
import scala.jdk.DurationConverters.ScalaDurationOps
import scala.util.{Failure, Success, Try}

object ExtSimSetup {

  private val log: Logger = LoggerFactory.getLogger(ExtSimSetup.getClass)

  /** Method to set up all external simulations defined via the given
    * [[ExtLinkInterface]]s.
    * @param extLinks
    *   Interfaces that hold information regarding external simulations.
    * @param args
    *   The main args the simulation is started with.
    * @param context
    *   The actor context of this actor system.
    * @param scheduler
    *   The scheduler of simona.
    * @param resolution
    *   The resolution of the power flow.
    * @return
    *   An [[ExtSimSetupData]] that holds information regarding the external
    *   data connections as well as the actor references of the created
    *   services.
    */
  def setupExtSim(
      extLinks: List[ExtLinkInterface],
      args: Array[String],
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      startTime: ZonedDateTime,
      resolution: FiniteDuration,
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

      Try {
        // sets up the external simulation
        extLink.setup(extSimAdapterData)
        extLink.getExtSimulation
      }.map { extSimulation =>
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
        updatedSetupData.update(extSimAdapter)
      } match {
        case Failure(exception) =>
          log.warn(
            s"External simulation of link '${extLink.getClass.getSimpleName}' could not be loaded, due to the following exception: ",
            exception,
          )

          extSimSetupData
        case Success(setupData) => setupData
      }
  }

  /** Method for connecting a given external simulation.
    * @param extSimulation
    *   To connect.
    * @param extSimSetupData
    *   That contains information about all external simulations.
    * @param context
    *   The actor context of this actor system.
    * @param scheduler
    *   The scheduler of simona.
    * @param extSimAdapterData
    *   The adapter data for the external simulation.
    * @param resolution
    *   The resolution of the power flow.
    * @return
    *   An updated [[ExtSimSetupData]].
    */
  private[setup] def connect(
      extSimulation: ExtSimulation,
      extSimSetupData: ExtSimSetupData,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapterData: ExtSimAdapterData,
      startTime: ZonedDateTime,
      resolution: FiniteDuration,
  ): ExtSimSetupData = {
    implicit val extSimAdapter: ClassicRef = extSimAdapterData.getAdapter

    // the data connections this external simulation provides
    val connections = extSimulation.getDataConnections.asScala

    log.info(
      s"Setting up external simulation `${extSimulation.getSimulationName}` with the following data connections: ${connections.map(_.getClass).mkString(",")}."
    )

    val updatedSetupData = connections.foldLeft(extSimSetupData) {
      case (setupData, connection) =>
        connection match {
          case extPrimaryDataConnection: ExtPrimaryDataConnection =>
            extPrimaryDataSetup(setupData, extPrimaryDataConnection)

          case extEmDataConnection: ExtEmDataConnection =>
            if (setupData.emDataConnection.nonEmpty) {
              throw ServiceException(
                s"Trying to connect another EmDataConnection. Currently only one is allowed."
              )
            }

            if (extEmDataConnection.getControlledEms.isEmpty) {
              log.warn(
                s"External em connection $extEmDataConnection is not used, because there are no controlled ems present!"
              )
              setupData
            } else {
              val serviceRef = setupInputService(
                extEmDataConnection,
                ExtEmDataService.props,
                "ExtEmDataService",
                InitExtEmData,
              )

              extSimSetupData.update(extEmDataConnection, serviceRef)
            }

          case extEvDataConnection: ExtEvDataConnection =>
            if (setupData.evDataConnection.nonEmpty) {
              throw ServiceException(
                s"Trying to connect another EvDataConnection. Currently only one is allowed."
              )
            }

            val serviceRef = setupInputService(
              extEvDataConnection,
              ExtEvDataService.props,
              "ExtEvDataService",
              InitExtEvData,
            )

            extSimSetupData.update(extEvDataConnection, serviceRef)

          case extResultDataConnection: ExtResultDataConnection =>
            extResultDataSetup(setupData, extResultDataConnection)

          case otherConnection =>
            log.warn(
              s"There is currently no implementation for the connection: $otherConnection."
            )
            setupData
        }
    }

    // validate data
    validatePrimaryData(updatedSetupData.primaryDataConnections)

    updatedSetupData
  }

  /** Method for setting up an external service, that provides input data.
    * @param extInputDataConnection
    *   the data connection.
    * @param props
    *   Function to create the service.
    * @param name
    *   Of the actor.
    * @param initData
    *   Data to initialize the service.
    * @param context
    *   The actor context of this actor system.
    * @param scheduler
    *   The scheduler of simona.
    * @param extSimAdapter
    *   The adapter for the external simulation.
    * @tparam T
    *   Type of [[ExtInputDataConnection]].
    * @return
    *   The reference to the service.
    */
  private[setup] def setupInputService[T <: ExtInputDataConnection](
      extInputDataConnection: T,
      props: ClassicRef => Props,
      name: String,
      initData: T => InitializeServiceStateData,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapter: ClassicRef,
  ): ClassicRef = {

    val extDataService = context.toClassic.simonaActorOf(
      props(scheduler.toClassic),
      name,
    )

    extDataService ! SimonaService.Create(
      initData(extInputDataConnection),
      ScheduleLock.singleKey(
        context,
        scheduler,
        INIT_SIM_TICK,
      ),
    )

    extInputDataConnection.setActorRefs(
      extDataService,
      extSimAdapter,
    )

    extDataService
  }

  /** Method to set up an external primary data service.
    * @param extSimSetupData
    *   that contains information about all external simulations
    * @param extPrimaryDataConnection
    *   the data connection
    * @param context
    *   the actor context of this actor system
    * @param scheduler
    *   the scheduler of simona
    * @param extSimAdapter
    *   the adapter for the external simulation
    * @return
    *   an updated [[ExtSimSetupData]]
    */
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

    extSimSetupData update (extPrimaryDataConnection, extPrimaryDataService)
  }

  /** Method to set up an external result data service.
    *
    * @param extSimSetupData
    *   that contains information about all external simulations
    * @param extResultDataConnection
    *   the data connection
    * @param context
    *   the actor context of this actor system
    * @param scheduler
    *   the scheduler of simona
    * @param extSimAdapter
    *   the adapter for the external simulation
    * @param startTime
    *   Of the simulation.
    * @param resolution
    *   Of the power flow.
    * @return
    *   an updated [[ExtSimSetupData]]
    */
  private[setup] def extResultDataSetup(
      extSimSetupData: ExtSimSetupData,
      extResultDataConnection: ExtResultDataConnection,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapter: ClassicRef,
      startTime: ZonedDateTime,
      resolution: FiniteDuration,
  ): ExtSimSetupData = {
    val extResultDataProvider =
      context.spawn(
        ExtResultDataProvider(scheduler, startTime),
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

    val powerFlowResolution = resolution.toSeconds

    extResultDataProvider ! ExtResultDataProvider.Create(
      InitExtResultData(extResultDataConnection, powerFlowResolution),
      ScheduleLock.singleKey(
        context,
        scheduler,
        INIT_SIM_TICK,
      ),
    )

    extSimSetupData update (extResultDataConnection, extResultDataProvider)
  }

  /** Method for validating the external primary data connections.
    * @param extPrimaryDataConnection
    *   All external primary data connections.
    */
  private[setup] def validatePrimaryData(
      extPrimaryDataConnection: Seq[ExtPrimaryDataConnection]
  ): Unit = {
    // check primary data for duplicate assets
    val duplicateAssets: Iterable[UUID] =
      extPrimaryDataConnection
        .flatMap(_.getPrimaryDataAssets.asScala)
        .groupBy(identity)
        .collect { case (uuid, values) if values.size > 1 => uuid }

    if (duplicateAssets.nonEmpty) {
      throw ServiceException(
        s"Multiple data connections provide primary data for assets: ${duplicateAssets.mkString(",")}"
      )
    }
  }
}
