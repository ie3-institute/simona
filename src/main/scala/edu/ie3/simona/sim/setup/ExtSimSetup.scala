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
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
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
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}
import scala.jdk.DurationConverters._
import scala.util.{Failure, Success, Try}

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
    *   to connect
    * @param extSimSetupData
    *   that contains information about all external simulations
    * @param context
    *   the actor context of this actor system
    * @param scheduler
    *   the scheduler of simona
    * @param extSimAdapterData
    *   the adapter data for the external simulation
    * @param simonaConfig
    *   the config
    * @return
    *   an updated [[ExtSimSetupData]]
    */
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

          case otherConnection =>
            log.warn(
              s"There is currently no implementation for the connection: $otherConnection."
            )
            setupData
        }
    }

    // validate data
    validatePrimaryData(updatedSetupData.extPrimaryDataServices.keySet)

    updatedSetupData
  }

  /** Method for setting up an external service, that provides input data.
    * @param extSimSetupData
    *   that contains information about all external simulations
    * @param extInputDataConnection
    *   the data connection
    * @param props
    *   function to create the service
    * @param name
    *   of the actor
    * @param initData
    *   data to initialize the service
    * @param context
    *   the actor context of this actor system
    * @param scheduler
    *   the scheduler of simona
    * @param extSimAdapterData
    *   the adapter data for the external simulation
    * @tparam T
    *   type of [[ExtInputDataConnection]]
    * @return
    *   an updated [[ExtSimSetupData]]
    */
  private[setup] def setupInputService[T <: ExtInputDataConnection](
      extSimSetupData: ExtSimSetupData,
      extInputDataConnection: T,
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
      initData(extInputDataConnection),
      ScheduleLock.singleKey(
        context,
        scheduler,
        INIT_SIM_TICK,
      ),
    )

    extInputDataConnection.setActorRefs(
      extDataService,
      extSimAdapterData.getAdapter,
    )

    extSimSetupData.update(extInputDataConnection, extDataService)
  }

  /** Method for validating the external primary data connections.
    * @param extPrimaryDataConnection
    *   all external primary data connections
    */
  private[setup] def validatePrimaryData(
      extPrimaryDataConnection: Set[ExtPrimaryDataConnection]
  ): Unit = {
    // check primary data for duplicate assets
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
