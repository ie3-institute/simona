/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.api.data.connection.{
  ExtEvDataConnection,
  ExtInputDataConnection,
  ExtPrimaryDataConnection,
}
import edu.ie3.simona.api.ontology.DataMessageFromExt
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.api.simulation.{ExtSimAdapterData, ExtSimulation}
import edu.ie3.simona.api.{ExtLinkInterface, ExtSimAdapter}
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.{SchedulerMessage, ServiceMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.service.primary.ExtPrimaryServiceWorker
import edu.ie3.simona.service.primary.ExtPrimaryServiceWorker.InitExtPrimaryData
import edu.ie3.simona.util.SimonaConstants.PRE_INIT_TICK
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}
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
    * @return
    *   An [[ExtSimSetupData]] that holds information regarding the external
    *   data connections as well as the actor references of the created
    *   services.
    */
  def setupExtSim(
      extLinks: List[ExtLinkInterface],
      args: Array[String],
  )(using
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
  ): ExtSimSetupData = extLinks.zipWithIndex.foldLeft(ExtSimSetupData.apply) {
    case (extSimSetupData, (extLink, index)) =>
      // external simulation always needs at least an ExtSimAdapter
      val extSimAdapter = context.spawn(
        ExtSimAdapter(scheduler),
        s"ExtSimAdapter-$index",
      )

      // creating the adapter data
      given extSimAdapterData: ExtSimAdapterData =
        new ExtSimAdapterData(extSimAdapter, args)

      Try {
        // sets up the external simulation
        extLink.setup(extSimAdapterData)
        extLink.getExtSimulation
      }.map { extSimulation =>
        // send init data right away, init activation is scheduled
        extSimAdapter ! ExtSimAdapter.Create(
          extSimAdapterData,
          ScheduleLock.singleKey(context, scheduler, PRE_INIT_TICK),
        )

        // setup data services that belong to this external simulation
        val updatedSetupData = connect(extSimulation, extSimSetupData, index)

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
    * @param index
    *   Index of the external link interface.
    * @param context
    *   The actor context of this actor system.
    * @param scheduler
    *   The scheduler of simona.
    * @param extSimAdapterData
    *   The adapter data for the external simulation.
    * @return
    *   An updated [[ExtSimSetupData]].
    */
  private[setup] def connect(
      extSimulation: ExtSimulation,
      extSimSetupData: ExtSimSetupData,
      index: Int,
  )(using
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapterData: ExtSimAdapterData,
  ): ExtSimSetupData = {
    given extSimAdapter: ActorRef[ControlResponseMessageFromExt] =
      extSimAdapterData.getAdapter

    // the data connections this external simulation provides
    val connections = extSimulation.getDataConnections.asScala

    log.info(
      s"Setting up external simulation `${extSimulation.getSimulationName}` with the following data connections: ${connections.map(_.getClass).mkString(",")}."
    )

    val updatedSetupData = connections.foldLeft(extSimSetupData) {
      case (setupData, connection) =>
        connection match {
          case extPrimaryDataConnection: ExtPrimaryDataConnection =>
            val serviceRef = context.spawn(
              ExtPrimaryServiceWorker(scheduler),
              "ExtPrimaryDataService_$index",
            )

            setupService(
              extPrimaryDataConnection,
              serviceRef,
              InitExtPrimaryData.apply,
            )

            extSimSetupData.update(extPrimaryDataConnection, serviceRef)

          case extEvDataConnection: ExtEvDataConnection =>
            if setupData.evDataConnection.nonEmpty then {
              throw ServiceException(
                s"Trying to connect another EvDataConnection. Currently only one is allowed."
              )
            }

            val serviceRef = context.spawn(
              ExtEvDataService(scheduler),
              "ExtEvDataService",
            )

            setupService(
              extEvDataConnection,
              serviceRef,
              InitExtEvData.apply,
            )

            extSimSetupData.update(extEvDataConnection, serviceRef)

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

  /** Method for setting up an external service.
    *
    * @param extInputDataConnection
    *   the data connection.
    * @param serviceRef
    *   The reference of the service.
    * @param initData
    *   Data to initialize the service.
    * @param context
    *   The actor context of this actor system.
    * @param scheduler
    *   The scheduler of simona.
    * @param extSimAdapter
    *   The adapter for the external simulation.
    * @tparam C
    *   Type of [[ExtInputDataConnection]].
    * @return
    *   The reference to the service.
    */
  private[setup] def setupService[C <: ExtInputDataConnection[?]](
      extInputDataConnection: C,
      serviceRef: ActorRef[ServiceMessage | DataMessageFromExt],
      initData: C => InitializeServiceStateData,
  )(using
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapter: ActorRef[ControlResponseMessageFromExt],
  ): Unit = {
    serviceRef ! ServiceMessage.Create(
      initData(extInputDataConnection),
      ScheduleLock.singleKey(
        context,
        scheduler,
        PRE_INIT_TICK,
      ),
    )

    extInputDataConnection.setActorRefs(
      serviceRef,
      extSimAdapter,
    )
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

    if duplicateAssets.nonEmpty then {
      throw ServiceException(
        s"Multiple data connections provide primary data for assets: ${duplicateAssets.mkString(",")}"
      )
    }
  }
}
