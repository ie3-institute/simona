/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.api.data.ExtInputDataConnection
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.api.simulation.ontology.ControlResponseMessageFromExt
import edu.ie3.simona.api.simulation.{ExtSimAdapterData, ExtSimulation}
import edu.ie3.simona.api.{ExtLinkInterface, ExtSimAdapter}
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.util.SimonaConstants.PRE_INIT_TICK
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.duration.FiniteDuration
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
      resolution: FiniteDuration,
  ): ExtSimSetupData = extLinks.zipWithIndex.foldLeft(ExtSimSetupData.apply) {
    case (extSimSetupData, (extLink, index)) =>
      // external simulation always needs at least an ExtSimAdapter
      val extSimAdapter = context.spawn(
        ExtSimAdapter(scheduler),
        s"ExtSimAdapter-$index",
      )

      val controlMessageAdapter = context.spawn(
        ExtSimAdapter.controlMessageAdapter(extSimAdapter),
        s"ExtSimAdapter-$index-external",
      )

      // creating the adapter data
      implicit val extSimAdapterData: ExtSimAdapterData =
        new ExtSimAdapterData(controlMessageAdapter, args)

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
      resolution: FiniteDuration,
  ): ExtSimSetupData = {
    implicit val extSimAdapter: ActorRef[ControlResponseMessageFromExt] =
      extSimAdapterData.getAdapter

    // the data connections this external simulation provides
    val connections = extSimulation.getDataConnections.asScala

    log.info(
      s"Setting up external simulation `${extSimulation.getSimulationName}` with the following data connections: ${connections.map(_.getClass).mkString(",")}."
    )

    val updatedSetupData = connections.foldLeft(extSimSetupData) {
      case (setupData, connection) =>
        connection match {
          case extEvDataConnection: ExtEvDataConnection =>
            if (setupData.evDataConnection.nonEmpty) {
              throw ServiceException(
                s"Trying to connect another EvDataConnection. Currently only one is allowed."
              )
            }

            val serviceRef = setupInputService(
              extEvDataConnection,
              ExtEvDataService.apply(scheduler),
              ExtEvDataService.adapter,
              "ExtEvDataService",
              InitExtEvData,
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

  /** Method for setting up an external service, that provides input data.
    *
    * @param extInputDataConnection
    *   the data connection.
    * @param behavior
    *   The initial behavior of the service.
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
  private[setup] def setupInputService[
      T <: ExtInputDataConnection,
      M >: ServiceMessage,
  ](
      extInputDataConnection: T,
      behavior: Behavior[M],
      adapterToExt: ActorRef[M] => Behavior[DataMessageFromExt],
      name: String,
      initData: T => InitializeServiceStateData,
  )(implicit
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapter: ActorRef[ControlResponseMessageFromExt],
  ): ActorRef[M] = {
    val extDataService = context.spawn(
      behavior,
      name,
    )

    extDataService ! ServiceMessage.Create(
      initData(extInputDataConnection),
      ScheduleLock.singleKey(
        context,
        scheduler,
        PRE_INIT_TICK,
      ),
    )

    val adapter = context.spawn(
      adapterToExt(extDataService),
      s"$name-adapter-to-external",
    )

    extInputDataConnection.setActorRefs(
      adapter,
      extSimAdapter,
    )

    extDataService
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
