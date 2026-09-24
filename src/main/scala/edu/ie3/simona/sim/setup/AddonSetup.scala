/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import com.typesafe.config.Config
import edu.ie3.datamodel.models.input.container.JointGridContainer
import edu.ie3.simona.api.data.SetupData
import edu.ie3.simona.api.data.connection.*
import edu.ie3.simona.api.loading.{AddonLoader, JarLoader, ProvidedData}
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.api.simulation.ExtSimulation
import edu.ie3.simona.api.{ExtLinkInterface, ExtSimAdapter}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.listener.ResultListener
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.em.ExtEmDataService
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.service.primary.ExtPrimaryServiceWorker
import edu.ie3.simona.service.primary.ExtPrimaryServiceWorker.InitExtPrimaryData
import edu.ie3.simona.service.results.ResultServiceProxy.AddListener
import edu.ie3.simona.service.results.{ExtResultProvider, ResultServiceProxy}
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Path
import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}
import scala.util.{Failure, Success, Try}

object AddonSetup {

  private given log: Logger = LoggerFactory.getLogger(AddonSetup.getClass)

  /** Method to set up all addons present in the provided data.
    *
    * @param providedData
    *   The provided data containing external simulations and external result
    *   listeners.
    * @param context
    *   The actor context of this actor system.
    * @param scheduler
    *   The scheduler of simona.
    * @param resultProxy
    *   The result service proxy.
    * @param startTime
    *   The start time of the simulation.
    * @return
    *   An [[AddonSetupData]] that holds information regarding the external data
    *   connections as well as the actor references of the created services.
    */
  def setupAddons(
      providedData: ProvidedData
  )(using
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      resultProxy: ActorRef[ResultServiceProxy.Message],
      startTime: ZonedDateTime,
  ): AddonSetupData = {
    var index = 0

    val simSetupData =
      providedData.extSimulations.asScala.foldLeft(AddonSetupData.apply) {
        case (extSimSetupData, extSimulation) =>
          // external simulation always needs at least an ExtSimAdapter
          given extSimAdapter: ActorRef[ExtSimAdapter.Request] =
            context.spawn(
              ExtSimAdapter(scheduler),
              s"ExtSimAdapter-$index",
            )

          index += 1

          // creating the data connection
          val extSimDataConnection = new ExtSimDataConnection(extSimAdapter)

          // sets the data connection and the setup data explicitly
          extSimulation.setDataConnection(extSimDataConnection)

          // send init data right away, init activation is scheduled
          extSimAdapter ! ExtSimAdapter.Create(
            extSimDataConnection,
            ScheduleLock.singleKey(context, scheduler, PRE_INIT_TICK),
          )

          // setup data services that belong to this external simulation
          val updatedSetupData = connect(extSimulation, extSimSetupData, index)

          // starting external simulation
          new Thread(extSimulation, s"External simulation $index")
            .start()

          // updating the data with newly connected external simulation
          updatedSetupData.updateAdapter(extSimAdapter)
      }

    providedData.extListeners.asScala.foldLeft(simSetupData) {
      case (extSimSetupData, extListener) =>
        val extResultEventListener = context.spawn(
          ResultListener.external(extListener),
          s"ExtResultListener_$index",
        )
        index += 1

        // add the external listener to the proxy
        resultProxy ! AddListener(extResultEventListener)

        extSimSetupData.update(extListener, extResultEventListener)
    }
  }

  /** Method for connecting a given external simulation.
    *
    * @param extSimulation
    *   To connect.
    * @param extSimSetupData
    *   That contains information about all external simulations.
    * @param index
    *   Index of the external link interface.
    * @param context
    *   The actor context of this actor system.
    * @param scheduler
    *   The scheduler of SIMONA.
    * @param extSimAdapter
    *   The adapter for the external simulation.
    * @return
    *   An updated [[AddonSetupData]].
    */
  private[setup] def connect(
      extSimulation: ExtSimulation,
      extSimSetupData: AddonSetupData,
      index: Int,
  )(using
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapter: ActorRef[ControlResponseMessageFromExt],
      resultProxy: ActorRef[ResultServiceProxy.Message],
      startTime: ZonedDateTime,
  ): AddonSetupData = {
    // the data connections this external simulation provides
    val connections = extSimulation.getDataConnections.asScala

    log.info(
      s"Setting up external simulation `${extSimulation.getSimulationName}` with the following data connections: ${connections.map(_.getClass).mkString(",")}."
    )

    val updatedSetupData = connections.foldLeft(extSimSetupData) {
      case (setupData, connection) => connect(connection, setupData, index)
    }

    // validate data
    validatePrimaryData(updatedSetupData.primaryDataConnections)

    updatedSetupData
  }

  private[setup] def connect(
      dataConnection: ExtDataConnection,
      extSimSetupData: AddonSetupData,
      index: Int,
  )(using
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapter: ActorRef[ControlResponseMessageFromExt],
      resultProxy: ActorRef[ResultServiceProxy.Message],
      startTime: ZonedDateTime,
  ): AddonSetupData = dataConnection match {
    case extPrimaryDataConnection: ExtPrimaryDataConnection =>
      val serviceRef = context.spawn(
        ExtPrimaryServiceWorker(
          scheduler,
          InitExtPrimaryData(extPrimaryDataConnection),
          ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
        ),
        "ExtPrimaryDataService_$index",
      )

      extPrimaryDataConnection.setActorRefs(
        serviceRef,
        extSimAdapter,
      )

      extSimSetupData.update(extPrimaryDataConnection, serviceRef)

    case extEmDataConnection: ExtEmDataConnection =>
      if extSimSetupData.emDataService.nonEmpty then {
        throw ServiceException(
          s"Trying to connect another EmDataConnection. Currently only one is allowed."
        )
      }

      if extEmDataConnection.getControlledEms.isEmpty then {
        log.warn(
          s"External em connection $extEmDataConnection is not used, because there are no controlled ems present!"
        )
        extSimSetupData
      } else {
        val serviceRef = context.spawn(
          ExtEmDataService(
            scheduler,
            InitExtEmData(scheduler, extEmDataConnection, startTime),
            ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
          ),
          "ExtEmDataService",
        )

        extEmDataConnection.setActorRefs(
          serviceRef,
          extSimAdapter,
        )

        extSimSetupData.update(extEmDataConnection, serviceRef)
      }

    case extEvDataConnection: ExtEvDataConnection =>
      if extSimSetupData.evDataService.nonEmpty then {
        throw ServiceException(
          s"Trying to connect another EvDataConnection. Currently only one is allowed."
        )
      }

      val serviceRef = context.spawn(
        ExtEvDataService(
          scheduler,
          InitExtEvData(extEvDataConnection),
          ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
        ),
        "ExtEvDataService",
      )

      extEvDataConnection.setActorRefs(
        serviceRef,
        extSimAdapter,
      )

      extSimSetupData.update(extEvDataConnection, serviceRef)

    case extResultDataConnection: ExtResultDataConnection =>
      val extResultProvider = context.spawn(
        ExtResultProvider(
          extResultDataConnection,
          scheduler,
          resultProxy,
        ),
        s"ExtResultProvider",
      )

      extResultDataConnection.setActorRefs(
        extResultProvider,
        extSimAdapter,
      )

      extSimSetupData.update(extResultDataConnection, extResultProvider)

    case extResultListener: ExtResultListener =>
      val extResultEventListener = context.spawn(
        ResultListener.external(extResultListener),
        s"ExtResultListener_$index",
      )

      // add the external listener to the proxy
      resultProxy ! AddListener(extResultEventListener)

      extSimSetupData.update(extResultListener, extResultEventListener)

    case otherConnection =>
      log.warn(
        s"There is currently no implementation for the connection: $otherConnection."
      )
      extSimSetupData
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
