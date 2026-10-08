/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.api.data.connection.*
import edu.ie3.simona.api.loading.ProvidedData
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.api.simulation.ExtSimulation
import edu.ie3.simona.event.listener.ResultListener
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
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

import java.util.UUID
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}

object AddonSetup {

  private given log: Logger = LoggerFactory.getLogger(AddonSetup.getClass)

  /** Method to set up all addons present in the provided data.
    *
    * @param providedData
    *   The provided data containing external simulations and external result
    *   listeners.
    * @param addonSetupData
    *   The addon setup data.
    * @param jarName
    *   The name of the jar that provided the data.
    * @param context
    *   The actor context of this actor system.
    * @param scheduler
    *   The scheduler of simona.
    * @param resultProxy
    *   The result service proxy.
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
  ): AddonSetupData = {

    val addonSetupData =
      providedData.extSimulations.asScala.foldLeft(AddonSetupData.apply) {
        case (data, extSimulation) =>
          val extSimName = extSimulation.getSimulationName

          // external simulation always needs at least an ExtSimAdapter
          given extSimAdapter: ActorRef[ExtSimAdapter.Request] =
            context.spawn(
              ExtSimAdapter(scheduler),
              s"ExtSimAdapter_$extSimName",
            )

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
          val updatedSetupData = connect(extSimulation, data, extSimName)

          // starting external simulation
          new Thread(extSimulation, s"External simulation $extSimName")
            .start()

          // updating the data with newly connected external simulation
          updatedSetupData.updateAdapter(extSimAdapter)
      }

    providedData.extListeners.asScala.zipWithIndex.foldLeft(addonSetupData) {
      case (data, (extListener, idx)) =>
        val extResultEventListener = context.spawn(
          ResultListener.external(extListener),
          s"ExtResultListener_${extListener.getClass}_$idx",
        )

        // add the external listener to the proxy
        resultProxy ! AddListener(extResultEventListener)

        data.update(extListener, extResultEventListener)
    }
  }

  /** Method for connecting a given external simulation.
    *
    * @param extSimulation
    *   To connect.
    * @param extSimSetupData
    *   That contains information about all external simulations.
    * @param extSimName
    *   Name of the external simulation that provided the connection.
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
      extSimName: String,
  )(using
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapter: ActorRef[ControlResponseMessageFromExt],
      resultProxy: ActorRef[ResultServiceProxy.Message],
  ): AddonSetupData = {
    // the data connections this external simulation provides
    val connections = extSimulation.getDataConnections.asScala

    log.info(
      s"Setting up external simulation `${extSimulation.getSimulationName}` with the following data connections: ${connections.map(_.getClass).mkString(",")}."
    )

    val updatedSetupData = connections.foldLeft(extSimSetupData) {
      case (setupData, connection) => connect(connection, setupData, extSimName)
    }

    // validate data
    validatePrimaryData(updatedSetupData.primaryDataConnections)

    updatedSetupData
  }

  private[setup] def connect(
      dataConnection: ExtDataConnection,
      extSimSetupData: AddonSetupData,
      extSimName: String,
  )(using
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      extSimAdapter: ActorRef[ControlResponseMessageFromExt],
      resultProxy: ActorRef[ResultServiceProxy.Message],
  ): AddonSetupData = dataConnection match {
    case extPrimaryDataConnection: ExtPrimaryDataConnection =>
      val serviceRef = context.spawn(
        ExtPrimaryServiceWorker(
          scheduler,
          InitExtPrimaryData(extPrimaryDataConnection),
          ScheduleLock.singleKey(context, scheduler, INIT_SIM_TICK),
        ),
        "ExtPrimaryDataService_$extSimName",
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
            InitExtEmData(scheduler, extEmDataConnection),
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
        s"ExtResultListener_$extSimName",
      )

      // add the external listener to the proxy
      resultProxy ! AddListener(extResultEventListener)

      extSimSetupData.update(extResultListener, extResultEventListener)

    case otherConnection =>
      throw new InitializationException(
        s"There is currently no implementation for the connection: $otherConnection."
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
