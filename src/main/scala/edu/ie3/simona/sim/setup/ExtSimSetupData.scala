/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.api.data.connection.*
import edu.ie3.simona.event.listener.ResultListener
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.service.em.ExtEmDataService
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.results.ExtResultProvider
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger

/** Case class that holds information regarding the external data connections as
  * well as the actor references of the created services.
  *
  * @param extSimAdapters
  *   All adapters to external simulations.
  * @param primaryDataServices
  *   Seq: external primary data connections to service references.
  * @param emDataService
  *   Option for an external em data service.
  * @param evDataService
  *   Option for an external ev data service.
  * @param resultListeners
  *   Seq: external result listeners.
  * @param resultProviders
  *   Seq: external result providers.
  */
final case class ExtSimSetupData(
    extSimAdapters: Iterable[ActorRef[ExtSimAdapter.Request]],
    primaryDataServices: Seq[
      (ExtPrimaryDataConnection, ActorRef[ServiceMessage])
    ],
    emDataService: Option[ActorRef[ExtEmDataService.Message]],
    evDataService: Option[ActorRef[ExtEvDataService.Message]],
    resultListeners: Seq[ActorRef[ResultListener.Message]],
    resultProviders: Seq[ActorRef[ExtResultProvider.Message]],
) {

  private[setup] def update(
      connection: ExtPrimaryDataConnection,
      ref: ActorRef[ServiceMessage],
  ): ExtSimSetupData =
    copy(primaryDataServices = primaryDataServices ++ Seq((connection, ref)))

  private[setup] def update(
      connection: ExtDataConnection,
      ref: ActorRef[?],
  )(using log: Logger): ExtSimSetupData = (connection, ref) match {
    case (
          primaryConnection: ExtPrimaryDataConnection,
          serviceRef: ActorRef[ServiceMessage],
        ) =>
      update(primaryConnection, serviceRef)
    case (
          _: ExtEmDataConnection,
          serviceRef: ActorRef[ExtEmDataService.Message],
        ) =>
      copy(emDataService = Some(serviceRef))
    case (
          _: ExtEvDataConnection,
          serviceRef: ActorRef[ExtEvDataService.Message],
        ) =>
      copy(evDataService = Some(serviceRef))
    case (_: ExtResultListener, serviceRef: ActorRef[ResultListener.Message]) =>
      copy(resultListeners = resultListeners ++ Seq(serviceRef))
    case (
          _: ExtResultDataConnection,
          serviceRef: ActorRef[ExtResultProvider.Message],
        ) =>
      copy(resultProviders = resultProviders ++ Seq(serviceRef))
    case (con, ref) =>
      log.warn(s"Cannot add service $ref with connection: $con")
      this
  }

  private[setup] def updateAdapter(
      extSimAdapter: ActorRef[ExtSimAdapter.Request]
  ): ExtSimSetupData =
    copy(extSimAdapters = extSimAdapters ++ Set(extSimAdapter))

  def primaryDataConnections: Seq[ExtPrimaryDataConnection] =
    primaryDataServices.map { case (connection: ExtPrimaryDataConnection, _) =>
      connection
    }

  def allServiceRefs: Iterable[ActorRef[?]] =
    Seq(
      emDataService,
      evDataService,
    ).flatten ++ resultListeners ++ resultProviders ++ primaryDataServices
      .map(_._2)
}

object ExtSimSetupData {

  /** Returns an empty [[ExtSimSetupData]].
    */
  def apply: ExtSimSetupData = ExtSimSetupData(
    Iterable.empty,
    Seq.empty,
    None,
    None,
    Seq.empty,
    Seq.empty,
  )
}
