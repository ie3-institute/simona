/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.api.data.ExtInputDataConnection
import edu.ie3.simona.api.data.em.ExtEmDataConnection
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.api.data.results.ExtResultDataConnection
import edu.ie3.simona.ontology.messages.services.{EvMessage, ServiceMessage}
import edu.ie3.simona.service.ev.ExtEvDataService
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.{ActorRef => ClassicRef}

/** Case class that holds information regarding the external data connections as
  * well as the actor references of the created services.
  *
  * @param extSimAdapters
  *   All adapters to external simulations.
  * @param extPrimaryDataServices
  *   Map: external primary data connections to service references.
  * @param extDataServices
  *   Map: external input data connection to service references.
  * @param extResultListeners
  *   Map: external result data connections to result data providers.
  */
final case class ExtSimSetupData(
    extSimAdapters: Iterable[ClassicRef],
    extPrimaryDataServices: Seq[(ExtPrimaryDataConnection, ClassicRef)],
    extDataServices: Map[Class[_], ActorRef[_ >: ServiceMessage]],
    extResultListeners: Seq[(ExtResultDataConnection, ActorRef[_])],
) {

  private[setup] def update(
      connection: ExtPrimaryDataConnection,
      ref: ClassicRef,
  ): ExtSimSetupData =
    copy(extPrimaryDataServices =
      extPrimaryDataServices ++ Seq((connection, ref))
    )

  private[setup] def update(
      connection: ExtInputDataConnection,
      ref: ClassicRef,
  ): ExtSimSetupData = connection match {
    case primaryConnection: ExtPrimaryDataConnection =>
      update(primaryConnection, ref)
    case _ =>
      copy(extDataServices = extDataServices ++ Seq((connection, ref)))
  }

  private[setup] def update(
      connection: ExtResultDataConnection,
      ref: ActorRef[_],
  ): ExtSimSetupData =
    copy(extResultListeners = extResultListeners ++ Seq((connection, ref)))

  private[setup] def update(extSimAdapter: ClassicRef): ExtSimSetupData =
    copy(extSimAdapters = extSimAdapters ++ Set(extSimAdapter))

  def evDataService: Option[ClassicRef] =
    extDataServices.collectFirst { case (_: ExtEvDataConnection, ref) => ref }

  def emDataService: Option[ClassicRef] =
    extDataServices.collectFirst { case (_: ExtEmDataConnection, ref) => ref }

  def evDataService: Option[ActorRef[EvMessage]] =
    extDataServices
      .get(classOf[ExtEvDataService.type])
      .map { case service: ActorRef[EvMessage] => service }

  def evDataConnection: Option[ExtEvDataConnection] =
    extDataServices.collectFirst { case (connection: ExtEvDataConnection, _) =>
      connection
    }

  def emDataConnection: Option[ExtEmDataConnection] =
    extDataServices.collectFirst { case (connection: ExtEmDataConnection, _) =>
      connection
    }

  def primaryDataConnections: Seq[ExtPrimaryDataConnection] =
    extPrimaryDataServices.map {
      case (connection: ExtPrimaryDataConnection, _) => connection
    }

  def resultDataConnections: Seq[ExtResultDataConnection] =
    extResultListeners.map { case (connection: ExtResultDataConnection, _) =>
      connection
    }
}

object ExtSimSetupData {

  /** Returns an empty [[ExtSimSetupData]].
    */
  def apply: ExtSimSetupData = ExtSimSetupData(
    Iterable.empty,
    Seq.empty,
    Map.empty,
    Seq.empty,
  )
}
