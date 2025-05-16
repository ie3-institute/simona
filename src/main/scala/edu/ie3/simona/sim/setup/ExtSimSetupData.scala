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
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.ServiceRef
import org.apache.pekko.actor.ActorRef as ClassicRef
import org.apache.pekko.actor.typed.ActorRef

/** Case class that holds information regarding the external data connections as
  * well as the actor references of the created services.
  *
  * @param extSimAdapters
  *   All adapters to external simulations.
  * @param extPrimaryDataServices
  *   Seq: external primary data connections to service references.
  * @param extDataServices
  *   Seq: external input data connection to service references.
  * @param extResultListeners
  *   Map: external result data connections to result data providers.
  */
final case class ExtSimSetupData(
    extSimAdapters: Iterable[ClassicRef],
    extPrimaryDataServices: Seq[(ExtPrimaryDataConnection, ServiceRef)],
    extDataServices: Seq[(? <: ExtInputDataConnection, ServiceRef)],
    extResultListeners: Seq[(ExtResultDataConnection, ServiceRef)],
) {

  private[setup] def update(
      connection: ExtPrimaryDataConnection,
      ref: ServiceRef,
  ): ExtSimSetupData =
    copy(extPrimaryDataServices =
      extPrimaryDataServices ++ Seq((connection, ref))
    )

  private[setup] def update(
      connection: ExtInputDataConnection,
      ref: ServiceRef,
  ): ExtSimSetupData = connection match {
    case primaryConnection: ExtPrimaryDataConnection =>
      update(primaryConnection, ref)
    case _ =>
      copy(extDataServices = extDataServices ++ Seq((connection, ref)))
  }

  private[setup] def update(
      connection: ExtResultDataConnection,
      ref: ServiceRef,
  ): ExtSimSetupData =
    copy(extResultListeners = extResultListeners ++ Seq((connection, ref)))

  private[setup] def update(extSimAdapter: ClassicRef): ExtSimSetupData =
    copy(extSimAdapters = extSimAdapters ++ Set(extSimAdapter))

  def evDataService: Option[ServiceRef] =
    extDataServices.collectFirst {
      case (_: ExtEvDataConnection, ref: ServiceRef) => ref
    }

  def emDataService: Option[ServiceRef] =
    extDataServices.collectFirst {
      case (_: ExtEmDataConnection, ref: ServiceRef) => ref
    }

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
    Seq.empty,
    Seq.empty,
  )
}
