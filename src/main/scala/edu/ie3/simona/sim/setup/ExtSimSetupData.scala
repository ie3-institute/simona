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
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.{ActorRef => ClassicRef}

/** Case class that holds information regarding the external data connections as
  * well as the actor references of the created services
  *
  * @param extSimAdapters
  *   all adapters to external simulations
  * @param extPrimaryDataServices
  *   map: external primary data connections to service references
  * @param extDataServices
  *   map: external input data connection to service references
  * @param extResultListeners
  *   map: external result data connections to result data providers
  */
final case class ExtSimSetupData(
    extSimAdapters: Iterable[ClassicRef],
    extPrimaryDataServices: Map[ExtPrimaryDataConnection, ClassicRef],
    extDataServices: Map[ExtInputDataConnection, ClassicRef],
    extResultListeners: Map[ExtResultDataConnection, ActorRef[_]],
) {

  private[setup] def +(
      connection: ExtPrimaryDataConnection,
      ref: ClassicRef,
  ): ExtSimSetupData =
    copy(extPrimaryDataServices =
      extPrimaryDataServices ++ Map(connection -> ref)
    )

  private[setup] def +(
      connection: ExtInputDataConnection,
      ref: ClassicRef,
  ): ExtSimSetupData =
    copy(extDataServices = extDataServices ++ Map(connection -> ref))

  private[setup] def +(
      connection: ExtResultDataConnection,
      ref: ActorRef[_],
  ): ExtSimSetupData =
    copy(extResultListeners = extResultListeners ++ Map(connection -> ref))

  def evDataService: Option[ClassicRef] =
    extDataServices.collectFirst { case (_: ExtEvDataConnection, ref) => ref }

  def extEmDataService: Option[ClassicRef] =
    extDataServices.collectFirst { case (_: ExtEmDataConnection, ref) => ref }

  def extEvDataConnection: Option[ExtEvDataConnection] =
    extDataServices.collectFirst { case (connection: ExtEvDataConnection, _) =>
      connection
    }

  def extEmDataConnection: Option[ExtEmDataConnection] =
    extDataServices.collectFirst { case (connection: ExtEmDataConnection, _) =>
      connection
    }

  def extResultDataConnection: Set[ExtResultDataConnection] =
    extResultListeners.keySet
}

object ExtSimSetupData {

  /** Returns an empty [[ExtSimSetupData]].
    */
  def apply(): ExtSimSetupData = ExtSimSetupData(
    Iterable.empty,
    Map.empty,
    Map.empty,
    Map.empty,
  )
}
