/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.api.data.em.ExtEmDataConnection
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.api.data.results.ExtResultDataConnection
import edu.ie3.simona.api.data.{
  ExtDataConnection,
  ExtInputDataConnection,
  ExtOutputDataConnection,
}
import edu.ie3.simona.service.results.ExtResultDataProvider
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.{ActorRef => ClassicRef}

final case class ExtSimSetupData(
    extSimAdapters: Iterable[ClassicRef],
    extPrimaryDataServices: Map[ExtPrimaryDataConnection, ClassicRef],
    extDataServices: Map[ExtInputDataConnection, ClassicRef],
    extResultListeners: Map[ExtResultDataConnection, ActorRef[
      ExtResultDataProvider.Request
    ]],
    extDataListener: Map[_ <: ExtOutputDataConnection, ClassicRef],
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
      ref: ActorRef[ExtResultDataProvider.Request],
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
  def apply(): ExtSimSetupData = ExtSimSetupData(
    Iterable.empty,
    Map.empty,
    Map.empty,
    Map.empty,
    Map.empty,
  )
}
