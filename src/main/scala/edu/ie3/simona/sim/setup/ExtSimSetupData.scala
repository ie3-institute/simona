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
    extPrimaryData: Map[ExtPrimaryDataConnection, ClassicRef],
    extDataServices: Map[Class[_ <: ExtInputDataConnection], ClassicRef],
    extResultListeners: Map[ExtResultDataConnection, ActorRef[
      ExtResultDataProvider.Request
    ]],
    extDataListener: Map[_ <: ExtOutputDataConnection, ClassicRef],
    extDatas: Set[ExtDataConnection],
) {

  def evDataService: Option[ClassicRef] =
    extDataServices.get(classOf[ExtEvDataConnection])

  def extEmDataService: Option[ClassicRef] =
    extDataServices.get(classOf[ExtEmDataConnection])

  def extEvData: Option[ExtEvDataConnection] = {
    extDatas.collectFirst { case extData: ExtEvDataConnection => extData }
  }

  def extEmData: Option[ExtEmDataConnection] = {
    extDatas.collectFirst { case extData: ExtEmDataConnection => extData }
  }

  def extResultData: Set[ExtResultDataConnection] = {
    extDatas.map { case extData: ExtResultDataConnection => extData }
  }

}
