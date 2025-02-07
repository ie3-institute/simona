/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.api.data.ExtDataConnection
import edu.ie3.simona.api.data.em.ExtEmDataConnection
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.api.data.results.ExtResultDataConnection
import edu.ie3.simona.service.em.ExtEmDataService
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.primary.ExtPrimaryDataService
import edu.ie3.simona.service.results.ExtResultDataProvider
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.{ActorRef => ClassicRef}

final case class ExtSimSetupData(
                                  extSimAdapters: Iterable[ClassicRef],
                                  extDataServices: Map[Class[_], ClassicRef],
                                  extDataListener: Map[Class[_], ActorRef[ExtResultDataProvider.Request]],
                                  extDatas: Set[ExtDataConnection],
) {

  def evDataService: Option[ClassicRef] =
    extDataServices.get(classOf[ExtEvDataService])

  def extPrimaryDataService: Option[ClassicRef] =
    extDataServices.get(classOf[ExtPrimaryDataService])

  def extEmDataService: Option[ClassicRef] =
    extDataServices.get(classOf[ExtEmDataService])

  def extResultDataService: Option[ActorRef[ExtResultDataProvider.Request]] =
    extDataListener.get(ExtResultDataProvider.getClass)

  def extEvData: Option[ExtEvDataConnection] = {
    extDatas.collectFirst { case extData: ExtEvDataConnection => extData }
  }
  def extPrimaryData: Option[ExtPrimaryDataConnection] = {
    extDatas.collectFirst { case extData: ExtPrimaryDataConnection => extData }
  }

  def extEmData: Option[ExtEmDataConnection] = {
    extDatas.collectFirst { case extData: ExtEmDataConnection => extData }
  }

  def extResultData: Option[ExtResultDataConnection] = {
    extDatas.collectFirst { case extData: ExtResultDataConnection => extData }
  }

}
