/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import org.apache.pekko.actor.ActorRef
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.primary.ExtPrimaryDataService
import edu.ie3.simona.service.results.ExtResultDataService
import edu.ie3.simona.api.data.ExtData
import edu.ie3.simona.api.data.primarydata.ExtPrimaryData
import edu.ie3.simona.api.data.results.ExtResultData
import edu.ie3.simona.api.data.ev.ExtEvData

final case class ExtSimSetupData(
    extSimAdapters: Iterable[ActorRef],
    extDataServices: Map[Class[_], ActorRef],
    extDatas: Set[ExtData],
) {

  def evDataService: Option[ActorRef] =
    extDataServices.get(classOf[ExtEvDataService])

  def extPrimaryDataService: Option[ActorRef] =
    extDataServices.get(classOf[ExtPrimaryDataService])

  def extResultDataService: Option[ActorRef] =
    extDataServices.get(classOf[ExtResultDataService])

  def extEvData: Option[ExtEvData] = {
    extDatas.collectFirst { case extData: ExtEvData => extData }
  }
  def extPrimaryData: Option[ExtPrimaryData] = {
    extDatas.collectFirst { case extData: ExtPrimaryData => extData }
  }
  def extResultData: Option[ExtResultData] = {
    extDatas.collectFirst { case extData: ExtResultData => extData }
  }
}
