/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.ontology.messages.SchedulerMessage
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import edu.ie3.simona.api.data.ExtData
import edu.ie3.simona.api.data.ev.ExtEvData
import edu.ie3.simona.api.data.primarydata.ExtPrimaryData
import edu.ie3.simona.api.data.results.ExtResultData
import edu.ie3.simona.api.data.em.ExtEmData
import edu.ie3.simona.service.em.ExtEmDataService
import edu.ie3.simona.service.ev.ExtEvDataService
import org.apache.pekko.actor.typed.ActorRef
import edu.ie3.simona.service.primary.ExtPrimaryDataService
import edu.ie3.simona.service.results.ExtResultDataProvider
import edu.ie3.simona.service.results.ExtResultDataProvider.Request

final case class ExtSimSetupData(
                                  extSimAdapters: Iterable[ClassicRef],
                                  extDataServices: Map[Class[_], ClassicRef],
                                  extDataListener: Map[Class[_], ActorRef[ExtResultDataProvider.Request]],
                                  extDatas: Set[ExtData],
                                  extScheduler: Option[ActorRef[SchedulerMessage]],
) {

  def evDataService: Option[ClassicRef] =
    extDataServices.get(classOf[ExtEvDataService])

  def extPrimaryDataService: Option[ClassicRef] =
    extDataServices.get(classOf[ExtPrimaryDataService])

  def extEmDataService: Option[ClassicRef] =
    extDataServices.get(classOf[ExtEmDataService])

  def extResultDataService: Option[ActorRef[ExtResultDataProvider.Request]] =
    extDataListener.get(ExtResultDataProvider.getClass)

  def extEvData: Option[ExtEvData] = {
    extDatas.collectFirst { case extData: ExtEvData => extData }
  }
  def extPrimaryData: Option[ExtPrimaryData] = {
    extDatas.collectFirst { case extData: ExtPrimaryData => extData }
  }

  def extEmData: Option[ExtEmData] = {
    extDatas.collectFirst { case extData: ExtEmData => extData }
  }

  def extResultData: Option[ExtResultData] = {
    extDatas.collectFirst { case extData: ExtResultData => extData }
  }
}
