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
import edu.ie3.simona.service.ev.ExtEvDataService
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.ActorRef
import edu.ie3.simona.service.primary.ExtPrimaryDataService
import edu.ie3.simona.service.results.ExtResultDataProvider
import org.apache.pekko.actor.ActorRef

final case class ExtSimSetupData(
    extSimAdapters: Iterable[ClassicRef],
    extDataServices: Map[Class[_], ClassicRef],
    extDatas: Set[ExtData],
    extScheduler: Option[ActorRef[SchedulerMessage]],
) {

  def evDataService: Option[ClassicRef] =
    extDataServices.get(classOf[ExtEvDataService])

  def extPrimaryDataService: Option[ActorRef] =
    extDataServices.get(classOf[ExtPrimaryDataService])

  def extResultDataService: Option[ActorRef] =
    extDataServices.get(ExtResultDataProvider.getClass)

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
