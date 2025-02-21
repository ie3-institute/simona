/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.ontology.messages.services.{EvMessage, ServiceMessage}
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import edu.ie3.simona.service.ev.ExtEvDataService
import org.apache.pekko.actor.typed.ActorRef

final case class ExtSimSetupData(
    extSimAdapters: Iterable[ClassicRef],
    extDataServices: Map[Class[_], ActorRef[_ >: ServiceMessage]],
) {

  def evDataService: Option[ActorRef[EvMessage]] =
    extDataServices
      .get(classOf[ExtEvDataService.type])
      .map { case service: ActorRef[EvMessage] => service }
}
