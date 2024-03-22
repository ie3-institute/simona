/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import org.apache.pekko.actor.{ActorRef => ClassicRef}
import edu.ie3.simona.service.ev.ExtEvDataService

final case class ExtSimSetupData(
    extSimAdapters: Iterable[ClassicRef],
    extDataServices: Map[Class[_], ClassicRef],
) {

  def evDataService: Option[ClassicRef] =
    extDataServices.get(classOf[ExtEvDataService])
}
