/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import org.apache.pekko.actor.ActorRef
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.primary.{ExtPrimaryDataService, ExtPrimaryServiceWorker}

final case class ExtSimSetupData(
    extSimAdapters: Iterable[ActorRef],
    extDataServices: Map[Class[_], ActorRef]
) {

  def evDataService: Option[ActorRef] =
    extDataServices.get(classOf[ExtEvDataService])

  def extPrimaryDataService: Option[ActorRef] = extDataServices.get(classOf[ExtPrimaryDataService])

}
