/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import akka.actor.ActorRef
import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.ontology.trigger.Trigger.InitializeServiceTrigger
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData

final case class ExtSimSetupData(
    extSimAdapters: Iterable[(ActorRef, ExtSimAdapter.Init)],
    extDataServices: Iterable[(ActorRef, InitializeServiceTrigger[_])]
) {

  def evDataService: Option[ActorRef] =
    extDataServices.collectFirst {
      case (actorRef, InitializeServiceTrigger(InitExtEvData(_))) =>
        actorRef
    }
}
