/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.ontology.trigger.Trigger.{
  InitializeExtSimAdapterTrigger,
  InitializeServiceTrigger,
  InitializeTrigger
}
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData

final case class ExtSimSetupData(
    extSimAdapters: Iterable[(SimonaActorRef, InitializeExtSimAdapterTrigger)],
    extDataServices: Iterable[(SimonaActorRef, InitializeServiceTrigger[_])]
) {

  def allActorsAndInitTriggers: Iterable[(SimonaActorRef, InitializeTrigger)] =
    extSimAdapters ++ extDataServices

  def evDataService: Option[SimonaActorRef] =
    extDataServices.collectFirst {
      case (actorRef, InitializeServiceTrigger(InitExtEvData(_))) =>
        actorRef
    }
}
