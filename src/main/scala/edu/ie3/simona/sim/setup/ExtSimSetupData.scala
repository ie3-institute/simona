/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import akka.actor.{ActorRef, actorRef2Scala}
import edu.ie3.simona.ontology.trigger.Trigger.{
  InitializeExtSimAdapterTrigger,
  InitializeServiceTrigger,
  InitializeTrigger
}
import edu.ie3.simona.service.dcopf.ExtOpfDataService.InitExtOpfData
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData

final case class ExtSimSetupData(
    extSimAdapters: Iterable[(ActorRef, InitializeExtSimAdapterTrigger)],
    extDataServices: Iterable[(ActorRef, InitializeServiceTrigger[_])]
) {

  def allActorsAndInitTriggers: Iterable[(ActorRef, InitializeTrigger)] =
    extSimAdapters ++ extDataServices

  def evDataService: Option[ActorRef] =
    extDataServices.collectFirst {
      case (actorRef, InitializeServiceTrigger(InitExtEvData(_))) =>
        actorRef
    }

  def dcopfDataService: Option[ActorRef] = extDataServices.collectFirst {
    case (actorRef, InitializeServiceTrigger(InitExtOpfData(_, _, _))) =>
      actorRef
  }
}
