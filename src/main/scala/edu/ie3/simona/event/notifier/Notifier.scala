/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.notifier

import akka.actor.{Actor, ActorRef}
import edu.ie3.simona.event.Event

trait Notifier extends Actor {

  def listener: Iterable[ActorRef]

  def notifyListener(event: Event): Unit = {
    listener.foreach(listener => listener ! event)
  }
}
