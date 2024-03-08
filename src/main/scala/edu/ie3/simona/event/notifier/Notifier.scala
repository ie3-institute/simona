/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.notifier

import edu.ie3.simona.event.Event
import org.apache.pekko.actor.ActorRef

trait Notifier {

  def listener: Iterable[ActorRef]

  def notifyListener(event: Event): Unit = {
    listener.foreach(listener => listener ! event)
  }
}
