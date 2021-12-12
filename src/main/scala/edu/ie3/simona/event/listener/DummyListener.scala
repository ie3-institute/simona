/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import akka.actor.{ActorRef, Props}
import edu.ie3.simona.event.Event

import scala.reflect.ClassTag

object DummyListener extends SimonaListenerCompanion {

  override def props[A: ClassTag](
      eventsToProcess: Option[List[String]] = None
  ): Props =
    Props(new DummyListener(eventsToProcess))
}

/** Listener implementation that logs all received events
  * @param eventsToProcess
  *   The events to process in this simulation
  */
class DummyListener(eventsToProcess: Option[List[String]] = None)
    extends SimonaListenerWithFilter(eventsToProcess) {

  /** Processes Event
    * @param event
    *   Event to process
    * @param sender
    *   ActorRef of the Event-message sender
    */
  def processEvent(event: Event, sender: ActorRef): Unit = {
    log.info(
      s"Received event $event with id ${event.id} for tick from $sender"
    )
  }
}
