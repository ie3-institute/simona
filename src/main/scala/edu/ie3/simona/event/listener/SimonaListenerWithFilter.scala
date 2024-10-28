/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import org.apache.pekko.actor.ActorRef
import edu.ie3.simona.event.Event
import edu.ie3.simona.logging.SimonaActorLogging

/** Implementations can be registered at simulation setup and will then receive
  * and process [[Event]] s from other Actors
  *
  * @param eventsToProcess
  *   Option of the events to process in this simulation. To make processing
  *   possible, events have to be implemented in processEvent <br> <br> None ->
  *   All implemented events will be processed <br> Some(List.empty) -> No
  *   events will be processed <br> Some(List(...)) -> All implemented events of
  *   this List will be processed
  */
abstract class SimonaListenerWithFilter(eventsToProcess: Option[List[String]])
    extends SimonaListener
    with SimonaActorLogging {

  /** Introduce handling for [[Event]] s via
    * [[edu.ie3.simona.event.listener.SimonaListenerWithFilter.receiveEvent]] DO
    * NOT OVERRIDE THIS METHOD IN IMPLEMENTATION! Use [[processEvent()]]
    * instead!
    *
    * @return
    */
  override def receive: Receive = {
    case e: Event => receiveEvent(e)
    case unknownMessage =>
      log.warning(s"Received unknown message: $unknownMessage")
  }

  /** Filters received event and processes it only if it is in
    * [[edu.ie3.simona.event.listener.SimonaListenerWithFilter.eventsToProcess]]
    * (or eventsToProcess is None)
    */
  private def receiveEvent: Receive = { case event: Event =>
    eventsToProcess match {
      case None => processEvent(event, sender())
      case Some(events) if events.contains(event.id) =>
        processEvent(event, sender())
      case _ =>
        log.debug(
          "Skipping event {} as it is not in the list of events to process.",
          event.id,
        )
    }
  }

  protected def processEvent(event: Event, ref: ActorRef): Unit

}
