/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import org.apache.pekko.actor.{Actor, DeadLetter, Props}
import com.typesafe.scalalogging.LazyLogging

/** Simply reads the message, it receives
  */
final class DeadLetterListener extends Actor with LazyLogging {
  override def receive: Receive = { case msg: DeadLetter =>
    logger.error(s"Received the following message from ${sender()}:\n$msg")
  }
}

object DeadLetterListener {
  def props(): Props = Props(new DeadLetterListener)
}
