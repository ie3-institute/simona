/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event

import edu.ie3.simona.io.result.ResultEntitySink

trait ResultMessage extends Event

object ResultMessage {
  private[event] final case class SinkResponse(
      response: Map[Class[_], ResultEntitySink]
  ) extends ResultMessage

  private[event] final case class Failed(ex: Exception) extends ResultMessage

  private[event] final case object Stop extends ResultMessage

}
