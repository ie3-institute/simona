/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import org.apache.pekko.actor.{ActorRef => ClassicRef}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ProvisionMessage

/** todo rather call ParticipantInputHandler? */
case class ParticipantDataCore(
    expectedData: Map[ClassicRef, Long],
    receivedData: Map[ClassicRef, Option[_ <: Data]],
    activeTick: Option[Long],
) {

  // holds active tick and received data,
  // knows what data is expected and can thus decide whether everything is complete

  // holds results as well? or no?

  def handleActivation(tick: Long): ParticipantDataCore = {
    // TODO
    this
  }

  def handleDataProvision(
      msg: ProvisionMessage[_ <: Data]
  ): ParticipantDataCore = {
    val updatedReceivedData = receivedData + (msg.serviceRef -> Some(msg.data))
    val updatedExpectedData = msg.nextDataTick
      .map { nextTick =>
        expectedData + (msg.serviceRef -> nextTick)
      }
      .getOrElse {
        expectedData - msg.serviceRef
      }

    copy(expectedData = updatedExpectedData, receivedData = updatedReceivedData)
  }

  def isComplete: Boolean = activeTick.nonEmpty && receivedData.forall {
    case (_, data) => data.nonEmpty
  }

  def getData: Seq[Data] =
    receivedData.values.flatten.toSeq

}
