/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import org.apache.pekko.actor.{ActorRef => ClassicRef}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  ActivationRequest,
  ProvideData,
}

final case class ParticipantInputHandler(
    expectedData: Map[ClassicRef, Long],
    receivedData: Map[ClassicRef, Option[_ <: Data]],
    activation: Option[ActivationRequest],
) {

  // holds active tick and received data,
  // knows what data is expected and can thus decide whether everything is complete

  // holds results as well? or no?

  def handleActivation(
      activation: ActivationRequest
  ): ParticipantInputHandler = {
    copy(activation = Some(activation))
  }

  def completeActivity(): ParticipantInputHandler = {
    copy(activation = None)
  }

  def handleDataProvision(
      msg: ProvideData[_ <: Data]
  ): ParticipantInputHandler = {
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

  def isComplete: Boolean = activation.exists { activationMsg =>
    expectedData.forall { case (_, nextTick) =>
      nextTick > activationMsg.tick
    }
  }

  def getNextActivationTick: Option[Long] =
    expectedData.values.minOption

  def getData: Seq[Data] =
    receivedData.values.flatten.toSeq

}

object ParticipantInputHandler {

  def apply(expectedData: Map[ClassicRef, Long]): ParticipantInputHandler =
    new ParticipantInputHandler(
      expectedData = expectedData,
      receivedData = Map.empty,
      activation = None,
    )
}
