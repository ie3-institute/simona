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
  DataInputMessage,
  DataProvision,
  NoDataProvision,
}
import edu.ie3.simona.agent.participant2.ParticipantInputHandler.ReceivedData

/** Holds active tick and received data, knows what data is expected and can
  * thus decide whether all input requirements have been fulfilled
  *
  * @param expectedData
  *   Map of service actor reference to the tick at which data is expected next.
  *   When data is received, the next tick is updated here.
  * @param receivedData
  *   Map of service actor reference to received data. Here, the most recent
  *   received data is saved, which might have been received at past ticks.
  * @param activation
  *   The activation message with which the participant agent was activated, if
  *   applicable. This is emptied after each tick is completed.
  */
final case class ParticipantInputHandler(
    expectedData: Map[ClassicRef, Long],
    receivedData: Map[ClassicRef, Option[ReceivedData]],
    activation: Option[ActivationRequest],
) {

  def handleActivation(
      activation: ActivationRequest
  ): ParticipantInputHandler =
    copy(activation = Some(activation))

  def completeActivation(): ParticipantInputHandler =
    copy(activation = None)

  def handleDataInputMessage(
      msg: DataInputMessage
  ): ParticipantInputHandler = {

    val updatedReceivedData =
      msg match {
        case DataProvision(tick, serviceRef, data, _) =>
          receivedData +
            (serviceRef -> Some(ReceivedData(data, tick)))
        case _: NoDataProvision =>
          receivedData
      }

    val updatedExpectedData = msg.nextDataTick
      .map { nextTick =>
        expectedData + (msg.serviceRef -> nextTick)
      }
      .getOrElse {
        expectedData - msg.serviceRef
      }

    copy(
      expectedData = updatedExpectedData,
      receivedData = updatedReceivedData,
    )
  }

  /** Determines whether all expected messages for the current tick (activation
    * and data input messages) have been received.
    *
    * @return
    *   Whether all expected messages were received for the current tick
    */
  def allMessagesReceived: Boolean = activation.exists { activationMsg =>
    expectedData.forall { case (_, nextTick) =>
      nextTick > activationMsg.tick
    }
  }

  /** Determines whether there has been new data received for the current
    * activation, which would mean that recalculation should happen
    *
    * @return
    *   Whether there's new data for the current tick or not
    */
  def hasNewData: Boolean =
    activation.exists { activationMsg =>
      receivedData.values.exists(
        _.exists(_.tick == activationMsg.tick)
      )
    }

  def getNextActivationTick: Option[Long] =
    expectedData.values.minOption

  /** Useful for the first calculation after initialization, when all data needs
    * to be present before first calculation
    *
    * @return
    *   The last tick at which data is expected currently
    */
  def getLastActivationTick: Option[Long] =
    expectedData.values.maxOption

  def getData: Seq[Data] =
    receivedData.values.flatten.map(_.data).toSeq

}

object ParticipantInputHandler {

  final case class ReceivedData(data: Data, tick: Long)

  def apply(expectedData: Map[ClassicRef, Long]): ParticipantInputHandler =
    new ParticipantInputHandler(
      expectedData = expectedData,
      receivedData = Map.empty,
      activation = None,
    )
}
