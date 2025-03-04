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

/** This class holds received data, knows what data is expected and can thus
  * decide whether all input requirements have been fulfilled.
  *
  * @param expectedData
  *   Map of service actor reference to the tick at which data is expected next.
  *   When data is received, the next tick is updated here.
  * @param receivedData
  *   Map of service actor reference to received data. Only data received at the
  *   current tick is stored here.
  * @param activation
  *   The activation message with which the participant agent was activated, if
  *   applicable. This is emptied after each tick is completed.
  */
final case class ParticipantInputHandler(
    expectedData: Map[ClassicRef, Long],
    receivedData: Map[ClassicRef, ReceivedData],
    activation: Option[ActivationRequest],
) {

  /** Handles a received [[ActivationRequest]] by storing the message.
    *
    * @param activation
    *   The activation.
    * @return
    *   An updated input handler.
    */
  def handleActivation(
      activation: ActivationRequest
  ): ParticipantInputHandler =
    copy(activation = Some(activation))

  /** Completes an activation by clearing out the stored activation message.
    *
    * @return
    *   An updated input handler.
    */
  def completeActivation(): ParticipantInputHandler =
    copy(activation = None, receivedData = Map.empty)

  /** Handles a received [[DataInputMessage]] by storing the message and
    * updating the expected data that remains to be received.
    *
    * @param msg
    *   The received data message.
    * @return
    *   An updated input handler.
    */
  def handleDataInputMessage(
      msg: DataInputMessage
  ): ParticipantInputHandler = {

    val updatedReceivedData =
      msg match {
        case DataProvision(tick, serviceRef, data, _) =>
          receivedData +
            (serviceRef -> ReceivedData(data, tick))
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
    *   Whether all expected messages were received for the current tick.
    */
  def allMessagesReceived: Boolean = activation.exists { activationMsg =>
    expectedData.forall { case (_, nextTick) =>
      nextTick > activationMsg.tick
    }
  }

  /** Determines whether there has been new data received for the current
    * activation, which would mean that re-determination of model parameters
    * should happen.
    *
    * @return
    *   Whether there's new data for the current tick or not.
    */
  def hasNewData: Boolean =
    activation.exists { activationMsg =>
      receivedData.values.exists(
        _.tick == activationMsg.tick
      )
    }

  /** Returns the next tick at which input data is expected.
    *
    * @return
    *   The next data tick.
    */
  def getNextDataTick: Option[Long] =
    expectedData.values.minOption

  /** Returns the tick at which all input data has been updated. Useful for the
    * first calculation after initialization, when all data needs to be present
    * before first calculation.
    *
    * @return
    *   The tick at which all data has been updated once.
    */
  def getDataCompletedTick: Option[Long] =
    expectedData.values.maxOption

  /** Returns all received input data.
    *
    * @return
    *   The received data.
    */
  def getData: Seq[Data] =
    receivedData.values.map(_.data).toSeq

}

object ParticipantInputHandler {

  /** Holds received data in combination with the tick at which it was received.
    */
  final case class ReceivedData(data: Data, tick: Long)

  /** Creates a new [[ParticipantInputHandler]] with the given expected data and
    * empty received data and activation fields.
    *
    * @param expectedData
    *   Map of service actor reference to the tick at which data is expected
    *   next.
    * @return
    *   A new [[ParticipantInputHandler]].
    */
  def apply(expectedData: Map[ClassicRef, Long]): ParticipantInputHandler =
    new ParticipantInputHandler(
      expectedData = expectedData,
      receivedData = Map.empty,
      activation = None,
    )
}
