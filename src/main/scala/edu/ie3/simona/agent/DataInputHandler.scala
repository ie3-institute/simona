/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent

import edu.ie3.simona.agent.DataInputHandler.ReceivedData
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  DataMessage,
  DataProvision,
  NoDataProvision,
}
import edu.ie3.simona.service.Data
import edu.ie3.simona.service.Data.SecondaryData
import org.apache.pekko.actor.typed.ActorRef

/** This class holds received data, knows what data is expected and can thus
  * decide whether all input requirements have been fulfilled.
  *
  * @param expectedData
  *   Map of service actor reference to the tick at which data is expected next.
  *   When data is received, the next tick is updated here.
  * @param receivedData
  *   Map of service actor reference to received data. Only data received at the
  *   current tick is stored here.
  */
final case class DataInputHandler(
    expectedData: Map[ActorRef[ServiceMessage], Long],
    receivedData: Map[ActorRef[ServiceMessage], ReceivedData],
) {

  /** Clears out the received data.
    *
    * @return
    *   An updated input handler.
    */
  def clear(): DataInputHandler =
    copy(receivedData = Map.empty)

  /** Handles a received [[DataMessage]] by storing the message and updating the
    * expected data that remains to be received.
    *
    * @param msg
    *   The received data message.
    * @return
    *   An updated input handler.
    */
  def handleDataMessage(
      msg: DataMessage
  ): DataInputHandler = {

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

  /** Determines whether all expected data for the current tick have been
    * received.
    *
    * @return
    *   Whether all expected data was received for the current tick.
    */
  def allMessagesReceived(currentTick: Long): Boolean =
    expectedData.forall { case (_, nextTick) =>
      nextTick > currentTick
    }

  /** Determines whether there has been new data received for the current tick,
    * which would mean that re-determination of model parameters should happen.
    *
    * @return
    *   Whether there's new data for the current tick or not.
    */
  def hasNewData(currentTick: Long): Boolean =
    receivedData.values.exists(
      _.tick == currentTick
    )

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
  def getDataUpdatedTick: Option[Long] =
    expectedData.values.maxOption

  /** Returns all received input data.
    *
    * @return
    *   The received data.
    */
  def getData: Seq[Data] =
    receivedData.values.map(_.data).toSeq

  /** Returns all received secondary data.
    *
    * @return
    *   The received secondary data.
    */
  def getSecondaryData: Seq[SecondaryData] =
    receivedData.values.flatMap {
      case ReceivedData(data: SecondaryData, _) =>
        Some(data)
      case _ => None
    }.toSeq

}

object DataInputHandler {

  /** Holds received data in combination with the tick at which it was received.
    */
  final case class ReceivedData(data: Data, tick: Long)

  /** Creates a new [[DataInputHandler]] with the given expected data and empty
    * received data field.
    *
    * @param expectedData
    *   Map of service actor reference to the tick at which data is expected
    *   next.
    * @return
    *   A new [[DataInputHandler]].
    */
  def apply(
      expectedData: Map[ActorRef[ServiceMessage], Long]
  ): DataInputHandler =
    new DataInputHandler(
      expectedData = expectedData,
      receivedData = Map.empty,
    )
}
