/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.data

import edu.ie3.simona.agent.grid.GridAgent
import org.apache.pekko.actor.typed.ActorRef

/** Case class that holds all received data.
  *
  * @param inferiorGridMap
  *   Map: inferior grid to received data.
  * @tparam T
  *   Type of data.
  */
final case class AwaitingData[T](
    inferiorGridMap: Map[ActorRef[GridAgent.Request], Option[T]]
) {

  /** Returns true if congestion data from inferior grids is expected and no
    * data was received yet.
    */
  def notDone: Boolean =
    inferiorGridMap.values.exists(_.isEmpty)

  /** Returns the received values
    */
  def values: Iterable[T] = inferiorGridMap.values.flatten.toSeq

  /** Return the mapping of all received values. This should only be called if
    * [[notDone]] == false.
    */
  def mappedValues: Map[ActorRef[GridAgent.Request], T] =
    inferiorGridMap.flatMap { case (ref, option) =>
      option.map(value => ref -> value)
    }

  /** Method for updating the data with received data.
    * @param sender
    *   Actor ref of the sender.
    * @param data
    *   Send data.
    * @return
    *   An updated object.
    */
  def update(sender: ActorRef[GridAgent.Request], data: T): AwaitingData[T] =
    handleReceivingData(Vector((sender, data)))

  /** Method for updating the data with the received data.
    *
    * @param receivedData
    *   Data that was received.
    * @return
    *   An updated copy of this data.
    */
  def handleReceivingData(
      receivedData: Seq[(ActorRef[GridAgent.Request], T)]
  ): AwaitingData[T] = {
    val mappedData = receivedData.map { case (ref, value) =>
      ref -> Some(value)
    }.toMap
    copy(inferiorGridMap = inferiorGridMap ++ mappedData)
  }
}

object AwaitingData {
  def apply[T](
      inferiorGridRefs: Set[ActorRef[GridAgent.Request]]
  ): AwaitingData[T] = {
    AwaitingData(inferiorGridRefs.map(ref => ref -> None).toMap)
  }
}
