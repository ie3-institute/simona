/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

/** Map that holds data that an actor is expected to receive over time and
  * efficiently determines if all expected data has been received.
  *
  * @tparam K
  *   The type of the key
  * @tparam V
  *   The type of the value
  */
final case class ReceiveMultiDataMap[K, V](
    private val maxTime: Int,
    private val keyToTime: Map[K, Int],
    private val values: Map[Int, ReceiveDataMap[K, V]],
) {
  def isComplete: Boolean = values(maxTime).isComplete

  def nonComplete: Boolean = values(maxTime).nonComplete

  def addKeys(keys: Set[K]): ReceiveMultiDataMap[K, V] = {
    val time = maxTime + 1

    copy(
      maxTime = time,
      keyToTime = keyToTime ++ keys.map(_ -> time),
      values = values + (time -> ReceiveDataMap(keys)),
    )
  }

  def addData(
      key: K,
      value: V,
  ): ReceiveMultiDataMap[K, V] = {
    val time = keyToTime(key)
    val updated = values(time).addData(key, value)

    copy(values = values + (time -> updated))
  }

  def removeLastFinished(): (Map[K, V], ReceiveMultiDataMap[K, V]) = {
    val time = maxTime - 1
    val map = values(maxTime).receivedData

    (map, copy(maxTime = time, values = values.removed(maxTime)))
  }
}

object ReceiveMultiDataMap {
  def apply[K, V](keys: Set[K]): ReceiveMultiDataMap[K, V] =
    empty.addKeys(keys)

  def empty[K, V]: ReceiveMultiDataMap[K, V] =
    ReceiveMultiDataMap(-1, Map.empty, Map.empty)

}
