/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

/** Map that holds data that an actor is expected to receive over time and
  * efficiently determines if all expected data has been received.
  *
  * @param keyToInferior
  *   The mapping of a key and a [[ReceiveDataMap]].
  * @tparam K
  *   The type of the key
  * @tparam V
  *   The type of the value
  */
final case class ReceiveMultiDataMap[K, V](
    private val maxTime: Int,
    private val lastFinished: Option[Int],
    private val unfinishedTimes: Set[Int],
    private val finished: Set[Int],
    private val keyToTime: Map[K, Int],
    private val keyToInferior: Map[K, ReceiveDataMap[K, V]],
) {
  def isComplete: Boolean = lastFinished.isDefined

  def nonComplete: Boolean = lastFinished.isEmpty

  def addKeys(keyMap: Map[K, Set[K]]): ReceiveMultiDataMap[K, V] = {

    keyMap.foreach { case (key, _) =>
      if (keyToInferior.contains(key)) {
        throw new RuntimeException(s"Received key $key is already registered!")
      }
    }

    val time = maxTime + 1
    val updatedKeyToTime = keyToTime ++ keyMap.keySet.map(_ -> time)

    val updated: Map[K, ReceiveDataMap[K, V]] = keyMap.map {
      case (key, inferiorKeys) =>
        key -> ReceiveDataMap(inferiorKeys)
    }

    copy(
      maxTime = time,
      unfinishedTimes = unfinishedTimes + time,
      keyToTime = updatedKeyToTime,
      keyToInferior = keyToInferior ++ updated,
    )
  }

  def addData(
      key: K,
      inferiorKey: K,
      value: V,
  ): ReceiveMultiDataMap[K, V] = {

    if (!keyToInferior.contains(key))
      throw new RuntimeException(
        s"Received value $value for key $key, but no data has been expected for this key."
      )

    val updated = keyToInferior(key).addData(inferiorKey, value)

    val updatedKeyToTime = if (updated.isComplete) {
      keyToTime.removed(key)
    } else keyToTime

    val updatedUnfinished = updatedKeyToTime.values.toSet
    val updatedFinished = unfinishedTimes.diff(updatedUnfinished)

    copy(
      lastFinished = updatedFinished.maxOption,
      unfinishedTimes = updatedUnfinished,
      finished = updatedFinished,
      keyToTime = updatedKeyToTime,
      keyToInferior = keyToInferior.updated(key, updated),
    )
  }

  def removeLastFinished(): (Map[K, Map[K, V]], ReceiveMultiDataMap[K, V]) = {
    val map = keyToInferior.filter { case (_, map) => map.isComplete }.map {
      case (k, v) => k -> v.receivedData
    }

    (map, copy(keyToInferior = keyToInferior.removedAll(map.keySet)))
  }

  def getExpectedKeys(key: K): Set[K] =
    keyToInferior.get(key).map(_.getExpectedKeys).getOrElse(Set.empty)

}

object ReceiveMultiDataMap {

  def apply[K, V](keyMap: Map[K, Set[K]]): ReceiveMultiDataMap[K, V] =
    empty.addKeys(keyMap)

  def empty[K, V]: ReceiveMultiDataMap[K, V] =
    ReceiveMultiDataMap(-1, None, Set.empty, Set.empty, Map.empty, Map.empty)

}
