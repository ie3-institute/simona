/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

/** Map that holds data that an actor is expected to receive over time and
  * efficiently determines if all expected data has been received.
  * @param expectedKeys
  *   The keys for which data is expected
  * @param receivedData
  *   The map holding all received data so far
  * @tparam K
  *   The type of the key
  * @tparam V
  *   The type of the value
  */
final case class ReceiveDataMap[K, V](
    private val expectedKeys: Set[K],
    receivedData: Map[K, V],
) {
  def isComplete: Boolean = expectedKeys.isEmpty

  def nonComplete: Boolean = expectedKeys.nonEmpty

  def addData(
      key: K,
      value: V,
  ): ReceiveDataMap[K, V] = {

    if !expectedKeys.contains(key) then
      throw new RuntimeException(
        s"Received value $value for key $key, but no data has been expected for this key."
      )

    copy(
      expectedKeys = expectedKeys.excl(key),
      receivedData.updated(key, value),
    )
  }

}

object ReceiveDataMap {

  def apply[K, V](
      expectedKeys: Set[K]
  ): ReceiveDataMap[K, V] =
    ReceiveDataMap(
      expectedKeys = expectedKeys,
      receivedData = Map.empty,
    )

  def empty[K, V]: ReceiveDataMap[K, V] =
    ReceiveDataMap(
      expectedKeys = Set.empty,
      receivedData = Map.empty,
    )

}
