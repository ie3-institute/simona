/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.simona.util.ReceiveMultiDataMap.log
import org.slf4j.{Logger, LoggerFactory}

final case class ReceiveMultiDataMap[K, V](
    private val expectedKeys: Map[K, Int],
    receivedData: Map[K, Seq[V]],
) {
  def isComplete: Boolean = expectedKeys.isEmpty

  def nonComplete: Boolean = expectedKeys.nonEmpty

  def expects(key: K): Boolean = expectedKeys.contains(key)

  def addData[A](
      key: K,
      value: V,
  ): ReceiveMultiDataMap[K, V] = {
    if !expectedKeys.contains(key) && !receivedData.contains(key) then {
      throw new RuntimeException(
        s"Received value $value for key $key, but no data has been expected or received for this key."
      )
    } else {
      val count = expectedKeys(key) - 1

      val newValue = receivedData.get(key) match {
        case Some(values) =>
          values.appended(value)
        case None =>
          Seq(value)
      }

      if count == 0 then {
        copy(
          expectedKeys = expectedKeys.removed(key),
          receivedData = receivedData.updated(key, newValue),
        )
      } else {
        copy(
          expectedKeys = expectedKeys.updated(key, count),
          receivedData = receivedData.updated(key, newValue),
        )
      }
    }
  }

  def addExpectedKeys(keys: Map[K, Int]): ReceiveMultiDataMap[K, V] =
    copy(expectedKeys = expectedKeys ++ keys.filter(_._2 > 0))

  def getExpectedKeys: Set[K] = expectedKeys.keySet

}

object ReceiveMultiDataMap {

  private val log: Logger = LoggerFactory.getLogger("ReceiveMultiDataMap")

  def apply[K, V](
      expectedKeys: Map[K, Int]
  ): ReceiveMultiDataMap[K, V] =
    ReceiveMultiDataMap(
      expectedKeys = expectedKeys,
      receivedData = Map.empty,
    )

  def empty[K, V]: ReceiveMultiDataMap[K, V] =
    ReceiveMultiDataMap(
      expectedKeys = Map.empty,
      receivedData = Map.empty,
    )

}
