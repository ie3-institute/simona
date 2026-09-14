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
    finishedKeys: Set[K],
    receivedData: Map[K, Seq[V]],
) {
  def isComplete: Boolean = expectedKeys.isEmpty

  def hasCompleted: Boolean = finishedKeys.nonEmpty

  def nonComplete: Boolean = expectedKeys.nonEmpty

  def expects(key: K): Boolean = expectedKeys.contains(key)

  def getExpected(key: K): Int = expectedKeys.getOrElse(key, 0)

  def getFinished: (Map[K, Seq[V]], ReceiveMultiDataMap[K, V]) = {
    val data = finishedKeys.map { key => key -> receivedData(key) }.toMap

    (
      data,
      copy(
        receivedData = receivedData.removedAll(finishedKeys),
        finishedKeys = Set.empty,
      ),
    )
  }

  def addData[A](
      key: K,
      value: V,
  ): ReceiveMultiDataMap[K, V] = {
    if !expectedKeys.contains(key) && !receivedData.contains(key) then {
      log.warn(
        s"Received value $value for key $key, but no data has been expected or received for this key."
      )
    }

    val count = expectedKeys.getOrElse(key, 1) - 1

    val newValue = receivedData.get(key) match {
      case Some(values) =>
        values.appended(value)
      case None =>
        Seq(value)
    }

    if count == 0 then {
      copy(
        expectedKeys = expectedKeys.removed(key),
        finishedKeys = finishedKeys + key,
        receivedData = receivedData.updated(key, newValue),
      )
    } else {
      copy(
        expectedKeys = expectedKeys.updated(key, count),
        receivedData = receivedData.updated(key, newValue),
      )
    }
  }

  def addExpectedKeys(keys: Set[K]): ReceiveMultiDataMap[K, V] =
    addExpectedKeys(keys.map(key => key -> 1).toMap)

  def addExpectedKeys(keys: Map[K, Int]): ReceiveMultiDataMap[K, V] = {
    val (add, remove) = keys.partition(_._2 > 0)
    val updated = (expectedKeys ++ add).removedAll(remove.keys)
    copy(expectedKeys = updated)
  }

  def getExpectedKeys: Set[K] = expectedKeys.keySet
  def getExpected: Map[K, Int] = expectedKeys
}

object ReceiveMultiDataMap {

  private val log: Logger = LoggerFactory.getLogger("ReceiveMultiDataMap")

  def apply[K, V](
      expectedKeys: Set[K]
  ): ReceiveMultiDataMap[K, V] =
    apply(expectedKeys.map(key => key -> 1).toMap)

  def apply[K, V](
      expectedKeys: Map[K, Int]
  ): ReceiveMultiDataMap[K, V] =
    ReceiveMultiDataMap(
      expectedKeys = expectedKeys,
      finishedKeys = Set.empty,
      receivedData = Map.empty,
    )

  def empty[K, V]: ReceiveMultiDataMap[K, V] =
    ReceiveMultiDataMap(
      expectedKeys = Map.empty,
      finishedKeys = Set.empty,
      receivedData = Map.empty,
    )

}
