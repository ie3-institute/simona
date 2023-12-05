/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

case class DataStore[K, V](private val expectedKeys: Set[K], data: Map[K, V]) {

  def isComplete: Boolean = expectedKeys.isEmpty

  def add(key: K, value: V): DataStore[K, V] = {
    copy(
      expectedKeys = expectedKeys.excl(key),
      data.updated(key, value)
    )
  }

}

object DataStore {
  def apply[K, V](expectedKeys: Iterable[K]): DataStore[K, V] = {
    DataStore(expectedKeys.toSet, Map.empty[K, V])
  }
}
