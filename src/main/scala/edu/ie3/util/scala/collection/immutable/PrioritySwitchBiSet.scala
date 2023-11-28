/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.immutable

import scala.collection.immutable

/** TODO scaladoc
  * @param orderedValues
  * @param valueToIndex
  * @param queue
  * @param back
  * @tparam K
  * @tparam V
  */
final case class PrioritySwitchBiSet[K, V](
    private val orderedValues: immutable.Vector[V],
    private val valueToIndex: Map[V, Int],
    private val queue: immutable.SortedMap[K, immutable.SortedSet[Int]],
    private val back: Map[V, K]
) {
  def headKeyOption: Option[K] =
    queue.headOption.map { case (key, _) => key }

  def headKeyIndexOption: Option[Int] =
    queue.headOption.flatMap { case (_, set) => set.headOption }

  def indexOf(value: V): Option[Int] = valueToIndex.get(value)

  def set(key: K, value: V): PrioritySwitchBiSet[K, V] =
    dequeue(value).add(key, value)

  private def dequeue(value: V): PrioritySwitchBiSet[K, V] =
    back
      .get(value)
      .map { key =>
        val updatedBack = back.removed(value)

        val updatedQueue = valueToIndex
          .get(value)
          .map { i =>
            val newSet =
              queue
                .get(key)
                .map(_.excl(i))
                .getOrElse(immutable.SortedSet.empty[Int])

            if (newSet.isEmpty)
              queue.removed(key)
            else
              queue.updated(key, newSet)
          }
          .getOrElse(queue)

        copy(
          queue = updatedQueue,
          back = updatedBack
        )
      }
      .getOrElse(this)

  private def add(key: K, value: V): PrioritySwitchBiSet[K, V] = {
    // add value to orderedValues and valueToIndex, if not present already
    val (updatedStruct, i) = orderedValues.indexOf(value) match {
      case -1 =>
        val newIndex = orderedValues.size
        (
          copy(
            orderedValues = orderedValues.appended(value),
            valueToIndex = valueToIndex.updated(value, newIndex)
          ),
          newIndex
        )
      case i =>
        (this, i)
    }

    // add value to key
    val updatedSet =
      queue.getOrElse(key, immutable.SortedSet.empty[Int]).incl(i)

    updatedStruct.copy(
      queue = queue.updated(key, updatedSet),
      back = back.updated(value, key)
    )
  }

  def nextValueFor(key: K): Option[(V, PrioritySwitchBiSet[K, V])] = {
    queue
      .get(key)
      .flatMap { set =>
        set.headOption.map(orderedValues).map((set, _))
      }
      .map { case (set, firstValue) =>
        val updatedQueue =
          if (set.size == 1)
            queue.removed(key)
          else
            queue.updated(key, set.drop(1))

        (firstValue, copy(queue = updatedQueue))
      }
  }

  /** Tests whether there is no value for any key in the queue.
    *
    * @return
    *   True if the queue is empty
    */
  def isEmpty: Boolean = queue.isEmpty

  /** Tests whether there is any value for any key in the queue.
    *
    * @return
    *   True if the queue is non-empty
    */
  def nonEmpty: Boolean = queue.nonEmpty

  def values: Vector[V] = orderedValues

}

object PrioritySwitchBiSet {
  def empty[K: Ordering, V]: PrioritySwitchBiSet[K, V] = PrioritySwitchBiSet(
    Vector.empty,
    Map.empty,
    immutable.SortedMap.empty,
    Map.empty
  )
}
