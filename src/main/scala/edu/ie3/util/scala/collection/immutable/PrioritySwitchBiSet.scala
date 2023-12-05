/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.immutable

import scala.collection.immutable

/** Queue that is specialized at holding many values of type [[V]] for the same
  * key of type [[K]], while only allowing each value to be linked to one key.
  * Mathematically, the relation between keys and values is thus not univalent
  * (right-unique), but injective.
  *
  * In contrast to the [[PrioritySwitchBiSet]], this data structure always
  * returns values stored at the same key in a fixed order that is determined by
  * the first storage of a value for any key. Also, only one value can be
  * returned at a time for some key, namely the value that is in order after the
  * last returned value for the same key.
  *
  * @param orderedValues
  *   Vector that holds values in a fixed order, in which they are always
  *   returned for all keys. This vector is never cleared, thus there should be
  *   a limited number of values stored in this data structure only.
  * @param valueToIndex
  *   A reverse map to [[orderedValues]], linking every value to the index it
  *   has been stored at.
  * @param queue
  *   A sorted map that holds the keys and the indices to values stored in
  *   [[orderedValues]]. Value indices are also ordered numerically within
  *   sorted sets.
  * @param back
  *   A map that links values back to keys. Used to fastly ensure every value is
  *   only stored once.
  * @tparam K
  *   Type of the key
  * @tparam V
  *   Type of the value
  */
final case class PrioritySwitchBiSet[K, V](
    private val orderedValues: immutable.Vector[V],
    private val valueToIndex: Map[V, Int],
    private val queue: immutable.SortedMap[K, immutable.SortedSet[Int]],
    private val back: Map[V, K]
) {

  /** Get the first key of the queue, if the queue is not empty.
    *
    * @return
    *   The first key
    */
  def headKeyOption: Option[K] =
    queue.headOption.map { case (key, _) => key }

  /** Get the first index of the first key of the queue, if the queue is not
    * empty.
    *
    * @return
    *   The first index of the first key
    */
  def headKeyIndexOption: Option[Int] =
    queue.headOption.flatMap { case (_, set) => set.headOption }

  /** Get the index that given value is stored at, if it exists.
    *
    * @param value
    *   Value to retrieve the index for
    * @return
    *   The index
    */
  def indexOf(value: V): Option[Int] = valueToIndex.get(value)

  /** Set given value to given key
    *
    * @param key
    *   The key to add the value for
    * @param value
    *   The value to add
    * @return
    *   The altered data structure
    */
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

  /** Retrieves the first element in the list of given key. The returned element
    * is also removed the queue here.
    *
    * If the list of values for given key is empty, the list is removed: There
    * are no empty lists in the queue, thus also keys only exist for non-empty
    * lists.
    *
    * @return
    *   The first element in the list of the first key and the changed data
    *   structure, if it is not empty.
    */
  def takeNextValueFor(key: K): Option[(V, PrioritySwitchBiSet[K, V])] = {
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
            // drop first value
            queue.updated(key, set.drop(1))

        (
          firstValue,
          copy(queue = updatedQueue, back = back.removed(firstValue))
        )
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

  /** Returns all values in order
    *
    * @return
    *   The value vector
    */
  def values: Vector[V] = orderedValues

}

object PrioritySwitchBiSet {

  /** Creates and returns an empty PrioritySwitchBiSet for given types.
    *
    * @tparam K
    *   Type of the key, which needs to be sortable by means of [[Ordering]]
    * @tparam V
    *   Type of the value
    * @return
    *   An empty PrioritySwitchBiSet
    */
  def empty[K: Ordering, V]: PrioritySwitchBiSet[K, V] = PrioritySwitchBiSet(
    Vector.empty,
    Map.empty,
    immutable.SortedMap.empty,
    Map.empty
  )
}
