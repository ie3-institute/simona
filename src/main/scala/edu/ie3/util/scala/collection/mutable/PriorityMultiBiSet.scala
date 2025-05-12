/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.mutable

import scala.collection.{SortedSet, mutable}

/** Queue that is specialized at holding many values of type [[V]] for the same
  * key of type [[K]], while only allowing each value to be linked to one key.
  * Mathematically, the relation between keys and values is thus not univalent
  * (right-unique), but injective. Values are stored in a [[mutable.Set]] (with
  * adding and removing items in about constant time).
  *
  * @param queue
  *   Queue that holds keys in order and thus provides a way to quickly retrieve
  *   the elements for the first key(s).
  * @param table
  *   HashMap that provides direct access to each list given the key that it was
  *   added with. This is useful for quickly adding values to new and existing
  *   keys, running in nearly O(1).
  * @param back
  *   HashMap that links values back to keys. Used to fastly ensure every value
  *   is only stored once.
  * @tparam K
  *   Type of the key
  * @tparam V
  *   Type of the value
  */
final case class PriorityMultiBiSet[K, V](
    private val queue: mutable.SortedSet[K],
    private val table: mutable.HashMap[K, mutable.Set[V]],
    private val back: mutable.HashMap[V, K],
) {

  /** Get the first key of the queue, if the queue is not empty. Runs in O(1).
    *
    * @return
    *   The first key
    */
  def headKeyOption: Option[K] =
    queue.headOption

  /** Get all keys in a sorted set.
    * @return
    *   The sorted keys
    */
  def keySet: SortedSet[K] = queue

  /** Get the key that given value is mapped for, if it exists.
    *
    * @param value
    *   Value to retrieve the key for
    * @return
    *   The key
    */
  def getKeyOf(value: V): Option[K] =
    back.get(value)

  /** Set given value to given key.
    *
    * @param key
    *   The key to add the value for
    * @param value
    *   The value to add
    */
  def set(key: K, value: V): Unit = {
    // remove old mapping for value in table and queue
    remove(value)
    // add new mapping
    back += (value -> key)

    table.get(key) match {
      case Some(set) =>
        // key already exists in both structures
        set += value
      case None =>
        // key doesn't exist yet, add to both structures
        queue += key
        table += (key -> mutable.Set(value))
    }
  }

  /** Removes the given value, if it exists.
    *
    * @param value
    *   The value
    * @return
    *   Whether the value existed somewhere in here
    */
  def remove(value: V): Boolean = {
    back.get(value).exists { key =>
      back.remove(value)

      table.get(key).exists { set =>
        val existed = set.remove(value)

        if (set.isEmpty) {
          table -= key
          queue -= key
        }

        existed
      }
    }

  }

  /** Retrieves the values stored for given key. The returned elements are also
    * removed from the queue here.
    *
    * @return
    *   All values stored for the first key
    */
  def getAndRemoveSet(key: K): Set[V] = {
    table
      .get(key)
      .map { set =>
        table -= key
        queue -= key

        // return an immutable set
        val immutableSet = Set.from(set)

        // also remove from reverse map
        immutableSet.foreach(back.remove)

        immutableSet
      }
      .getOrElse(Set.empty)
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
}
object PriorityMultiBiSet {

  /** Creates and returns an empty PriorityMultiQueue for given types.
    *
    * @tparam K
    *   Type of the key, which needs to be sortable by means of [[Ordering]]
    * @tparam V
    *   Type of the value
    * @return
    *   An empty PriorityMultiQueue
    */
  def empty[K: Ordering, V]: PriorityMultiBiSet[K, V] =
    PriorityMultiBiSet(
      mutable.SortedSet.empty[K],
      mutable.HashMap[K, mutable.Set[V]](),
      mutable.HashMap[V, K](),
    )

  /** Creates and returns an empty PriorityMultiQueue for given types. The
    * initialKeyCapacity and loadFactor are used in the creation of the
    * HashMaps.
    *
    * @param initialKeyCapacity
    *   The initial capacity of both HashMaps. The capacity increments (i.e. the
    *   map is recreated with a higher capacity) once the amount denoted by
    *   loadFactor is hit.
    * @param loadFactor
    *   The loadFactor of the HashMaps. If the size of the map reaches capacity
    *   * loadFactor, the underlying table is replaced with a larger one.
    * @tparam K
    *   Type of the key, which needs to be sortable by means of [[Ordering]]
    * @tparam V
    *   Type of the value
    * @return
    *   An empty PriorityMultiQueue
    */
  def empty[K: Ordering, V](
      initialKeyCapacity: Int,
      loadFactor: Double = mutable.HashMap.defaultLoadFactor,
  ): PriorityMultiBiSet[K, V] =
    PriorityMultiBiSet(
      mutable.SortedSet.empty[K],
      new mutable.HashMap[K, mutable.Set[V]](
        initialKeyCapacity,
        loadFactor,
      ),
      new mutable.HashMap[V, K](
        initialKeyCapacity,
        loadFactor,
      ),
    )
}
