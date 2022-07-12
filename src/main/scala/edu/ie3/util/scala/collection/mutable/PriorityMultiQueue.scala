/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.mutable

import scala.collection.{SortedSet, mutable}

/** Queue that is specialized at holding many values of type [[V]] for the same
  * key of type [[K]]. Mutable structure. Values are stored in a
  * [[mutable.ListBuffer]], which corresponds to a linked list (with adding and
  * removing items to/from its head/tail in constant time).
  * @param queue
  *   Queue that holds keys in order and provides the first element in O(1)
  * @param table
  *   HashMap that provides access to lists with a specific key in O(1)
  * @tparam K
  *   Type of the key
  * @tparam V
  *   Type of the value
  */
final case class PriorityMultiQueue[K, V] private (
    private val queue: mutable.SortedMap[K, mutable.ListBuffer[V]],
    private val table: mutable.HashMap[K, mutable.ListBuffer[V]]
) {

  /** Get the first key of the queue, if the queue is not empty.
    * @return
    *   The first key
    */
  def headKeyOption: Option[K] =
    queue.headOption.map { case (key, _) => key }

  /** Get all keys in a sorted set.
    * @return
    *   The sorted keys
    */
  def keySet: SortedSet[K] = queue.keySet

  /** Add given value to the end of the list that belongs to given key
    * @param key
    *   The key to add the value for
    * @param value
    *   The value to add
    */
  def add(key: K, value: V): Unit = {
    table.get(key) match {
      case Some(list) =>
        // list already exists in both structures
        list.addOne(value)
      case None =>
        // list doesn't exist yet, add to both structures
        val list = mutable.ListBuffer(value)
        queue.addOne(key, list)
        table.addOne(key, list)
    }
  }

  /** Retrieves the first element in the list of the first key. The returned
    * element is also removed the queue here.
    *
    * If the list of values for given key is empty, the list is removed: There
    * are no empty lists in the queue, thus also keys only exist for non-empty
    * lists.
    * @return
    *   The first element in the list of the first key
    */
  def poll(): Option[V] = {
    queue.headOption.map { case (key, list) =>
      if (list.size <= 1) {
        // if this was the last value for this key, remove the list
        queue.remove(key)
        table.remove(key)
      }

      list.remove(0)
    }
  }

  /** Get all values for all keys as an iterable.
    * @return
    *   All values
    */
  def allValues: Iterable[V] = queue.values.flatten

  /** Tests whether there is no value for any key in the queue.
    * @return
    *   True if the queue is empty
    */
  def isEmpty: Boolean = queue.isEmpty

  /** Tests whether there is any value for any key in the queue.
    * @return
    *   True if the queue is non-empty
    */
  def nonEmpty: Boolean = queue.nonEmpty
}

object PriorityMultiQueue {

  /** Creates and returns an empty PriorityMultiQueue for given types.
    * @param ev
    *   Ordering of key
    * @tparam K
    *   Type of the key
    * @tparam V
    *   Type of the value
    * @return
    *   An empty PriorityMultiQueue
    */
  def empty[K, V](implicit ev: K => Ordered[K]): PriorityMultiQueue[K, V] =
    PriorityMultiQueue(
      mutable.SortedMap[K, mutable.ListBuffer[V]](),
      mutable.HashMap[K, mutable.ListBuffer[V]]()
    )

  /** Creates and returns an empty PriorityMultiQueue for given types. The
    * initialKeyCapacity and loadFactor are used in the creation of the HashMap.
    * @param initialKeyCapacity
    *   The initialKeyCapacity of the HashMap
    * @param loadFactor
    *   The loadFactor of the HashMap
    * @param ev
    *   Ordering of key
    * @tparam K
    *   Type of the key
    * @tparam V
    *   Type of the value
    * @return
    *   An empty PriorityMultiQueue
    */
  def empty[K, V](
      initialKeyCapacity: Int,
      loadFactor: Double = mutable.HashMap.defaultLoadFactor
  )(implicit ev: K => Ordered[K]): PriorityMultiQueue[K, V] =
    PriorityMultiQueue(
      mutable.SortedMap[K, mutable.ListBuffer[V]](),
      new mutable.HashMap[K, mutable.ListBuffer[V]](
        initialKeyCapacity,
        loadFactor
      )
    )
}
