/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.mutable

import edu.ie3.util.scala.collection.mutable.PriorityMultiSet.KeyWrapper

import scala.collection.{SortedSet, immutable, mutable}

/** Queue that is specialized at holding many values of type [[V]] for the same
  * key of type Long. Mutable structure. Values are stored in a
  * [[mutable.ListBuffer]], which corresponds to a linked list (with adding and
  * removing items to/from its head/tail in constant time).
  * @param queue
  *   Queue that holds keys in order and thus provides a way to quickly retrieve
  *   the elements for the first key(s).
  * @param table
  *   HashMap that provides direct access to each list given the key that it was
  *   added with. This is useful for quickly adding values to new and existing
  *   keys, running in nearly O(1).
  * @tparam V
  *   Type of the value
  */
final case class PriorityMultiSet[V] private (
    private val queue: mutable.SortedMap[KeyWrapper, mutable.Set[V]],
    private val table: mutable.HashMap[KeyWrapper, mutable.Set[V]]
) {

  /** Get the first key of the queue, if the queue is not empty. Runs in O(1).
    * @return
    *   The first key
    */
  def headKeyOption: Option[Long] =
    queue.headOption.map { case (wrapper, _) => wrapper.key }

  /** Get all keys in a sorted set.
    * @return
    *   The sorted keys
    */
  def keySet: SortedSet[Long] = queue.keySet.map(_.key)

  /** Add given value to the end of the list that belongs to given key
    * @param key
    *   The key to add the value for
    * @param value
    *   The value to add
    */
  def add(key: Long, value: V): Unit = {
    val wrapper = KeyWrapper(key)

    table.get(wrapper) match {
      case Some(set) =>
        // list already exists in both structures
        set.addOne(value)
      case None =>
        // list doesn't exist yet, add to both structures
        val set = mutable.Set(value)
        queue.addOne(wrapper, set)
        table.addOne(wrapper, set)
    }
  }

  /** Removes value from the set that belongs to given key
    *
    * @param key
    *   The key to remove the value for
    * @param value
    *   The value to remove
    */
  def remove(key: Long, value: V): Unit = {
    val wrapper = KeyWrapper(key)

    table.get(wrapper).foreach { set =>
      set.remove(value)

      if (set.isEmpty) {
        queue.remove(wrapper)
        table.remove(wrapper)
      }
    }
  }

  // TODO scaladoc
  // TODO test this
  def get(key: Long): Option[Seq[V]] = {
    // make a copy of list, the original is mutable
    // FIXME maybe only List.from works?
    table.get(KeyWrapper(key)).map(immutable.Seq.from)
  }

  /** Retrieves all elements for keys that are smaller or equal to given key.
    * The returned elements are also removed from the queue here.
    *
    * @return
    *   All elements for keys up to and including the given key. An empty
    *   Iterable is returned if this queue is empty or all keys are greater than
    *   the given key.
    */
  def pollTo(key: Long): Iterable[V] = {
    // a copy has to be made here because the resulting Map of
    // rangeTo is linked to the original map. This means that
    // the map with the values to be returned would be depleted
    // with the subtractions below
    val polledValues = mutable.SortedMap.from(queue.rangeTo(KeyWrapper(key)))

    val keys = polledValues.keySet
    queue --= keys
    table --= keys

    polledValues.values.flatten
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

object PriorityMultiSet {

  case class KeyWrapper(key: Long) extends Ordered[KeyWrapper] {
    override def compare(that: KeyWrapper): Int = key.compareTo(that.key)
  }

  /** Creates and returns an empty PriorityMultiQueue for given types.
    * @tparam V
    *   Type of the value
    * @return
    *   An empty PriorityMultiQueue
    */
  def empty[V]: PriorityMultiSet[V] =
    PriorityMultiSet(
      mutable.SortedMap[KeyWrapper, mutable.Set[V]](),
      mutable.HashMap[KeyWrapper, mutable.Set[V]]()
    )

  /** Creates and returns an empty PriorityMultiQueue for given types. The
    * initialKeyCapacity and loadFactor are used in the creation of the HashMap.
    * @param initialKeyCapacity
    *   The initial capacity of of the HashMap for keys. The capacity increments
    *   (i.e. the map is recreated with a higher capacity) once the amount
    *   denoted by loadFactor is hit.
    * @param loadFactor
    *   The loadFactor of the HashMap. If the size of the map reaches capacity *
    *   loadFactor, the underlying table is replaced with a larger one.
    * @tparam V
    *   Type of the value
    * @return
    *   An empty PriorityMultiQueue
    */
  def empty[V](
      initialKeyCapacity: Int,
      loadFactor: Double = mutable.HashMap.defaultLoadFactor
  ): PriorityMultiSet[V] =
    PriorityMultiSet(
      mutable.SortedMap[KeyWrapper, mutable.Set[V]](),
      new mutable.HashMap[KeyWrapper, mutable.Set[V]](
        initialKeyCapacity,
        loadFactor
      )
    )
}
