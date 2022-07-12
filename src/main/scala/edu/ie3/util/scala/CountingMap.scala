/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala

import scala.collection.mutable

/** A map that holds a counter of type [[Long]] for every key. Furthermore,
  * during usage a sorted set is populated with all keys used, so that the
  * minimal key can be retrieved easily.
  *
  * @param map
  *   The counting map
  * @param sortedKeySet
  *   The sorted key set
  * @tparam K
  *   The type of keys
  */
final case class CountingMap[K] private (
    private val map: mutable.HashMap[K, Long],
    private val sortedKeySet: mutable.SortedSet[K]
) {

  /** Tests whether this map contains a binding for a key.
    * @param key
    *   The key
    * @return
    *   True if there is a binding for key in this map, false otherwise.
    */
  def contains(key: K): Boolean = map.contains(key)

  /** Optionally returns the value associated with a key.
    *
    * @param key
    *   The key value
    * @return
    *   An [[Option]] containing the value associated with key in this map, or
    *   None if none exists.
    */
  def get(key: K): Option[Long] = map.get(key)

  /** Returns the the minimal key of all.
    *
    * @return
    *   The minimal key of all
    */
  def minKeyOption: Option[K] = sortedKeySet.headOption

  /** Increase the counter for given key by 1.
    * @param key
    *   The key
    */
  def add(key: K): Unit = {
    map.get(key) match {
      case Some(count) =>
        // a count for given key already exists, increase by 1
        map.update(key, count + 1L)
      case None =>
        // no count for given key exists yet, create one with count 1
        map.update(key, 1L)
        sortedKeySet.add(key)
    }
  }

  /** Decrease the counter for given key by 1. If the counter hits 0, the
    * key-value mapping is removed.
    * @param key
    *   The key
    */
  def subtract(key: K): Unit =
    map.get(key) match {
      case Some(count) =>
        if (count <= 1L) {
          // if the counter would hit 0 at this point, remove the key and value altogether
          map.remove(key)
          sortedKeySet.remove(key)
        } else
          map.update(key, count - 1L)

      case None =>
      // in case that the key does not exist, do nothing
    }

  /** Tests whether there is no value for any key in the queue.
    * @return
    *   True if the queue is empty
    */
  def isEmpty: Boolean = map.isEmpty

}

object CountingMap {
  def empty[K](implicit
      ev: K => Ordered[K]
  ): CountingMap[K] =
    CountingMap(
      mutable.HashMap.empty[K, Long],
      mutable.SortedSet.empty[K]
    )

  def from[K](source: Iterable[(K, Long)])(implicit
      ev: K => Ordered[K]
  ): CountingMap[K] = {
    CountingMap(
      mutable.HashMap.from(source),
      mutable.SortedSet.from(source.map { case (key, _) => key })
    )
  }
}
