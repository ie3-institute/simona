/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.immutable

/** Extension for a `Map[K, Set[V]]`, i.e. a map with sets as values.
  */
object RichMultiMap {

  extension [K, V](map: Map[K, Set[V]]) {

    /** Tests if given value is contained in the set for given key.
      */
    def contains(key: K, value: V): Boolean =
      map.get(key).exists(_.contains(value))

    /** Adds given value to the set of given key. Creates a new set, if such
      * does not exist for the key.
      */
    def added(key: K, value: V): Map[K, Set[V]] =
      map.updated(key, getOrEmptySet(key).incl(value))

    /** Removes given value from the set of given key. Removes the updated set,
      * if it is empty after removal.
      */
    def removed(key: K, value: V): Map[K, Set[V]] = {
      val updatedSet = getOrEmptySet(key).excl(value)

      if updatedSet.isEmpty then map.removed(key)
      else map.updated(key, updatedSet)
    }

    private def getOrEmptySet(key: K): Set[V] =
      map.getOrElse(key, Set.empty)

  }

}
