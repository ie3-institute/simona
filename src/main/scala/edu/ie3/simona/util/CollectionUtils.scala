/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import squants.Quantity

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.math.Ordering.Double

object CollectionUtils {

  /** fast implementation to test if a list contains duplicates. See
    * https://stackoverflow.com/questions/3871491/functional-programming-does-a-list-only-contain-unique-items
    * for details
    *
    * @param list
    *   List to check
    * @tparam T
    *   Generic type of the entries in the list
    * @return
    *   true, if list contains duplicates, false otherwise
    */
  def listHasDuplicates[T](list: List[T]): Boolean =
    !isUniqueList(list, new HashSet[T])

  /** Checks, if the given list only contains unique entries. Found at <a
    * href="https://stackoverflow.com/questions/3871491/functional-programming-does-a-list-only-contain-unique-items">
    * Alexey Romanov's StackOverFlow post</a>.
    *
    * @param list
    *   List to check
    * @tparam T
    *   Generic type of the entries in the list
    * @return
    *   true, if all entries are unique, false if not
    */
  def isUniqueList[T](list: List[T]): Boolean =
    isUniqueList(list, new HashSet[T]())

  /** Recursively adds the entries of the list to a set and returns false as
    * fast as it finds a duplicate entry. Found at <a
    * href="https://stackoverflow.com/questions/3871491/functional-programming-does-a-list-only-contain-unique-items">
    * Alexey Romanov's StackOverFlow post</a>.
    *
    * @param list
    *   List to check
    * @param set
    *   Set of already visited entries
    * @tparam T
    *   Generic type of the entries in the list
    * @return
    *   true, if all entries have been traveled and no duplicate found, false,
    *   if duplicate has been found
    */
  @tailrec
  private def isUniqueList[T](list: List[T], set: Set[T]): Boolean =
    list match {
      case Nil => true
      case headEntry :: tailList =>
        if (set(headEntry)) false else isUniqueList(tailList, set + headEntry)
    }

  /** Checks if the provided list is sorted in accordance to the provided
    * ordering
    *
    * @param list
    *   the list to be checked
    * @param ord
    *   the order
    * @tparam T
    *   element type
    * @return
    *   true if the list is sorted according to the provided order, false
    *   otherwise
    */
  @tailrec
  private def isSorted[T](list: List[T])(implicit ord: Ordering[T]): Boolean =
    list match {
      case Nil      => true // an empty list is sorted
      case _ :: Nil => true // a single-element list is sorted
      case x :: xs :: tail =>
        ord.lteq(x, xs) && isSorted(
          xs :: tail
        ) // if the first two elements are ordered and the rest are sorted, the full list is sorted too
    }

  /** Given a map with doubles (e.g. x,y pairs) and a provided key the closest
    * key(s)-value(s) are searched for, the provided method returns the (k,v)
    * (if the provided key is equal to map key) or the two closest (k,v) (if the
    * provided key is between two map keys). If the provided key is bigger than
    * the biggest map key, only the (k,v) to the biggest map key is provided. If
    * the key is smaller than the smallest map key, only the (k,v) to the
    * smallest map key is provided.
    *
    * @param map
    *   containing the (k,v) pairs (e.g. x,y pairs)
    * @param key
    *   the key values are needed for
    * @return
    *   either a Seq with one or two (k,v) pairs
    */
  def closestKeyValuePairs[A <: Quantity[A], O <: Quantity[O]](
      map: Map[A, O],
      key: A,
  ): Seq[(A, O)] = {
    import scala.collection.immutable.TreeMap
    implicit val ordering: Double.IeeeOrdering.type =
      Ordering.Double.IeeeOrdering
    val treeMap = TreeMap(map.toSeq: _*) // preserves order

    Seq(
      treeMap.rangeTo(key).lastOption,
      treeMap.rangeFrom(key).headOption,
    ).flatten.distinct
      .map { case (k, v) => (k, v) }
  }

}
