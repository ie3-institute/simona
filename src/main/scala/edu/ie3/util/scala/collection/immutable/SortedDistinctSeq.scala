/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.immutable

import edu.ie3.util.scala.collection.immutable

/** Simple wrapper around [[Seq]], that enforces the user to use the
  * [[SortedDistinctSeq#apply()]] builder. It ensures, that the sequence only
  * contains distinct entries and that it is sorted.
  *
  * @param internalSequence
  *   The internal sequence with distinct, sorted entries
  * @tparam V
  *   Type of value, the sequence may carry.
  */
final case class SortedDistinctSeq[V] private (
    internalSequence: IndexedSeq[V]
) extends Seq[V] {
  override def apply(i: Int): V = internalSequence.apply(i)

  override def length: Int = internalSequence.length

  override def iterator: Iterator[V] = internalSequence.iterator

  def pop: (Option[V], SortedDistinctSeq[V]) = {
    val maybeHead = internalSequence.headOption
    val newSeq = new immutable.SortedDistinctSeq(
      maybeHead
        .map(head => internalSequence.filter(_ != head))
        .getOrElse(IndexedSeq.empty[V])
    )
    (maybeHead, newSeq)
  }
}

case object SortedDistinctSeq {
  def apply[V](
      entries: Seq[V]
  )(implicit ord: Ordering[V]): SortedDistinctSeq[V] =
    new SortedDistinctSeq(entries.toIndexedSeq.distinct.sorted)

  def empty[V] = new SortedDistinctSeq[V](IndexedSeq.empty[V])
}
