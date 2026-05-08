/*
 * © 2021-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.immutable

/** Class that holds activation ticks in a queue. Only dropping the first
  * element is allowed. Usage of the accompanying factory is enforced, which
  * ensures that the internal list only contains distinct entries and that it is
  * sorted.
  *
  * @param ticks
  *   The linked list holding future activation ticks, with the first one being
  *   the imminent tick.
  */
final case class ActivationTickQueue private (
    private val ticks: List[Long]
) {
  def length: Int = ticks.length

  /** Returns the next (imminent) tick at the head of the queue.
    */
  def nextTick: Option[Long] = ticks.headOption

  /** Returns a copy of the queue with the current head removed.
    */
  def dropFirst: ActivationTickQueue =
    copy(
      ticks match {
        case Nil       => ticks
        case _ :: tail => tail
      }
    )
}

object ActivationTickQueue {

  /** Creates an [[ActivationTickQueue]] from given sequence by sorting and
    * removing duplicates.
    *
    * @param entries
    *   The entries to create the [[ActivationTickQueue]] from.
    * @return
    *   The [[ActivationTickQueue]].
    */
  def apply(
      entries: Seq[Long]
  ): ActivationTickQueue =
    new ActivationTickQueue(entries.distinct.sorted.toList)

  def empty[V] = new ActivationTickQueue(List.empty)
}
