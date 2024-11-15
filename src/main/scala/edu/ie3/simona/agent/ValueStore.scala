/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent

import edu.ie3.simona.util.SimonaConstants
import squants.Dimensionless

import scala.collection.SortedMap

/** Represents a value store to hold data of former ticks
  *
  * @param maxTickSpan
  *   Maximum period of ticks to store
  * @param store
  *   An initial state of the store
  * @tparam D
  *   Type of data to hold
  */
final case class ValueStore[+D](
    maxTickSpan: Long,
    private val store: SortedMap[Long, D] = SortedMap.empty[Long, D],
) {

  /** Determine the lastly known data tick, if available. Includes the given
    * request tick.
    *
    * @param requestTick
    *   The requested tick after the last known tick
    * @return
    *   The last known tick or None
    */
  def lastKnownTick(requestTick: Long): Option[Long] =
    last(requestTick).map(_._1)

  /** Get the last known entry before or at the requested tick.
    *
    * @param requestedTick
    *   Requested tick
    * @return
    *   An Option to the last entry
    */
  def last(requestedTick: Long): Option[(Long, D)] =
    store.rangeTo(requestedTick).lastOption

  /** Get the last known entry (with the highest tick)
    *
    * @return
    *   An Option to the last entry
    */
  def last(): Option[(Long, D)] =
    store.lastOption

  /** Optionally returns the entry for given tick
    * @param tick
    *   The tick
    * @return
    *   The data for the tick if it exists, otherwise [[None]]
    */
  def get(tick: Long): Option[D] =
    store.get(tick)

  /** Returns the data associated with a tick, or a default value if no data
    * exists for the tick.
    * @param tick
    *   The tick
    * @return
    *   the data associated with `tick` if it exists, otherwise the result of
    *   the `default` function.
    */
  def getOrElse[D2 >: D](tick: Long, default: => D2): D2 =
    store.getOrElse(tick, default)

  /** Acquires the stored information within the specified tick window
    *
    * @param requestStart
    *   First tick INCLUDED in the requested window
    * @param requestEnd
    *   Last tick INCLUDED in the requested window
    * @return
    *   Held information within the tick window. Map is empty, if no entries are
    *   in the value store
    */
  def get(requestStart: Long, requestEnd: Long): Map[Long, D] =
    store.rangeFrom(requestStart).rangeTo(requestEnd).toMap

  def asMap: Map[Long, D] =
    store.toMap
}

object ValueStore {

  /** Create a default "empty" voltage value store which requires an initial
    * voltage value to be set for tick 0
    *
    * @param maxTickSpan
    *   maximum period of ticks to store
    * @param initialPerUnit
    *   the initial voltage value @ tick 0 in p.u.
    * @return
    *   default pre-initialized voltage value store instance
    */
  def forVoltage(
      maxTickSpan: Long,
      initialPerUnit: Dimensionless,
  ): ValueStore[Dimensionless] =
    new ValueStore(
      maxTickSpan,
      SortedMap(SimonaConstants.FIRST_TICK_IN_SIMULATION -> initialPerUnit),
    )

  /** Create a value store for result values. A result value store requires a
    * longer history (normally at least time bin * 2) because it is used to
    * determine previously provided results.
    *
    * @param maxTickSpan
    *   maximum period of ticks to store
    * @param multiplier
    *   multiplier the max tick span is multiplied with to extend the size of
    *   the result value store
    * @tparam D
    *   type of data to hold
    * @return
    *   value store for result data
    */
  def forResult[D](maxTickSpan: Long, multiplier: Long): ValueStore[D] =
    new ValueStore[D](maxTickSpan * multiplier)

  /** Updates the value store. Additionally, the size of the store is limited to
    * it's defined maximum capacity. Therefore, the oldest entries are removed.
    *
    * @param valueStore
    *   Former value store to update
    * @param tick
    *   The tick, for which the new entry is valid
    * @param newEntry
    *   The new entry to add
    * @return
    *   The updated and sliced value store
    */
  def updateValueStore[D](
      valueStore: ValueStore[D],
      tick: Long,
      newEntry: D,
  ): ValueStore[D] = {
    val updatedStore = valueStore.store ++ SortedMap(tick -> newEntry)

    // always keep at least 3 entries
    val minKeep = 3

    valueStore.copy(
      store = if (updatedStore.size > minKeep) {
        val (rest, keep) = updatedStore.splitAt(updatedStore.size - minKeep)
        val restPruned = rest.rangeFrom(tick - valueStore.maxTickSpan + 1L)

        restPruned ++ keep
      } else
        updatedStore
    )
  }
}
