/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.model.participant.evcs.ChargingSchedule.Entry
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.Power
import scala.collection.immutable.{SortedSet, TreeSet}

/** Charging schedule for an EV for a slice in time
  *
  * @param ev
  *   Unique identifier of the car, it applies to
  * @param schedule
  *   Actual schedule
  */
final case class ChargingSchedule(ev: UUID, schedule: SortedSet[Entry])

object ChargingSchedule {
  def apply(ev: EvModel, entries: Seq[Entry]) =
    new ChargingSchedule(ev.getUuid, TreeSet.from(entries))

  final case class Entry(
      tickStart: Long,
      tickStop: Long,
      chargingPower: ComparableQuantity[Power]
  ) extends Ordered[Entry] {
    override def compare(that: Entry): Int = tickStart.compare(that.tickStart)
  }
}
