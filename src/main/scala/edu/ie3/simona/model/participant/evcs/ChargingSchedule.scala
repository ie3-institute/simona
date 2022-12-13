/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.model.participant.evcs.ChargingSchedule.Entry

import java.util.UUID
import scala.collection.immutable.{SortedSet, TreeSet}

/** Charging schedule for an EV for several time intervals
  *
  * @param ev
  *   Unique identifier of the car
  * @param schedule
  *   Actual schedule
  */
final case class ChargingSchedule(ev: UUID, schedule: SortedSet[Entry]) {}

object ChargingSchedule {
  def apply(ev: EvModelWrapper, entries: Seq[Entry]) =
    new ChargingSchedule(ev.uuid, TreeSet.from(entries))

  /** Schedule entry specifying a time interval in which the EV should be
    * charged with some given power
    *
    * @param tickStart
    *   start of charging interval
    * @param tickStop
    *   end of charging interval
    * @param chargingPower
    *   charging power for the charging interval
    */
  final case class Entry(
      tickStart: Long,
      tickStop: Long,
      chargingPower: squants.Power
  ) extends Ordered[Entry] {
    override def compare(that: Entry): Int = {
      val startComp = tickStart.compare(that.tickStart)
      if (startComp != 0)
        startComp
      else {
        // important for checking equality: consider other fields as well
        val stopComp = tickStop.compare(that.tickStop)
        if (stopComp != 0)
          stopComp
        else
          chargingPower.compareTo(that.chargingPower)
      }
    }
  }
}
