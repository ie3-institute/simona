/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.util.quantities.interfaces.EnergyPrice
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import javax.measure.quantity.Dimensionless

object SchedulingTimeWindows {

  /** Time window used for the scheduling of ev charging. */
  trait SchedulingTimeWindow {
    val start: ZonedDateTime
    val end: ZonedDateTime
    val length: Long
    val parkedEvs: Set[EvModel]
  }

  /** Time window used for the scheduling of ev charging. Additional information
    * on the voltages in the time window.
    *
    * @param start
    *   start of time window
    * @param end
    *   end of time window
    * @param voltage
    *   predicted voltage in this time window
    * @param voltageDeviation
    *   deviation from reference voltage in the schedule time
    * @param length
    *   length of the time window in seconds
    * @param size
    *   size of the time window expressed as voltage deviation * length
    * @param parkedEvs
    *   still parked ev in this time window
    */
  case class SchedulingTimeWindowWithVoltage(
      start: ZonedDateTime,
      end: ZonedDateTime,
      voltage: ComparableQuantity[Dimensionless],
      voltageDeviation: ComparableQuantity[Dimensionless],
      length: Long,
      size: Double,
      parkedEvs: Set[EvModel]
  ) extends SchedulingTimeWindow {
    override def toString: String =
      s"SchedulingTimeWindow(start=$start, end=$end, voltage=$voltage, voltageDeviation=$voltageDeviation, " +
        s"timeBoxLength=$length, timeBoxSize=$size, parkedEvs=${parkedEvs
          .foldLeft(Set.empty[String])((names: Set[String], ev: EvModel) => {
            names + ev.getId
          })})"
  }

  /** Time window used for the scheduling of ev charging. Additional information
    * on the voltages in the time window.
    *
    * @param start
    *   start of time window
    * @param end
    *   end of time window
    * @param price
    *   predicted price in this time window
    * @param length
    *   length of the time window in seconds
    * @param parkedEvs
    *   still parked ev in this time window
    */
  case class SchedulingTimeWindowWithPrice(
      start: ZonedDateTime,
      end: ZonedDateTime,
      price: ComparableQuantity[EnergyPrice],
      length: Long,
      parkedEvs: Set[EvModel]
  ) extends SchedulingTimeWindow {
    override def toString: String =
      s"SchedulingTimeWindow(start=$start, end=$end, price=$price, timeBoxLength=$length, parkedEvs=${parkedEvs
        .foldLeft(Set.empty[String])((names: Set[String], ev: EvModel) => {
          names + ev.getId
        })})"
  }

}
