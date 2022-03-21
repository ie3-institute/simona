/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.util.quantities.interfaces.EnergyPrice
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Dimensionless

object SchedulingTimeWindows {

  /** Time window used for the scheduling of ev charging. */
  trait SchedulingTimeWindow {
    val start: Long
    val end: Long

    /** @return
      *   Length of the window in seconds
      */
    def length: Long = start - end + 1
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
    * @param size
    *   size of the time window expressed as voltage deviation * length
    * @param parkedEvs
    *   still parked ev in this time window
    */
  case class SchedulingTimeWindowWithVoltage(
      override val start: Long,
      override val end: Long,
      voltage: ComparableQuantity[Dimensionless],
      voltageDeviation: ComparableQuantity[Dimensionless],
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
    */
  case class SchedulingSliceWithPrice(
      start: Long,
      end: Long,
      price: ComparableQuantity[EnergyPrice]
  ) extends SchedulingTimeWindow {
    override def toString: String =
      s"SchedulingTimeWindow(start=$start, end=$end, price=$price, timeBoxLength=$length)"
  }

}
