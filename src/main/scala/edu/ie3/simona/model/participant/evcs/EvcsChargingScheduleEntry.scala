/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.api.data.ev.model.EvModel
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Power

/** Schedule entry determining in which time interval e specific EV should be
  * charged with which power.
  *
  * @param tickStart
  *   start of charging interval
  * @param tickStop
  *   stop of charging interval
  * @param ev
  *   EV to be charged
  * @param chargingPower
  *   charging power for the charging interval
  */
@deprecated("Use ChargingSchedule instead")
case class EvcsChargingScheduleEntry(
    tickStart: Long,
    tickStop: Long,
    ev: EvModel,
    chargingPower: ComparableQuantity[Power]
)
