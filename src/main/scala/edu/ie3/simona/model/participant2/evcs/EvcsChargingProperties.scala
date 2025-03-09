/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import edu.ie3.datamodel.models.ElectricCurrentType
import squants.Power

/** Provides some relevant EVCS properties to [[EvcsChargingStrategy]].
  */
trait EvcsChargingProperties {

  /** The rated active power of the charging station.
    */
  protected val pRated: Power

  /** The type of current used by the charging station.
    */
  val currentType: ElectricCurrentType

  /** The lowest allowed SOC of all connected EVs. Below this threshold,
    * charging is mandatory.
    */
  val lowestEvSoc: Double

  /** Returns the maximum available charging power for an EV, which depends on
    * EV and charging station limits for AC and DC current.
    *
    * @param ev
    *   The EV for which the max charging power should be returned.
    * @return
    *   The maximum charging power for the EV at this charging station.
    */
  def getMaxAvailableChargingPower(
      ev: EvModelWrapper
  ): Power = {
    val evPower = currentType match {
      case ElectricCurrentType.AC =>
        ev.pRatedAc
      case ElectricCurrentType.DC =>
        ev.pRatedDc
    }
    /* Limit the charging power to the minimum of EV's and EVCS' permissible power */
    evPower.min(pRated)
  }
}
