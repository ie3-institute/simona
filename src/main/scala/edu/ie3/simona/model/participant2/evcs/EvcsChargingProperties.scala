/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import edu.ie3.datamodel.models.ElectricCurrentType
import squants.Power

trait EvcsChargingProperties {

  /** Charging station rated power
    */
  protected val pRated: Power

  val currentType: ElectricCurrentType

  val lowestEvSoc: Double

  /** Returns the maximum available charging power for an EV, which depends on
    * ev and charging station limits for AC and DC current
    *
    * @param ev
    *   ev for which the max charging power should be returned
    * @return
    *   maximum charging power for the EV at this charging station
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
    /* Limit the charging power to the minimum of ev's and evcs' permissible power */
    evPower.min(pRated)
  }
}
