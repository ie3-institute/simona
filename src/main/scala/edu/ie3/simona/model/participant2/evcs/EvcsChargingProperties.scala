/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import squants.Power

trait EvcsChargingProperties {

  /** Charging station rated power
    */
  val sRated: Power

  val currentType: ElectricCurrentType

  val lowestEvSoc: Double

  def getMaxAvailableChargingPower(ev: EvModelWrapper): Power
}
