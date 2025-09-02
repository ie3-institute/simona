/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs
import edu.ie3.datamodel.models.ElectricCurrentType
import squants.Power
import squants.energy.Kilowatts

object MockEvcsChargingProperties extends EvcsChargingProperties {

  override protected val pRated: Power = Kilowatts(43)
  override val currentType: ElectricCurrentType = ElectricCurrentType.AC
  override val lowestEvSoc: Double = 0.2

}
