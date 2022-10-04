/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import javax.measure.quantity.Energy
import scala.util.Random

trait RandomStorageState {
  this: ThermalStorage =>
  private val seed: Long = -517L

  override def startingState: ThermalStorage.ThermalStorageState = {
    def rnd: Double = new Random(seed).nextDouble()
    def storedEnergy: ComparableQuantity[Energy] = getMinEnergyThreshold.add(
      getMaxEnergyThreshold.subtract(getMinEnergyThreshold).multiply(rnd)
    )

    ThermalStorageState(
      -1L,
      storedEnergy,
      Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
    )
  }
}
