/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.Energy

import scala.util.Random

trait RandomStorageState {
  this: ThermalStorage =>
  private val seed: Long = -517L

  override def startingState: ThermalStorage.ThermalStorageState = {
    def rnd: Double = new Random(seed).nextDouble()
    def storedEnergy: Energy =
      getMinEnergyThreshold + (getMaxEnergyThreshold - getMinEnergyThreshold) * rnd

    ThermalStorageState(
      -1L,
      storedEnergy,
      zeroKW,
    )
  }
}
