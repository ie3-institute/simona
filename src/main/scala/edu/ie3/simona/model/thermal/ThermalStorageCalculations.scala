/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.util.scala.quantities.ThermalCapacitance
import edu.ie3.util.scala.quantities.SquantsUtils.calcVolume
import squants.space.Volume
import squants.{Energy, Temperature}

trait ThermalStorageCalculations {

  /** Equation from docs for the relation between needed volume and energy.
    *
    * @param volume
    *   needed/available volume
    * @param c
    *   Thermal capacitance (volumetric heat capacity)
    * @param inletTemp
    *   Inlet temperature
    * @param returnTemp
    *   Return temperature
    * @return
    *   energy
    */
  def volumeToEnergy(
      volume: Volume,
      c: ThermalCapacitance,
      inletTemp: Temperature,
      returnTemp: Temperature,
  ): Energy = {
    c.calcEnergy(returnTemp, inletTemp, volume)
  }

  /** Equation from docs for the relation between stored heat and volume change.
    *
    * @param energy
    *   available energy
    * @param c
    *   Thermal capacitance (volumetric heat capacity)
    * @param inletTemp
    *   Inlet temperature
    * @param returnTemp
    *   Return temperature
    * @return
    *   volume
    */
  def energyToVolume(
      energy: Energy,
      c: ThermalCapacitance,
      inletTemp: Temperature,
      returnTemp: Temperature,
  ): Volume = {
    val energyDensity = c.calcEnergyDensity(returnTemp, inletTemp)

    energy.calcVolume(energyDensity)
  }
}
