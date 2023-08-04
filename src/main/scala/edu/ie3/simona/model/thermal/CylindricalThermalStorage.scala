/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{
  CylindricalStorageInput,
  ThermalBusInput
}
import edu.ie3.util.scala.quantities.SquantsUtils.RichEnergy
import edu.ie3.util.scala.quantities.{
  DefaultQuantities,
  KilowattHoursPerKelvinCubicMeters,
  SpecificHeatCapacity
}
import squants.space.CubicMeters
import squants.thermal.Celsius

import java.util.UUID

/** A cylindrical thermal storage used for implementations, which require a
  * mutable storage. <p> <strong>Important:</strong> The field storageLvl is a
  * variable.
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human readable id
  * @param operatorInput
  *   Operator input
  * @param operationTime
  *   Operation time
  * @param bus
  *   Thermal bus input
  * @param storageVolumeLvlMax
  *   Maximal volume
  * @param storageVolumeLvlMin
  *   Minimal volume (can be greater than zero)
  * @param inletTemp
  *   Inlet temperature
  * @param returnTemp
  *   Return temperature
  * @param c
  *   Specific heat capacity
  */
final case class CylindricalThermalStorage(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput,
    storageVolumeLvlMax: squants.Volume,
    storageVolumeLvlMin: squants.Volume,
    inletTemp: squants.Temperature,
    returnTemp: squants.Temperature,
    c: SpecificHeatCapacity,
    override protected var _storedEnergy: squants.Energy
) extends ThermalStorage(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus
    )
    with MutableStorage {

  private def minEnergyThreshold: squants.Energy =
    CylindricalThermalStorage.volumeToEnergy(
      storageVolumeLvlMin,
      c,
      inletTemp,
      returnTemp
    )

  private def maxEnergyThreshold: squants.Energy =
    CylindricalThermalStorage.volumeToEnergy(
      storageVolumeLvlMax,
      c,
      inletTemp,
      returnTemp
    )

  override def usableThermalEnergy: squants.Energy =
    _storedEnergy - minEnergyThreshold

  override def tryToStoreAndReturnRemainder(
      addedEnergy: squants.Energy
  ): Option[squants.Energy] = {
    if (addedEnergy > zeroEnergy) {
      _storedEnergy = _storedEnergy + addedEnergy
      if (_storedEnergy > maxEnergyThreshold) {
        val surplus = _storedEnergy - maxEnergyThreshold
        _storedEnergy = maxEnergyThreshold
        return Option(surplus)
      }
    }
    Option.empty
  }

  override def tryToTakeAndReturnLack(
      takenEnergy: squants.Energy
  ): Option[squants.Energy] = {
    if (takenEnergy > zeroEnergy) {
      _storedEnergy = _storedEnergy - takenEnergy
      if (_storedEnergy < minEnergyThreshold) {
        val lack = minEnergyThreshold - _storedEnergy
        _storedEnergy = minEnergyThreshold
        return Option(lack)
      }
    }
    Option.empty
  }
}

case object CylindricalThermalStorage {

  /** Function to construct a new [[CylindricalThermalStorage]] based on a
    * provided [[CylindricalStorageInput]]
    *
    * @param input
    *   instance of [[CylindricalStorageInput]] this storage should be built
    *   from
    * @return
    *   a ready-to-use [[CylindricalThermalStorage]] with referenced electric
    *   parameters
    */
  def apply(
      input: CylindricalStorageInput,
      initialStoredEnergy: squants.Energy = DefaultQuantities.zeroKWH
  ): CylindricalThermalStorage =
    new CylindricalThermalStorage(
      input.getUuid,
      input.getId,
      input.getOperator,
      input.getOperationTime,
      input.getThermalBus,
      CubicMeters(input.getStorageVolumeLvl.getValue.doubleValue()),
      CubicMeters(input.getStorageVolumeLvlMin.getValue.doubleValue()),
      Celsius(input.getInletTemp.getValue.doubleValue()),
      Celsius(input.getReturnTemp.getValue.doubleValue()),
      KilowattHoursPerKelvinCubicMeters(input.getC.getValue.doubleValue()),
      initialStoredEnergy
    )

  /** Equation from docs for the relation between needed volume and energy.
    *
    * @param volume
    *   needed/available volume
    * @param c
    *   Specific heat capacity
    * @param inletTemp
    *   Inlet temperature
    * @param returnTemp
    *   Return temperature
    * @return
    *   energy
    */
  def volumeToEnergy(
      volume: squants.Volume,
      c: SpecificHeatCapacity,
      inletTemp: squants.Temperature,
      returnTemp: squants.Temperature
  ): squants.Energy = {
    c.multiply(returnTemp, inletTemp, volume)
  }

  /** Equation from docs for the relation between stored heat and volume change.
    *
    * @param energy
    *   available energy
    * @param c
    *   Specific heat capacity
    * @param inletTemp
    *   Inlet temperature
    * @param returnTemp
    *   Return temperature
    * @return
    *   volume
    */
  def energyToVolume(
      energy: squants.Energy,
      c: SpecificHeatCapacity,
      inletTemp: squants.Temperature,
      returnTemp: squants.Temperature
  ): squants.Volume = {
    val energyDensity = c * (returnTemp - inletTemp)

    energy.divideByEnergyDensity(energyDensity)
  }
}
