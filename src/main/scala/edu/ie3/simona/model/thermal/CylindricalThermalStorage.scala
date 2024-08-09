package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{CylindricalStorageInput, ThermalBusInput}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.{DefaultQuantities, KilowattHoursPerKelvinCubicMeters, SpecificHeatCapacity}
import squants.space.{CubicMeters, Volume}
import squants.thermal.Celsius
import squants.time.Hours
import squants.{Energy, Power, Temperature}
import tech.units.indriya.unit.Units

import java.util.UUID

final case class CylindricalThermalStorage(
  uuid: UUID,
  id: String,
  operatorInput: OperatorInput,
  operationTime: OperationTime,
  bus: ThermalBusInput,
  minEnergyThreshold: Energy,
  maxEnergyThreshold: Energy,
  chargingPower: Power,
  override protected var _storedEnergy: Energy
) extends ThermalStorage(
  uuid,
  id,
  operatorInput,
  operationTime,
  bus,
  minEnergyThreshold,
  maxEnergyThreshold,
  chargingPower
) with MutableStorage with ThermalStorageCalculations

object CylindricalThermalStorage extends ThermalStorageCalculations {

  /** Function to construct a new [[CylindricalThermalStorage]] based on a
   * provided [[CylindricalStorageInput]]
   *
   * @param input
   * instance of [[CylindricalStorageInput]] this storage should be built
   * @param initialStoredEnergy
   * initial stored energy
   * @return
   * a ready-to-use [[CylindricalThermalStorage]] with referenced electric
   * parameters
   */
  def apply(
    input: CylindricalStorageInput,
    initialStoredEnergy: Energy = zeroKWH
  ): CylindricalThermalStorage = {
    val minEnergyThreshold = volumeToEnergy(
      CubicMeters(input.getStorageVolumeLvlMin.to(Units.CUBIC_METRE).getValue.doubleValue),
      KilowattHoursPerKelvinCubicMeters(input.getC.to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE).getValue.doubleValue),
      Celsius(input.getInletTemp.to(Units.CELSIUS).getValue.doubleValue),
      Celsius(input.getReturnTemp.to(Units.CELSIUS).getValue.doubleValue)
    )

    val maxEnergyThreshold = volumeToEnergy(
      CubicMeters(input.getStorageVolumeLvl.to(Units.CUBIC_METRE).getValue.doubleValue),
      KilowattHoursPerKelvinCubicMeters(input.getC.to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE).getValue.doubleValue),
      Celsius(input.getInletTemp.to(Units.CELSIUS).getValue.doubleValue),
      Celsius(input.getReturnTemp.to(Units.CELSIUS).getValue.doubleValue)
    )

    val chargingPower = (maxEnergyThreshold - minEnergyThreshold) / Hours(1)

    new CylindricalThermalStorage(
      input.getUuid,
      input.getId,
      input.getOperator,
      input.getOperationTime,
      input.getThermalBus,
      minEnergyThreshold,
      maxEnergyThreshold,
      chargingPower,
      initialStoredEnergy
    )
  }
}