/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{CylindricalStorageInput, ThermalBusInput}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.{DefaultQuantities, KilowattHoursPerKelvinCubicMeters}
import squants.space.CubicMeters
import squants.thermal.Celsius
import squants.time.Hours
import squants.{Energy, Power}
import tech.units.indriya.unit.Units

import java.util.UUID

/** A domestic hot water storage used for implementations, which require a
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
  * @param minEnergyThreshold
  *   Minimum permissible energy stored in the storage
  * @param maxEnergyThreshold
  *   Maximum permissible energy stored in the storage
  * @param chargingPower
  *   Thermal power, that can be charged / discharged
  */
final case class DomesticHotWaterStorage(
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

object DomesticHotWaterStorage {

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
      initialStoredEnergy: Energy = DefaultQuantities.zeroKWH,
  ): DomesticHotWaterStorage = {
    val minEnergyThreshold: Energy =
      CylindricalThermalStorage.volumeToEnergy(
        CubicMeters(
          input.getStorageVolumeLvlMin
            .to(Units.CUBIC_METRE)
            .getValue
            .doubleValue
        ),
        KilowattHoursPerKelvinCubicMeters(
          input.getC
            .to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE)
            .getValue
            .doubleValue
        ),
        Celsius(input.getInletTemp.to(Units.CELSIUS).getValue.doubleValue()),
        Celsius(input.getReturnTemp.to(Units.CELSIUS).getValue.doubleValue()),
      )

    val maxEnergyThreshold: Energy =
      CylindricalThermalStorage.volumeToEnergy(
        CubicMeters(
          input.getStorageVolumeLvl.to(Units.CUBIC_METRE).getValue.doubleValue
        ),
        KilowattHoursPerKelvinCubicMeters(
          input.getC
            .to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE)
            .getValue
            .doubleValue
        ),
        Celsius(input.getInletTemp.to(Units.CELSIUS).getValue.doubleValue()),
        Celsius(input.getReturnTemp.to(Units.CELSIUS).getValue.doubleValue()),
      )

    /* TODO: Currently, the input model does not define any maximum charge power. Assume, that the usable energy can
     *   be charged / discharged within the interval of an hour */
    val chargingPower = (maxEnergyThreshold - minEnergyThreshold) / Hours(1d)

    new DomesticHotWaterStorage(
      input.getUuid,
      input.getId,
      input.getOperator,
      input.getOperationTime,
      input.getThermalBus,
      minEnergyThreshold,
      maxEnergyThreshold,
      chargingPower,
      initialStoredEnergy,
    )
  }
}
