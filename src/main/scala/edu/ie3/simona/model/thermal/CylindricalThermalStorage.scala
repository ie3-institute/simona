/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import java.util.UUID
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{
  CylindricalStorageInput,
  ThermalBusInput
}
import edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR
import edu.ie3.util.quantities.interfaces.SpecificHeatCapacity

import javax.measure.quantity.{Energy, Temperature, Volume}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

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
    storageVolumeLvlMax: ComparableQuantity[Volume],
    storageVolumeLvlMin: ComparableQuantity[Volume],
    inletTemp: ComparableQuantity[Temperature],
    returnTemp: ComparableQuantity[Temperature],
    c: ComparableQuantity[SpecificHeatCapacity],
    override protected var _storedEnergy: ComparableQuantity[Energy]
) extends ThermalStorage(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus
    )
    with MutableStorage {

  private def minEnergyThreshold: ComparableQuantity[Energy] =
    CylindricalThermalStorage.volumeToEnergy(
      storageVolumeLvlMin,
      c,
      inletTemp,
      returnTemp
    )

  private def maxEnergyThreshold: ComparableQuantity[Energy] =
    CylindricalThermalStorage.volumeToEnergy(
      storageVolumeLvlMax,
      c,
      inletTemp,
      returnTemp
    )

  override def usableThermalEnergy: ComparableQuantity[Energy] =
    _storedEnergy.subtract(minEnergyThreshold)

  override def tryToStoreAndReturnRemainder(
      addedEnergy: ComparableQuantity[Energy]
  ): Option[ComparableQuantity[Energy]] = {
    if (addedEnergy isGreaterThan zeroEnergy) {
      _storedEnergy = _storedEnergy add addedEnergy
      if (_storedEnergy isGreaterThan maxEnergyThreshold) {
        val surplus = _storedEnergy subtract maxEnergyThreshold
        _storedEnergy = maxEnergyThreshold
        return Option(surplus)
      }
    }
    Option.empty
  }

  override def tryToTakeAndReturnLack(
      takenEnergy: ComparableQuantity[Energy]
  ): Option[ComparableQuantity[Energy]] = {
    if (takenEnergy isGreaterThan zeroEnergy) {
      _storedEnergy = _storedEnergy subtract takenEnergy
      if (_storedEnergy isLessThan minEnergyThreshold) {
        val lack = minEnergyThreshold subtract _storedEnergy
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
      initialStoredEnergy: ComparableQuantity[Energy] =
        Quantities.getQuantity(0, KILOWATTHOUR)
  ): CylindricalThermalStorage =
    new CylindricalThermalStorage(
      input.getUuid,
      input.getId,
      input.getOperator,
      input.getOperationTime,
      input.getThermalBus,
      input.getStorageVolumeLvl,
      input.getStorageVolumeLvlMin,
      input.getInletTemp,
      input.getReturnTemp,
      input.getC,
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
      volume: ComparableQuantity[Volume],
      c: ComparableQuantity[SpecificHeatCapacity],
      inletTemp: ComparableQuantity[Temperature],
      returnTemp: ComparableQuantity[Temperature]
  ): ComparableQuantity[Energy] =
    volume
      .multiply(c)
      .multiply(returnTemp.subtract(inletTemp))
      .asType(classOf[Energy])

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
      energy: ComparableQuantity[Energy],
      c: ComparableQuantity[SpecificHeatCapacity],
      inletTemp: ComparableQuantity[Temperature],
      returnTemp: ComparableQuantity[Temperature]
  ): ComparableQuantity[Volume] =
    energy
      .divide(c.multiply(returnTemp.subtract(inletTemp)))
      .asType(classOf[Volume])

}
