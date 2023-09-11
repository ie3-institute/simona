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
  * @param minEnergyThreshold
  *   Minimum permissible energy stored in the storage
  * @param maxEnergyThreshold
  *   Maximum permissible energy stored in the storage
  * @param chargingPower
  *   Thermal power, that can be charged / discharged
  */
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
    )
    with MutableStorage {
  //TODO DF Squants
  /** Updates the given last state. Based on the then set thermal influx, the
    * current state is calculated. Positive values of influx are consider to
    * flow into the storage. Additionally, the tick, when the next threshold is
    * reached, is calculated as well.
    *
    * @param tick
    *   Tick, where this change happens
    * @param qDot
    *   Influx
    * @param lastState
    *   Last known state
    * @return
    *   The updated state as well as the tick, when a threshold is reached
    */
  override def updateState(
      tick: Long,
      qDot: ComparableQuantity[Power],
      lastState: ThermalStorageState
  ): (ThermalStorageState, Option[ThermalThreshold]) = {
    /* Determine new state based on time difference and given state */
    val energyBalance = lastState.qDot
      .multiply(Quantities.getQuantity(tick - lastState.tick, Units.SECOND))
      .asType(classOf[Energy])
      .to(StandardUnits.ENERGY_IN)
    val newEnergy = lastState.storedEnergy add energyBalance
    val updatedEnergy =
      if (newEnergy.isGreaterThan(maxEnergyThreshold))
        maxEnergyThreshold
      else if (newEnergy.isLessThan(minEnergyThreshold))
        minEnergyThreshold
      else
        newEnergy

    /* Determine, when a threshold is reached */
    val nextThreshold =
      if (
        qDot.isGreaterThan(
          Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
        )
      ) {
        val duration =
          ((maxEnergyThreshold subtract updatedEnergy) divide qDot) asType classOf[
            Time
          ] to Units.SECOND
        Some(StorageFull(tick + max(duration.getValue.longValue(), 0L)))
      } else if (
        qDot.isLessThan(
          Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
        )
      ) {
        val duration =
          ((updatedEnergy subtract minEnergyThreshold) divide qDot.multiply(
            -1
          )) asType classOf[Time] to Units.SECOND
        Some(StorageEmpty(tick + max(duration.getValue.longValue(), 0L)))
      } else {
        return (ThermalStorageState(tick, updatedEnergy, qDot), None)
      }
    (ThermalStorageState(tick, updatedEnergy, qDot), nextThreshold)
  }

  override def usableThermalEnergy: Energy =
    _storedEnergy - minEnergyThreshold

  override def tryToStoreAndReturnRemainder(
      addedEnergy: Energy
  ): Option[Energy] = {
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
      takenEnergy: Energy
  ): Option[Energy] = {
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

  override def startingState: ThermalStorageState = ThermalStorageState(
    -1L,
    getMinEnergyThreshold,
    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
  )
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
      initialStoredEnergy: Energy = DefaultQuantities.zeroKWH
  ): CylindricalThermalStorage = {
    val minEnergyThreshold: ComparableQuantity[Energy] =
      CylindricalThermalStorage.volumeToEnergy(
        input.getStorageVolumeLvlMin,
        input.getC,
        input.getInletTemp,
        input.getReturnTemp
      )

    val maxEnergyThreshold: ComparableQuantity[Energy] =
      CylindricalThermalStorage.volumeToEnergy(
        input.getStorageVolumeLvl,
        input.getC,
        input.getInletTemp,
        input.getReturnTemp
      )

    /* TODO: Currently, the input model does not define any maximum charge power. Assume, that the usable energy can
     *   be charged / discharged within the interval of an hour */
    val chargingPower = maxEnergyThreshold
      .subtract(minEnergyThreshold)
      .divide(Quantities.getQuantity(1d, Units.HOUR))
      .asType(classOf[Power])
      .to(StandardUnits.ACTIVE_POWER_IN)
    new CylindricalThermalStorage(
      input.getUuid,
      input.getId,
      input.getOperator,
      input.getOperationTime,
      input.getThermalBus,
      //TODO DF Squants
      minEnergyThreshold,
      maxEnergyThreshold,
      chargingPower,
      initialStoredEnergy
    )
  }

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
      volume: Volume,
      c: SpecificHeatCapacity,
      inletTemp: Temperature,
      returnTemp: Temperature
  ): Energy = {
    c.calcEnergy(returnTemp, inletTemp, volume)
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
      energy: Energy,
      c: SpecificHeatCapacity,
      inletTemp: Temperature,
      returnTemp: Temperature
  ): Volume = {
    val energyDensity = c.calcEnergyDensity(returnTemp, inletTemp)

    energy.calcVolume(energyDensity)
  }
}
