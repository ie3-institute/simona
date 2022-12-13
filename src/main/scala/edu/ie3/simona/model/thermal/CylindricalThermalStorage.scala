/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{
  CylindricalStorageInput,
  ThermalBusInput
}
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull
}
import edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR
import edu.ie3.util.quantities.interfaces.SpecificHeatCapacity
import squants.energy.{KilowattHours, Kilowatts, Megawatts}
import squants.time.{Hours, Seconds}
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity._

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
    minEnergyThreshold: squants.Energy,
    maxEnergyThreshold: squants.Energy,
    chargingPower: squants.Power,
    override protected var _storedEnergy: squants.Energy
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
      qDot: squants.Power,
      lastState: ThermalStorageState
  ): (ThermalStorageState, Option[ThermalThreshold]) = {
    /* Determine new state based on time difference and given state */
    val energyBalance = lastState.qDot * Seconds(tick - lastState.tick)
    val newEnergy = lastState.storedEnergy + energyBalance
    val updatedEnergy =
      if (isFull(newEnergy))
        maxEnergyThreshold
      else if (isEmpty(newEnergy))
        minEnergyThreshold
      else
        newEnergy

    /* Determine, when a threshold is reached */
    val nextThreshold =
      if (qDot > Megawatts(0d)) {
        val duration = (maxEnergyThreshold - updatedEnergy) / qDot
        val durationInTicks = Math.round(duration.toSeconds)
        if (durationInTicks <= 0L)
          None
        else
          Some(StorageFull(tick + durationInTicks))
      } else if (qDot < Megawatts(0d)) {
        val duration = (updatedEnergy - minEnergyThreshold) / qDot * (-1)
        val durationInTicks = Math.round(duration.toSeconds)
        if (durationInTicks <= 0L)
          None
        else
          Some(StorageEmpty(tick + durationInTicks))
      } else {
        return (ThermalStorageState(tick, updatedEnergy, qDot), None)
      }
    (ThermalStorageState(tick, updatedEnergy, qDot), nextThreshold)
  }

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

  override def startingState: ThermalStorageState = ThermalStorageState(
    -1L,
    getMinEnergyThreshold,
    Kilowatts(0d)
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
      initialStoredEnergy: squants.Energy = KilowattHours(0d)
  ): CylindricalThermalStorage = {
    val minEnergyThreshold: squants.Energy =
      KilowattHours(
        CylindricalThermalStorage
          .volumeToEnergy(
            input.getStorageVolumeLvlMin,
            input.getC,
            input.getInletTemp,
            input.getReturnTemp
          )
          .to(KILOWATTHOUR)
          .getValue
          .doubleValue
      )

    val maxEnergyThreshold: squants.Energy =
      KilowattHours(
        CylindricalThermalStorage
          .volumeToEnergy(
            input.getStorageVolumeLvl,
            input.getC,
            input.getInletTemp,
            input.getReturnTemp
          )
          .to(KILOWATTHOUR)
          .getValue
          .doubleValue
      )

    /* TODO: Currently, the input model does not define any maximum charge power. Assume, that the usable energy can
     *   be charged / discharged within the interval of an hour */
    val chargingPower = (maxEnergyThreshold - minEnergyThreshold) / Hours(1d)

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
      .to(StandardUnits.ENERGY_IN)
}
