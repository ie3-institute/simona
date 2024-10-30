/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{
  DomesticHotWaterStorageInput,
  ThermalBusInput,
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull,
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.{
  DefaultQuantities,
  KilowattHoursPerKelvinCubicMeters,
}
import squants.energy.Kilowatts
import squants.space.CubicMeters
import squants.thermal.Celsius
import squants.time.Seconds
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
    maxEnergyThreshold: Energy,
    chargingPower: Power,
    override protected var _storedEnergy: Energy,
) extends ThermalStorage(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus,
      maxEnergyThreshold,
      chargingPower,
    )
    with MutableStorage
    with ThermalStorageCalculations {

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
      qDot: Power,
      lastState: ThermalStorageState,
  ): (ThermalStorageState, Option[ThermalThreshold]) = {
    /* Determine new state based on time difference and given state */
    val energyBalance = lastState.qDot * Seconds(tick - lastState.tick)
    val newEnergy = lastState.storedEnergy + energyBalance
    val updatedEnergy =
      if (isFull(newEnergy))
        maxEnergyThreshold
      else if (isEmpty(newEnergy))
        zeroKWH
      else
        newEnergy

    /* Determine, when a threshold is reached */
    val nextThreshold =
      if (qDot > zeroMW) {
        val duration = (maxEnergyThreshold - updatedEnergy) / qDot
        val durationInTicks = Math.round(duration.toSeconds)
        if (durationInTicks <= 0L)
          None
        else
          Some(StorageFull(tick + durationInTicks))
      } else if (qDot < zeroMW) {
        val duration = updatedEnergy / qDot * (-1)
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

  override def startingState: ThermalStorageState = ThermalStorageState(
    -1L,
    maxEnergyThreshold,
    zeroKW,
  )

  @deprecated("Use thermal storage state instead")
  override def usableThermalEnergy: Energy =
    _storedEnergy

  @deprecated("Use thermal storage state instead")
  override def tryToStoreAndReturnRemainder(
      addedEnergy: Energy
  ): Option[Energy] = {
    if (addedEnergy > zeroKWH) {
      _storedEnergy = _storedEnergy + addedEnergy
      if (_storedEnergy > maxEnergyThreshold) {
        val surplus = _storedEnergy - maxEnergyThreshold
        _storedEnergy = maxEnergyThreshold
        return Option(surplus)
      }
    }
    Option.empty
  }

  @deprecated("Use thermal storage state instead")
  override def tryToTakeAndReturnLack(
      takenEnergy: Energy
  ): Option[Energy] = {
    if (takenEnergy > zeroKWH) {
      _storedEnergy = _storedEnergy - takenEnergy
      if (_storedEnergy < zeroKWH) {
        val lack = zeroKWH - _storedEnergy
        _storedEnergy = zeroKWH
        return Some(lack)
      }
    }
    None
  }

}

object DomesticHotWaterStorage extends ThermalStorageCalculations {

  /** Function to construct a new [[DomesticHotWaterStorage]] based on a
    * provided [[DomesticHotWaterStorageInput]]
    *
    * @param input
    *   instance of [[DomesticHotWaterStorageInput]] this storage should be
    *   built from
    * @return
    *   a ready-to-use [[DomesticHotWaterStorageStorage]] with referenced
    *   electric parameters
    */
  def apply(
      input: DomesticHotWaterStorageInput,
      initialStoredEnergy: Energy = DefaultQuantities.zeroKWH,
  ): DomesticHotWaterStorage = {
    val maxEnergyThreshold: Energy =
      volumeToEnergy(
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

    val chargingPower = Kilowatts(
      input
        .getpThermalMax()
        .to(PowerSystemUnits.KILOWATT)
        .getValue
        .doubleValue()
    )

    new DomesticHotWaterStorage(
      input.getUuid,
      input.getId,
      input.getOperator,
      input.getOperationTime,
      input.getThermalBus,
      maxEnergyThreshold,
      chargingPower,
      initialStoredEnergy,
    )
  }
}
