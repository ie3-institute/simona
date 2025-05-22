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
  ThermalBusInput,
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull,
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import edu.ie3.util.scala.quantities.KilowattHoursPerKelvinCubicMeters
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  PowerConversionSimona,
  TemperatureConversionSimona,
  VolumeConversionSimona,
  SpecificHeatCapacityConversionSimona,
}
import squants.time.Seconds
import squants.{Energy, Power}

import java.util.UUID

/** A cylindrical thermal storage used for implementations, which require a
  * mutable storage. <p> <strong>Important:</strong> The field storageLvl is a
  * variable.
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human-readable id
  * @param operatorInput
  *   Operator input
  * @param operationTime
  *   Operation time
  * @param bus
  *   Thermal bus input
  * @param maxEnergyThreshold
  *   Maximum permissible energy stored in the storage
  * @param pThermalMax
  *   Thermal power, that can be charged / discharged
  * @param storedEnergy
  *   Energy stored in the thermal storage
  */
final case class CylindricalThermalStorage(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput,
    maxEnergyThreshold: Energy,
    pThermalMax: Power,
    storedEnergy: Energy,
) extends ThermalStorage(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus,
      maxEnergyThreshold,
      pThermalMax,
    )
    with ThermalStorageCalculations {

  /** Updates the given last state. Based on the then set thermal influx, the
    * current state is calculated. Positive values of influx are consider to
    * flow into the storage.
    *
    * @param tick
    *   Tick, where this change happens.
    * @param lastThermalStorageState
    *   Last state of the heat storage.
    * @param qDotHeatStorage
    *   Influx of the heat storage.
    * @return
    *   The state of the instance.
    */
  override def determineState(
      tick: Long,
      lastThermalStorageState: ThermalStorageState,
      qDotHeatStorage: Power,
  ): ThermalStorageState = {
    /* Determine new state based on time difference and given state */
    val energyBalance =
      qDotHeatStorage * Seconds(
        tick - lastThermalStorageState.tick
      )
    val newEnergy = lastThermalStorageState.storedEnergy + energyBalance
    val updatedEnergy =
      if (isFull(newEnergy))
        maxEnergyThreshold
      else if (isEmpty(newEnergy))
        zeroKWh
      else
        newEnergy

    ThermalStorageState(tick, updatedEnergy)
  }

  /** Calculates the tick, when the next threshold of the instance is reached.
    *
    * @param thermalStorageState
    *   State of the heat storage.
    * @param qDotHeatStorage
    *   Operating point of the heat storage.
    * @return
    *   The next threshold if there is one.
    */
  override def determineNextThreshold(
      thermalStorageState: ThermalStorageState,
      qDotHeatStorage: Power,
  ): Option[ThermalThreshold] = {
    if (qDotHeatStorage > zeroKW) {
      val duration =
        (maxEnergyThreshold - thermalStorageState.storedEnergy) / qDotHeatStorage
      val durationInTicks = Math.floor(duration.toSeconds).toLong
      if (durationInTicks <= 0L)
        None
      else
        Some(StorageFull(thermalStorageState.tick + durationInTicks))
    } else if (qDotHeatStorage < zeroKW) {
      val duration =
        thermalStorageState.storedEnergy / qDotHeatStorage * -1
      val durationInTicks = Math.floor(duration.toSeconds).toLong
      if (durationInTicks <= 0L)
        None
      else
        Some(StorageEmpty(thermalStorageState.tick + durationInTicks))
    } else
      None
  }

  override def startingState: ThermalStorageState = ThermalStorageState(
    0L,
    zeroKWh,
  )
}

object CylindricalThermalStorage extends ThermalStorageCalculations {

  /** Function to construct a new [[CylindricalThermalStorage]] based on a
    * provided [[CylindricalStorageInput]]
    *
    * @param input
    *   instance of [[CylindricalStorageInput]] this storage should be built
    *   from
    * @param initialStoredEnergy
    *   initial stored energy
    * @return
    *   a ready-to-use [[CylindricalThermalStorage]] with referenced electric
    *   parameters
    */
  def apply(
      input: CylindricalStorageInput,
      initialStoredEnergy: Energy = zeroKWh,
  ): CylindricalThermalStorage = {
    val maxEnergyThreshold = volumeToEnergy(
      input.getStorageVolumeLvl.toSquants,
      input.getC.toSquants,
      input.getInletTemp.toSquants,
      input.getReturnTemp.toSquants,
    )

    val pThermalMax = input.getpThermalMax().toSquants

    new CylindricalThermalStorage(
      input.getUuid,
      input.getId,
      input.getOperator,
      input.getOperationTime,
      input.getThermalBus,
      maxEnergyThreshold,
      pThermalMax,
      initialStoredEnergy,
    )
  }
}
