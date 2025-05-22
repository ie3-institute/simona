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
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  SpecificHeatCapacityConversionSimona,
  TemperatureConversionSimona,
  VolumeConversionSimona,
}
import squants.energy.Kilowatts
import squants.time.Seconds
import squants.{Energy, Power}

import java.util.UUID

/** A domestic hot water storage used for implementations, which require a
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
final case class DomesticHotWaterStorage(
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
    * @param thermalStorageState
    *   Last state of the heat storage.
    * @param qDotHeatStorage
    *   Influx of the heat storage.
    * @return
    *   The state of the instance.
    */
  override def determineState(
      tick: Long,
      thermalStorageState: ThermalStorageState,
      qDotWaterStorage: Power,
  ): ThermalStorageState = {
    /* Determine new state based on time difference and given state */
    val energyBalance =
      qDotWaterStorage * Seconds(
        tick - thermalStorageState.tick
      )
    val newEnergy = thermalStorageState.storedEnergy + energyBalance
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
    * @param domesticWaterStorageState
    *   State of the heat storage.
    * @param qDotWaterStorage
    *   Operating point of the domestic hot water storage.
    * @return
    *   The next threshold if there is one.
    */

  override def determineNextThreshold(
      domesticWaterStorageState: ThermalStorageState,
      qDotWaterStorage: Power,
  ): Option[ThermalThreshold] = {
    if (qDotWaterStorage > zeroKW) {
      val duration =
        (maxEnergyThreshold - domesticWaterStorageState.storedEnergy) / qDotWaterStorage
      val durationInTicks = Math.floor(duration.toSeconds).toLong
      if (durationInTicks <= 0L)
        None
      else
        Some(StorageFull(domesticWaterStorageState.tick + durationInTicks))
    } else if (qDotWaterStorage < zeroKW) {
      val duration =
        domesticWaterStorageState.storedEnergy / qDotWaterStorage * -1
      val durationInTicks = Math.floor(duration.toSeconds).toLong
      if (durationInTicks <= 0L)
        None
      else
        Some(StorageEmpty(domesticWaterStorageState.tick + durationInTicks))
    } else
      None
  }

  override def startingState: ThermalStorageState = ThermalStorageState(
    0L,
    maxEnergyThreshold,
  )
}

object DomesticHotWaterStorage extends ThermalStorageCalculations {

  /** Function to construct a new [[DomesticHotWaterStorage]] based on a
    * provided [[DomesticHotWaterStorageInput]]
    *
    * @param input
    *   instance of [[DomesticHotWaterStorageInput]] this storage should be
    *   built from
    * @param initialStoredEnergy
    *   initial stored energy
    * @return
    *   a ready-to-use [[DomesticHotWaterStorageStorage]] with referenced
    *   electric parameters
    */
  def apply(
      input: DomesticHotWaterStorageInput,
      initialStoredEnergy: Energy = zeroKWh,
  ): DomesticHotWaterStorage = {
    val maxEnergyThreshold = volumeToEnergy(
      input.getStorageVolumeLvl.toSquants,
      input.getC.toSquants,
      input.getInletTemp.toSquants,
      input.getReturnTemp.toSquants,
    )

    val pThermalMax = Kilowatts(
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
      pThermalMax,
      initialStoredEnergy,
    )
  }
}
