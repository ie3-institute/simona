/*
 * © 2020-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{
  AbstractStorageInput,
  ThermalBusInput,
}
import edu.ie3.simona.model.participant.ParticipantModel.ModelState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import squants.time.Seconds
import squants.{Energy, Power}

import java.util.UUID

/** Thermal storage model.
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
  */
abstract class ThermalStorage(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput,
    maxEnergyThreshold: Energy,
    pThermalMax: Power,
) {

  /** In order to avoid faulty flexibility options, we want to avoid offering
    * charging/discharging that could last less than one second.
    */
  private val toleranceMargin = pThermalMax * Seconds(1d)

  def getUuid: UUID = uuid

  def getMaxEnergyThreshold: Energy = maxEnergyThreshold

  def getpThermalMax: Power = pThermalMax

  def startingState: ThermalStorageState

  def isFull(energy: Energy): Boolean =
    energy > (maxEnergyThreshold - toleranceMargin)

  def isEmpty(energy: Energy): Boolean =
    energy < (zeroKWh + toleranceMargin)

  def determineState(
      tick: Long,
      lastThermalStorageState: ThermalStorageState,
      qDotHeatStorage: Power,
  ): ThermalStorageState

  def determineNextThreshold(
      state: ThermalStorageState,
      qDotHeatStorage: Power,
  ): Option[ThermalThreshold]
}

/** Abstract implementation of thermal storage with common behavior for state
  * determination and threshold calculations.
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
  */
abstract class AbstractThermalStorage(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput,
    maxEnergyThreshold: Energy,
    pThermalMax: Power,
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

  /** Abstract method to define the initial energy level for this storage type
    */
  protected def initialEnergyLevel: Energy

  /** Updates the given last state. Based on the then set thermal influx, the
    * current state is calculated. Positive values of influx are consider to
    * flow into the storage.
    *
    * @param tick
    *   Tick, where this change happens.
    * @param lastStorageState
    *   Last state of the storage.
    * @param qDotStorage
    *   Influx of the storage.
    * @return
    *   The state of the instance.
    */
  override def determineState(
      tick: Long,
      lastStorageState: ThermalStorageState,
      qDotStorage: Power,
  ): ThermalStorageState = {
    /* Determine new state based on time difference and given state */
    val energyBalance =
      qDotStorage * Seconds(tick - lastStorageState.tick)
    val newEnergy = lastStorageState.storedEnergy + energyBalance
    val updatedEnergy =
      if isFull(newEnergy) then maxEnergyThreshold
      else if isEmpty(newEnergy) then zeroKWh
      else newEnergy

    ThermalStorageState(tick, updatedEnergy)
  }

  /** Calculates the tick, when the next threshold of the instance is reached.
    *
    * @param storageState
    *   State of the storage.
    * @param qDotStorage
    *   Operating point of the storage.
    * @return
    *   The next threshold if there is one.
    */
  override def determineNextThreshold(
      storageState: ThermalStorageState,
      qDotStorage: Power,
  ): Option[ThermalThreshold] = {
    if qDotStorage > zeroKW then {
      val duration =
        (maxEnergyThreshold - storageState.storedEnergy) / qDotStorage
      val durationInTicks = Math.floor(duration.toSeconds).toLong
      if durationInTicks <= 0L then None
      else
        Some(
          ThermalStorage.ThermalStorageThreshold.StorageFull(
            storageState.tick + durationInTicks
          )
        )
    } else if qDotStorage < zeroKW then {
      val duration = storageState.storedEnergy / qDotStorage * -1
      val durationInTicks = Math.floor(duration.toSeconds).toLong
      if durationInTicks <= 0L then None
      else
        Some(
          ThermalStorage.ThermalStorageThreshold.StorageEmpty(
            storageState.tick + durationInTicks
          )
        )
    } else None
  }

  override def startingState: ThermalStorageState = ThermalStorageState(
    0L,
    initialEnergyLevel,
  )
}

object ThermalStorage {

  /** State of a thermal storage
    *
    * @param tick
    *   Last tick of storage state change.
    * @param storedEnergy
    *   Energy stored in the storage at this tick.
    */
  final case class ThermalStorageState(
      override val tick: Long,
      storedEnergy: Energy,
  ) extends ModelState

  object ThermalStorageThreshold {
    final case class StorageEmpty(override val tick: Long)
        extends ThermalThreshold
    final case class StorageFull(override val tick: Long)
        extends ThermalThreshold
  }
}

object AbstractThermalStorage extends ThermalStorageCalculations {

  /** Common method to calculate max energy threshold from storage input
    *
    * @param input
    *   Storage input with volume, capacity, and temperature parameters
    * @return
    *   Calculated maximum energy threshold
    */
  def calculateMaxEnergyThreshold(
      input: AbstractStorageInput
  ): Energy = {
    volumeToEnergy(
      input.getStorageVolumeLvl.toSquants,
      input.getC.toSquants,
      input.getInletTemp.toSquants,
      input.getReturnTemp.toSquants,
    )
  }
}
