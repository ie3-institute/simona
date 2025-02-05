/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import squants.{Energy, Power, Seconds}

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
  * @param minEnergyThreshold
  *   Minimum permissible energy stored in the storage
  * @param maxEnergyThreshold
  *   Maximum permissible energy stored in the storage
  * @param chargingPower
  *   Thermal power, that can be charged / discharged
  */
abstract class ThermalStorage(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput,
    minEnergyThreshold: Energy,
    maxEnergyThreshold: Energy,
    chargingPower: Power,
) {

  /** In order to avoid faulty flexibility options, we want to avoid offering
    * charging/discharging that could last less than one second.
    */
  private val toleranceMargin = chargingPower * Seconds(1d)

  def getUuid: UUID = uuid

  def getMinEnergyThreshold: Energy = minEnergyThreshold

  def getMaxEnergyThreshold: Energy = maxEnergyThreshold

  def getChargingPower: Power = chargingPower

  def startingState: ThermalStorageState

  def isFull(energy: Energy): Boolean =
    energy > (maxEnergyThreshold - toleranceMargin)

  def isEmpty(energy: Energy): Boolean =
    energy < (minEnergyThreshold + toleranceMargin)

  def updateState(
      tick: Long,
      qDot: Power,
      lastState: ThermalStorageState,
  ): (ThermalStorageState, Option[ThermalThreshold])
}

object ThermalStorage {

  /** State of a thermal storage
    *
    * @param tick
    *   Last tick of storage state change
    * @param storedEnergy
    *   Energy stored in the storage at this tick
    * @param qDot
    *   Infeed to the heat storage (positive: Storage is charging, negative:
    *   Storage is discharging)
    */

  final case class ThermalStorageState(
      tick: Long,
      storedEnergy: Energy,
      qDot: Power,
  )

  object ThermalStorageThreshold {
    final case class StorageEmpty(override val tick: Long)
        extends ThermalThreshold
    final case class StorageFull(override val tick: Long)
        extends ThermalThreshold
  }
}
