/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import java.util.UUID
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities

import javax.measure.quantity.{Energy, Power}
import tech.units.indriya.ComparableQuantity

/** Thermal storage model.
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
abstract class ThermalStorage(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput,
    minEnergyThreshold: ComparableQuantity[Energy],
    maxEnergyThreshold: ComparableQuantity[Energy],
    chargingPower: ComparableQuantity[Power]
) {
  protected val zeroEnergy: ComparableQuantity[Energy] =
    DefaultQuantities.zeroKWH

  /** In order to avoid faulty flexibility options, we want to avoid offering
    * charging/discharging that could last less than one second.
    */
  private val toleranceMargin = chargingPower
    .multiply(1d.asSecond)
    .asType(classOf[Energy])

  def getUuid: UUID = uuid

  def getMinEnergyThreshold: ComparableQuantity[Energy] = minEnergyThreshold

  def getMaxEnergyThreshold: ComparableQuantity[Energy] = maxEnergyThreshold

  def getChargingPower: ComparableQuantity[Power] = chargingPower

  def startingState: ThermalStorageState

  def isFull(energy: ComparableQuantity[Energy]): Boolean =
    energy.isGreaterThan(maxEnergyThreshold.subtract(toleranceMargin))

  def isEmpty(energy: ComparableQuantity[Energy]): Boolean =
    energy.isLessThan(minEnergyThreshold.add(toleranceMargin))

  def updateState(
      tick: Long,
      qDot: ComparableQuantity[Power],
      lastState: ThermalStorageState
  ): (ThermalStorageState, Option[ThermalThreshold])
}

object ThermalStorage {
  final case class ThermalStorageState(
      tick: Long,
      storedEnergy: ComparableQuantity[Energy],
      qDot: ComparableQuantity[Power]
  )

  object ThermalStorageThreshold {
    final case class StorageEmpty(override val tick: Long)
        extends ThermalThreshold
    final case class StorageFull(override val tick: Long)
        extends ThermalThreshold
  }
}
