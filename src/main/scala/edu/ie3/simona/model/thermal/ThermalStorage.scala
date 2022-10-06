/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import java.util.UUID
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.simona.model.thermal.ThermalStorage.{
  ThermalStorageState,
  ThermalStorageThreshold
}
import edu.ie3.util.scala.quantities.DefaultQuantities

import javax.measure.quantity.{Energy, Power}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

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
  */
abstract class ThermalStorage(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput,
    minEnergyThreshold: ComparableQuantity[Energy],
    maxEnergyThreshold: ComparableQuantity[Energy]
) {
  protected val zeroEnergy: ComparableQuantity[Energy] =
    DefaultQuantities.zeroKWH

  def getUuid: UUID = uuid

  def getMinEnergyThreshold: ComparableQuantity[Energy] = minEnergyThreshold

  def getMaxEnergyThreshold: ComparableQuantity[Energy] = maxEnergyThreshold

  def startingState: ThermalStorageState

  def updateState(
      tick: Long,
      qDot: ComparableQuantity[Power],
      lastState: ThermalStorageState
  ): (ThermalStorageState, Option[ThermalStorageThreshold])
}

object ThermalStorage {
  final case class ThermalStorageState(
      tick: Long,
      storedEnergy: ComparableQuantity[Energy],
      qDot: ComparableQuantity[Power]
  )

  sealed trait ThermalStorageThreshold {
    val tick: Long
  }
  object ThermalHouseThreshold {
    final case class StorageEmpty(override val tick: Long)
        extends ThermalStorageThreshold
    final case class StorageFull(override val tick: Long)
        extends ThermalStorageThreshold
  }
}
