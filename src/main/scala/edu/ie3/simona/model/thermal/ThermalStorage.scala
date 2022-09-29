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
  */
abstract class ThermalStorage(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput
) {
  protected val zeroEnergy: ComparableQuantity[Energy] =
    DefaultQuantities.zeroKWH
}

object ThermalStorage {
  final case class ThermalStorageState(
      tick: Long,
      storedEnergy: ComparableQuantity[Energy],
      qDot: ComparableQuantity[Power]
  )
}
