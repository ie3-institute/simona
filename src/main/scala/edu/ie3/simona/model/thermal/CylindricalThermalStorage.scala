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
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKWh
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import squants.{Energy, Power}

import java.util.UUID

/** A cylindrical thermal storage.
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
    override val uuid: UUID,
    override val id: String,
    override val operatorInput: OperatorInput,
    override val operationTime: OperationTime,
    override val bus: ThermalBusInput,
    override val maxEnergyThreshold: Energy,
    override val pThermalMax: Power,
    storedEnergy: Energy,
) extends AbstractThermalStorage(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus,
      maxEnergyThreshold,
      pThermalMax,
    ) {

  /** Cylindrical storage starts empty */
  override protected def initialEnergyLevel: Energy = zeroKWh
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
    *   a ready-to-use [[CylindricalThermalStorage]] with referenced parameters
    */
  def apply(
      input: CylindricalStorageInput,
      initialStoredEnergy: Energy = zeroKWh,
  ): CylindricalThermalStorage = {
    val maxEnergyThreshold =
      AbstractThermalStorage.calculateMaxEnergyThreshold(input)
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
