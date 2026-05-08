/*
 * © 2024-2026. TU Dortmund University,
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
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKWh
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import squants.{Energy, Power}

import java.util.UUID

/** A domestic hot water storage.
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
) extends AbstractThermalStorage(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus,
      maxEnergyThreshold,
      pThermalMax,
    ) {

  /** DHW storage starts full */
  override protected def initialEnergyLevel: Energy = maxEnergyThreshold
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
    *   a ready-to-use [[DomesticHotWaterStorage]] with referenced parameters
    */
  def apply(
      input: DomesticHotWaterStorageInput,
      initialStoredEnergy: Energy = zeroKWh,
  ): DomesticHotWaterStorage = {
    val maxEnergyThreshold =
      AbstractThermalStorage.calculateMaxEnergyThreshold(input)
    val pThermalMax = input.getpThermalMax().toSquants

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
