/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.thermal.{
  DomesticHotWaterStorageInput,
  ThermalStorageInput,
}
import edu.ie3.datamodel.models.input.{OperatorInput, container}
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.util.UUID
import scala.jdk.CollectionConverters.SeqHasAsJava

trait ThermalGridITInputTestData
    extends NodeInputTestData
    with PvInputTestData
    with LoadInputTestData
    with HpInputTestData {

  protected val littleDomesticHotWaterStorageInput =
    new DomesticHotWaterStorageInput(
      UUID.fromString("e5997094-958a-486a-b4ea-863bf6cf42ec"),
      "domestic hot water storage to storage less than demand of one day for one person",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      thermalBusInput,
      Quantities.getQuantity(28.7, Units.LITRE),
      Quantities.getQuantity(55.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(10.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(1.16, StandardUnits.SPECIFIC_HEAT_CAPACITY),
      Quantities.getQuantity(11.0, PowerSystemUnits.KILOWATT),
    )

  protected val thermalGridForThermalGridIT = new container.ThermalGrid(
    thermalBusInput,
    Seq(typicalThermalHouse).asJava,
    Seq[ThermalStorageInput](typicalThermalStorage).asJava,
    Seq[ThermalStorageInput](littleDomesticHotWaterStorageInput).asJava,
  )
}
