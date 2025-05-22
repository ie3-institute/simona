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
import edu.ie3.simona.model.InputModelContainer.WithHeatInputContainer
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.util.UUID
import scala.jdk.CollectionConverters.SeqHasAsJava

trait ThermalGridITInputTestData
    extends NodeInputTestData
    with PvInputTestData
    with LoadInputTestData
    with HpInputTestData
    with EmInputTestData {

  protected val littleDomesticHotWaterStorageInput =
    new DomesticHotWaterStorageInput(
      UUID.fromString("e5997094-958a-486a-b4ea-863bf6cf42ec"),
      "very little domestic hot water storage to storage less than demand of half a day for one person",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      thermalBusInput,
      Quantities.getQuantity(10.0, Units.LITRE),
      Quantities.getQuantity(55.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(10.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(1.16, StandardUnits.SPECIFIC_HEAT_CAPACITY),
      Quantities.getQuantity(5.0, PowerSystemUnits.KILOWATT),
    )

  protected val smallDomesticHotWaterStorageInput =
    new DomesticHotWaterStorageInput(
      UUID.fromString("1f64c8c5-0e48-4005-9bf1-7b030f24957a"),
      "domestic hot water storage to storage less than demand of one day for one person",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      thermalBusInput,
      Quantities.getQuantity(28.7, Units.LITRE),
      Quantities.getQuantity(55.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(10.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(1.16, StandardUnits.SPECIFIC_HEAT_CAPACITY),
      Quantities.getQuantity(5.0, PowerSystemUnits.KILOWATT),
    )

  protected val thermalGridForThermalGridITLittleWaterStorage =
    new container.ThermalGrid(
      thermalBusInput,
      Seq(typicalThermalHouse).asJava,
      Seq[ThermalStorageInput](typicalThermalStorage).asJava,
      Seq[ThermalStorageInput](littleDomesticHotWaterStorageInput).asJava,
    )

  protected val thermalGridForThermalGridITSmallWaterStorage =
    new container.ThermalGrid(
      thermalBusInput,
      Seq(typicalThermalHouse).asJava,
      Seq[ThermalStorageInput](typicalThermalStorage).asJava,
      Seq[ThermalStorageInput](smallDomesticHotWaterStorageInput).asJava,
    )

  protected val hpInputContainerLittleWaterStorage =
    WithHeatInputContainer(
      typicalHpInputModel,
      thermalGridForThermalGridITLittleWaterStorage,
    )

  protected val hpInputContainerSmallWaterStorage =
    WithHeatInputContainer(
      typicalHpInputModel,
      thermalGridForThermalGridITSmallWaterStorage,
    )
}
