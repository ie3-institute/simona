/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.datamodel.models.input.system.`type`.HpTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.thermal.{
  CylindricalStorageInput,
  DomesticHotWaterStorageInput,
  ThermalHouseInput,
  ThermalStorageInput,
}
import edu.ie3.datamodel.models.input.{OperatorInput, container}
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.simona.model.participant.HpModel
import edu.ie3.simona.model.participant.HpModel.HpRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal._
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import squants.energy.{KilowattHours, Kilowatts}
import squants.thermal.Celsius
import squants.{Power, Temperature}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.util.UUID
import scala.jdk.CollectionConverters.{SeqHasAsJava, _}

trait HpInputTestData
    extends NodeInputTestData
    with ThermalGridTestData
    with DefaultTestData {

  protected val hpTypeInput = new HpTypeInput(
    UUID.fromString("9802bf35-2a4e-4ff5-be9b-cd9e6a78dcd6"),
    "HpTypeInput",
    Quantities.getQuantity(10000d, PowerSystemUnits.EURO),
    Quantities.getQuantity(200d, PowerSystemUnits.EURO_PER_MEGAWATTHOUR),
    Quantities.getQuantity(100, PowerSystemUnits.KILOVOLTAMPERE),
    0.95,
    Quantities.getQuantity(15, PowerSystemUnits.KILOWATT),
  )

  protected val hpInputModel = new HpInput(
    UUID.fromString("7832dea4-8703-4b37-8752-e67b86e957df"),
    "HpInput",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    thermalBusInput,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
    null,
    hpTypeInput,
  )

  protected def hpModel(thermalGrid: ThermalGrid) = new HpModel(
    UUID.randomUUID(),
    "HpModel",
    OperationInterval.apply(0L, 86400L),
    QControl.CosPhiFixed(0.95),
    Kilowatts(100d),
    0.95,
    Kilowatts(15d),
    thermalGrid,
  )
  protected val defaultThermalHouse = new ThermalHouseInput(
    UUID.fromString("91940626-bdd0-41cf-96dd-47c94c86b20e"),
    "Thermal house",
    thermalBusInput,
    Quantities.getQuantity(0.325, StandardUnits.THERMAL_TRANSMISSION),
    Quantities.getQuantity(75, StandardUnits.HEAT_CAPACITY),
    Quantities.getQuantity(21.0, StandardUnits.TEMPERATURE),
    Quantities.getQuantity(22.0, StandardUnits.TEMPERATURE),
    Quantities.getQuantity(20.0, StandardUnits.TEMPERATURE),
    "house",
    2.0,
  )

  protected val defaultDomesticHotWaterStorageInput =
    new DomesticHotWaterStorageInput(
      UUID.fromString("5a3935c0-14ff-4d7b-9e69-a101f41a3b73"),
      "default domestic hot water storage",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      thermalBusInput,
      Quantities.getQuantity(300.0, Units.LITRE),
      Quantities.getQuantity(60.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(10.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(1.16, StandardUnits.SPECIFIC_HEAT_CAPACITY),
      Quantities.getQuantity(11.0, PowerSystemUnits.KILOWATT),
    )

  protected val defaultThermalGrid = new container.ThermalGrid(
    thermalBusInput,
    Seq(defaultThermalHouse).asJava,
    Seq.empty[ThermalStorageInput].asJava,
    Seq(
      defaultDomesticHotWaterStorageInput.asInstanceOf[ThermalStorageInput]
    ).asJava,
  )

  protected val typicalThermalHouse = new ThermalHouseInput(
    UUID.fromString("74ac67b4-4743-416a-b731-1b5fe4a0a4e7"),
    "thermal house",
    thermalBusInput,
    Quantities.getQuantity(0.1, StandardUnits.THERMAL_TRANSMISSION),
    Quantities.getQuantity(7.5, StandardUnits.HEAT_CAPACITY),
    Quantities.getQuantity(20.0, StandardUnits.TEMPERATURE),
    Quantities.getQuantity(22.0, StandardUnits.TEMPERATURE),
    Quantities.getQuantity(18.0, StandardUnits.TEMPERATURE),
    "house",
    2.0,
  )

  protected val typicalThermalStorage: CylindricalStorageInput =
    new CylindricalStorageInput(
      UUID.fromString("4b8933dc-aeb6-4573-b8aa-59d577214150"),
      "thermal storage",
      thermalBusInput,
      Quantities.getQuantity(300.0, Units.LITRE),
      Quantities.getQuantity(60.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(30.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(1.16, StandardUnits.SPECIFIC_HEAT_CAPACITY),
      Quantities.getQuantity(11.0, PowerSystemUnits.KILOWATT),
    )

  protected val domesticHotWaterStorageInput: DomesticHotWaterStorageInput =
    new DomesticHotWaterStorageInput(
      UUID.fromString("77579045-6695-4cd3-be52-ffe81502182d"),
      "domestic hot water storage",
      thermalBusInput,
      Quantities.getQuantity(300.0, Units.LITRE),
      Quantities.getQuantity(60.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(30.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(1.16, StandardUnits.SPECIFIC_HEAT_CAPACITY),
      Quantities.getQuantity(11.0, PowerSystemUnits.KILOWATT),
    )

  protected val typicalThermalGrid = new container.ThermalGrid(
    thermalBusInput,
    Seq(typicalThermalHouse).asJava,
    Set[ThermalStorageInput](typicalThermalStorage).asJava,
    Seq[ThermalStorageInput](domesticHotWaterStorageInput).asJava,
  )

  protected val typicalHpTypeInput = new HpTypeInput(
    UUID.fromString("2829d5eb-352b-40df-a07f-735b65a0a7bd"),
    "TypicalHpTypeInput",
    Quantities.getQuantity(7500d, PowerSystemUnits.EURO),
    Quantities.getQuantity(200d, PowerSystemUnits.EURO_PER_MEGAWATTHOUR),
    Quantities.getQuantity(4, PowerSystemUnits.KILOVOLTAMPERE),
    0.95,
    Quantities.getQuantity(11, PowerSystemUnits.KILOWATT),
  )

  protected val typicalHpInputModel = new HpInput(
    UUID.fromString("1b5e928e-65a3-444c-b7f2-6a48af092224"),
    "TypicalHpInput",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    thermalBusInput,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
    null,
    typicalHpTypeInput,
  )

  protected def thermalGrid(
      thermalHouse: ThermalHouse,
      thermalStorage: Option[ThermalStorage] = None,
      domesticWaterStorage: Option[ThermalStorage] = None,
  ): ThermalGrid =
    ThermalGrid(
      Some(thermalHouse),
      thermalStorage,
      domesticWaterStorage,
    )

  protected def thermalHouse(
      lowerTemperatureBoundary: Double,
      upperTemperatureBoundary: Double,
  ): ThermalHouse = ThermalHouse(
    new ThermalHouseInput(
      UUID.fromString("75a43a0f-7c20-45ca-9568-949b728804ca"),
      "Thermal house",
      null,
      Quantities.getQuantity(1.0, StandardUnits.THERMAL_TRANSMISSION),
      Quantities.getQuantity(10.0, StandardUnits.HEAT_CAPACITY),
      Quantities.getQuantity(
        (lowerTemperatureBoundary + upperTemperatureBoundary) / 2.0,
        Units.CELSIUS,
      ),
      Quantities.getQuantity(upperTemperatureBoundary, Units.CELSIUS),
      Quantities.getQuantity(lowerTemperatureBoundary, Units.CELSIUS),
      "house",
      2.0,
    )
  )

  protected def thermalStorage: ThermalStorage = CylindricalThermalStorage(
    UUID.fromString("d57ddc54-48bd-4c59-babf-330c7ba71a74"),
    "thermal storage",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    thermalBusInput,
    KilowattHours(500d),
    Kilowatts(10d),
    KilowattHours(0d),
  )

  protected def domesticHotWaterStorage: ThermalStorage =
    DomesticHotWaterStorage(
      UUID.fromString("d57ddc54-48bd-4c59-babf-330c7ba71a74"),
      "domestic hot water storage",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      thermalBusInput,
      KilowattHours(250d),
      Kilowatts(1e-3),
      KilowattHours(250d),
    )

  protected def thermalState(
      temperature: Temperature,
      qDot: Power = Kilowatts(0d),
  ): ThermalGridState = ThermalGridState(
    Some(
      ThermalHouseState(
        0L,
        temperature,
        qDot,
      )
    ),
    None,
    None,
  )

  protected def hpData: HpRelevantData =
    HpRelevantData(7200, Celsius(10d), defaultSimulationStart, houseInhabitants)

}
