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
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.Kilovoltamperes
import squants.energy.{KilowattHours, Kilowatts}
import squants.thermal.Celsius
import squants.{Power, Temperature}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.util.UUID
import scala.jdk.CollectionConverters.SeqHasAsJava

trait HpInputTestData extends NodeInputTestData with ThermalGridTestData {

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
    Kilovoltamperes(100d),
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
  )

  protected val defaultThermalGrid = new container.ThermalGrid(
    thermalBusInput,
    Seq(defaultThermalHouse).asJava,
    Seq.empty[ThermalStorageInput].asJava,
  )

  protected def thermalGrid(
      thermalHouse: ThermalHouse,
      thermalStorage: Option[ThermalStorage] = None,
  ): ThermalGrid =
    ThermalGrid(
      Some(thermalHouse),
      thermalStorage,
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
    )
  )

  protected def thermalStorage: ThermalStorage = CylindricalThermalStorage(
    UUID.fromString("d57ddc54-48bd-4c59-babf-330c7ba71a74"),
    "thermal storage",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    thermalBusInput,
    KilowattHours(20d),
    KilowattHours(500d),
    Kilowatts(10d),
    KilowattHours(0d),
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
  )

  protected def hpData: HpRelevantData =
    HpRelevantData(7200, Celsius(10d))

}
