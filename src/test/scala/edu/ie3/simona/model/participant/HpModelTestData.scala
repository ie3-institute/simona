/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.datamodel.models.input.system.`type`.HpTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.thermal.ThermalHouseInput
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.simona.model.participant.HpModel.{HpRelevantData, HpState}
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.{CylindricalThermalStorage, ThermalGrid, ThermalHouse, ThermalStorage}
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.util.UUID

trait HpModelTestData {
  protected val hpTypeInput = new HpTypeInput(
    UUID.randomUUID(),
    "HpTypeInput",
    null,
    null,
    Quantities.getQuantity(100, PowerSystemUnits.KILOVOLTAMPERE),
    0.95,
    Quantities.getQuantity(15, PowerSystemUnits.KILOWATT)
  )

  protected val hpInputModel = new HpInput(
    UUID.randomUUID(),
    "HpInput",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    null,
    null,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
    hpTypeInput
  )

  protected def hpModel(thermalGrid: ThermalGrid) = new HpModel(
    UUID.randomUUID(),
    "HpModel",
    null,
    1.0,
    null,
    Quantities.getQuantity(100, PowerSystemUnits.KILOWATT),
    0.95,
    Quantities.getQuantity(15, PowerSystemUnits.KILOWATT),
    thermalGrid
  )

  protected def thermalGrid(
      thermalHouse: ThermalHouse,
      thermalStorage: Option[ThermalStorage] = None
  ): ThermalGrid =
    ThermalGrid(
      Some(thermalHouse),
      thermalStorage
    )

  private val thermHouseUuid: UUID =
    UUID.fromString("75a43a0f-7c20-45ca-9568-949b728804ca")

  private val thermalStorageUuid: UUID =
    UUID.fromString("d57ddc54-48bd-4c59-babf-330c7ba71a74")

  protected def thermalHouse(
      lowerTemperatureBoundary: Double,
      upperTemperatureBoundary: Double
  ): ThermalHouse = ThermalHouse(
    new ThermalHouseInput(
      thermHouseUuid,
      "Thermal house",
      null,
      Quantities.getQuantity(1.0, StandardUnits.THERMAL_TRANSMISSION),
      Quantities.getQuantity(10.0, StandardUnits.HEAT_CAPACITY),
      Quantities.getQuantity(
        (lowerTemperatureBoundary + upperTemperatureBoundary) / 2.0,
        Units.CELSIUS
      ),
      Quantities.getQuantity(upperTemperatureBoundary, Units.CELSIUS),
      Quantities.getQuantity(lowerTemperatureBoundary, Units.CELSIUS)
    )
  )

  protected def thermalStorage: ThermalStorage = CylindricalThermalStorage(
    thermalStorageUuid,
    "thermal storage",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    null,
    Quantities.getQuantity(20d, StandardUnits.ENERGY_IN),
    Quantities.getQuantity(500d, StandardUnits.ENERGY_IN),
    Quantities.getQuantity(10d, StandardUnits.ACTIVE_POWER_IN),
    Quantities.getQuantity(0d, StandardUnits.ENERGY_IN)
  )

  protected def thermalState(
      temperature: Double,
      qDot: Double = 0d
  ): ThermalGridState = ThermalGridState(
    Some(
      ThermalHouseState(
        0L,
        Quantities.getQuantity(temperature, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(qDot, StandardUnits.ACTIVE_POWER_IN)
      )
    ),
    None
  )

  protected def hpData(hpState: HpState): HpRelevantData =
    HpRelevantData(hpState, 7200, Quantities.getQuantity(10, Units.CELSIUS))

}
