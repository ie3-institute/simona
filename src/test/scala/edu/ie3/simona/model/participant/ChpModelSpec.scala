/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.ChpInput
import edu.ie3.datamodel.models.input.system.`type`.ChpTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.thermal.{
  CylindricalStorageInput,
  ThermalBusInput,
}
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.simona.model.participant.ChpModel.{ChpRelevantData, ChpState}
import edu.ie3.simona.model.thermal.CylindricalThermalStorage
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits.{
  EURO,
  EURO_PER_MEGAWATTHOUR,
  KILOVOLTAMPERE,
  KILOWATT,
}
import edu.ie3.util.scala.quantities._
import org.scalatest.BeforeAndAfterAll
import squants.energy.{KilowattHours, Kilowatts}
import squants.space.CubicMeters
import squants.thermal.Celsius
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.Units.PERCENT

import java.util.UUID

class ChpModelSpec extends UnitSpec with BeforeAndAfterAll {

  implicit val Tolerance: Double = 1e-12
  val chpStateNotRunning: ChpState =
    ChpState(isRunning = false, 0, Kilowatts(0), KilowattHours(0))
  val chpStateRunning: ChpState =
    ChpState(isRunning = true, 0, Kilowatts(0), KilowattHours(0))
  var storageInput: CylindricalStorageInput = _
  var chpInput: ChpInput = _

  override def beforeAll(): Unit = {
    setupSpec()
  }

  def setupSpec(): Unit = {
    val thermalBus = new ThermalBusInput(UUID.randomUUID(), "thermal bus")

    storageInput = new CylindricalStorageInput(
      UUID.randomUUID(),
      "ThermalStorage",
      thermalBus,
      getQuantity(100, StandardUnits.VOLUME),
      getQuantity(20, StandardUnits.VOLUME),
      getQuantity(30, StandardUnits.TEMPERATURE),
      getQuantity(40, StandardUnits.TEMPERATURE),
      getQuantity(1.15, StandardUnits.SPECIFIC_HEAT_CAPACITY),
    )

    val chpTypeInput = new ChpTypeInput(
      UUID.randomUUID(),
      "ChpTypeInput",
      Quantities.getQuantity(10000d, EURO),
      Quantities.getQuantity(200, EURO_PER_MEGAWATTHOUR),
      Quantities.getQuantity(19, PERCENT),
      Quantities.getQuantity(76, PERCENT),
      Quantities.getQuantity(100, KILOVOLTAMPERE),
      0.95,
      Quantities.getQuantity(50d, KILOWATT),
      Quantities.getQuantity(0, KILOWATT),
    )

    chpInput = new ChpInput(
      UUID.randomUUID(),
      "ChpInput",
      null,
      OperationTime.notLimited(),
      null,
      thermalBus,
      new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
      null,
      chpTypeInput,
      null,
      false,
    )
  }

  def buildChpModel(thermalStorage: CylindricalThermalStorage): ChpModel = {
    ChpModel(
      UUID.randomUUID(),
      "ChpModel",
      null,
      null,
      Kilowatts(100),
      0.95,
      Kilowatts(50),
      thermalStorage,
    )
  }

  def buildChpRelevantData(
      chpState: ChpState,
      heatDemand: Double,
  ): ChpRelevantData = {
    ChpRelevantData(chpState, KilowattHours(heatDemand), 7200)
  }

  def buildThermalStorage(
      storageInput: CylindricalStorageInput,
      storageLvl: Double,
  ): CylindricalThermalStorage = {
    val storedEnergy = CylindricalThermalStorage.volumeToEnergy(
      CubicMeters(storageLvl),
      KilowattHoursPerKelvinCubicMeters(1),
      Celsius(110.0),
      Celsius(90.0),
    )
    CylindricalThermalStorage(storageInput, storedEnergy)
  }

  "A ChpModel" should {
    "calculate active power correctly" in {
      val chpData = buildChpRelevantData(chpStateNotRunning, 0)
      val thermalStorage = buildThermalStorage(storageInput, 90)
      val chpModel = buildChpModel(thermalStorage)

      val activePower = chpModel.calculateNextState(chpData).activePower
      activePower.toKilowatts should be(0)
    }

    "calculate thermal energy correctly" in {
      val chpData = buildChpRelevantData(chpStateRunning, 10)
      val thermalStorage = buildThermalStorage(storageInput, 90)
      val chpModel = buildChpModel(thermalStorage)

      val nextState = chpModel.calculateNextState(chpData)
      val thermalEnergy = nextState.thermalEnergy
      thermalEnergy.toKilowattHours should be(-640)
    }

    "store energy correctly" in {
      val chpData = buildChpRelevantData(chpStateRunning, 1)
      val thermalStorage = buildThermalStorage(storageInput, 92)
      val chpModel = buildChpModel(thermalStorage)

      chpModel.calculateNextState(chpData)
      thermalStorage._storedEnergy.toKilowattHours should be(1150)
    }
  }
}
