/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.ChpInput
import edu.ie3.datamodel.models.input.system.`type`.ChpTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.thermal.{
  CylindricalStorageInput,
  ThermalBusInput,
}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.simona.model.participant.ChpModel.{ChpRelevantData, ChpState}
import edu.ie3.simona.model.thermal.CylindricalThermalStorage
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits.{
  EURO,
  EURO_PER_MEGAWATTHOUR,
  KILOVOLTAMPERE,
  KILOWATT,
}
import edu.ie3.util.scala.quantities._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{KilowattHours, Kilowatts}
import squants.space.CubicMeters
import squants.thermal.Celsius
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.Units
import tech.units.indriya.unit.Units.PERCENT
import testutils.TestObjectFactory

import java.util.UUID

class ChpModelSpec
    extends UnitSpec
    with BeforeAndAfterAll
    with TableDrivenPropertyChecks
    with DefaultTestData {

  implicit val Tolerance: Double = 1e-12
  val chpStateNotRunning: ChpState =
    ChpState(isRunning = false, 0, Kilowatts(0), KilowattHours(0))
  val chpStateRunning: ChpState =
    ChpState(isRunning = true, 0, Kilowatts(42), KilowattHours(42))

  val (storageInput, chpInput) = setupSpec()

  def setupSpec(): (CylindricalStorageInput, ChpInput) = {
    val thermalBus = new ThermalBusInput(UUID.randomUUID(), "thermal bus")

    val storageInput = new CylindricalStorageInput(
      UUID.randomUUID(),
      "ThermalStorage",
      thermalBus,
      getQuantity(100, StandardUnits.VOLUME),
      getQuantity(0, StandardUnits.VOLUME),
      getQuantity(30, StandardUnits.TEMPERATURE),
      getQuantity(40, StandardUnits.TEMPERATURE),
      getQuantity(1.15, StandardUnits.SPECIFIC_HEAT_CAPACITY),
    )

    val chpTypeInput = new ChpTypeInput(
      UUID.randomUUID(),
      "ChpTypeInput",
      getQuantity(10000d, EURO),
      getQuantity(200, EURO_PER_MEGAWATTHOUR),
      getQuantity(19, PERCENT),
      getQuantity(76, PERCENT),
      getQuantity(100, KILOVOLTAMPERE),
      0.95,
      getQuantity(50d, KILOWATT),
      getQuantity(0, KILOWATT),
    )

    val chpInput = new ChpInput(
      UUID.randomUUID(),
      "ChpInput",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      TestObjectFactory
        .buildNodeInput(false, GermanVoltageLevelUtils.MV_10KV, 0),
      thermalBus,
      new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
      null,
      chpTypeInput,
      null,
      false,
    )

    (storageInput, chpInput)
  }

  def buildChpModel(thermalStorage: CylindricalThermalStorage): ChpModel = {
    ChpModel(
      UUID.randomUUID(),
      "ChpModel",
      null,
      null,
      Kilovoltamperes(100),
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
      volume: Double,
  ): CylindricalThermalStorage = {
    val storedEnergy = CylindricalThermalStorage.volumeToEnergy(
      CubicMeters(volume),
      KilowattHoursPerKelvinCubicMeters(
        storageInput.getC
          .to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE)
          .getValue
          .doubleValue
      ),
      Celsius(
        storageInput.getInletTemp.to(Units.CELSIUS).getValue.doubleValue()
      ),
      Celsius(
        storageInput.getReturnTemp.to(Units.CELSIUS).getValue.doubleValue()
      ),
    )
    CylindricalThermalStorage(storageInput, storedEnergy)
  }

  "A ChpModel" should {
    "Check active power after calculating next state with #chpState and heat demand #heatDemand kWh:" in {
      val testCases = Table(
        ("chpState", "storageLvl", "heatDemand", "expectedActivePower"),
        (chpStateNotRunning, 70, 0, 0), // tests case (false, false, true)
        (
          chpStateNotRunning,
          70,
          8 * 115,
          95,
        ), // tests case (false, true, false)
        (chpStateNotRunning, 70, 10, 0), // tests case (false, true, true)
        (chpStateRunning, 70, 0, 95), // tests case (true, false, true)
        (chpStateRunning, 70, 8 * 115, 95), // tests case (true, true, false)
        (chpStateRunning, 70, 10, 95), // tests case (true, true, true)
        (
          chpStateRunning,
          70,
          7 * 115 + 1,
          95,
        ), // test case (_, true, false) and demand covered together with chp
        (
          chpStateRunning,
          70,
          9 * 115,
          95,
        ), // test case (_, true, false) and demand not covered together with chp
        (
          chpStateRunning,
          72,
          1,
          95,
        ), // test case (true, true, true) and storage volume exceeds maximum
      )

      forAll(testCases) {
        (chpState, storageLvl, heatDemand, expectedActivePower) =>
          val chpData = buildChpRelevantData(chpState, heatDemand)
          val thermalStorage = buildThermalStorage(storageInput, storageLvl)
          val chpModel = buildChpModel(thermalStorage)

          val activePower = chpModel.calculateNextState(chpData).activePower
          activePower.toKilowatts shouldEqual expectedActivePower
      }
    }

    "Check total energy after calculating next state with #chpState and heat demand #heatDemand kWh:" in {
      val testCases = Table(
        ("chpState", "storageLvl", "heatDemand", "expectedTotalEnergy"),
        (chpStateNotRunning, 70, 0, 0), // tests case (false, false, true)
        (
          chpStateNotRunning,
          70,
          8 * 115,
          100,
        ), // tests case (false, true, false)
        (chpStateNotRunning, 70, 10, 0), // tests case (false, true, true)
        (chpStateRunning, 70, 0, 100), // tests case (true, false, true)
        (chpStateRunning, 70, 8 * 115, 100), // tests case (true, true, false)
        (chpStateRunning, 70, 10, 100), // tests case (true, true, true)
        (
          chpStateRunning,
          70,
          7 * 115 + 1,
          100,
        ), // test case (_, true, false) and demand covered together with chp
        (
          chpStateRunning,
          70,
          9 * 115,
          100,
        ), // test case (_, true, false) and demand not covered together with chp
        (
          chpStateRunning,
          72,
          1,
          100,
        ), // test case (true, true, true) and storage volume exceeds maximum
      )

      forAll(testCases) {
        (chpState, storageLvl, heatDemand, expectedTotalEnergy) =>
          val chpData = buildChpRelevantData(chpState, heatDemand)
          val thermalStorage = buildThermalStorage(storageInput, storageLvl)
          val chpModel = buildChpModel(thermalStorage)

          val nextState = chpModel.calculateNextState(chpData)
          val thermalEnergy = nextState.thermalEnergy
          thermalEnergy.toKilowattHours shouldEqual expectedTotalEnergy
      }
    }

    "Check storage level after calculating next state with #chpState and heat demand #heatDemand kWh:" in {
      val testCases = Table(
        ("chpState", "storageLvl", "heatDemand", "expectedStoredEnergy"),
        (chpStateNotRunning, 70, 0, 805), // tests case (false, false, true)
        (
          chpStateNotRunning,
          70,
          8 * 115,
          0,
        ), // tests case (false, true, false)
        (chpStateNotRunning, 70, 10, 795), // tests case (false, true, true)
        (chpStateRunning, 70, 0, 905), // tests case (true, false, true)
        (chpStateRunning, 70, 8 * 115, 0), // tests case (true, true, false)
        (chpStateRunning, 70, 10, 895), // tests case (true, true, true)
        (
          chpStateRunning,
          70,
          806,
          99,
        ), // test case (_, true, false) and demand covered together with chp
        (
          chpStateRunning,
          70,
          9 * 115,
          0,
        ), // test case (_, true, false) and demand not covered together with chp
        (
          chpStateRunning,
          72,
          1,
          927,
        ), // test case (true, true, true) and storage volume exceeds maximum
      )

      forAll(testCases) {
        (chpState, storageLvl, heatDemand, expectedStoredEnergy) =>
          val chpData = buildChpRelevantData(chpState, heatDemand)
          val thermalStorage = buildThermalStorage(storageInput, storageLvl)
          val chpModel = buildChpModel(thermalStorage)

          chpModel.calculateNextState(chpData)
          thermalStorage._storedEnergy.toKilowattHours should be(
            expectedStoredEnergy
          )
      }
    }

    "Check time tick and running status after calculating next state with #chpState and heat demand #heatDemand kWh:" in {
      val testCases = Table(
        (
          "chpState",
          "storageLvl",
          "heatDemand",
          "expectedTick",
          "expectedRunningStatus",
        ),
        (
          chpStateNotRunning,
          70,
          0,
          7200,
          false,
        ), // Test case (false, false, true)
        (
          chpStateNotRunning,
          70,
          8 * 115,
          7200,
          true,
        ), // Test case (false, true, false)
        (
          chpStateNotRunning,
          70,
          10,
          7200,
          false,
        ), // Test case (false, true, true)
        (chpStateRunning, 70, 0, 7200, true), // Test case (true, false, true)
        (
          chpStateRunning,
          70,
          8 * 115,
          7200,
          true,
        ), // Test case (true, true, false)
        (chpStateRunning, 70, 10, 7200, true), // Test case (true, true, true)
        (
          chpStateRunning,
          70,
          806,
          7200,
          true,
        ), // Test case (_, true, false) and demand covered together with chp
        (
          chpStateRunning,
          70,
          9 * 115,
          7200,
          true,
        ), // Test case (_, true, false) and demand not covered together with chp
        (
          chpStateRunning,
          72,
          1,
          7200,
          false,
        ), // Test case (true, true, true) and storage volume exceeds maximum
      )

      forAll(testCases) {
        (
            chpState,
            storageLvl,
            heatDemand,
            expectedTick,
            expectedRunningStatus,
        ) =>
          val chpData = buildChpRelevantData(chpState, heatDemand)
          val thermalStorage = buildThermalStorage(storageInput, storageLvl)
          val chpModel = buildChpModel(thermalStorage)

          val nextState = chpModel.calculateNextState(chpData)

          nextState.lastTimeTick shouldEqual expectedTick
          nextState.isRunning shouldEqual expectedRunningStatus
      }
    }

    "apply, validate, and build correctly" in {
      val thermalStorage = buildThermalStorage(storageInput, 90)
      val chpModelCaseClass = buildChpModel(thermalStorage)
      val startDate =
        TimeUtil.withDefaults.toZonedDateTime("2021-01-01T00:00:00Z")
      val endDate = startDate.plusSeconds(86400L)
      val chpModelCaseObject = ChpModel(
        chpInput,
        startDate,
        endDate,
        null,
        1.0,
        thermalStorage,
      )

      chpModelCaseClass.sRated shouldEqual chpModelCaseObject.sRated
      chpModelCaseClass.cosPhiRated shouldEqual chpModelCaseObject.cosPhiRated
      chpModelCaseClass.pThermal shouldEqual chpModelCaseObject.pThermal
      chpModelCaseClass.storage shouldEqual chpModelCaseObject.storage
    }
  }
}
