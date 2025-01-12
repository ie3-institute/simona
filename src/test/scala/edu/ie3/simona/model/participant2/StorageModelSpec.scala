/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.input.system.`type`.StorageTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.config.SimonaConfig.StorageRuntimeConfig
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  FixedRelevantData,
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.scalatest.matchers.should.Matchers
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.quantity.Quantities.getQuantity

import java.util.UUID

class StorageModelSpec extends UnitSpec with Matchers {

  final val inputModel: StorageInput = createStorageInput()
  implicit val powerTolerance: Power = Kilowatts(1e-10)
  implicit val energyTolerance: Energy = KilowattHours(1e-10)

  def createStorageInput(): StorageInput = {
    val nodeInput = new NodeInput(
      UUID.fromString("ad39d0b9-5ad6-4588-8d92-74c7d7de9ace"),
      "NodeInput",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      Quantities.getQuantity(1, PowerSystemUnits.PU),
      false,
      NodeInput.DEFAULT_GEO_POSITION,
      GermanVoltageLevelUtils.LV,
      -1,
    )

    val typeInput = new StorageTypeInput(
      UUID.fromString("fbee4995-24dd-45e4-9c85-7d986fe99ff3"),
      "Test_StorageTypeInput",
      Quantities.getQuantity(10000d, EURO),
      getQuantity(0.05d, EURO_PER_MEGAWATTHOUR),
      Quantities.getQuantity(100d, KILOWATTHOUR),
      getQuantity(13d, KILOVOLTAMPERE),
      0.997,
      getQuantity(10d, KILOWATT),
      getQuantity(0.03, PU_PER_HOUR),
      getQuantity(0.9, PU),
    )

    new StorageInput(
      UUID.randomUUID(),
      "Test_StorageInput",
      new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
      OperationTime.notLimited(),
      nodeInput,
      CosPhiFixed.CONSTANT_CHARACTERISTIC,
      null,
      typeInput,
    )
  }

  def buildStorageModel(
      targetSoc: Option[Double] = Option.empty
  ): StorageModel = {
    val runtimeConfig = new StorageRuntimeConfig(
      calculateMissingReactivePowerWithModel = false,
      scaling = 1d,
      uuids = List.empty, // not used
      initialSoc = 0d, // not used
      targetSoc = targetSoc,
    )

    StorageModel.apply(
      inputModel,
      runtimeConfig,
    )
  }

  "StorageModel" should {

    "Determine the current state" in {
      val storageModel = buildStorageModel()

      val lastTick = 3600L

      val testCases = Table(
        ("lastEnergy", "power", "duration", "expEnergy"),
        /* empty storage */
        // zero power
        (0.0, 0.0, 3600, 0.0),
        // zero duration
        (0.0, 5.0, 0, 0.0),
        // charging a tiny bit
        (0.0, 1.0, 1, 0.00025),
        // charging until half
        (0.0, 10.0, 20000, 50.0),
        // charging until almost full
        (0.0, 10.0, 39999, 99.9975),
        // charging until full
        (0.0, 10.0, 40000, 100.0),
        // overcharging a tiny bit
        (0.0, 10.0, 40001, 100.0),
        // discharging
        (0.0, -10.0, 3600, 0.0),
        /* half full storage */
        // zero power
        (50.0, 0.0, 3600, 50.0),
        // zero duration
        (50.0, 5.0, 0, 50.0),
        // charging a tiny bit
        (50.0, 1.0, 1, 50.00025),
        // charging until almost full
        (50.0, 10.0, 19999, 99.9975),
        // charging until full
        (50.0, 10.0, 20000, 100.0),
        // overcharging a tiny bit
        (50.0, 10.0, 20001, 100.0),
        // discharging a tiny bit
        (50.0, -0.81, 1, 49.99975),
        // discharging until almost empty
        (50.0, -8.1, 19999, 0.0025),
        // discharging until empty
        (50.0, -8.1, 20000, 0.0),
        // undercharging a tiny bit
        (50.0, -8.1, 20001, 0.0),
        /* full storage */
        // zero power
        (100.0, 0.0, 3600, 100.0),
        // zero duration
        (100.0, -5.0, 0, 100.0),
        // discharging a tiny bit
        (100.0, -0.81, 1, 99.99975),
        // discharging until half
        (100.0, -8.1, 20000, 50.0),
        // discharging until almost empty
        (100.0, -8.1, 39999, 0.0025),
        // discharging until empty
        (100.0, -8.1, 40000, 0.0),
        // undercharging a tiny bit
        (100.0, -8.1, 40001, 0.0),
        // charging
        (100.0, 10.0, 3600, 100.0),
      )

      forAll(testCases) {
        (lastEnergy: Double, power: Double, duration: Int, expEnergy: Double) =>
          val lastState = StorageModel.StorageState(
            KilowattHours(lastEnergy),
            lastTick,
          )

          val operatingPoint =
            ActivePowerOperatingPoint(Kilowatts(power))

          val currentTick = lastTick + duration

          val newState = storageModel.determineState(
            lastState,
            operatingPoint,
            currentTick,
          )

          newState.tick shouldBe currentTick
          newState.storedEnergy should approximate(KilowattHours(expEnergy))
      }
    }

    "Calculate flex options" in {
      val storageModel = buildStorageModel()
      val tick = 3600L

      val testCases = Table(
        ("storedEnergy", "pRef", "pMin", "pMax"),
        // completely empty
        (0.0, 0.0, 0.0, 10.0),
        // at a tiny bit above empty
        (0.011, 0.0, -10.0, 10.0),
        // at mid-level charge
        (60.0, 0.0, -10.0, 10.0),
        // almost fully charged
        (99.989, 0.0, -10.0, 10.0),
        // fully charged
        (100.0, 0.0, -10.0, 0.0),
      )

      forAll(testCases) {
        (storedEnergy: Double, pRef: Double, pMin: Double, pMax: Double) =>
          val state = StorageModel.StorageState(
            KilowattHours(storedEnergy),
            tick,
          )

          storageModel.calcFlexOptions(state, FixedRelevantData) match {
            case result: ProvideMinMaxFlexOptions =>
              result.ref should approximate(Kilowatts(pRef))
              result.min should approximate(Kilowatts(pMin))
              result.max should approximate(Kilowatts(pMax))
            case _ =>
              fail("Expected result of type ProvideMinMaxFlexOptions")
          }
      }
    }

    "Calculate flex options with target SOC" in {
      val storageModel = buildStorageModel(Some(0.5d))
      val tick = 3600L

      val testCases = Table(
        ("storedEnergy", "pRef", "pMin", "pMax"),
        // completely empty
        (0.0, 10.0, 0.0, 10.0),
        // below margin of ref power target
        (49.9974, 10.0, -10.0, 10.0),
        // within margin below ref power target
        (49.9976, 0.0, -10.0, 10.0),
        // exactly at ref power target
        (50.0, 0.0, -10.0, 10.0),
        // within margin above ref power target
        (50.0030, 0.0, -10.0, 10.0),
        // above margin of ref power target
        (50.0031, -10.0, -10.0, 10.0),
        // at mid-level charge
        (60.0, -10.0, -10.0, 10.0),
        // fully charged
        (100.0, -10.0, -10.0, 0.0),
      )

      forAll(testCases) {
        (storedEnergy: Double, pRef: Double, pMin: Double, pMax: Double) =>
          val state = StorageModel.StorageState(
            KilowattHours(storedEnergy),
            tick,
          )

          storageModel.calcFlexOptions(state, FixedRelevantData) match {
            case result: ProvideMinMaxFlexOptions =>
              result.ref should approximate(Kilowatts(pRef))
              result.min should approximate(Kilowatts(pMin))
              result.max should approximate(Kilowatts(pMax))
            case _ =>
              fail("Expected result of type ProvideMinMaxFlexOptions")
          }
      }
    }

    "Handle controlled power change" in {
      val storageModel = buildStorageModel()
      val tick = 3600L
      // not used in calculation
      val flexOptions =
        ProvideMinMaxFlexOptions.noFlexOption(inputModel.getUuid, zeroKW)

      val testCases = Table(
        (
          "storedEnergy",
          "setPower",
          "expPower",
          "expActiveNext",
          "expScheduled",
          "expDelta",
        ),
        // no power
        (0.0, 0.0, 0.0, false, false, 0.0),
        (50.0, 0.0, 0.0, false, false, 0.0),
        (100.0, 0.0, 0.0, false, false, 0.0),
        // charging on empty
        (0.0, 1.0, 1.0, true, true, 100 * 3600 / 0.9),
        (0.0, 2.5, 2.5, true, true, 40 * 3600 / 0.9),
        (0.0, 5.0, 5.0, true, true, 20 * 3600 / 0.9),
        (0.0, 10.0, 10.0, true, true, 10 * 3600 / 0.9),
        // charging on half full
        (50.0, 5.0, 5.0, false, true, 10 * 3600 / 0.9),
        (50.0, 10.0, 10.0, false, true, 5 * 3600 / 0.9),
        // discharging on half full
        (50.0, -4.5, -4.5, false, true, 10 * 3600.0),
        (50.0, -9.0, -9.0, false, true, 5 * 3600.0),
        // discharging on full
        (100.0, -4.5, -4.5, true, true, 20 * 3600.0),
        (100.0, -9.0, -9.0, true, true, 10 * 3600.0),
      )

      forAll(testCases) {
        (
            storedEnergy: Double,
            setPower: Double,
            expPower: Double,
            expActiveNext: Boolean,
            expScheduled: Boolean,
            expDelta: Double,
        ) =>
          val state = StorageModel.StorageState(
            KilowattHours(storedEnergy),
            tick,
          )

          val (operatingPoint, changeIndicator) =
            storageModel.handlePowerControl(
              state,
              FixedRelevantData,
              flexOptions,
              Kilowatts(setPower),
            )

          operatingPoint.activePower should approximate(Kilowatts(expPower))

          changeIndicator.changesAtTick.isDefined shouldBe expScheduled
          changeIndicator.changesAtTick.forall(
            _ == (tick + expDelta)
          ) shouldBe true
          changeIndicator.changesAtNextActivation shouldBe expActiveNext
      }
    }

    "Handle controlled power change with ref target SOC" in {
      val storageModel = buildStorageModel(Some(0.5d))
      val tick = 3600L
      // not used in calculation
      val flexOptions =
        ProvideMinMaxFlexOptions.noFlexOption(inputModel.getUuid, zeroKW)

      val testCases = Table(
        (
          "storedEnergy",
          "setPower",
          "expPower",
          "expActiveNext",
          "expScheduled",
          "expDelta",
        ),
        // no power
        (0.0, 0.0, 0.0, false, false, 0.0),
        (50.0, 0.0, 0.0, false, false, 0.0),
        (100.0, 0.0, 0.0, false, false, 0.0),
        // charging on empty
        (0.0, 1.0, 1.0, true, true, 50 * 3600 / 0.9),
        (0.0, 2.5, 2.5, true, true, 20 * 3600 / 0.9),
        (0.0, 5.0, 5.0, true, true, 10 * 3600 / 0.9),
        (0.0, 10.0, 10.0, true, true, 5 * 3600 / 0.9),
        // charging on target ref
        (50.0, 5.0, 5.0, true, true, 10 * 3600 / 0.9),
        (50.0, 10.0, 10.0, true, true, 5 * 3600 / 0.9),
        // discharging on target ref
        (50.0, -4.5, -4.5, true, true, 10 * 3600.0),
        (50.0, -9.0, -9.0, true, true, 5 * 3600.0),
        // discharging on full
        (100.0, -4.5, -4.5, true, true, 10 * 3600.0),
        (100.0, -9.0, -9.0, true, true, 5 * 3600.0),
      )

      forAll(testCases) {
        (
            storedEnergy: Double,
            setPower: Double,
            expPower: Double,
            expActiveNext: Boolean,
            expScheduled: Boolean,
            expDelta: Double,
        ) =>
          val state = StorageModel.StorageState(
            KilowattHours(storedEnergy),
            tick,
          )

          val (operatingPoint, changeIndicator) =
            storageModel.handlePowerControl(
              state,
              FixedRelevantData,
              flexOptions,
              Kilowatts(setPower),
            )

          operatingPoint.activePower should approximate(Kilowatts(expPower))

          changeIndicator.changesAtTick.isDefined shouldBe expScheduled
          changeIndicator.changesAtTick.forall(
            _ == (tick + expDelta)
          ) shouldBe true
          changeIndicator.changesAtNextActivation shouldBe expActiveNext
      }
    }

    "Handle the edge case of discharging in tolerance margins" in {
      val storageModel = buildStorageModel()
      val tick = 1800L
      // not used in calculation
      val flexOptions =
        ProvideMinMaxFlexOptions.noFlexOption(inputModel.getUuid, zeroKW)

      // margin is at ~ 0.0030864 kWh
      val state = StorageModel.StorageState(
        KilowattHours(0.002d),
        tick,
      )

      val (operatingPoint, changeIndicator) =
        storageModel.handlePowerControl(
          state,
          FixedRelevantData,
          flexOptions,
          Kilowatts(-5d),
        )

      operatingPoint.activePower should approximate(zeroKW)

      changeIndicator.changesAtTick.isDefined shouldBe false
      changeIndicator.changesAtNextActivation shouldBe true
    }

    "Handle the edge case of charging in tolerance margins" in {
      val storageModel = buildStorageModel()
      val tick = 1800L
      // not used in calculation
      val flexOptions =
        ProvideMinMaxFlexOptions.noFlexOption(inputModel.getUuid, zeroKW)

      // margin is at ~ 99.9975 kWh
      val state = StorageModel.StorageState(
        KilowattHours(99.999d),
        tick,
      )

      val (operatingPoint, changeIndicator) =
        storageModel.handlePowerControl(
          state,
          FixedRelevantData,
          flexOptions,
          Kilowatts(9d),
        )

      operatingPoint.activePower should approximate(zeroKW)

      changeIndicator.changesAtTick.isDefined shouldBe false
      changeIndicator.changesAtNextActivation shouldBe true
    }

    "Handle the edge case of discharging in positive target margin" in {
      val storageModel = buildStorageModel(Some(0.3d))
      val tick = 1800L
      // not used in calculation
      val flexOptions =
        ProvideMinMaxFlexOptions.noFlexOption(inputModel.getUuid, zeroKW)

      // margin is at ~ 30.0025 kWh
      val state = StorageModel.StorageState(
        KilowattHours(30.0024d),
        tick,
      )

      val (operatingPoint, changeIndicator) =
        storageModel.handlePowerControl(
          state,
          FixedRelevantData,
          flexOptions,
          Kilowatts(-9d),
        )

      operatingPoint.activePower should approximate(Kilowatts(-9d))

      changeIndicator.changesAtTick should be(
        Some(tick + 10801L)
      )
      changeIndicator.changesAtNextActivation shouldBe true
    }

    "Handle the edge case of charging in negative target margin" in {
      val storageModel = buildStorageModel(Some(0.4d))
      val tick = 1800L
      // not used in calculation
      val flexOptions =
        ProvideMinMaxFlexOptions.noFlexOption(inputModel.getUuid, zeroKW)

      // margin is at ~ 39.9975 kWh
      val state = StorageModel.StorageState(
        KilowattHours(39.998d),
        tick,
      )

      val (operatingPoint, changeIndicator) =
        storageModel.handlePowerControl(
          state,
          FixedRelevantData,
          flexOptions,
          Kilowatts(5d),
        )

      operatingPoint.activePower should approximate(Kilowatts(5d))

      changeIndicator.changesAtTick should be(
        Some(tick + 48002L)
      )
      changeIndicator.changesAtNextActivation shouldBe true
    }
  }
}
