/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.config.RuntimeConfig.StorageRuntimeConfig
import edu.ie3.simona.model.participant.ParticipantModel.ActivePowerOperatingPoint
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.StorageInputTestData
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.scalatest.matchers.should.Matchers
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power}

import java.time.ZonedDateTime

class StorageModelSpec
    extends UnitSpec
    with StorageInputTestData
    with Matchers {

  // Testing tolerances
  given Power = Kilowatts(1e-10)
  given Energy = KilowattHours(1e-10)

  private val dateTime: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02T03:04:05Z")

  def createModel(
      targetSoc: Option[Double] = Option.empty
  ): StorageModel =
    StorageModel
      .Factory(
        storageInput,
        StorageRuntimeConfig(targetSoc = targetSoc),
      )
      .create()

  "StorageModel" should {

    "Determine the current state" in {
      val storageModel = createModel()

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
            dateTime,
          )

          newState.tick shouldBe currentTick
          newState.storedEnergy should approximate(KilowattHours(expEnergy))
      }
    }

    "Handle controlled power change" in {
      val storageModel = createModel()
      val tick = 3600L

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
            storageModel.determineOperatingPoint(
              state,
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
      val storageModel = createModel(Some(0.5d))
      val tick = 3600L

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
            storageModel.determineOperatingPoint(
              state,
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
      val storageModel = createModel()
      val tick = 1800L

      // margin is at ~ 0.0030864 kWh
      val state = StorageModel.StorageState(
        KilowattHours(0.002d),
        tick,
      )

      val (operatingPoint, changeIndicator) =
        storageModel.determineOperatingPoint(
          state,
          Kilowatts(-5d),
        )

      operatingPoint.activePower should approximate(zeroKW)

      changeIndicator.changesAtTick.isDefined shouldBe false
      changeIndicator.changesAtNextActivation shouldBe true
    }

    "Handle the edge case of charging in tolerance margins" in {
      val storageModel = createModel()
      val tick = 1800L

      // margin is at ~ 99.9975 kWh
      val state = StorageModel.StorageState(
        KilowattHours(99.999d),
        tick,
      )

      val (operatingPoint, changeIndicator) =
        storageModel.determineOperatingPoint(
          state,
          Kilowatts(9d),
        )

      operatingPoint.activePower should approximate(zeroKW)

      changeIndicator.changesAtTick.isDefined shouldBe false
      changeIndicator.changesAtNextActivation shouldBe true
    }

    "Handle the edge case of discharging in positive target margin" in {
      val storageModel = createModel(Some(0.3d))
      val tick = 1800L

      // margin is at ~ 30.0025 kWh
      val state = StorageModel.StorageState(
        KilowattHours(30.0024d),
        tick,
      )

      val (operatingPoint, changeIndicator) =
        storageModel.determineOperatingPoint(
          state,
          Kilowatts(-9d),
        )

      operatingPoint.activePower should approximate(Kilowatts(-9d))

      changeIndicator.changesAtTick should be(
        Some(tick + 10800L)
      )
      changeIndicator.changesAtNextActivation shouldBe true
    }

    "Handle the edge case of charging in negative target margin" in {
      val storageModel = createModel(Some(0.4d))
      val tick = 1800L

      // margin is at ~ 39.9975 kWh
      val state = StorageModel.StorageState(
        KilowattHours(39.998d),
        tick,
      )

      val (operatingPoint, changeIndicator) =
        storageModel.determineOperatingPoint(
          state,
          Kilowatts(5d),
        )

      operatingPoint.activePower should approximate(Kilowatts(5d))

      changeIndicator.changesAtTick should be(
        Some(tick + 48001L)
      )
      changeIndicator.changesAtNextActivation shouldBe true
    }
  }
}
