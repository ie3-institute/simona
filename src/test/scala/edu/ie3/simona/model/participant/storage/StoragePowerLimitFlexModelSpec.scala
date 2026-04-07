/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.model.participant.ParticipantModel.ActivePowerOperatingPoint
import edu.ie3.simona.ontology.messages.flex.PowerLimitFlexOptions
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.StorageInputTestData
import edu.ie3.simona.test.helper.TableDrivenHelper
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power}

class StoragePowerLimitFlexModelSpec
    extends UnitSpec
    with TableDrivenHelper
    with StorageInputTestData {

  // Testing tolerances
  given Power = Kilowatts(1e-10)
  given Energy = KilowattHours(1e-10)

  "A Storage PowerLimitFlexModel" should {

    "Calculate flex options" in {
      val flexModel = StoragePowerLimitFlexModel(createTestModel())
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

          flexModel.determineFlexOptions(state, DataTimeType.Current) match {
            case result: PowerLimitFlexOptions =>
              result.ref should approximate(Kilowatts(pRef))
              result.min should approximate(Kilowatts(pMin))
              result.max should approximate(Kilowatts(pMax))
            case _ =>
              fail("Expected result of type PowerLimitFlexOptions")
          }
      }
    }

    "Calculate flex options with target SOC" in {
      val flexModel = StoragePowerLimitFlexModel(createTestModel(Some(0.5d)))
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

          flexModel.determineFlexOptions(state, DataTimeType.Current) match {
            case result: PowerLimitFlexOptions =>
              result.ref should approximate(Kilowatts(pRef))
              result.min should approximate(Kilowatts(pMin))
              result.max should approximate(Kilowatts(pMax))
            case _ =>
              fail("Expected result of type PowerLimitFlexOptions")
          }
      }
    }

    "Determine the next activation tick" in {
      val flexModel = StoragePowerLimitFlexModel(createTestModel())
      val tick = 3600L

      val testCases = Table(
        (
          "storedEnergy",
          "setPower",
          "expActiveNext",
          "expDelta",
        ),
        // no power
        (0.0, 0.0, false, N),
        (50.0, 0.0, false, N),
        (100.0, 0.0, false, N),
        // charging on empty
        (0.0, 1.0, true, S(100 * 3600 / 0.9)),
        (0.0, 2.5, true, S(40 * 3600 / 0.9)),
        (0.0, 5.0, true, S(20 * 3600 / 0.9)),
        (0.0, 10.0, true, S(10 * 3600 / 0.9)),
        // charging on half full
        (50.0, 5.0, false, S(10 * 3600 / 0.9)),
        (50.0, 10.0, false, S(5 * 3600 / 0.9)),
        // discharging on half full
        (50.0, -4.5, false, S(10 * 3600.0)),
        (50.0, -9.0, false, S(5 * 3600.0)),
        // discharging on full
        (100.0, -4.5, true, S(20 * 3600.0)),
        (100.0, -9.0, true, S(10 * 3600.0)),
      )

      forAll(testCases) {
        (
            storedEnergy: Double,
            setPower: Double,
            expActiveNext: Boolean,
            expDelta: Option[Double],
        ) =>
          val state = StorageModel.StorageState(
            KilowattHours(storedEnergy),
            tick,
          )

          val power = Kilowatts(setPower)
          val changeIndicator =
            flexModel.determineNextActivation(
              state,
              ActivePowerOperatingPoint(power),
              power,
              DataTimeType.Current,
            )

          val expChangesAtTick = expDelta.map(tick + _.toLong)
          changeIndicator.changesAtTick shouldBe expChangesAtTick
          changeIndicator.changesAtNextActivation shouldBe expActiveNext
      }
    }

    "Determine the next activation tick with target SOC" in {
      val flexModel = StoragePowerLimitFlexModel(createTestModel())
      val tick = 3600L

      val testCases = Table(
        (
          "storedEnergy",
          "setPower",
          "expActiveNext",
          "expDelta",
        ),
        // no power
        (0.0, 0.0, false, N),
        (50.0, 0.0, false, N),
        (100.0, 0.0, false, N),
        // charging on empty
        (0.0, 1.0, true, S(50 * 3600 / 0.9)),
        (0.0, 2.5, true, S(20 * 3600 / 0.9)),
        (0.0, 5.0, true, S(10 * 3600 / 0.9)),
        (0.0, 10.0, true, S(5 * 3600 / 0.9)),
        // charging on target ref
        (50.0, 5.0, true, S(10 * 3600 / 0.9)),
        (50.0, 10.0, true, S(5 * 3600 / 0.9)),
        // discharging on target ref
        (50.0, -4.5, true, S(10 * 3600.0)),
        (50.0, -9.0, true, S(5 * 3600.0)),
        // discharging on full
        (100.0, -4.5, true, S(10 * 3600.0)),
        (100.0, -9.0, true, S(5 * 3600.0)),
      )

      forAll(testCases) {
        (
            storedEnergy: Double,
            setPower: Double,
            expActiveNext: Boolean,
            expDelta: Option[Double],
        ) =>
          val state = StorageModel.StorageState(
            KilowattHours(storedEnergy),
            tick,
          )

          val power = Kilowatts(setPower)
          val changeIndicator =
            flexModel.determineNextActivation(
              state,
              ActivePowerOperatingPoint(power),
              power,
              DataTimeType.Current,
            )

          val expChangesAtTick = expDelta.map(tick + _.toLong)
          changeIndicator.changesAtTick shouldBe expChangesAtTick
          changeIndicator.changesAtNextActivation shouldBe expActiveNext
      }
    }

  }
}
