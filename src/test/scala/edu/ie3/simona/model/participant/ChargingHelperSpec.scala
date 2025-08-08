/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.test.common.UnitSpec
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Each, Energy, Power}

class ChargingHelperSpec extends UnitSpec {

  private implicit val powerTolerance: Power = Kilowatts(1e-10)
  private implicit val energyTolerance: Energy = KilowattHours(1e-10)

  "A ChargingHelper" should {

    "calculate charged energy correctly" in {

      val cases = Table(
        (
          "storedEnergy",
          "lastStateTick",
          "currentTick",
          "power",
          "expectedStored",
        ),
        // empty battery
        (0.0, 0L, 3600L, 5.0, 5.0),
        (0.0, 0L, 3600L, 2.5, 2.5),
        (0.0, 0L, 1800L, 5.0, 2.5),
        (0.0, 900L, 2700L, 5.0, 2.5),
        (0.0, 0L, 3600L, -5.0, 0.0),
        // half full battery
        (5.0, 0L, 3600L, 5.0, 10.0),
        (5.0, 0L, 3600L, 2.5, 7.5),
        (5.0, 0L, 1800L, 5.0, 7.5),
        (5.0, 900L, 2700L, 5.0, 7.5),
        (5.0, 0L, 3600L, -5.0, 0.0),
        (5.0, 0L, 3600L, -2.5, 2.5),
        (5.0, 0L, 1800L, -5.0, 2.5),
        (5.0, 900L, 2700L, -5.0, 2.5),
        // full battery
        (10.0, 0L, 3600L, -5.0, 5.0),
        (10.0, 0L, 3600L, -2.5, 7.5),
        (10.0, 0L, 1800L, -5.0, 7.5),
        (10.0, 900L, 2700L, -5.0, 7.5),
        (10.0, 0L, 3600L, 5.0, 10.0),
      )

      forAll(cases) {
        (
            storedEnergy,
            startTick,
            endTick,
            power,
            expectedEnergy,
        ) =>
          val newEnergy = ChargingHelper.calcEnergy(
            KilowattHours(storedEnergy),
            Kilowatts(power),
            startTick,
            endTick,
            KilowattHours(10.0),
          )

          newEnergy should approximate(KilowattHours(expectedEnergy))
      }

    }

    "calculate the next tick correctly" in {

      val tick = 3600L

      val testCases = Table(
        (
          "storedEnergy",
          "setPower",
          "expScheduled",
          "expDelta",
        ),
        // no power
        (0.0, 0.0, false, 0.0),
        (50.0, 0.0, false, 0.0),
        (100.0, 0.0, false, 0.0),
        // charging on empty
        (0.0, 1.0, true, 100 * 3600 / 0.8),
        (0.0, 2.5, true, 40 * 3600 / 0.8),
        (0.0, 5.0, true, 20 * 3600 / 0.8),
        (0.0, 10.0, true, 10 * 3600 / 0.8),
        // charging on half charged
        (50.0, 5.0, true, 10 * 3600 / 0.8),
        (50.0, 10.0, true, 5 * 3600 / 0.8),
        // discharging on half charged
        (50.0, -4.0, true, 10 * 3600.0),
        (50.0, -8.0, true, 5 * 3600.0),
        // discharging on full
        (100.0, -4.0, true, 20 * 3600.0),
        (100.0, -8.0, true, 10 * 3600.0),
      )

      forAll(testCases) {
        (
            storedEnergy: Double,
            setPower: Double,
            expScheduled: Boolean,
            expDelta: Double,
        ) =>
          val actualNextTick = ChargingHelper.calcNextEventTick(
            KilowattHours(storedEnergy),
            Kilowatts(setPower),
            tick,
            () => KilowattHours(100),
            () => KilowattHours(0),
            Each(0.8),
          )

          actualNextTick.isDefined shouldBe expScheduled
          actualNextTick.forall(_ == (tick + expDelta)) shouldBe true
      }

    }

    "calculate net power correctly" in {

      val cases = Table(
        (
          "setPower",
          "eta",
          "expectedNetPower",
        ),
        // charging
        (10.0, 0.0, 0.0),
        (10.0, 0.8, 8.0),
        (10.0, 1.0, 10.0),
        // discharging
        (-10.0, 0.8, -12.5),
        (-10.0, 1.0, -10.0),
      )

      forAll(cases) {
        (
            setPower,
            eta,
            expectedNetPower,
        ) =>
          val actualNetPower = ChargingHelper.calcNetPower(
            Kilowatts(setPower),
            Each(eta),
          )

          actualNetPower should approximate(Kilowatts(expectedNetPower))
      }

    }

  }

}
