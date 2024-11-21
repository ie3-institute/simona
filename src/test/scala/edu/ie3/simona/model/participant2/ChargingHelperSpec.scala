/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.simona.test.common.UnitSpec
import squants.energy.{KilowattHours, Kilowatts}

class ChargingHelperSpec extends UnitSpec {

  private implicit val energyTolerance: squants.Energy = KilowattHours(1e-10)

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
  }
}
