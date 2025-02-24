/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load.profile

import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile._
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant2.load.LoadModelTestHelper
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.LoadInputTestData
import edu.ie3.simona.test.matchers.DoubleMatchers
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.{
  ApparentPower,
  Kilovoltamperes,
  Voltamperes,
}
import squants.Percent
import squants.energy.{KilowattHours, Power, Watts}
import tech.units.indriya.quantity.Quantities

class ProfileLoadModelSpec
    extends UnitSpec
    with DoubleMatchers
    with LoadModelTestHelper
    with LoadInputTestData {

  private implicit val powerTolerance: ApparentPower = Voltamperes(1e-2)
  private implicit val doubleTolerance: Double = 1e-6

  private val simulationStartDate =
    TimeUtil.withDefaults.toZonedDateTime("2022-01-01T00:00:00Z")

  "A profile load model" should {

    "be instantiated correctly with power reference" in {

      forAll(
        Table(
          ("profile", "sRated", "expectedScalingFactor"),
          (H0, 282.736842, 1.0),
          (H0, 1000.0, 3.536858),
          (L0, 253.052632, 1.0),
          (L0, 1000.0, 3.951747),
          (G0, 253.052632, 1.0),
          (G0, 1000.0, 3.951747),
        )
      ) { (profile, sRated, expectedScalingFactor) =>
        val config = LoadRuntimeConfig(modelBehaviour = "profile")
        val model = ProfileLoadModel(
          loadInput
            .copy()
            .loadprofile(profile)
            .sRated(Quantities.getQuantity(sRated, PowerSystemUnits.VOLTAMPERE))
            .build(),
          config,
        )

        model.referenceScalingFactor should approximate(expectedScalingFactor)
      }
    }

    "be instantiated correctly with energy reference" in {

      forAll(
        Table(
          ("profile", "eConsAnnual", "expectedScalingFactor", "expectedSRated"),
          (H0, 1000.0, 1.0, 282.74),
          (H0, 3000.0, 3.0, 848.22),
          (L0, 1000.0, 1.0, 253.053),
          (L0, 3000.0, 3.0, 759.158),
          (G0, 1000.0, 1.0, 253.053),
          (G0, 3000.0, 3.0, 759.158),
        )
      ) { (profile, eConsAnnual, expectedScalingFactor, expectedSRated) =>
        val config = LoadRuntimeConfig(
          modelBehaviour = "profile",
          reference = "energy",
        )
        val model = ProfileLoadModel(
          loadInput
            .copy()
            .loadprofile(profile)
            .eConsAnnual(
              Quantities.getQuantity(eConsAnnual, PowerSystemUnits.KILOWATTHOUR)
            )
            .build(),
          config,
        )

        model.referenceScalingFactor should approximate(expectedScalingFactor)
        model.sRated should approximate(Voltamperes(expectedSRated))
      }
    }

    "reach the targeted annual energy consumption in a simulated year" in {
      forAll(
        Table("profile", H0, L0, G0)
      ) { profile =>
        val input = loadInput.copy().loadprofile(profile).build()
        val config = LoadRuntimeConfig(
          modelBehaviour = "profile",
          reference = "energy",
        )

        val targetEnergyConsumption = KilowattHours(
          loadInput
            .geteConsAnnual()
            .to(PowerSystemUnits.KILOWATTHOUR)
            .getValue
            .doubleValue
        )

        val model = ProfileLoadModel(input, config)

        /* Test against a permissible deviation of 2 %. As per official documentation of the bdew load profiles
         * [https://www.bdew.de/media/documents/2000131_Anwendung-repraesentativen_Lastprofile-Step-by-step.pdf], 1.5 %
         * are officially permissible. But, as we currently do not take (bank) holidays into account, we cannot reach
         * this accuracy. */

        calculateEnergyDiffForYear(
          model,
          simulationStartDate,
          targetEnergyConsumption,
        ) should be < Percent(2)
      }
    }

    "approximately reach the maximum power in a simulated year" in {
      forAll(
        Table("profile", H0, L0, G0)
      ) { profile =>
        val input = loadInput.copy().loadprofile(profile).build()
        val config = LoadRuntimeConfig(modelBehaviour = "profile")

        val model = ProfileLoadModel(input, config)

        val targetMaximumPower = Kilovoltamperes(
          input
            .getsRated()
            .to(PowerSystemUnits.KILOVOLTAMPERE)
            .getValue
            .doubleValue
        ).toActivePower(input.getCosPhiRated)

        val maximumPower = calculatePowerForYear(
          model,
          simulationStartDate,
        ).maxOption.value

        // the maximum value depends on the year of the simulation,
        // since the maximum value for h0 will be reached on Saturdays in the winter
        // and since the dynamization function reaches its maximum on day 366 (leap year)
        implicit val tolerance: Power = Watts(1)
        maximumPower should approximate(targetMaximumPower)
      }
    }

  }
}
