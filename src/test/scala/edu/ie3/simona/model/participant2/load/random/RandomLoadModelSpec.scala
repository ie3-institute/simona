/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load.random

import de.lmu.ifi.dbs.elki.math.statistics.distribution.GeneralizedExtremeValueDistribution
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant2.load.LoadModelTestHelper
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.LoadInputTestData
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.{
  ApparentPower,
  Kilovoltamperes,
  Voltamperes,
}
import squants.Percent
import squants.energy.KilowattHours
import tech.units.indriya.quantity.Quantities

class RandomLoadModelSpec
    extends UnitSpec
    with LoadModelTestHelper
    with LoadInputTestData {

  implicit val powerTolerance: ApparentPower = Voltamperes(1e-2)
  private implicit val doubleTolerance: Double = 1e-6

  private val simulationStartDate =
    TimeUtil.withDefaults.toZonedDateTime("2019-01-01T00:00:00Z")

  "A random load model" should {

    "be instantiated correctly with power reference" in {

      forAll(
        Table(
          ("sRated", "expectedScalingFactor"),
          (167.368421, 1.0),
          (1000.0, 5.9748428),
        )
      ) { (sRated, expectedScalingFactor) =>
        val config = LoadRuntimeConfig(modelBehaviour = "random")
        val model = RandomLoadModel
          .Factory(
            loadInput
              .copy()
              .sRated(
                Quantities.getQuantity(sRated, PowerSystemUnits.VOLTAMPERE)
              )
              .build(),
            config,
          )
          .create()

        model.referenceScalingFactor should approximate(expectedScalingFactor)
      }
    }

    "be instantiated correctly with energy reference" in {

      forAll(
        Table(
          ("eConsAnnual", "expectedScalingFactor", "expectedSRated"),
          (1000.0, 1.3955921, 256.936),
          (2000.0, 2.7911842, 513.8717),
          (3000.0, 4.1867763, 770.808),
        )
      ) { (eConsAnnual, expectedScalingFactor, expectedSRated) =>
        val config = LoadRuntimeConfig(
          modelBehaviour = "random",
          reference = "energy",
        )
        val model = RandomLoadModel
          .Factory(
            loadInput
              .copy()
              .eConsAnnual(
                Quantities
                  .getQuantity(eConsAnnual, PowerSystemUnits.KILOWATTHOUR)
              )
              .build(),
            config,
          )
          .create()

        model.referenceScalingFactor should approximate(expectedScalingFactor)
        model.sRated should approximate(Voltamperes(expectedSRated))
      }
    }

    "deliver the correct distribution on request" in {
      val model = RandomLoadModel
        .Factory(
          loadInput,
          LoadRuntimeConfig(
            uuids = List.empty,
            modelBehaviour = "random",
            reference = "energy",
          ),
        )
        .create()

      /* Working day, 61st quarter-hour */
      val queryDate =
        TimeUtil.withDefaults.toZonedDateTime("2019-07-19T15:21:00Z")
      val expectedParams = new RandomLoadParameters(
        0.405802458524704,
        0.0671483352780342,
        0.0417016632854939,
      )

      /* First query leeds to generation of distribution */
      val getGevDistribution =
        PrivateMethod[GeneralizedExtremeValueDistribution](
          Symbol("getGevDistribution")
        )

      def firstHit = model invokePrivate getGevDistribution(queryDate)

      firstHit.getK shouldBe expectedParams.k
      firstHit.getMu shouldBe expectedParams.my
      firstHit.getSigma shouldBe expectedParams.sigma

      /* Second query is only look up in storage */
      def secondHit = model invokePrivate getGevDistribution(queryDate)

      secondHit shouldBe firstHit
    }

    "reach the targeted annual energy consumption in a simulated year" in {
      val config = LoadRuntimeConfig(
        modelBehaviour = "random",
        reference = "energy",
      )

      val model = RandomLoadModel
        .Factory(
          loadInput,
          config,
        )
        .create()

      val targetEnergyConsumption = KilowattHours(
        loadInput
          .geteConsAnnual()
          .to(PowerSystemUnits.KILOWATTHOUR)
          .getValue
          .doubleValue
      )

      calculateEnergyDiffForYear(
        model,
        simulationStartDate,
        targetEnergyConsumption,
      ) should be < Percent(1d)
    }

    "approximately reach the maximum power in a simulated year" in {
      val config = LoadRuntimeConfig(modelBehaviour = "random")

      val model = RandomLoadModel
        .Factory(
          loadInput,
          config,
        )
        .create()

      val targetMaximumPower = Kilovoltamperes(
        loadInput
          .getsRated()
          .to(PowerSystemUnits.KILOVOLTAMPERE)
          .getValue
          .doubleValue
      ).toActivePower(loadInput.getCosPhiRated)

      val powers = calculatePowerForYear(
        model,
        simulationStartDate,
      ).toIndexedSeq.sorted.toArray

      val quantile95 = get95Quantile(powers)

      getRelativeDifference(
        quantile95,
        targetMaximumPower,
      ) should be < Percent(2d)
    }

  }
}
