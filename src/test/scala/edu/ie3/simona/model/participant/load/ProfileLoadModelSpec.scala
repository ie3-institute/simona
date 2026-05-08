/*
 * © 2021-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile.*
import edu.ie3.datamodel.models.profile.PowerProfileKey
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.load.ProfileLoadModel.{
  LoadModelState,
  ProfileLoadFactoryData,
}
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexType,
}
import edu.ie3.simona.service.Data.SecondaryData.{
  LoadDataFunction,
  SecondarySeriesData,
}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.service.load.LoadProfileStore
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.LoadInputTestData
import edu.ie3.simona.test.matchers.DoubleMatchers
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.asKiloWattHour
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKW, zeroKWh}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  toApparent,
  toSquants,
}
import edu.ie3.util.scala.quantities.{ApparentPower, Voltamperes}
import squants.energy.*
import squants.time.{Hours, Minutes}
import squants.{Energy, Percent}
import tech.units.indriya.quantity.Quantities

import scala.collection.immutable.SortedMap

class ProfileLoadModelSpec
    extends UnitSpec
    with DoubleMatchers
    with LoadModelTestHelper
    with LoadInputTestData {

  private val simulationStartDate =
    TimeUtil.withDefaults.toZonedDateTime("2022-01-01T00:00:00Z")

  private val loadProfileStore = LoadProfileStore()

  // testing tolerances
  private given ApparentPower = Voltamperes(1e-2)
  private given Energy = WattHours(1e-6)
  private given Double = 1e-6

  "A profile load model" should {

    val sampleDataFunc = LoadDataFunction(() => Kilowatts(1))
    val samplePowerSeries = SortedMap(
      0L -> Kilowatts(1),
      900L -> Kilowatts(2),
      1800L -> Kilowatts(3),
      2700L -> Kilowatts(4),
    )
    val sampleDataSeries = SecondarySeriesData(
      series = samplePowerSeries.map { case (tick, power) =>
        tick -> LoadDataFunction(() => power)
      }
    )

    def additionalData(loadProfile: PowerProfileKey): ProfileLoadFactoryData =
      loadProfileStore
        .getProfileLoadFactoryData(loadProfile)
        .getOrElse(fail(s"No data found for profile: $loadProfile"))

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
        val model = ProfileLoadModel
          .Factory(
            loadInput
              .copy()
              .loadProfile(profile.getKey)
              .sRated(
                Quantities.getQuantity(sRated, PowerSystemUnits.VOLTAMPERE)
              )
              .build(),
            config,
          )
          .update(additionalData(profile.getKey))
          .create()

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
        val model = ProfileLoadModel
          .Factory(
            loadInput
              .copy()
              .loadProfile(profile.getKey)
              .eConsAnnual(
                Quantities
                  .getQuantity(eConsAnnual, PowerSystemUnits.KILOWATTHOUR)
              )
              .build(),
            config,
          )
          .update(additionalData(profile.getKey))
          .create()

        model.referenceScalingFactor should approximate(expectedScalingFactor)
        model.sRated should approximate(Voltamperes(expectedSRated))
      }
    }

    "handle singular power data by storing it into state" in {
      val model = ProfileLoadModel
        .Factory(loadInput, LoadRuntimeConfig())
        .update(additionalData(loadInput.getLoadProfile))
        .create()

      val oldState = LoadModelState(0L)

      val actualState =
        model.handleInput(oldState, Seq(sampleDataFunc), onePU)

      actualState.tick shouldEqual oldState.tick
      actualState.powerData shouldEqual SortedMap(
        0L -> sampleDataFunc.powerSupplier()
      )
    }

    "handle power series data by storing it into state" in {
      val model = ProfileLoadModel
        .Factory(loadInput, LoadRuntimeConfig())
        .update(additionalData(loadInput.getLoadProfile))
        .create()

      val oldState = LoadModelState(0L)

      val actualState =
        model.handleInput(oldState, Seq(sampleDataSeries), onePU)

      actualState.tick shouldEqual oldState.tick
      actualState.powerData shouldEqual samplePowerSeries
    }

    "reach the targeted annual energy consumption in a simulated year" in {
      forAll(
        Table("profile", H0, L0, G0)
      ) { profile =>
        val input = loadInput.copy().loadProfile(profile.getKey).build()
        val config = LoadRuntimeConfig(
          modelBehaviour = "profile",
          reference = "energy",
        )

        val targetEnergyConsumption = loadInput.geteConsAnnual.toSquants

        val model = ProfileLoadModel
          .Factory(input, config)
          .update(additionalData(profile.getKey))
          .create()

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
        val input = loadInput.copy().loadProfile(profile.getKey).build()
        val config = LoadRuntimeConfig(modelBehaviour = "profile")

        val model = ProfileLoadModel
          .Factory(input, config)
          .update(additionalData(profile.getKey))
          .create()

        val targetMaximumPower =
          input.getsRated.toApparent.toActivePower(input.getCosPhiRated)

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

    "calculate forecast flex power series correctly" in {
      val model = ProfileLoadModel
        .Factory(
          // parameters that result in scaling factor of 1
          loadInput.copy().eConsAnnual(1000d.asKiloWattHour).build(),
          LoadRuntimeConfig(
            reference = "energy"
          ),
        )
        .update(additionalData(loadInput.getLoadProfile))
        .create()

      val state = LoadModelState(tick = 0L, powerData = samplePowerSeries)
      val dataTimeType = DataTimeType.CurrentAndForecast(
        forecastLength = Hours(1),
        forecastResolution = Minutes(15),
      )

      val flexOptions =
        model
          .flexModels(FlexType.EnergyBoundaries)
          .determineFlexOptions(state, dataTimeType)

      flexOptions match {
        case EnergyBoundariesFlexOptions(boundaries) =>
          boundaries should have size 1

          val energyLimits = boundaries.headOption.value.energyLimits
          energyLimits should have size sampleDataSeries.series.size + 1

          samplePowerSeries
            // adding dummy value so that last energy is tested
            .updated(3600L, zeroKW)
            .foldLeft(zeroKWh) { case (expectedEnergy, (tick, expectedPower)) =>
              energyLimits(tick).getUpper should approximate(expectedEnergy)
              energyLimits(tick).getLower should approximate(expectedEnergy)

              expectedEnergy + expectedPower * dataTimeType.forecastResolution
            }
        case unexpected => fail(s"Received unexpected flex options $unexpected")
      }

    }

  }

  "A profile load model with random profile" should {

    val randomProfileLoadFactoryData = ProfileLoadFactoryData(
      Some(Watts(159)),
      Some(KilowattHours(716.5416966513656)),
    )

    "be instantiated correctly with power reference" in {

      forAll(
        Table(
          ("sRated", "expectedScalingFactor"),
          (167.368421, 1.0),
          (1000.0, 5.9748428),
        )
      ) { (sRated, expectedScalingFactor) =>
        val config = LoadRuntimeConfig(modelBehaviour = "random")
        val model = ProfileLoadModel
          .Factory(
            randomLoadInput
              .copy()
              .sRated(
                Quantities.getQuantity(sRated, PowerSystemUnits.VOLTAMPERE)
              )
              .build(),
            config,
          )
          .update(randomProfileLoadFactoryData)
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
        val model = ProfileLoadModel
          .Factory(
            randomLoadInput
              .copy()
              .eConsAnnual(
                Quantities
                  .getQuantity(eConsAnnual, PowerSystemUnits.KILOWATTHOUR)
              )
              .build(),
            config,
          )
          .update(randomProfileLoadFactoryData)
          .create()

        model.referenceScalingFactor should approximate(expectedScalingFactor)
        model.sRated should approximate(Voltamperes(expectedSRated))
      }
    }

    "reach the targeted annual energy consumption in a simulated year" in {
      val config = LoadRuntimeConfig(
        modelBehaviour = "random",
        reference = "energy",
      )

      val model = ProfileLoadModel
        .Factory(
          randomLoadInput,
          config,
        )
        .update(randomProfileLoadFactoryData)
        .create()

      val targetEnergyConsumption = randomLoadInput.geteConsAnnual.toSquants

      calculateEnergyDiffForYear(
        model,
        simulationStartDate,
        targetEnergyConsumption,
      ) should be < Percent(1d)
    }

    "approximately reach the maximum power in a simulated year" in {
      val config = LoadRuntimeConfig(modelBehaviour = "random")

      val model = ProfileLoadModel
        .Factory(
          randomLoadInput,
          config,
        )
        .update(randomProfileLoadFactoryData)
        .create()

      val targetMaximumPower = randomLoadInput.getsRated.toApparent
        .toActivePower(randomLoadInput.getCosPhiRated)

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
