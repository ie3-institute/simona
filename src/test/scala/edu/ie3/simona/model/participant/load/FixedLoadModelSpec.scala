/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.ParticipantModel.FixedState
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexType,
}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.LoadInputTestData
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKWh
import squants.{Energy, Power}
import squants.energy.{WattHours, Watts}
import squants.time.Hours

class FixedLoadModelSpec extends UnitSpec with LoadInputTestData {

  // tolerances
  given Power = Watts(1e-2)
  given Energy = WattHours(1e-2)

  "A fixed load model" should {

    "return the desired power in 1,000 calculations" in {

      val cases = Table(
        ("reference", "expectedPower"),
        ("power", Watts(268.6)),
        ("energy", Watts(342.47)),
      )

      forAll(cases) { (reference, expectedPower) =>
        val config = LoadRuntimeConfig(
          modelBehaviour = "fixed",
          reference = reference,
        )

        val model = FixedLoadModel
          .Factory(
            loadInput,
            config,
          )
          .create()

        (0 until 1000).foreach { tick =>
          val (operatingPoint, nextTick) = model.determineOperatingPoint(
            FixedState(tick)
          )

          operatingPoint.activePower should approximate(expectedPower)
          operatingPoint.reactivePower shouldBe None
          nextTick shouldBe None
        }

      }
    }

    "calculate forecast flex power series correctly" in {
      val model = FixedLoadModel
        .Factory(
          loadInput,
          LoadRuntimeConfig(),
        )
        .create()

      val dataTimeType = DataTimeType.CurrentAndForecast(
        forecastLength = Hours(12),
        forecastResolution = Hours(1),
      )

      val flexOptions =
        model
          .flexModels(FlexType.EnergyBoundaries)
          .determineFlexOptions(FixedState(tick = 0L), dataTimeType)

      flexOptions match {
        case EnergyBoundariesFlexOptions(boundaries) =>
          boundaries should have size 1

          val assetBoundaries = boundaries.headOption.value

          val energyLimits = assetBoundaries.energyLimits
          energyLimits should have size 13
          energyLimits.headOption.value shouldBe (
            0L -> new ClosedInterval(
              zeroKWh,
              zeroKWh,
            )
          )

          val (lastKey, lastValue) = energyLimits.lastOption.value
          lastKey shouldBe 12 * 3600L
          val expectedTotalEnergy = model.pRated * dataTimeType.forecastLength
          lastValue.getUpper should approximate(expectedTotalEnergy)
          lastValue.getLower should approximate(expectedTotalEnergy)

          assetBoundaries.powerLimits.getUpper should approximate(model.pRated)
          assetBoundaries.powerLimits.getLower should approximate(model.pRated)

        case unexpected => fail(s"Received unexpected flex options $unexpected")
      }

    }

  }
}
