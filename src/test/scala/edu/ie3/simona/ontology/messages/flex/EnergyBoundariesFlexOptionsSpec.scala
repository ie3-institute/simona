/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.datamodel.models.result.system.EnergyBoundariesFlexOptionsResult
import edu.ie3.simona.exceptions.FlexException
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptionsSpec.limits
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueNoControl,
  IssuePowerControl,
}
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.quantities.QuantityUtils.{asMegaWatt, asMegaWattHour}
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKW, zeroKWh}
import squants.{Each, Energy}
import squants.energy.{KilowattHours, Kilowatts}
import squants.time.Hours

import java.util.UUID
import scala.collection.immutable.SortedMap

class EnergyBoundariesFlexOptionsSpec extends UnitSpec with DefaultTestData {

  private val flexOptions = EnergyBoundariesFlexOptions(
    AssetEnergyBoundaries(
      eStorage = KilowattHours(10),
      currentEnergy = KilowattHours(5),
      pMax = Kilowatts(5),
      etaCharge = onePU,
      etaDischarge = onePU,
      currentTick = 0L,
    )
  )

  "Determining flex power" should {

    "succeed if set power is within limits" in {
      val setPower = Kilowatts(2)
      EnergyBoundariesFlexOptions.determineFlexPower(
        flexOptions,
        IssuePowerControl(0L, setPower),
      ) shouldBe setPower
    }

    "fail if set power is above limit" in {
      intercept[FlexException] {
        EnergyBoundariesFlexOptions.determineFlexPower(
          flexOptions,
          IssuePowerControl(0L, Kilowatts(6)),
        )
      }
    }

    "fail if set power is below limit" in {
      intercept[FlexException] {
        EnergyBoundariesFlexOptions.determineFlexPower(
          flexOptions,
          IssuePowerControl(0L, Kilowatts(-6)),
        )
      }
    }

    "set 0 kW upon no control message" in {
      EnergyBoundariesFlexOptions.determineFlexPower(
        flexOptions,
        IssueNoControl(0L),
      ) shouldBe zeroKW
    }

  }

  "Creating a result entity from flex options" should {

    val uuid: UUID = UUID.fromString("0-0-0-0-1")

    "succeed if proper flex options are provided" in {
      EnergyBoundariesFlexOptions.createResult(
        flexOptions,
        uuid,
        defaultSimulationStart,
      ) shouldBe new EnergyBoundariesFlexOptionsResult(
        defaultSimulationStart,
        uuid,
        0.005.asMegaWattHour,
        0.0.asMegaWattHour,
        0.01.asMegaWattHour,
        -0.005.asMegaWatt,
        0.005.asMegaWatt,
      )
    }

  }

  "Tightening energy boundaries should result in proper energy limits" when {

    val startTick = 3600L
    val sampleTicks = 1800L
    val sampleTime = Hours(0.5)
    val predictionHorizon = Hours(4)

    "providing typical battery storage options with high capacity" in {

      val adaptedBoundaries = AssetEnergyBoundaries.tighten(
        boundaries = AssetEnergyBoundaries(
          eStorage = KilowattHours(50),
          currentEnergy = KilowattHours(20),
          pMax = Kilowatts(10),
          etaCharge = Each(0.8),
          etaDischarge = Each(0.8),
          currentTick = startTick,
        ),
        sampleTime = sampleTime,
        predictionHorizon = predictionHorizon,
      )

      adaptedBoundaries.energyLimits shouldBe SortedMap(
        startTick + 0 * sampleTicks -> limits(20, 20),
        startTick + 1 * sampleTicks -> limits(13.75, 24),
        startTick + 2 * sampleTicks -> limits(7.5, 28),
        startTick + 3 * sampleTicks -> limits(1.25, 32),
        startTick + 4 * sampleTicks -> limits(0, 36),
        startTick + 5 * sampleTicks -> limits(0, 40),
        startTick + 6 * sampleTicks -> limits(0, 44),
        startTick + 7 * sampleTicks -> limits(0, 48),
        startTick + 8 * sampleTicks -> limits(0, 50),
      )

    }

    "providing typical battery storage options with low capacity" in {

      val adaptedBoundaries = AssetEnergyBoundaries.tighten(
        boundaries = AssetEnergyBoundaries(
          eStorage = KilowattHours(8),
          currentEnergy = KilowattHours(4),
          pMax = Kilowatts(10),
          etaCharge = Each(0.8),
          etaDischarge = Each(0.8),
          currentTick = startTick,
        ),
        sampleTime = sampleTime,
        predictionHorizon = predictionHorizon,
      )

      adaptedBoundaries.energyLimits shouldBe SortedMap(
        startTick + 0 * sampleTicks -> limits(4, 4),
        startTick + 1 * sampleTicks -> limits(0, 8),
        startTick + 2 * sampleTicks -> limits(0, 8),
        startTick + 3 * sampleTicks -> limits(0, 8),
        startTick + 4 * sampleTicks -> limits(0, 8),
        startTick + 5 * sampleTicks -> limits(0, 8),
        startTick + 6 * sampleTicks -> limits(0, 8),
        startTick + 7 * sampleTicks -> limits(0, 8),
        startTick + 8 * sampleTicks -> limits(0, 8),
      )

    }

    "providing typical EV options" in {

      val adaptedBoundaries = AssetEnergyBoundaries.tighten(
        boundaries = AssetEnergyBoundaries(
          currentEnergy = KilowattHours(10),
          energyLimits = SortedMap(
            startTick -> new ClosedInterval(
              zeroKWh,
              KilowattHours(50),
            ),
            18000L -> new ClosedInterval(
              KilowattHours(40),
              KilowattHours(50),
            ),
          ),
          powerLimits = ClosedInterval(zeroKW, Kilowatts(10)),
          tickDisconnect = Some(18000L),
        ),
        sampleTime = sampleTime,
        predictionHorizon = predictionHorizon,
      )

      adaptedBoundaries.energyLimits shouldBe SortedMap(
        startTick + 0 * sampleTicks -> limits(10, 10),
        startTick + 1 * sampleTicks -> limits(10, 15),
        startTick + 2 * sampleTicks -> limits(10, 20),
        startTick + 3 * sampleTicks -> limits(15, 25),
        startTick + 4 * sampleTicks -> limits(20, 30),
        startTick + 5 * sampleTicks -> limits(25, 35),
        startTick + 6 * sampleTicks -> limits(30, 40),
        startTick + 7 * sampleTicks -> limits(35, 45),
        startTick + 8 * sampleTicks -> limits(40, 50),
      )

    }

  }

}

object EnergyBoundariesFlexOptionsSpec {

  private def limits(lower: Double, upper: Double): ClosedInterval[Energy] =
    new ClosedInterval(KilowattHours(lower), KilowattHours(upper))

}
