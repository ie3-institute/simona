/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.config.RuntimeConfig.EvcsRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.evcs.EvcsModel.EvcsState
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.service.DataTimeType.{Current, CurrentAndForecast}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.quantities.QuantityUtils.*
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import squants.Seconds
import squants.energy.{KilowattHours, Kilowatts}

import scala.collection.immutable.SortedMap

class EvcsEnergyBoundariesFlexModelSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with EvcsInputTestData {

  private def createModel(
      vehicle2Grid: Boolean = true
  ): EvcsEnergyBoundariesFlexModel = {
    val model = EvcsModel
      .Factory(
        evcsInputModel.copy().v2gSupport(vehicle2Grid).build(),
        EvcsRuntimeConfig(departureTargetSoc = 0.8),
      )
      .create()

    EvcsEnergyBoundariesFlexModel(model)
  }

  // uneven tick for testing purposes
  private val currentTick = 4321L
  private val forecastResolution = 1800L
  private val forecastLength = forecastResolution * 4
  private val forecastEnd = currentTick + forecastLength

  "An EVCS EnergyBoundariesFlexModel" should {

    "adapt disconnecting tick according to forecast settings" in {

      val flexModel = createModel()

      val cases = Table(
        ("ticksUntilDeparture", "expectedTicksUntilDisconnect"),
        /* departure before first step */
        (1, Some(1800)),
        (900, Some(1800)),
        (1800, Some(1800)),

        /* departure close to a step */
        (2699, Some(1800)),
        (2700, Some(3600)),
        (4499, Some(3600)),
        (4500, Some(5400)),

        /* departure close to or beyond forecast end */
        (7200, Some(7200)),
        (7201, None),
        (54321, None),
      )

      forAll(cases) { (ticksUntilDeparture, expectedTicksUntilDisconnect) =>

        val departureTick = currentTick + ticksUntilDeparture
        val expectedDisconnectTick =
          expectedTicksUntilDisconnect.map(currentTick + _)

        val flexOptions = flexModel
          .determineEvFlexOptions(
            EvModelWrapper(ev1.copyWithDeparture(departureTick)),
            currentTick,
            forecastResolution,
            forecastEnd,
          )

        flexOptions.tickDisconnect shouldBe expectedDisconnectTick

        val expectedLimits =
          // if EV is disconnecting, there should be requirements for lowest SOC
          if expectedDisconnectTick.isDefined then 2
          // if EV is not disconnecting, we don't consider lowest SOC
          else 1
        flexOptions.energyLimits should have size expectedLimits

      }

    }

    "adapt disconnecting SOC requirements based on feasibility" in {

      val flexModel = createModel()

      val departureTick = currentTick + 7200L

      val cases = Table(
        ("storedEnergy", "expectedLowerLimit"),
        /* SOC target can not be achieved, thus reachable target is used */
        (0.0, 10.0),
        (1.0, 11.0),
        (1.99, 11.99),

        /* regular SOC target can be met */
        (2.0, 12.0),
        (2.01, 12.0),
        (10.0, 12.0),
        (15.0, 12.0),
      )

      forAll(cases) { (storedEnergy, expectedLowerLimit) =>

        val stored = KilowattHours(storedEnergy)

        val ev = EvModelWrapper(
          ev5
            .copyWith(storedEnergy.asKiloWattHour)
            .copyWithDeparture(departureTick)
        )

        val energyLimits = flexModel
          .determineEvFlexOptions(
            ev,
            currentTick,
            forecastResolution,
            forecastEnd,
          )
          .energyLimits

        energyLimits should have size 2
        energyLimits(currentTick) shouldBe
          new ClosedInterval(-stored, ev.eStorage - stored)
        energyLimits(departureTick) shouldBe
          new ClosedInterval(
            KilowattHours(expectedLowerLimit) - stored,
            ev.eStorage - stored,
          )
      }

    }

    "allow or disallow discharging based on vehicle2grid settings" in {

      val ev = EvModelWrapper(ev1)

      // allow discharging if V2G is enabled
      createModel()
        .determineEvFlexOptions(
          ev,
          currentTick,
          forecastResolution,
          forecastEnd,
        )
        .powerLimits shouldBe
        new ClosedInterval(
          -ev.pRatedAc,
          ev.pRatedAc,
        )

      // disallow discharging if V2G is disabled
      createModel(vehicle2Grid = false)
        .determineEvFlexOptions(
          ev,
          currentTick,
          forecastResolution,
          forecastEnd,
        )
        .powerLimits shouldBe
        new ClosedInterval(
          zeroKW,
          ev.pRatedAc,
        )

    }

    "create flex options for a state and forecast parameters" in {

      val departureTick = currentTick + 3600

      val flexOptions = createModel().determineFlexOptions(
        state = EvcsState(
          evs = Seq(
            EvModelWrapper(
              ev1.copyWithDeparture(departureTick)
            )
          ),
          tick = currentTick,
        ),
        dataTimeType = CurrentAndForecast(
          forecastLength = Seconds(forecastLength),
          forecastResolution = Seconds(forecastResolution),
        ),
      )

      flexOptions match {
        case EnergyBoundariesFlexOptions(boundaries :: Nil) =>
          boundaries shouldBe AssetEnergyBoundaries(
            energyLimits = SortedMap(
              currentTick -> new ClosedInterval(
                KilowattHours(-5),
                KilowattHours(5),
              ),
              departureTick -> new ClosedInterval(
                KilowattHours(3),
                KilowattHours(5),
              ),
            ),
            powerLimits = new ClosedInterval(
              Kilowatts(-5),
              Kilowatts(5),
            ),
            tickDisconnect = Some(departureTick),
          )
        case _ => fail(s"Unexpected flex options $flexOptions")
      }

    }

    "throw an exception if the wrong data time type is supplied" in {

      intercept[CriticalFailureException] {
        createModel().determineFlexOptions(
          state = EvcsState(
            evs = Seq.empty,
            tick = currentTick,
          ),
          dataTimeType = Current,
        )
      }

    }

  }

}
