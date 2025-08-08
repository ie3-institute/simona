/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.config.RuntimeConfig.EvcsRuntimeConfig
import edu.ie3.simona.model.participant.evcs.EvcsModel.EvcsState
import edu.ie3.simona.ontology.messages.flex.PowerLimitFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.helper.TableDrivenHelper
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.*
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power}

import java.time.ZonedDateTime

class EvcsPowerLimitFlexModelSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with TableDrivenHelper
    with EvcsInputTestData {

  private val dateTime: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02T03:04:05Z")

  private def createModel(
      chargingStrategy: String,
      vehicle2Grid: Boolean = true,
  ): EvcsPowerLimitFlexModel = {
    val model = EvcsModel
      .Factory(
        evcsInputModel.copy().v2gSupport(vehicle2Grid).build(),
        EvcsRuntimeConfig(
          chargingStrategy = chargingStrategy
        ),
      )
      .create()

    EvcsPowerLimitFlexModel(model)
  }

  // Testing tolerances
  given Energy = KilowattHours(1e-10)
  given Power = Kilowatts(1e-10)

  "An EVCS PowerLimitFlexModel" should {

    "calculate flex options correctly" when {

      "charging with constant power and allowing v2g" in {
        val flexModel = createModel("constantPower")

        val currentTick = 7200L

        val cases = Table(
          (
            "stored1",
            "stored2",
            "expectedPRef",
            "expectedPMin",
            "expectedPMax",
          ),

          /* 1: empty */
          // 2: empty
          (0.0, 0.0, 15.0, 15.0, 15.0),
          // 2: at lower margin
          (0.0, 3.0, 15.0, 10.0, 15.0),
          // 2: mid-way full, forced charging
          (0.0, 7.5, 13.75, 10.0, 15.0),
          // 2: almost full, forced charging
          (0.0, 12.5, 11.25, 10.0, 15.0),
          // 2: full, forced charging
          (0.0, 15.0, 10.0, 10.0, 10.0),

          /* 1: at lower margin (set to 2 kWh) */
          // 2: empty
          (2.0, 0.0, 13.0, 5.0, 15.0),
          // 2: at lower margin
          (2.0, 3.0, 13.0, 0.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh)
          (2.0, 7.5, 11.75, -5.0, 15.0),
          // 2: almost full
          (2.0, 12.5, 9.25, -5.0, 15.0),
          // 2: full
          (2.0, 15.0, 8.0, -5.0, 10.0),

          /* 1: mid-way full (set to 5 kWh) */
          // 2: empty, forced charging
          (5.0, 0.0, 10.0, 5.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh)
          (5.0, 7.5, 8.75, -15.0, 15.0),
          // 2: almost full
          (5.0, 12.5, 6.25, -15.0, 15.0),
          // 2: full
          (5.0, 15.0, 5.0, -15.0, 10.0),

          /* 1: full (set to 10 kWh) */
          // 2: empty, forced charging
          (10.0, 0.0, 5.0, 5.0, 5.0),
          // 2: mid-way full
          (10.0, 7.5, 3.75, -15.0, 5.0),
          // 2: almost full
          (10.0, 12.5, 1.25, -15.0, 5.0),
          // 2: full
          (10.0, 15.0, 0.0, -15.0, 0.0),
        )

        forAll(cases) {
          (
              stored1,
              stored2,
              expectedPRef,
              expectedPMin,
              expectedPMax,
          ) =>
            // stays one more hour
            val evA = EvModelWrapper(
              ev4.copyWith(stored1.asKiloWattHour)
            )

            // stays two more hours
            val evB = EvModelWrapper(
              ev5.copyWith(stored2.asKiloWattHour)
            )

            flexModel.determineFlexOptions(
              EvcsState(
                Seq(evA, evB),
                currentTick,
              )
            ) match {
              case PowerLimitFlexOptions(
                    refPower,
                    minPower,
                    maxPower,
                  ) =>
                refPower should approximate(Kilowatts(expectedPRef))
                minPower should approximate(Kilowatts(expectedPMin))
                maxPower should approximate(Kilowatts(expectedPMax))
            }
        }

      }

      "charging with maximum power and allowing v2g" in {
        val flexModel = createModel("maxPower")

        val currentTick = 7200L

        val cases = Table(
          (
            "stored1",
            "stored2",
            "expectedPRef",
            "expectedPMin",
            "expectedPMax",
          ),

          /* 1: empty */
          // 2: empty
          (0.0, 0.0, 15.0, 15.0, 15.0),
          // 2: at lower margin
          (0.0, 3.0, 15.0, 10.0, 15.0),
          // 2: mid-way full, forced charging
          (0.0, 7.5, 15.0, 10.0, 15.0),
          // 2: almost full, forced charging
          (0.0, 12.5, 15.0, 10.0, 15.0),
          // 2: full
          (0.0, 15.0, 10.0, 10.0, 10.0),

          /* 1: at lower margin (set to 2 kWh) */
          // 2: empty
          (2.0, 0.0, 15.0, 5.0, 15.0),
          // 2: at lower margin
          (2.0, 3.0, 15.0, 0.0, 15.0),
          // 2: mid-way full
          (2.0, 7.5, 15.0, -5.0, 15.0),
          // 2: almost full
          (2.0, 12.5, 15.0, -5.0, 15.0),
          // 2: full
          (2.0, 15.0, 10.0, -5.0, 10.0),

          /* 1: mid-way full (set to 5 kWh) */
          // 2: empty, forced charging
          (5.0, 0.0, 15.0, 5.0, 15.0),
          // 2: mid-way full
          (5.0, 7.5, 15.0, -15.0, 15.0),
          // 2: almost full
          (5.0, 12.5, 15.0, -15.0, 15.0),
          // 2: full
          (5.0, 15.0, 10.0, -15.0, 10.0),

          /* 1: full (set to 10 kWh) */
          // 2: empty, forced charging
          (10.0, 0.0, 5.0, 5.0, 5.0),
          // 2: mid-way full
          (10.0, 7.5, 5.0, -15.0, 5.0),
          // 2: almost full
          (10.0, 12.5, 5.0, -15.0, 5.0),
          // 2: full
          (10.0, 15.0, 0.0, -15.0, 0.0),
        )

        forAll(cases) {
          (
              stored1,
              stored2,
              expectedPRef,
              expectedPMin,
              expectedPMax,
          ) =>
            val evA = EvModelWrapper(
              ev4.copyWith(stored1.asKiloWattHour)
            )

            val evB = EvModelWrapper(
              ev5.copyWith(stored2.asKiloWattHour).copyWithDeparture(10800L)
            )

            flexModel.determineFlexOptions(
              EvcsState(
                Seq(evA, evB),
                currentTick,
              )
            ) match {
              case PowerLimitFlexOptions(
                    refPower,
                    minPower,
                    maxPower,
                  ) =>
                refPower should approximate(Kilowatts(expectedPRef))
                minPower should approximate(Kilowatts(expectedPMin))
                maxPower should approximate(Kilowatts(expectedPMax))
            }
        }

      }

      "disallowing v2g" in {
        val flexModel = createModel("constantPower", vehicle2Grid = false)

        val currentTick = 7200L

        val ev1 = EvModelWrapper(
          ev4.copyWith(5.0.asKiloWattHour)
        )

        flexModel.determineFlexOptions(
          EvcsState(
            Seq(ev1),
            currentTick,
          )
        ) match {
          case PowerLimitFlexOptions(
                refPower,
                minPower,
                maxPower,
              ) =>
            refPower should approximate(Kilowatts(5.0)) // one hour left
            minPower should approximate(Kilowatts(0d)) // no v2g allowed!
            maxPower should approximate(ev1.pRatedAc)
        }

      }

      "holding almost full EV" in {
        val flexModel = createModel("constantPower")

        val currentTick = 7200L

        // 9.997222222222222 kWh is the margin including tolerance
        val ev = EvModelWrapper(
          ev4.copyWith(9.998.asKiloWattHour)
        )

        flexModel.determineFlexOptions(
          EvcsState(Seq(ev), currentTick)
        ) match {
          case PowerLimitFlexOptions(
                refPower,
                minPower,
                maxPower,
              ) =>
            // ev in top tolerance margin
            refPower should approximate(Kilowatts(0))
            minPower should approximate(Kilowatts(-10))
            maxPower should approximate(Kilowatts(0))
        }

      }

    }
  }

}
