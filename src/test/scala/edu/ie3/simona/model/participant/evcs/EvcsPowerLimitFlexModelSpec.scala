/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.config.RuntimeConfig.EvcsRuntimeConfig
import edu.ie3.simona.model.participant.evcs.EvcsModel.EvcsState
import edu.ie3.simona.ontology.messages.flex.PowerLimitFlexOptions
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.helper.TableDrivenHelper
import edu.ie3.util.quantities.QuantityUtils.*
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power}

class EvcsPowerLimitFlexModelSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with TableDrivenHelper
    with EvcsInputTestData {

  private def createModel(
      chargingStrategy: String,
      vehicle2Grid: Boolean = true,
  ): EvcsPowerLimitFlexModel = {
    val model = EvcsModel
      .Factory(
        evcsInputModel.copy().v2gSupport(vehicle2Grid).build(),
        EvcsRuntimeConfig(
          chargingStrategy = chargingStrategy,
          departureTargetSoc = 0.8,
        ),
      )
      .create()

    EvcsPowerLimitFlexModel(model)
  }

  private val currentTick: Long = 7200L

  // Testing tolerances
  given Energy = KilowattHours(1e-10)
  given Power = Kilowatts(1e-10)

  "An EVCS PowerLimitFlexModel" should {

    "calculate flex options correctly with constant power and allowing v2g" when {

      val flexModel = createModel("constantPower")

      "one EV is connected" in {

        val cases = Table(
          (
            "stored",
            "expectedPRef",
            "expectedPMin",
            "expectedPMax",
          ),

          // empty -> forced charging
          (0.0, 5.0, 5.0, 5.0),
          // at lower margin -> forced charging
          (2.0, 5.0, 5.0, 5.0),
          // just above lower margin -> no forced charging
          (2.01, 4.995, -5.0, 5.0),
          // mid-way full -> no forced charging
          (7.5, 2.25, -5.0, 5.0),
          // at target -> no charging as preference
          (12.0, 0.0, -5.0, 5.0),
          // almost full -> no charging as preference
          (14.5, 0.0, -5.0, 5.0),
          // in the margin of full (14.998611111111112 kWh) -> only discharging allowed
          (14.999, 0.0, -5.0, 0.0),
          // full -> only discharging allowed
          (15.0, 0.0, -5.0, 0.0),
        )

        forAll(cases) {
          (
              stored,
              expectedPRef,
              expectedPMin,
              expectedPMax,
          ) =>
            // 15 kWh capacity, 12 kWh target, 5 kW max power, stays two hours
            val ev = EvModelWrapper(
              ev5.copyWith(stored.asKiloWattHour)
            )

            flexModel.determineFlexOptions(
              EvcsState(Seq(ev), currentTick),
              DataTimeType.Current,
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

      "two EVs are connected" in {

        val cases = Table(
          (
            "stored1",
            "stored2",
            "expectedPRef",
            "expectedPMin",
            "expectedPMax",
          ),

          /* REMINDER: if at least one EV is forced to
             charge, there is no discharging for both */

          /* 1: empty -> forced charging */
          // 2: empty -> forced charging
          (0.0, 0.0, 15.0, 15.0, 15.0),
          // 2: mid-way full -> no forced charging
          (0.0, 7.5, 12.25, 10.0, 15.0),
          // 2: full -> no forced charging
          (0.0, 15.0, 10.0, 10.0, 10.0),

          /* 1: mid-way full -> no forced charging */
          // 2: empty -> forced charging
          (5.0, 0.0, 11.0, 5.0, 15.0),
          // 2: mid-way full -> no forced charging
          (5.0, 7.5, 8.25, -15.0, 15.0),
          // 2: full -> no forced charging
          (5.0, 15.0, 6.0, -15.0, 10.0),

          /* 1: full -> no forced charging */
          // 2: empty -> forced charging
          (10.0, 0.0, 5.0, 5.0, 5.0),
          // 2: mid-way full -> no forced charging
          (10.0, 7.5, 2.25, -15.0, 5.0),
          // 2: full -> no forced charging
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
            // 10 kWh capacity, 8 kWh target, 10 kW max power, stays half an hour
            val evA = EvModelWrapper(
              ev4
                .copyWith(stored1.asKiloWattHour)
                .copyWithDeparture(currentTick + 1800L)
            )

            // 15 kWh capacity, 12 kWh target, 5 kW max power, stays two hours
            val evB = EvModelWrapper(
              ev5.copyWith(stored2.asKiloWattHour)
            )

            flexModel.determineFlexOptions(
              EvcsState(Seq(evA, evB), currentTick),
              DataTimeType.Current,
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

    }

    "calculate flex options correctly with maximum power and allowing v2g" when {

      val flexModel = createModel("maxPower")

      "one EV is connected" in {

        val cases = Table(
          (
            "stored",
            "expectedPRef",
            "expectedPMin",
            "expectedPMax",
          ),

          // empty -> forced charging
          (0.0, 5.0, 5.0, 5.0),
          // at lower margin -> forced charging
          (2.0, 5.0, 5.0, 5.0),
          // just above lower margin -> no forced charging
          (2.01, 5.0, -5.0, 5.0),
          // mid-way full -> no forced charging
          (7.5, 5.0, -5.0, 5.0),
          // at target -> no charging as preference
          (12.0, 0.0, -5.0, 5.0),
          // almost full -> no charging as preference
          (14.5, 0.0, -5.0, 5.0),
          // in the margin of full (14.998611111111112 kWh) -> only discharging allowed
          (14.999, 0.0, -5.0, 0.0),
          // full -> only discharging allowed
          (15.0, 0.0, -5.0, 0.0),
        )

        forAll(cases) {
          (
              stored,
              expectedPRef,
              expectedPMin,
              expectedPMax,
          ) =>
            // 15 kWh capacity, 12 kWh target, 5 kW max power, stays two hours
            val ev = EvModelWrapper(
              ev5.copyWith(stored.asKiloWattHour)
            )

            flexModel.determineFlexOptions(
              EvcsState(Seq(ev), currentTick),
              DataTimeType.Current,
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

      "two EVs are connected" in {

        val cases = Table(
          (
            "stored1",
            "stored2",
            "expectedPRef",
            "expectedPMin",
            "expectedPMax",
          ),

          /* REMINDER: if at least one EV is forced to
             charge, there is no discharging for both */

          /* 1: empty -> forced charging */
          // 2: empty -> forced charging
          (0.0, 0.0, 15.0, 15.0, 15.0),
          // 2: mid-way full -> forced charging
          (0.0, 7.5, 15.0, 10.0, 15.0),
          // 2: full -> no forced charging
          (0.0, 15.0, 10.0, 10.0, 10.0),

          /* 1: mid-way full -> no forced charging */
          // 2: empty, forced charging
          (5.0, 0.0, 15.0, 5.0, 15.0),
          // 2: mid-way full -> no forced charging
          (5.0, 7.5, 15.0, -15.0, 15.0),
          // 2: full -> no forced charging
          (5.0, 15.0, 10.0, -15.0, 10.0),

          /* 1: full -> no forced charging */
          // 2: empty, forced charging
          (10.0, 0.0, 5.0, 5.0, 5.0),
          // 2: mid-way full -> no forced charging
          (10.0, 7.5, 5.0, -15.0, 5.0),
          // 2: full -> no forced charging
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
            // 10 kWh capacity, 8 kWh target, 10 kW max power, stays half an hour
            val evA = EvModelWrapper(
              ev4
                .copyWith(stored1.asKiloWattHour)
                .copyWithDeparture(currentTick + 1800L)
            )

            // 15 kWh capacity, 12 kWh target, 5 kW max power, stays one hour
            val evB = EvModelWrapper(
              ev5.copyWith(stored2.asKiloWattHour).copyWithDeparture(10800L)
            )

            flexModel.determineFlexOptions(
              EvcsState(Seq(evA, evB), currentTick),
              DataTimeType.Current,
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

    }

    "calculate flex options correctly with disallowing v2g" in {

      val flexModel = createModel("constantPower", vehicle2Grid = false)

      // 10 kWh capacity, 8 kWh target, 10 kW max power, stays one hour
      val ev1 = EvModelWrapper(
        ev4.copyWith(3.0.asKiloWattHour)
      )

      flexModel.determineFlexOptions(
        EvcsState(Seq(ev1), currentTick),
        DataTimeType.Current,
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

  }

}
