/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.model.participant.ParticipantModel.OperationChangeIndicator
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsOperatingPoint,
  EvcsState,
}
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

  private val currentTick: Long = 7200L

  // Testing tolerances
  given Energy = KilowattHours(1e-10)
  given Power = Kilowatts(1e-10)

  "An EVCS PowerLimitFlexModel" should {

    "calculate flex options correctly with constant power and allowing v2g" when {

      val flexModel = EvcsPowerLimitFlexModel(
        createTestModel(
          chargingStrategy = "constantPower",
          departureTargetSoc = 0.8,
        )
      )

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

      val flexModel = EvcsPowerLimitFlexModel(
        createTestModel(
          chargingStrategy = "maxPower",
          departureTargetSoc = 0.8,
        )
      )

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

      val flexModel = EvcsPowerLimitFlexModel(
        createTestModel(
          chargingStrategy = "constantPower",
          departureTargetSoc = 0.8,
          vehicle2Grid = false,
        )
      )

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

    "determine the next activation tick correctly" when {
      val currentTick = 3600L

      val flexModel = EvcsPowerLimitFlexModel(
        createTestModel(
          chargingStrategy = "constantPower",
          departureTargetSoc = 0.8,
        )
      )

      "dealing with one ev" in {

        val cases = Table(
          (
            "stored",
            "setPower",
            "evPower1",
            "expNextActivation",
            "expNextTick",
          ),

          /* setPower is 0 kW, tick is departure */
          (8.0, 0.0, 0.0, false, S(10800L)),
          (10.0, 0.0, 0.0, false, S(10800L)),

          /* setPower is 0 kW, tick is last chance for target achievement */
          (0.0, 0.0, 0.0, false, S(7920L)),
          (5.0, 0.0, 0.0, false, S(9720L)),

          /* setPower is positive (charging), tick is departure */
          (5.0, 2.0, 2.0, false, S(10800L)),
          (8.0, 0.5, 0.5, false, S(10800L)),

          /* setPower is positive (charging), tick is last chance for target achievement */
          (0.0, 2.0, 2.0, true, S(9000L)),
          (0.0, 1.0, 1.0, true, S(8400L)),
          (2.0, 2.0, 2.0, false, S(9900L)),

          /* setPower is positive (charging), tick is when storage reaches full capacity */
          (0.0, 10.0, 10.0, true, S(7200L)),
          (0.0, 5.0, 5.0, true, S(10800L)),
          (4.0, 4.0, 4.0, false, S(9000L)),
          (8.0, 4.0, 4.0, false, S(5400L)),
          (8.0, 2.0, 2.0, false, S(7200L)),

          /* setPower is set to > ev (charging), tick is when storage reaches full capacity */
          (0.0, 11.0, 10.0, true, S(7200L)),
          (5.0, 15.0, 10.0, false, S(5400L)),

          /* setPower is negative (discharging), tick is departure */
          (10.0, -1.0, -1.0, true, S(10800L)),
          (10.0, -0.5, -0.5, true, S(10800L)),

          /* setPower is negative (discharging), tick is last chance for target achievement */
          (10.0, -6.0, -6.0, true, S(8550L)),
          (10.0, -5.0, -5.0, true, S(8880L)),
          (8.0, -2.0, -2.0, false, S(9600L)),

          /* setPower is negative (discharging), tick is when storage reaches empty capacity */
          (7.5, -10.0, -10.0, false, S(6300L)),
          (5.0, -10.0, -10.0, false, S(5400L)),
          (2.0, -8.0, -8.0, false, S(4500L)),

          /* setPower is set to > ev (discharging), tick is when storage reaches empty capacity */
          (10.0, -11.0, -10.0, true, S(7200L)),
          (5.0, -15.0, -10.0, false, S(5400L)),
        )

        forAll(cases) {
          (
              stored: Double,
              setPower: Double,
              evPower1: Double,
              expNextActivation: Boolean,
              expNextTick: Option[Long],
          ) =>
            // 10 kWh capacity, 10 kWh target, 10 kW max power, stays two hours
            val ev = EvModelWrapper(
              ev4
                .copyWith(stored.asKiloWattHour)
                .copyWithDeparture(currentTick + 7200L)
            )
            val state = EvcsState(Seq(ev), currentTick)
            val op = EvcsOperatingPoint(
              Map(ev.uuid -> Kilowatts(evPower1))
            )

            flexModel.determineNextActivation(
              state,
              op,
              Kilowatts(setPower),
              DataTimeType.Current,
            ) shouldBe OperationChangeIndicator(
              expNextActivation,
              expNextTick,
            )
        }
      }

      "dealing with two evs" in {

        val cases = Table(
          (
            "stored1",
            "stored2",
            "setPower",
            "evPower1",
            "evPower2",
            "expNextActivation",
            "expNextTick",
          ),

          /* setPower is 0 kW */
          (0.0, 0.0, 0.0, 0.0, 0.0, false, S(4320L)),
          (10.0, 5.0, 0.0, 0.0, 0.0, false, S(5760L)),
          (5.0, 15.0, 0.0, 0.0, 0.0, false, S(6120L)),

          /* setPower is positive (charging) */
          (0.0, 0.0, 4.0, 0.0, 4.0, true, S(4320L)),
          (0.0, 10.0, 4.0, 2.0, 2.0, true, S(4500L)),
          (10.0, 14.0, 4.0, 0.0, 4.0, false, S(4500L)),

          /* setPower is set to > (ev2 * 2) (charging) */
          (7.0, 0.0, 11.0, 6.0, 5.0, true, S(5400L)),
          (0.0, 5.0, 15.0, 10.0, 5.0, true, S(7200L)),
          (5.0, 7.5, 15.0, 10.0, 5.0, false, S(5400L)),

          /* setPower is negative (discharging) */
          (10.0, 15.0, -4.0, -2.0, -2.0, true, S(7200L)),
          (0.0, 4.0, -4.0, 0.0, -4.0, false, S(4320L)),
          (7.5, 0.0, -5.0, -5.0, 0.0, false, S(5880L)),

          /* setPower is set to > (ev2 * 2) (discharging) */
          (10.0, 15.0, -13.0, -8.0, -5.0, true, S(6000L)),
          (5.0, 15.0, -15.0, -10.0, -5.0, true, S(4860L)),
          (10.0, 15.0, -15.0, -10.0, -5.0, true, S(5760L)),
        )

        forAll(cases) {
          (
              stored1: Double,
              stored2: Double,
              setPower: Double,
              evPower1: Double,
              evPower2: Double,
              expNextActivation: Boolean,
              expNextTick: Option[Long],
          ) =>
            // 10 kWh capacity, 10 kWh target, 10 kW max power, stays one hour
            val evA = EvModelWrapper(
              ev4.copyWith(stored1.asKiloWattHour).copyWithDeparture(7200L)
            )
            // 15 kWh capacity, 15 kWh target, 5 kW max power, stays two hours
            val evB = EvModelWrapper(
              ev5.copyWith(stored2.asKiloWattHour).copyWithDeparture(10800L)
            )
            val state = EvcsState(Seq(evA, evB), currentTick)
            val op = EvcsOperatingPoint(
              Map(
                evA.uuid -> Kilowatts(evPower1),
                evB.uuid -> Kilowatts(evPower2),
              )
            )

            flexModel.determineNextActivation(
              state,
              op,
              Kilowatts(setPower),
              DataTimeType.Current,
            ) shouldBe OperationChangeIndicator(
              expNextActivation,
              expNextTick,
            )
        }

      }

    }

  }

}
