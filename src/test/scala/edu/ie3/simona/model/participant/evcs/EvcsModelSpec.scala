/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.simona.api.data.ev.ontology.builder.EvcsMovementsBuilder
import edu.ie3.simona.model.participant.FlexChangeIndicator
import edu.ie3.simona.model.participant.evcs.ChargingSchedule.Entry
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsRelevantData,
  EvcsState
}
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.MockEvModel
import edu.ie3.simona.test.common.model.participant.EvcsTestData
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.scalatest.prop.TableDrivenPropertyChecks
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.PERCENT

import java.util.UUID
import javax.measure.quantity.Dimensionless

class EvcsModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with EvcsTestData {

  private val simulationStart = evcsStandardModel.simulationStartDate

  "An EVCS model" should {

    "calculate new schedules correctly" when {

      "configured as a charging hub" in {
        val evcsModel = evcsStandardModel.copy(
          strategy = ChargingStrategy.CONSTANT_POWER,
          locationType = EvcsLocationType.CHARGING_HUB_TOWN
        )

        val evModel = new MockEvModel(
          UUID.randomUUID(),
          "Mock EV",
          10.0.asKiloWatt, // AC is relevant,
          20.0.asKiloWatt, // DC is not
          20.0.asKiloWattHour,
          5.0.asKiloWattHour,
          10800L
        )

        val actualSchedule = evcsModel.calculateNewScheduling(
          EvcsRelevantData(
            3600L,
            new EvcsMovementsBuilder().build(),
            Map.empty // should be irrelevant
          ),
          Set(evModel)
        )

        actualSchedule shouldBe Map(
          evModel -> Some(
            // ending early at 9000 because of max power charging
            ChargingSchedule(evModel, Seq(Entry(3600L, 9000L, 10.0.asKiloWatt)))
          )
        )
      }

      "configured as a home cs with constant power strategy" in {
        val evcsModel = evcsStandardModel.copy(
          strategy = ChargingStrategy.CONSTANT_POWER,
          locationType = EvcsLocationType.HOME
        )

        val evModel = new MockEvModel(
          UUID.randomUUID(),
          "Mock EV",
          10.0.asKiloWatt, // AC is relevant,
          20.0.asKiloWatt, // DC is not
          20.0.asKiloWattHour,
          15.0.asKiloWattHour,
          10800L
        )

        val actualSchedule = evcsModel.calculateNewScheduling(
          EvcsRelevantData(
            3600L,
            new EvcsMovementsBuilder().build(),
            Map.empty // should be irrelevant
          ),
          Set(evModel)
        )

        actualSchedule shouldBe Map(
          evModel -> Some(
            // using 2.5 kW with constant power charging
            ChargingSchedule(evModel, Seq(Entry(3600L, 10800L, 2.5.asKiloWatt)))
          )
        )
      }
    }

    "apply schedules correctly" when {

      "being provided with a ChargingSchedule consisting of one entry" in {
        val evcsModel = evcsStandardModel

        val currentTick = 3600L

        val cases = Table(
          (
            "storedEnergy",
            "chargeStart",
            "chargeEnd",
            "lastCalcTick",
            "power",
            "expectedStored"
          ),
          // charging ends before currentTick
          (0.0, 0L, 2700L, 0L, 5.0, 3.75),
          (0.0, 0L, 1800L, 0L, 2.5, 1.25),
          (0.0, 900L, 2700L, 0L, 5.0, 2.5),
          (2.5, 0L, 2700L, 1800L, 5.0, 3.75),
          (2.5, 0L, 2700L, 900L, 5.0, 5.0),
          // charging ends at currentTick
          (0.0, 0L, 3600L, 0L, 5.0, 5.0),
          (0.0, 0L, 3600L, 0L, 2.5, 2.5),
          (0.0, 900L, 3600L, 0L, 5.0, 3.75),
          (2.5, 0L, 3600L, 1800L, 5.0, 5.0),
          (2.5, 0L, 3600L, 2700L, 5.0, 3.75),
          // charging ends after currentTick
          (0.0, 0L, 7200L, 0L, 5.0, 5.0),
          (0.0, 0L, 7200L, 0L, 2.5, 2.5),
          (0.0, 900L, 7200L, 0L, 5.0, 3.75),
          (2.5, 0L, 7200L, 1800L, 5.0, 5.0),
          (2.5, 0L, 7200L, 2700L, 5.0, 3.75)
        )

        forAll(cases) {
          (
              storedEnergy,
              chargeStart,
              chargeEnd,
              lastCalcTick,
              power,
              expectedStored
          ) =>
            val ev = new MockEvModel(
              UUID.randomUUID(),
              "TestEv1",
              5.0.asKiloWatt, // using AC charging here
              10.0.asKiloWatt,
              10.0.asKiloWattHour,
              storedEnergy.asKiloWattHour,
              7200L // is ignored here
            )

            val schedule = ChargingSchedule(
              ev,
              Seq(Entry(chargeStart, chargeEnd, power.asKiloWatt))
            )

            val voltage =
              Quantities.getQuantity(1d, StandardUnits.VOLTAGE_MAGNITUDE)

            val state = EvcsState(
              Set(ev),
              Map(ev -> Some(schedule)),
              lastCalcTick
            )

            val actualOutput = evcsModel.applySchedule(
              currentTick,
              voltage,
              state
            )

            val expectedChargeStart = math.max(chargeStart, lastCalcTick)
            val expectedChargeEnd = math.min(chargeEnd, currentTick)

            actualOutput should have size 1
            val (actualEv, actualResults, actualPowerEntries) =
              actualOutput.headOption.getOrElse(
                fail("No charging schedule provided.")
              )

            actualEv.getUuid shouldBe ev.getUuid
            actualEv.getId shouldBe ev.getId
            actualEv.getSRatedAC shouldBe ev.getSRatedAC
            actualEv.getSRatedDC shouldBe ev.getSRatedDC
            actualEv.getEStorage shouldBe ev.getEStorage
            actualEv.getStoredEnergy should equalWithTolerance(
              expectedStored.asKiloWattHour
            )
            actualEv.getDepartureTick shouldBe ev.getDepartureTick

            actualResults match {
              case Seq(evResult) =>
                evResult.getTime shouldBe expectedChargeStart.toDateTime(
                  simulationStart
                )
                evResult.getInputModel shouldBe ev.getUuid
                evResult.getP should equalWithTolerance(power.asKiloWatt)
                // soc at the start
                evResult.getSoc shouldBe ev.getStoredEnergy
                  .divide(ev.getEStorage)
                  .asType(classOf[Dimensionless])
                  .to(PERCENT)

              case unexpected => fail(s"Unexpected EvResults: $unexpected")
            }

            actualPowerEntries match {
              case Seq(entry) =>
                entry.start shouldBe expectedChargeStart
                entry.end shouldBe expectedChargeEnd
                entry.power.p shouldBe power.asKiloWatt

              case unexpected => fail(s"Unexpected PowerEntries: $unexpected")
            }

        }

      }
    }

    "handle flexibility correctly" when {
      val evcsModel =
        evcsStandardModel.copy(strategy = ChargingStrategy.CONSTANT_POWER)

      "calculate flex options for two evs correctly" in {
        val currentTick = 7200L

        val data = EvcsRelevantData(
          currentTick,
          new EvcsMovementsBuilder().build(),
          Map.empty
        )

        val cases = Table(
          (
            "lastStored1",
            "lastStored2",
            "lastPower2",
            "expectedPRef",
            "expectedPMin",
            "expectedPMax"
          ),

          /* 1: empty */
          // 2: empty
          (0.0, 0.0, 0.0, 15.0, 0.0, 15.0),
          // 2: mid-way full (charged to 7.5 kWh)
          (0.0, 0.0, 5.0, 15.0, -5.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh)
          (0.0, 7.5, 0.0, 15.0, -5.0, 15.0),
          // 2: almost full (12.5 kWh)
          (0.0, 5.0, 5.0, 12.5, -5.0, 15.0),
          // 2: full (set)
          (0.0, 15.0, 0.0, 10.0, -5.0, 10.0),

          /* 1: mid-way full (set to 5 kWh) */
          // 2: empty
          (5.0, 0.0, 0.0, 10.0, -10.0, 15.0),
          // 2: mid-way full (charged to 7.5 kWh)
          (5.0, 0.0, 5.0, 10.0, -15.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh)
          (5.0, 7.5, 0.0, 10.0, -15.0, 15.0),
          // 2: almost full (12.5 kWh)
          (5.0, 5.0, 5.0, 7.5, -15.0, 15.0),
          // 2: full (set)
          (5.0, 15.0, 0.0, 5.0, -15.0, 10.0),

          /* 1: full (set to 10 kWh) */
          // 2: empty
          (10.0, 0.0, 0.0, 5.0, -10.0, 5.0),
          // 2: mid-way full (charged to 7.5 kWh)
          (10.0, 0.0, 5.0, 5.0, -15.0, 5.0),
          // 2: mid-way full (set to 7.5 kWh)
          (10.0, 7.5, 0.0, 5.0, -15.0, 5.0),
          // 2: almost full (12.5 kWh)
          (10.0, 5.0, 5.0, 2.5, -15.0, 5.0),
          // 2: full (set)
          (10.0, 15.0, 0.0, 0.0, -15.0, 0.0)
        )

        forAll(cases) {
          (
              lastStored1,
              lastStored2,
              lastPower2,
              expectedPRef,
              expectedPMin,
              expectedPMax
          ) =>
            val ev1 = new MockEvModel(
              UUID.randomUUID(),
              "Mock EV 1",
              10.0.asKiloWatt, // AC is relevant,
              20.0.asKiloWatt, // DC is not
              10.0.asKiloWattHour,
              lastStored1.asKiloWattHour,
              10800L
            )

            val schedule1 = ChargingSchedule(
              ev1,
              Seq(ChargingSchedule.Entry(3600L, 7200L, zeroKW))
            )

            val ev2 = new MockEvModel(
              UUID.randomUUID(),
              "Mock EV 2",
              5.0.asKiloWatt, // AC is relevant,
              10.0.asKiloWatt, // DC is not
              15.0.asKiloWattHour,
              lastStored2.asKiloWattHour,
              10800L
            )

            val schedule2 = ChargingSchedule(
              ev1,
              Seq(ChargingSchedule.Entry(0L, 5400L, lastPower2.asKiloWatt))
            )

            evcsModel.determineFlexOptions(
              data,
              EvcsState(
                Set(ev1, ev2),
                Map(ev1 -> Some(schedule1), ev2 -> Some(schedule2)),
                0L
              )
            ) match {
              case ProvideMinMaxFlexOptions(
                    modelUuid,
                    refPower,
                    minPower,
                    maxPower
                  ) =>
                modelUuid shouldBe evcsModel.getUuid
                refPower should equalWithTolerance(expectedPRef.asKiloWatt)
                minPower should equalWithTolerance(expectedPMin.asKiloWatt)
                maxPower should equalWithTolerance(expectedPMax.asKiloWatt)
            }
        }

      }

      "handle controlled power change for two evs correctly" in {
        val currentTick = 3600L

        val data = EvcsRelevantData(
          currentTick,
          new EvcsMovementsBuilder().build(),
          Map.empty
        )

        val cases = Table(
          (
            "stored1",
            "stored2",
            "setPower",
            "expPowerAndTick1",
            "expPowerAndTick2",
            "expNextActivation",
            "expNextTick"
          ),

          /* setPower is 0 kWh */
          (0.0, 0.0, 0.0, N, N, false, N),
          (10.0, 5.0, 0.0, N, N, false, N),
          (5.0, 15.0, 0.0, N, N, false, N),
          (10.0, 15.0, 0.0, N, N, false, N),

          /* setPower is positive (charging) */
          (0.0, 0.0, 4.0, S(2.0, 7200L), S(2.0, 10800L), true, S(7200L)),
          (5.0, 0.0, 4.0, S(2.0, 7200L), S(2.0, 10800L), true, S(7200L)),
          (0.0, 7.5, 4.0, S(2.0, 7200L), S(2.0, 10800L), true, S(7200L)),
          (9.0, 0.0, 4.0, S(2.0, 5400L), S(2.0, 10800L), true, S(5400L)),
          (5.0, 14.0, 4.0, S(2.0, 7200L), S(2.0, 5400L), false, S(5400L)),
          (9.0, 14.0, 4.0, S(2.0, 5400L), S(2.0, 5400L), false, S(5400L)),
          (10.0, 14.0, 4.0, N, S(4.0, 4500L), false, S(4500L)),
          (6.0, 15.0, 4.0, S(4.0, 7200L), N, false, S(7200L)),

          /* setPower is set to > (ev2 * 2) (charging) */
          (0.0, 0.0, 13.0, S(8.0, 7200L), S(5.0, 10800L), true, S(7200L)),
          (7.0, 0.0, 11.0, S(6.0, 5400L), S(5.0, 10800L), true, S(5400L)),
          (0.0, 5.0, 15.0, S(10.0, 7200L), S(5.0, 10800L), true, S(7200L)),
          (0.0, 12.5, 15.0, S(10.0, 7200L), S(5.0, 5400L), true, S(5400L)),
          (0.0, 0.0, 15.0, S(10.0, 7200L), S(5.0, 10800L), true, S(7200L)),
          (5.0, 7.5, 15.0, S(10.0, 5400L), S(5.0, 9000L), false, S(5400L)),

          /* setPower is negative (discharging) */
          (10.0, 15.0, -4.0, S(-2.0, 7200L), S(-2.0, 10800L), true, S(7200L)),
          (5.0, 15.0, -4.0, S(-2.0, 7200L), S(-2.0, 10800L), true, S(7200L)),
          (10.0, 7.5, -4.0, S(-2.0, 7200L), S(-2.0, 10800L), true, S(7200L)),
          (3.0, 15.0, -4.0, S(-2.0, 5400L), S(-2.0, 10800L), true, S(5400L)),
          (5.0, 4.0, -4.0, S(-2.0, 7200L), S(-2.0, 5400L), false, S(5400L)),
          (3.0, 4.0, -4.0, S(-2.0, 5400L), S(-2.0, 5400L), false, S(5400L)),
          (0.0, 4.0, -4.0, N, S(-4.0, 4500L), false, S(4500L)),
          (6.0, 0.0, -4.0, S(-4.0, 7200L), N, false, S(7200L)),

          /* setPower is set to > (ev2 * 2) (discharging) */
          (10.0, 15.0, -13.0, S(-8.0, 7200L), S(-5.0, 10800L), true, S(7200L)),
          (5.0, 15.0, -11.0, S(-6.0, 5400L), S(-5.0, 10800L), true, S(5400L)),
          (10.0, 8.0, -15.0, S(-10.0, 6480L), S(-5.0, 7200L), true, S(6480L)),
          (10.0, 5.5, -15.0, S(-10.0, 6480L), S(-5.0, 5400L), true, S(5400L)),
          (10.0, 15.0, -15.0, S(-10.0, 6480L), S(-5.0, 10800L), true, S(6480L)),
          (7.0, 10.5, -15.0, S(-10.0, 5400L), S(-5.0, 9000L), false, S(5400L))
        )

        forAll(cases) {
          (
              stored1: Double,
              stored2: Double,
              setPower: Double,
              expPowerAndTick1: Option[(Double, Long)],
              expPowerAndTick2: Option[(Double, Long)],
              expNextActivation: Boolean,
              expNextTick: Option[Long]
          ) =>
            val ev1 = new MockEvModel(
              UUID.randomUUID(),
              "Mock EV 1",
              10.0.asKiloWatt, // AC is relevant,
              20.0.asKiloWatt, // DC is not
              10.0.asKiloWattHour,
              stored1.asKiloWattHour,
              7200L
            )

            val ev2 = new MockEvModel(
              UUID.randomUUID(),
              "Mock EV 2",
              5.0.asKiloWatt, // AC is relevant,
              10.0.asKiloWatt, // DC is not
              15.0.asKiloWattHour,
              stored2.asKiloWattHour,
              10800L
            )

            evcsModel.handleControlledPowerChange(
              data,
              EvcsState(
                Set(ev1, ev2),
                Map(ev1 -> None, ev2 -> None),
                0L
              ),
              setPower.asKiloWatt
            ) match {
              case (
                    EvcsState(actualEvs, actualSchedules, actualTick),
                    FlexChangeIndicator(actualNextActivation, actualNextTick)
                  ) =>
                // evs have not changed here since no schedules were given as input
                actualEvs shouldBe Set(ev1, ev2)

                actualSchedules.getOrElse(ev1, None).map {
                  case ChargingSchedule(_, entries) =>
                    entries.size shouldBe 1
                    val entry = entries.headOption
                      .getOrElse(fail("No charging schedule entry for ev1"))
                    entry.tickStart shouldBe currentTick

                    (
                      entry.chargingPower.to(KILOWATT).getValue.doubleValue(),
                      entry.tickStop
                    )
                } shouldBe expPowerAndTick1
                actualSchedules.getOrElse(ev2, None).map {
                  case ChargingSchedule(_, entries) =>
                    entries.size shouldBe 1
                    val entry = entries.headOption
                      .getOrElse(fail("No charging schedule entry for ev2"))
                    entry.tickStart shouldBe currentTick

                    (
                      entry.chargingPower.to(KILOWATT).getValue.doubleValue(),
                      entry.tickStop
                    )
                } shouldBe expPowerAndTick2

                actualTick shouldBe currentTick

                actualNextActivation shouldBe expNextActivation
                actualNextTick shouldBe expNextTick
            }
        }

      }
    }
  }

  /** Shortcut for Some type to make case tables more concise */
  def S[T](value: T): Some[T] = Some(value)

  /** Shortcut for None type to make case tables more concise */
  def N: None.type = None
}
