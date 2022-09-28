/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.simona.model.participant.evcs.ChargingSchedule.Entry
import edu.ie3.simona.model.participant.evcs.EvcsModel.EvcsRelevantData
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.MockEvModel
import edu.ie3.simona.test.common.model.participant.EvcsTestData
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.scalatest.prop.TableDrivenPropertyChecks
import tech.units.indriya.unit.Units.PERCENT

import java.util.UUID
import javax.measure.quantity.Dimensionless

class EvcsModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with EvcsTestData {

  private val simulationStart =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")

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
          3600L,
          simulationStart,
          EvcsRelevantData(
            Set(evModel),
            Map.empty, // should be irrelevant
            Map.empty // should be irrelevant
          )
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
          3600L,
          simulationStart,
          EvcsRelevantData(
            Set(evModel),
            Map.empty, // should be irrelevant
            Map.empty // should be irrelevant
          )
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

            val data = EvcsRelevantData(
              Set(ev),
              Map(ev -> Some(schedule)),
              Map.empty
            )

            val actualOutput = evcsModel.applySchedule(
              currentTick,
              simulationStart,
              lastCalcTick,
              data
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
  }
}
