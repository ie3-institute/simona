/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.simona.model.participant.FlexChangeIndicator
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsRelevantData,
  EvcsState,
  ScheduleEntry,
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.common.model.MockEvModel
import edu.ie3.simona.test.helper.TableDrivenHelper
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.Each
import squants.energy.{KilowattHours, Kilowatts}

import java.util.UUID
import scala.collection.immutable.SortedSet

class EvcsModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with TableDrivenHelper
    with EvcsInputTestData {

  private val simulationStart = evcsStandardModel.simulationStartDate

  private implicit val energyTolerance: squants.Energy = KilowattHours(1e-10)
  private implicit val powerTolerance: squants.Power = Kilowatts(1e-10)

  "An EVCS model" should {

    "calculate new schedules correctly" when {

      "configured as a charging hub" in {
        val evcsModel = evcsStandardModel.copy(
          strategy = ChargingStrategy.CONSTANT_POWER,
          locationType = EvcsLocationType.CHARGING_HUB_TOWN,
        )

        val evModel = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "Mock EV",
            10.0.asKiloWatt, // AC is relevant,
            20.0.asKiloWatt, // DC is not
            20.0.asKiloWattHour,
            5.0.asKiloWattHour,
            10800L,
          )
        )

        val actualSchedule = evcsModel.calculateNewScheduling(
          EvcsRelevantData(
            3600L,
            Seq.empty,
          ),
          Seq(evModel),
        )

        actualSchedule shouldBe Map(
          evModel.uuid ->
            // ending early at 9000 because of max power charging
            SortedSet(ScheduleEntry(3600L, 9000L, Kilowatts(10.0)))
        )
      }

      "configured as a home cs with constant power strategy" in {
        val evcsModel = evcsStandardModel.copy(
          strategy = ChargingStrategy.CONSTANT_POWER,
          locationType = EvcsLocationType.HOME,
        )

        val evModel = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "Mock EV",
            10.0.asKiloWatt, // AC is relevant,
            20.0.asKiloWatt, // DC is not
            20.0.asKiloWattHour,
            15.0.asKiloWattHour,
            10800L,
          )
        )

        val actualSchedule = evcsModel.calculateNewScheduling(
          EvcsRelevantData(
            3600L,
            Seq.empty,
          ),
          Seq(evModel),
        )

        actualSchedule shouldBe Map(
          evModel.uuid ->
            // using 2.5 kW with constant power charging
            SortedSet(ScheduleEntry(3600L, 10800L, Kilowatts(2.5)))
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
            "expectedStored",
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
          (2.5, 0L, 7200L, 2700L, 5.0, 3.75),
        )

        forAll(cases) {
          (
              storedEnergy,
              chargeStart,
              chargeEnd,
              lastCalcTick,
              power,
              expectedStored,
          ) =>
            val ev = EvModelWrapper(
              new MockEvModel(
                UUID.randomUUID(),
                "TestEv1",
                5.0.asKiloWatt, // using AC charging here
                10.0.asKiloWatt,
                10.0.asKiloWattHour,
                storedEnergy.asKiloWattHour,
                7200L, // is ignored here
              )
            )

            val entry =
              SortedSet(ScheduleEntry(chargeStart, chargeEnd, Kilowatts(power)))

            val state = EvcsState(
              Seq(ev),
              Map(ev.uuid -> entry),
              lastCalcTick,
            )

            val actualOutput = evcsModel.applySchedule(
              state,
              currentTick,
            )

            actualOutput should have size 1
            val actualEv =
              actualOutput.headOption.getOrElse(
                fail("No charging schedule provided.")
              )

            actualEv.uuid shouldBe ev.uuid
            actualEv.id shouldBe ev.id
            actualEv.sRatedAc shouldBe ev.sRatedAc
            actualEv.pRatedDc shouldBe ev.pRatedDc
            actualEv.eStorage shouldBe ev.eStorage
            actualEv.storedEnergy should approximate(
              KilowattHours(expectedStored)
            )
            actualEv.departureTick shouldBe ev.departureTick

        }

      }
    }

    "calculate results correctly" when {

      "one EV is parked and charging" in {

        val ev = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "TestEv1",
            5.0.asKiloWatt, // using AC charging here
            10.0.asKiloWatt,
            10.0.asKiloWattHour,
            0d.asKiloWattHour,
            10800L,
          )
        )

        val schedule = SortedSet(
          ScheduleEntry(3600L, 5400L, Kilowatts(2d)),
          ScheduleEntry(7200L, 9000L, Kilowatts(4d)),
        )

        // tick, p in kW
        val generalEvResults =
          IndexedSeq(
            (0L, 0d),
            (3600L, 2d),
            (5400L, 0d),
            (7200L, 4d),
            (9000L, 0d),
          )

        val cases = Table(
          (
            "lastTick",
            "currentTick",
            "firstResultIndex",
            "lastResultIndex",
          ),
          (1800L, 10800L, 0, 5),
          (3600L, 10000L, 1, 5),
          (2700L, 9000L, 0, 4),
          (3600L, 9000L, 1, 4),
          (3660L, 9000L, 1, 4),
          (5400L, 9001L, 2, 5),
          (5400L, 8999L, 2, 4),
          (8999L, 9000L, 3, 4),
          (8999L, 9060L, 3, 5),
        )

        forAll(cases) {
          (
              lastTick,
              currentTick,
              firstResultIndex,
              lastResultIndex,
          ) =>
            val lastState = EvcsState(
              Seq(ev),
              Map(ev.uuid -> schedule),
              lastTick,
            )

            val (actualEvResults, actualEvcsResults) =
              evcsStandardModel.createResults(
                lastState,
                currentTick,
                Each(1d),
              )

            val (_, firstPower) = generalEvResults(firstResultIndex)

            val expectedEvResults = generalEvResults
              .slice(firstResultIndex + 1, lastResultIndex)
              .prepended(lastTick, firstPower)

            actualEvResults should have size expectedEvResults.size
            actualEvResults.zip(expectedEvResults).foreach {
              case (actual, (startTick, p)) =>
                actual.getTime shouldBe startTick.toDateTime(simulationStart)
                actual.getInputModel shouldBe ev.uuid
                actual.getP should beEquivalentTo(p.asKiloWatt)
                actual.getQ should beEquivalentTo(0d.asKiloVar)
            }

            actualEvcsResults should have size expectedEvResults.size
            actualEvcsResults.zip(expectedEvResults).foreach {
              case (actual, (startTick, p)) =>
                actual.getTime shouldBe startTick.toDateTime(simulationStart)
                actual.getInputModel shouldBe evcsStandardModel.getUuid
                actual.getP should beEquivalentTo(p.asKiloWatt)
                actual.getQ should beEquivalentTo(0d.asKiloVar)
            }
        }

      }

      "two EVs are parked and charging" in {

        val ev1 = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "TestEv1",
            5.0.asKiloWatt, // using AC charging here
            10.0.asKiloWatt,
            10.0.asKiloWattHour,
            0d.asKiloWattHour,
            18000L,
          )
        )

        val ev2 = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "TestEv2",
            5.0.asKiloWatt, // using AC charging here
            10.0.asKiloWatt,
            10.0.asKiloWattHour,
            0d.asKiloWattHour,
            18000L,
          )
        )

        val schedule1 =
          SortedSet(
            ScheduleEntry(3600L, 7200L, Kilowatts(2d)),
            ScheduleEntry(9000L, 14400L, Kilowatts(3d)),
          )

        val schedule2 = SortedSet(
          ScheduleEntry(5400L, 9000L, Kilowatts(2d))
        )

        val lastTick = 1800L
        val currentTick = 10800L

        val lastState = EvcsState(
          Seq(ev1, ev2),
          Map(ev1.uuid -> schedule1, ev2.uuid -> schedule2),
          lastTick,
        )

        val (actualEvResults, actualEvcsResults) =
          evcsStandardModel.createResults(
            lastState,
            currentTick,
            Each(1d),
          )

        // tick, p in kW, soc in %
        val expectedEv1Results =
          Seq(
            (1800L, 0d, 0d),
            (3600L, 2d, 0d),
            (7200L, 0d, 20d),
            (9000L, 3d, 20d),
          )

        // tick, p in kW, soc in %
        val expectedEv2Results =
          Seq(
            (1800L, 0d, 0d),
            (5400L, 2d, 0d),
            (9000L, 0d, 20d),
          )

        // tick, p in kW
        val expectedEvcsResults =
          Seq(
            (1800L, 0d),
            (3600L, 2d),
            (5400L, 4d),
            (7200L, 2d),
            (9000L, 3d),
          )

        actualEvResults should have size expectedEv1Results.size + expectedEv2Results.size

        val actualEv1Results =
          actualEvResults.filter(_.getInputModel == ev1.uuid)
        actualEv1Results should have size expectedEv1Results.size
        actualEv1Results.zip(expectedEv1Results).foreach {
          case (actual, (startTick, p, soc)) =>
            actual.getTime shouldBe startTick.toDateTime(simulationStart)
            actual.getP should beEquivalentTo(p.asKiloWatt)
            actual.getQ should beEquivalentTo(0d.asKiloVar)
            actual.getSoc should beEquivalentTo(soc.asPercent)
        }

        val actualEv2Results =
          actualEvResults.filter(_.getInputModel == ev2.uuid)
        actualEv2Results should have size expectedEv2Results.size
        actualEv2Results.zip(expectedEv2Results).foreach {
          case (actual, (startTick, p, soc)) =>
            actual.getTime shouldBe startTick.toDateTime(simulationStart)
            actual.getP should beEquivalentTo(p.asKiloWatt)
            actual.getQ should beEquivalentTo(0d.asKiloVar)
            actual.getSoc should beEquivalentTo(soc.asPercent)
        }

        actualEvcsResults should have size expectedEvcsResults.size
        actualEvcsResults.zip(expectedEvcsResults).foreach {
          case (actual, (startTick, p)) =>
            actual.getTime shouldBe startTick.toDateTime(simulationStart)
            actual.getInputModel shouldBe evcsStandardModel.getUuid
            actual.getP should beEquivalentTo(p.asKiloWatt)
            actual.getQ should beEquivalentTo(0d.asKiloVar)
        }
      }

      "EV is departing at current tick" in {

        val ev = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "TestEv",
            5.0.asKiloWatt, // using AC charging here
            10.0.asKiloWatt,
            10.0.asKiloWattHour,
            0d.asKiloWattHour,
            7200L, // equals the current tick
          )
        )

        val schedule = SortedSet(
          ScheduleEntry(3600L, 7200L, Kilowatts(2d))
        )

        val lastTick = 1800L
        val currentTick = 7200L

        val lastState = EvcsState(
          Seq(ev),
          Map(ev.uuid -> schedule),
          lastTick,
        )

        val (actualEvResults, actualEvcsResults) =
          evcsStandardModel.createResults(
            lastState,
            currentTick,
            Each(1d),
          )

        // tick, p in kW, soc in %
        val expectedEvResults =
          Seq(
            (1800L, 0d, 0d),
            (3600L, 2d, 0d),
            // this result normally does not appear
            // if EV does not depart at current tick
            (7200L, 0d, 20d),
          )

        // tick, p in kW
        val expectedEvcsResults =
          Seq(
            (1800L, 0d),
            (3600L, 2d),
          )

        actualEvResults should have size expectedEvResults.size

        actualEvResults should have size expectedEvResults.size
        actualEvResults.zip(expectedEvResults).foreach {
          case (actual, (startTick, p, soc)) =>
            actual.getTime shouldBe startTick.toDateTime(simulationStart)
            actual.getP should beEquivalentTo(p.asKiloWatt)
            actual.getQ should beEquivalentTo(0d.asKiloVar)
            actual.getSoc should beEquivalentTo(soc.asPercent)
        }

        actualEvcsResults should have size expectedEvcsResults.size
        actualEvcsResults.zip(expectedEvcsResults).foreach {
          case (actual, (startTick, p)) =>
            actual.getTime shouldBe startTick.toDateTime(simulationStart)
            actual.getInputModel shouldBe evcsStandardModel.getUuid
            actual.getP should beEquivalentTo(p.asKiloWatt)
            actual.getQ should beEquivalentTo(0d.asKiloVar)
        }
      }

    }

    "calculate flex options correctly" when {

      "charging with constant power and allowing v2g" in {
        val evcsModel =
          evcsStandardModel.copy(strategy = ChargingStrategy.CONSTANT_POWER)

        val currentTick = 7200L

        val data = EvcsRelevantData(
          currentTick,
          Seq.empty,
        )

        val cases = Table(
          (
            "lastStored1",
            "lastStored2",
            "lastPower2",
            "expectedPRef",
            "expectedPMin",
            "expectedPMax",
          ),

          /* 1: empty */
          // 2: empty
          (0.0, 0.0, 0.0, 15.0, 15.0, 15.0),
          // 2: at lower margin
          (0.0, 3.0, 0.0, 15.0, 10.0, 15.0),
          // 2: mid-way full (charged to 7.5 kWh), forced charging
          (0.0, 0.0, 5.0, 13.75, 10.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh), forced charging
          (0.0, 7.5, 0.0, 13.75, 10.0, 15.0),
          // 2: almost full (12.5 kWh), forced charging
          (0.0, 5.0, 5.0, 11.25, 10.0, 15.0),
          // 2: full (set), forced charging
          (0.0, 15.0, 0.0, 10.0, 10.0, 10.0),

          /* 1: at lower margin (set to 2 kWh) */
          // 2: empty
          (2.0, 0.0, 0.0, 13.0, 5.0, 15.0),
          // 2: at lower margin
          (2.0, 3.0, 0.0, 13.0, 0.0, 15.0),
          // 2: mid-way full (charged to 7.5 kWh)
          (2.0, 0.0, 5.0, 11.75, -5.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh)
          (2.0, 7.5, 0.0, 11.75, -5.0, 15.0),
          // 2: almost full (12.5 kWh)
          (2.0, 5.0, 5.0, 9.25, -5.0, 15.0),
          // 2: full (set)
          (2.0, 15.0, 0.0, 8.0, -5.0, 10.0),

          /* 1: mid-way full (set to 5 kWh) */
          // 2: empty, forced charging
          (5.0, 0.0, 0.0, 10.0, 5.0, 15.0),
          // 2: mid-way full (charged to 7.5 kWh)
          (5.0, 0.0, 5.0, 8.75, -15.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh)
          (5.0, 7.5, 0.0, 8.75, -15.0, 15.0),
          // 2: almost full (12.5 kWh)
          (5.0, 5.0, 5.0, 6.25, -15.0, 15.0),
          // 2: full (set)
          (5.0, 15.0, 0.0, 5.0, -15.0, 10.0),

          /* 1: full (set to 10 kWh) */
          // 2: empty, forced charging
          (10.0, 0.0, 0.0, 5.0, 5.0, 5.0),
          // 2: mid-way full (charged to 7.5 kWh)
          (10.0, 0.0, 5.0, 3.75, -15.0, 5.0),
          // 2: mid-way full (set to 7.5 kWh)
          (10.0, 7.5, 0.0, 3.75, -15.0, 5.0),
          // 2: almost full (12.5 kWh)
          (10.0, 5.0, 5.0, 1.25, -15.0, 5.0),
          // 2: full (set)
          (10.0, 15.0, 0.0, 0.0, -15.0, 0.0),
        )

        forAll(cases) {
          (
              lastStored1,
              lastStored2,
              lastPower2,
              expectedPRef,
              expectedPMin,
              expectedPMax,
          ) =>
            // stays one more hour
            val ev1 = EvModelWrapper(
              new MockEvModel(
                UUID.randomUUID(),
                "Mock EV 1",
                10.0.asKiloWatt, // AC is relevant,
                20.0.asKiloWatt, // DC is not
                10.0.asKiloWattHour,
                lastStored1.asKiloWattHour,
                10800L,
              )
            )

            // has not been charging before
            val schedule1 = SortedSet(
              ScheduleEntry(3600L, 7200L, Kilowatts(0d))
            )

            // stays two more hours
            val ev2 = EvModelWrapper(
              new MockEvModel(
                UUID.randomUUID(),
                "Mock EV 2",
                5.0.asKiloWatt, // AC is relevant,
                10.0.asKiloWatt, // DC is not
                15.0.asKiloWattHour,
                lastStored2.asKiloWattHour,
                14400L,
              )
            )

            // has been charging for 1.5 hours with given power
            val schedule2 = SortedSet(
              ScheduleEntry(0L, 5400L, Kilowatts(lastPower2))
            )

            evcsModel.determineFlexOptions(
              data,
              EvcsState(
                Seq(ev1, ev2),
                Map(ev1.uuid -> schedule1, ev2.uuid -> schedule2),
                0L,
              ),
            ) match {
              case ProvideMinMaxFlexOptions(
                    modelUuid,
                    refPower,
                    minPower,
                    maxPower,
                  ) =>
                modelUuid shouldBe evcsModel.getUuid
                refPower should approximate(Kilowatts(expectedPRef))
                minPower should approximate(Kilowatts(expectedPMin))
                maxPower should approximate(Kilowatts(expectedPMax))
            }
        }

      }

      "charging with maximum power and allowing v2g" in {
        val evcsModel =
          evcsStandardModel.copy(strategy = ChargingStrategy.MAX_POWER)

        val currentTick = 7200L

        val data = EvcsRelevantData(
          currentTick,
          Seq.empty,
        )

        val cases = Table(
          (
            "lastStored1",
            "lastStored2",
            "lastPower2",
            "expectedPRef",
            "expectedPMin",
            "expectedPMax",
          ),

          /* 1: empty */
          // 2: empty
          (0.0, 0.0, 0.0, 15.0, 15.0, 15.0),
          // 2: at lower margin
          (0.0, 3.0, 0.0, 15.0, 10.0, 15.0),
          // 2: mid-way full (charged to 7.5 kWh), forced charging
          (0.0, 0.0, 5.0, 15.0, 10.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh), forced charging
          (0.0, 7.5, 0.0, 15.0, 10.0, 15.0),
          // 2: almost full (charged to 12.5 kWh), forced charging
          (0.0, 5.0, 5.0, 15.0, 10.0, 15.0),
          // 2: full (set to 15 kWh)
          (0.0, 15.0, 0.0, 10.0, 10.0, 10.0),

          /* 1: at lower margin (set to 2 kWh) */
          // 2: empty
          (2.0, 0.0, 0.0, 15.0, 5.0, 15.0),
          // 2: at lower margin
          (2.0, 3.0, 0.0, 15.0, 0.0, 15.0),
          // 2: mid-way full (charged to 7.5 kWh)
          (2.0, 0.0, 5.0, 15.0, -5.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh)
          (2.0, 7.5, 0.0, 15.0, -5.0, 15.0),
          // 2: almost full (charged to 12.5 kWh)
          (2.0, 5.0, 5.0, 15.0, -5.0, 15.0),
          // 2: full (set to 15 kWh)
          (2.0, 15.0, 0.0, 10.0, -5.0, 10.0),

          /* 1: mid-way full (set to 5 kWh) */
          // 2: empty, forced charging
          (5.0, 0.0, 0.0, 15.0, 5.0, 15.0),
          // 2: mid-way full (charged to 7.5 kWh)
          (5.0, 0.0, 5.0, 15.0, -15.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh)
          (5.0, 7.5, 0.0, 15.0, -15.0, 15.0),
          // 2: almost full (charged to 12.5 kWh)
          (5.0, 5.0, 5.0, 15.0, -15.0, 15.0),
          // 2: full (set to 15 kWh)
          (5.0, 15.0, 0.0, 10.0, -15.0, 10.0),

          /* 1: full (set to 10 kWh) */
          // 2: empty, forced charging
          (10.0, 0.0, 0.0, 5.0, 5.0, 5.0),
          // 2: mid-way full (charged to 7.5 kWh)
          (10.0, 0.0, 5.0, 5.0, -15.0, 5.0),
          // 2: mid-way full (set to 7.5 kWh)
          (10.0, 7.5, 0.0, 5.0, -15.0, 5.0),
          // 2: almost full (charged to 12.5 kWh)
          (10.0, 5.0, 5.0, 5.0, -15.0, 5.0),
          // 2: full (set to 15 kWh)
          (10.0, 15.0, 0.0, 0.0, -15.0, 0.0),
        )

        forAll(cases) {
          (
              lastStored1,
              lastStored2,
              lastPower2,
              expectedPRef,
              expectedPMin,
              expectedPMax,
          ) =>
            val ev1 = EvModelWrapper(
              new MockEvModel(
                UUID.randomUUID(),
                "Mock EV 1",
                10.0.asKiloWatt, // AC is relevant,
                20.0.asKiloWatt, // DC is not
                10.0.asKiloWattHour,
                lastStored1.asKiloWattHour,
                10800L,
              )
            )

            val schedule1 = SortedSet(
              ScheduleEntry(3600L, 7200L, Kilowatts(0d))
            )

            val ev2 = EvModelWrapper(
              new MockEvModel(
                UUID.randomUUID(),
                "Mock EV 2",
                5.0.asKiloWatt, // AC is relevant,
                10.0.asKiloWatt, // DC is not
                15.0.asKiloWattHour,
                lastStored2.asKiloWattHour,
                10800L,
              )
            )

            val schedule2 = SortedSet(
              ScheduleEntry(0L, 5400L, Kilowatts(lastPower2))
            )

            evcsModel.determineFlexOptions(
              data,
              EvcsState(
                Seq(ev1, ev2),
                Map(ev1.uuid -> schedule1, ev2.uuid -> schedule2),
                0L,
              ),
            ) match {
              case ProvideMinMaxFlexOptions(
                    modelUuid,
                    refPower,
                    minPower,
                    maxPower,
                  ) =>
                modelUuid shouldBe evcsModel.getUuid
                refPower should approximate(Kilowatts(expectedPRef))
                minPower should approximate(Kilowatts(expectedPMin))
                maxPower should approximate(Kilowatts(expectedPMax))
            }
        }

      }

      "disallowing v2g" in {
        val evcsModel = evcsStandardModel.copy(
          vehicle2grid = false,
          strategy = ChargingStrategy.CONSTANT_POWER,
        )

        val currentTick = 7200L

        val data = EvcsRelevantData(
          currentTick,
          Seq.empty,
        )

        val ev1 = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "Mock EV 1",
            10.0.asKiloWatt, // AC is relevant,
            20.0.asKiloWatt, // DC is not
            10.0.asKiloWattHour,
            0.0.asKiloWattHour,
            10800L,
          )
        )

        val schedule1 = SortedSet(
          ScheduleEntry(3600L, 7200L, Kilowatts(5.0))
        )

        evcsModel.determineFlexOptions(
          data,
          EvcsState(
            Seq(ev1),
            Map(ev1.uuid -> schedule1),
            0L,
          ),
        ) match {
          case ProvideMinMaxFlexOptions(
                modelUuid,
                refPower,
                minPower,
                maxPower,
              ) =>
            modelUuid shouldBe evcsModel.getUuid
            refPower should approximate(Kilowatts(5.0)) // one hour left
            minPower should approximate(Kilowatts(0d)) // no v2g allowed!
            maxPower should approximate(ev1.sRatedAc.toPower(1.0))
        }

      }

    }

    "handle flexibility correctly" when {
      val evcsModel =
        evcsStandardModel.copy(strategy = ChargingStrategy.CONSTANT_POWER)

      "handle controlled power change for two evs correctly" in {
        val currentTick = 3600L

        val data = EvcsRelevantData(
          currentTick,
          Seq.empty,
        )

        val cases = Table(
          (
            "stored1",
            "stored2",
            "setPower",
            "expPowerAndTick1",
            "expPowerAndTick2",
            "expNextActivation",
            "expNextTick",
          ),

          /* setPower is 0 kWh */
          (0.0, 0.0, 0.0, N, N, false, N),
          (10.0, 5.0, 0.0, N, N, false, N),
          (5.0, 15.0, 0.0, N, N, false, N),
          (10.0, 15.0, 0.0, N, N, false, N),

          /* setPower is positive (charging) */
          (0.0, 0.0, 4.0, S(2.0, 7200L), S(2.0, 9000L), true, S(7200L)),
          (5.0, 0.0, 4.0, N, S(4.0, 6300L), true, S(6300L)),
          (0.0, 7.5, 4.0, S(4.0, 5400L), N, true, S(5400L)),
          (9.0, 0.0, 4.0, N, S(4.0, 6300L), true, S(6300L)),
          (5.0, 14.0, 4.0, S(2.0, 7200L), S(2.0, 5400L), false, S(5400L)),
          (9.0, 14.0, 4.0, S(2.0, 5400L), S(2.0, 5400L), false, S(5400L)),
          (10.0, 14.0, 4.0, N, S(4.0, 4500L), false, S(4500L)),
          (6.0, 15.0, 4.0, S(4.0, 7200L), N, false, S(7200L)),

          /* setPower is set to > (ev2 * 2) (charging) */
          (0.0, 0.0, 13.0, S(8.0, 4500L), S(5.0, 5760L), true, S(4500L)),
          (7.0, 0.0, 11.0, S(6.0, 5400L), S(5.0, 5760L), true, S(5400L)),
          (0.0, 5.0, 15.0, S(10.0, 4320L), S(5.0, 10800L), true, S(4320L)),
          (0.0, 12.5, 15.0, S(10.0, 4320L), S(5.0, 5400L), true, S(4320L)),
          (0.0, 0.0, 15.0, S(10.0, 4320L), S(5.0, 5760L), true, S(4320L)),
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
          (7.0, 10.5, -15.0, S(-10.0, 5400L), S(-5.0, 9000L), false, S(5400L)),
        )

        forAll(cases) {
          (
              stored1: Double,
              stored2: Double,
              setPower: Double,
              expPowerAndTick1: Option[(Double, Long)],
              expPowerAndTick2: Option[(Double, Long)],
              expNextActivation: Boolean,
              expNextTick: Option[Long],
          ) =>
            val ev1 = EvModelWrapper(
              new MockEvModel(
                UUID.randomUUID(),
                "Mock EV 1",
                10.0.asKiloWatt, // AC is relevant,
                20.0.asKiloWatt, // DC is not
                10.0.asKiloWattHour,
                stored1.asKiloWattHour,
                7200L,
              )
            )

            val ev2 = EvModelWrapper(
              new MockEvModel(
                UUID.randomUUID(),
                "Mock EV 2",
                5.0.asKiloWatt, // AC is relevant,
                10.0.asKiloWatt, // DC is not
                15.0.asKiloWattHour,
                stored2.asKiloWattHour,
                10800L,
              )
            )

            evcsModel.handleControlledPowerChange(
              data,
              EvcsState(
                Seq(ev1, ev2),
                Map.empty,
                0L,
              ),
              Kilowatts(setPower),
            ) match {
              case (
                    EvcsState(actualEvs, actualSchedules, actualTick),
                    FlexChangeIndicator(actualNextActivation, actualNextTick),
                  ) =>
                // evs have not changed here since no schedules were given as input
                actualEvs should have size 2
                actualEvs should contain allOf (ev1, ev2)

                actualSchedules.get(ev1.uuid).map { entries =>
                  entries.size shouldBe 1
                  val entry = entries.headOption
                    .getOrElse(fail("No charging schedule entry for ev1"))
                  entry.tickStart shouldBe currentTick

                  (
                    entry.chargingPower.toKilowatts,
                    entry.tickStop,
                  )
                } shouldBe expPowerAndTick1
                actualSchedules.get(ev2.uuid).map { entries =>
                  entries.size shouldBe 1
                  val entry = entries.headOption
                    .getOrElse(fail("No charging schedule entry for ev2"))
                  entry.tickStart shouldBe currentTick

                  (
                    entry.chargingPower.toKilowatts,
                    entry.tickStop,
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

}
