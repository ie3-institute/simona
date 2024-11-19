/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.uncontrolled

import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.model.participant.evcs.EvcsModel.ScheduleEntry
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.common.model.MockEvModel
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.Kilowatts

import java.util.UUID
import scala.collection.immutable.SortedSet

class MaximumPowerChargingSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with EvcsInputTestData {

  "Calculating maximum power charging schedules" should {
    val evcsModel = evcsStandardModel

    "not charge evs if they are fully charged" in {
      val ev = EvModelWrapper(
        new MockEvModel(
          UUID.randomUUID(),
          "Test EV",
          5.0.asKiloWatt,
          10.0.asKiloWatt,
          20.0.asKiloWattHour,
          20.0.asKiloWattHour,
          3600,
        )
      )

      val actualSchedule = evcsModel.chargeWithMaximumPower(
        1800L,
        Seq(ev),
      )

      actualSchedule shouldBe Map.empty
    }

    "work correctly with one ev" in {
      val offset = 1800L

      val cases = Table(
        ("stayingTicks", "storedEnergy", "expectedDuration"),
        // empty battery
        (3600L, 0.0, 3600L), // stay shorter than full
        (7200L, 0.0, 7200L), // exactly full
        (14400L, 0.0, 7200L), // full before end of stay
        // half full battery
        (1800L, 5.0, 1800L), // stay shorter than full
        (3600L, 5.0, 3600L), // exactly full
        (14400L, 5.0, 3600L), // full before end of stay
      )

      forAll(cases) { (stayingTicks, storedEnergy, expectedDuration) =>
        val ev = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "Test EV",
            5.0.asKiloWatt, // using AC charging here
            10.0.asKiloWatt,
            10.0.asKiloWattHour,
            storedEnergy.asKiloWattHour,
            offset + stayingTicks,
          )
        )

        val chargingMap = evcsModel.chargeWithMaximumPower(
          offset,
          Seq(ev),
        )

        chargingMap shouldBe Map(
          ev.uuid -> SortedSet(
            ScheduleEntry(
              offset,
              offset + expectedDuration,
              ev.pRatedAc,
            )
          )
        )
      }

    }

    "work correctly with two evs" in {
      val offset = 3600L

      val cases = Table(
        ("stayingTicks", "storedEnergy", "expectedDuration"),
        // empty battery
        (3600L, 0.0, 3600L), // stay shorter than full
        (7200L, 0.0, 7200L), // exactly full
        (14400L, 0.0, 7200L), // full before end of stay
        // half full battery
        (1800L, 5.0, 1800L), // stay shorter than full
        (3600L, 5.0, 3600L), // exactly full
        (14400L, 5.0, 3600L), // full before end of stay
      )

      forAll(cases) { (stayingTicks, storedEnergy, expectedDuration) =>
        val givenEv = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "First EV",
            5.0.asKiloWatt, // using AC charging here
            10.0.asKiloWatt,
            10.0.asKiloWattHour,
            5.0.asKiloWattHour,
            offset + 3600L,
          )
        )

        val ev = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "Test EV",
            5.0.asKiloWatt, // using AC charging here
            10.0.asKiloWatt,
            10.0.asKiloWattHour,
            storedEnergy.asKiloWattHour,
            offset + stayingTicks,
          )
        )

        val chargingMap = evcsModel.chargeWithMaximumPower(
          offset,
          Seq(givenEv, ev),
        )

        chargingMap shouldBe Map(
          givenEv.uuid -> SortedSet(
            ScheduleEntry(
              offset,
              offset + 3600L,
              Kilowatts(5.0),
            )
          ),
          ev.uuid -> SortedSet(
            ScheduleEntry(
              offset,
              offset + expectedDuration,
              Kilowatts(5.0),
            )
          ),
        )
      }

    }
  }
}
