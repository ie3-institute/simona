/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.MockEvModel
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.Kilowatts

import java.util.UUID

class MaximumPowerChargingSpec extends UnitSpec with TableDrivenPropertyChecks {

  "Calculating maximum power charging schedules" should {

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

      val actualSchedule = MaximumPowerCharging.determineChargingPowers(
        Seq(ev),
        1800L,
        MockEvcsChargingProperties,
      )

      actualSchedule shouldBe Map.empty
    }

    "work correctly with one ev" in {
      val offset = 1800L

      val cases = Table(
        ("stayingTicks", "storedEnergy"),
        // empty battery
        (3600L, 0.0), // stay shorter than full
        (7200L, 0.0), // exactly full
        (14400L, 0.0), // full before end of stay
        // half full battery
        (1800L, 5.0), // stay shorter than full
        (3600L, 5.0), // exactly full
        (14400L, 5.0), // full before end of stay
      )

      forAll(cases) { (stayingTicks, storedEnergy) =>
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

        val chargingMap = MaximumPowerCharging.determineChargingPowers(
          Seq(ev),
          offset,
          MockEvcsChargingProperties,
        )

        chargingMap shouldBe Map(
          ev.uuid -> ev.pRatedAc
        )
      }

    }

    "work correctly with two evs" in {
      val offset = 3600L

      val cases = Table(
        ("stayingTicks", "storedEnergy"),
        // empty battery
        (3600L, 0.0), // stay shorter than full
        (7200L, 0.0), // exactly full
        (14400L, 0.0), // full before end of stay
        // half full battery
        (1800L, 5.0), // stay shorter than full
        (3600L, 5.0), // exactly full
        (14400L, 5.0), // full before end of stay
      )

      forAll(cases) { (stayingTicks, storedEnergy) =>
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

        val chargingMap = MaximumPowerCharging.determineChargingPowers(
          Seq(givenEv, ev),
          offset,
          MockEvcsChargingProperties,
        )

        chargingMap shouldBe Map(
          givenEv.uuid -> Kilowatts(5.0),
          ev.uuid -> Kilowatts(5.0),
        )
      }

    }
  }
}
