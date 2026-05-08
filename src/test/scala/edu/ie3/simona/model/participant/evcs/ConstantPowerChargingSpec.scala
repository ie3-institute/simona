/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.util.quantities.QuantityUtils.*
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.Kilowatts

import java.util.UUID

class ConstantPowerChargingSpec
    extends UnitSpec
    with EvcsInputTestData
    with TableDrivenPropertyChecks {

  "Calculating constant power for departure target" should {

    "not charge evs if they are above departure target" in {
      // 10 kWh capacity, 8 kWh target, 5 kW max power
      val ev = EvModelWrapper(
        ev1.copyWith(8.0.asKiloWattHour)
      )

      val actualSchedule = ConstantPowerCharging.determineChargingPowers(
        Seq(ev),
        1800L,
        MockEvcsChargingProperties,
      )

      actualSchedule shouldBe Map.empty
    }

    "work correctly with one ev" in {
      val offset = 1800L

      val cases = Table(
        ("stayingHours", "storedEnergy", "expectedPower"),
        // empty battery
        (1.0, 0.0, 5.0), // more than max power, limited
        (1.6, 0.0, 5.0), // exactly max power
        (2.0, 0.0, 4.0), // less than max power
        (100.0, 0.0, 0.08), // long stay: 100 hours
        // half full battery
        (0.5, 5.0, 5.0), // more than max power, limited
        (0.6, 5.0, 5.0), // exactly max power
        (0.75, 5.0, 4.0), // less than max power
        (50.0, 5.0, 0.06), // long stay: 50 hours
      )

      forAll(cases) { (stayingHours, storedEnergy, expectedPower) =>

        // 10 kWh capacity, 8 kWh target, 5 kW max power
        val ev = EvModelWrapper(
          ev1
            .copyWith(storedEnergy.asKiloWattHour)
            .copyWithDeparture(offset + (stayingHours * 3600L).toLong)
        )

        val chargingMap = ConstantPowerCharging.determineChargingPowers(
          Seq(ev),
          offset,
          MockEvcsChargingProperties,
        )

        chargingMap shouldBe Map(
          ev.uuid -> Kilowatts(expectedPower)
        )
      }

    }

    "work correctly with two evs" in {
      val offset = 3600L

      val cases = Table(
        ("stayingHours", "storedEnergy", "expectedPower"),
        // empty battery
        (1.0, 0.0, 5.0), // more than max power, limited
        (1.6, 0.0, 5.0), // exactly max power
        (2.0, 0.0, 4.0), // less than max power
        (100.0, 0.0, 0.08), // long stay: 100 hours
        // half full battery
        (0.5, 5.0, 5.0), // more than max power, limited
        (0.6, 5.0, 5.0), // exactly max power
        (0.75, 5.0, 4.0), // less than max power
        (50.0, 5.0, 0.06), // long stay: 50 hours
      )

      forAll(cases) { (stayingHours, storedEnergy, expectedPower) =>

        // 10 kWh capacity, 8 kWh target, 5 kW max power, staying one hour
        val givenEv = EvModelWrapper(
          ev1.copyWithDeparture(offset + 3600L)
        )

        // 10 kWh capacity, 8 kWh target, 5 kW max power
        val ev = EvModelWrapper(
          ev2
            .copyWith(storedEnergy.asKiloWattHour)
            .copyWithDeparture(offset + (stayingHours * 3600L).toLong)
        )

        val chargingMap = ConstantPowerCharging.determineChargingPowers(
          Seq(givenEv, ev),
          offset,
          MockEvcsChargingProperties,
        )

        chargingMap shouldBe Map(
          givenEv.uuid -> Kilowatts(3.0),
          ev.uuid -> Kilowatts(expectedPower),
        )
      }

    }
  }

}
