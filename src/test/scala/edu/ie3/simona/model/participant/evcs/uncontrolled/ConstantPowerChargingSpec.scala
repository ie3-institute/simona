/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.uncontrolled

import edu.ie3.simona.model.participant.evcs.{ChargingSchedule, EvModelWrapper}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.MockEvModel
import edu.ie3.simona.test.common.model.participant.EvcsTestData
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.Kilowatts

import java.util.UUID

class ConstantPowerChargingSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with EvcsTestData {

  "Calculating constant power charging schedules" should {
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
          3600L
        )
      )

      val actualSchedule = evcsModel.chargeWithConstantPower(
        1800L,
        Set(ev)
      )

      actualSchedule shouldBe Map(
        ev -> None
      )
    }

    "work correctly with one ev" in {
      val offset = 1800L

      val cases = Table(
        ("stayingTicks", "storedEnergy", "expectedPower"),
        // empty battery
        (3600L, 0.0, 5.0), // more than max power, limited
        (7200L, 0.0, 5.0), // exactly max power
        (14400L, 0.0, 2.5), // less than max power
        (360000L, 0.0, 0.1), // long stay: 100 hours
        // half full battery
        (1800L, 5.0, 5.0), // more than max power, limited
        (3600L, 5.0, 5.0), // exactly max power
        (7200L, 5.0, 2.5), // less than max power
        (180000L, 5.0, 0.1) // long stay: 100 hours
      )

      forAll(cases) { (stayingTicks, storedEnergy, expectedPower) =>
        val ev = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "Test EV",
            5.0.asKiloWatt, // using AC charging here
            10.0.asKiloWatt,
            10.0.asKiloWattHour,
            storedEnergy.asKiloWattHour,
            offset + stayingTicks
          )
        )

        val chargingMap = evcsModel.chargeWithConstantPower(
          offset,
          Set(ev)
        )

        chargingMap shouldBe Map(
          ev -> Some(
            ChargingSchedule(
              ev,
              Seq(
                ChargingSchedule.Entry(
                  offset,
                  offset + stayingTicks,
                  Kilowatts(expectedPower)
                )
              )
            )
          )
        )
      }

    }

    "work correctly with two evs" in {
      val offset = 3600L

      val cases = Table(
        ("stayingTicks", "storedEnergy", "expectedPower"),
        // empty battery
        (3600L, 0.0, 5.0), // more than max power, limited
        (7200L, 0.0, 5.0), // exactly max power
        (14400L, 0.0, 2.5), // less than max power
        (360000L, 0.0, 0.1), // long stay: 100 hours
        // half full battery
        (1800L, 5.0, 5.0), // more than max power, limited
        (3600L, 5.0, 5.0), // exactly max power
        (7200L, 5.0, 2.5), // less than max power
        (180000L, 5.0, 0.1) // long stay: 100 hours
      )

      forAll(cases) { (stayingTicks, storedEnergy, expectedPower) =>
        val givenEv = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "First EV",
            5.0.asKiloWatt, // using AC charging here
            10.0.asKiloWatt,
            10.0.asKiloWattHour,
            5.0.asKiloWattHour,
            offset + 3600L
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
            offset + stayingTicks
          )
        )

        val chargingMap = evcsModel.chargeWithConstantPower(
          offset,
          Set(givenEv, ev)
        )

        chargingMap shouldBe Map(
          givenEv -> Some(
            ChargingSchedule(
              givenEv,
              Seq(
                ChargingSchedule.Entry(
                  offset,
                  offset + 3600L,
                  Kilowatts(5.0)
                )
              )
            )
          ),
          ev -> Some(
            ChargingSchedule(
              ev,
              Seq(
                ChargingSchedule.Entry(
                  offset,
                  offset + stayingTicks,
                  Kilowatts(expectedPower)
                )
              )
            )
          )
        )
      }

    }
  }

}
