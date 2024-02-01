/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.simona.model.thermal.ThermalGrid.ThermalEnergyDemand
import edu.ie3.simona.test.common.UnitSpec
import squants.energy.{MegawattHours, WattHours, Watts}
import squants.thermal.Celsius
import squants.{Energy, Power, Temperature}

class ThermalGridSpec extends UnitSpec {

  implicit val tempTolerance: Temperature = Celsius(1e-3)
  implicit val powerTolerance: Power = Watts(1e-3)
  implicit val energyTolerance: Energy = WattHours(1e-3)

  "Testing the thermal energy demand" when {
    "instantiating it from given values" should {
      "correct non-sensible input" in {
        val possible = MegawattHours(40d)
        val required = MegawattHours(42d)

        val energyDemand = ThermalEnergyDemand(required, possible)

        equalWithTolerance(energyDemand.required, possible)
        equalWithTolerance(energyDemand.possible, possible)
      }

      "set the correct values, if they are sensible" in {
        val possible = MegawattHours(45d)
        val required = MegawattHours(42d)

        val energyDemand = ThermalEnergyDemand(required, possible)

        equalWithTolerance(energyDemand.required, required)
        equalWithTolerance(energyDemand.possible, possible)
      }
    }

    "defining no demand" should {
      "actually have no demand" in {
        val energyDemand = ThermalEnergyDemand.noDemand

        equalWithTolerance(energyDemand.required, MegawattHours(0d))
        equalWithTolerance(energyDemand.possible, MegawattHours(0d))
      }
    }

    "checking for required and additional demand" should {
      "return proper information, if no required but additional demand is apparent" in {
        val required = MegawattHours(0d)
        val possible = MegawattHours(45d)

        val energyDemand = ThermalEnergyDemand(required, possible)
        energyDemand.hasRequiredDemand shouldBe false
        energyDemand.hasAdditionalDemand shouldBe true
      }

      "return proper information, if required but no additional demand is apparent" in {
        val required = MegawattHours(45d)
        val possible = MegawattHours(45d)

        val energyDemand = ThermalEnergyDemand(required, possible)
        energyDemand.hasRequiredDemand shouldBe true
        energyDemand.hasAdditionalDemand shouldBe false
      }

      "return proper information, if required and additional demand is apparent" in {
        val required = MegawattHours(45d)
        val possible = MegawattHours(47d)

        val energyDemand = ThermalEnergyDemand(required, possible)
        energyDemand.hasRequiredDemand shouldBe true
        energyDemand.hasAdditionalDemand shouldBe true
      }
    }

    "adding two demands" should {
      "deliver proper results" in {
        val energyDemand1 = ThermalEnergyDemand(
          MegawattHours(45d),
          MegawattHours(47d)
        )
        val energyDemand2 = ThermalEnergyDemand(
          MegawattHours(23d),
          MegawattHours(28d)
        )

        val totalDemand = energyDemand1 + energyDemand2

        equalWithTolerance(totalDemand.required, MegawattHours(68d))
        equalWithTolerance(totalDemand.possible, MegawattHours(75d))
      }
    }
  }
}
