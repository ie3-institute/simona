/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalEnergyDemand
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.quantity.Quantities

class ThermalGridSpec extends UnitSpec {
  "Testing the thermal energy demand" when {
    "instantiating it from given values" should {
      "correct non-sensible input" in {
        val possible = Quantities.getQuantity(40d, StandardUnits.ENERGY_RESULT)
        val required = Quantities.getQuantity(42d, StandardUnits.ENERGY_RESULT)

        val energyDemand = ThermalEnergyDemand(required, possible)

        energyDemand.required should equalWithTolerance(possible)
        energyDemand.possible should equalWithTolerance(possible)
      }

      "properly corrects the units" in {
        val possible =
          Quantities.getQuantity(45d, PowerSystemUnits.KILOWATTHOUR)
        val required = Quantities.getQuantity(42d, PowerSystemUnits.WATTHOUR)

        val energyDemand = ThermalEnergyDemand(required, possible)

        energyDemand.required should equalWithTolerance(
          required.to(StandardUnits.ENERGY_RESULT)
        )
        energyDemand.possible should equalWithTolerance(
          possible.to(StandardUnits.ENERGY_RESULT)
        )
      }

      "set the correct values, if they are sensible" in {
        val possible = Quantities.getQuantity(45d, StandardUnits.ENERGY_RESULT)
        val required = Quantities.getQuantity(42d, StandardUnits.ENERGY_RESULT)

        val energyDemand = ThermalEnergyDemand(required, possible)

        energyDemand.required should equalWithTolerance(required)
        energyDemand.possible should equalWithTolerance(possible)
      }
    }

    "defining no demand" should {
      "actually have no demand" in {
        val energyDemand = ThermalEnergyDemand.noDemand

        energyDemand.required should equalWithTolerance(
          Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
        )
        energyDemand.possible should equalWithTolerance(
          Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
        )
      }
    }

    "checking for required and additional demand" should {
      "return proper information, if no required but additional demand is apparent" in {
        val required = Quantities.getQuantity(0, StandardUnits.ENERGY_RESULT)
        val possible = Quantities.getQuantity(45d, StandardUnits.ENERGY_RESULT)

        val energyDemand = ThermalEnergyDemand(required, possible)
        energyDemand.hasRequiredDemand shouldBe false
        energyDemand.hasAdditionalDemand shouldBe true
      }

      "return proper information, if required but no additional demand is apparent" in {
        val required = Quantities.getQuantity(45d, StandardUnits.ENERGY_RESULT)
        val possible = Quantities.getQuantity(45d, StandardUnits.ENERGY_RESULT)

        val energyDemand = ThermalEnergyDemand(required, possible)
        energyDemand.hasRequiredDemand shouldBe true
        energyDemand.hasAdditionalDemand shouldBe false
      }

      "return proper information, if required and additional demand is apparent" in {
        val required = Quantities.getQuantity(45d, StandardUnits.ENERGY_RESULT)
        val possible = Quantities.getQuantity(47d, StandardUnits.ENERGY_RESULT)

        val energyDemand = ThermalEnergyDemand(required, possible)
        energyDemand.hasRequiredDemand shouldBe true
        energyDemand.hasAdditionalDemand shouldBe true
      }
    }

    "adding two demands" should {
      "deliver proper results" in {
        val energyDemand1 = ThermalEnergyDemand(
          Quantities.getQuantity(45d, StandardUnits.ENERGY_RESULT),
          Quantities.getQuantity(47d, StandardUnits.ENERGY_RESULT)
        )
        val energyDemand2 = ThermalEnergyDemand(
          Quantities.getQuantity(23d, StandardUnits.ENERGY_RESULT),
          Quantities.getQuantity(28d, StandardUnits.ENERGY_RESULT)
        )

        val totalDemand = energyDemand1 + energyDemand2

        totalDemand.required should equalWithTolerance(
          Quantities.getQuantity(68d, StandardUnits.ENERGY_RESULT)
        )
        totalDemand.possible should equalWithTolerance(
          Quantities.getQuantity(75d, StandardUnits.ENERGY_RESULT)
        )
      }
    }
  }
}
