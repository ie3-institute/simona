/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.datamodel.models.result.system.EnergyBoundariesFlexOptionsResult
import edu.ie3.simona.exceptions.FlexException
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueNoControl,
  IssuePowerControl,
}
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.quantities.QuantityUtils.{asMegaWatt, asMegaWattHour}
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKW}
import squants.energy.{KilowattHours, Kilowatts}

import java.util.UUID

class EnergyBoundariesFlexOptionsSpec extends UnitSpec with DefaultTestData {

  private val flexOptions = EnergyBoundariesFlexOptions(
    AssetEnergyBoundaries(
      eStorage = KilowattHours(10),
      currentEnergy = KilowattHours(5),
      pMax = Kilowatts(5),
      etaCharge = onePU,
      etaDischarge = onePU,
      currentTick = 0L,
    )
  )

  "Determining flex power" should {

    "succeed if set power is within limits" in {
      val setPower = Kilowatts(2)
      EnergyBoundariesFlexOptions.determineFlexPower(
        flexOptions,
        IssuePowerControl(0L, setPower),
      ) shouldBe setPower
    }

    "fail if set power is above limit" in {
      intercept[FlexException] {
        EnergyBoundariesFlexOptions.determineFlexPower(
          flexOptions,
          IssuePowerControl(0L, Kilowatts(6)),
        )
      }
    }

    "fail if set power is below limit" in {
      intercept[FlexException] {
        EnergyBoundariesFlexOptions.determineFlexPower(
          flexOptions,
          IssuePowerControl(0L, Kilowatts(-6)),
        )
      }
    }

    "set 0 kW upon no control message" in {
      EnergyBoundariesFlexOptions.determineFlexPower(
        flexOptions,
        IssueNoControl(0L),
      ) shouldBe zeroKW
    }

  }

  "Creating a result entity from flex options" should {

    val uuid: UUID = UUID.fromString("0-0-0-0-1")

    "succeed if proper flex options are provided" in {
      EnergyBoundariesFlexOptions.createResult(
        flexOptions,
        uuid,
        defaultSimulationStart,
      ) shouldBe new EnergyBoundariesFlexOptionsResult(
        defaultSimulationStart,
        uuid,
        -0.005.asMegaWattHour,
        0.005.asMegaWattHour,
        -0.005.asMegaWatt,
        0.005.asMegaWatt,
      )
    }

  }

}
