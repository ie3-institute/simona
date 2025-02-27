/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.HpInputTestData
import edu.ie3.util.TimeUtil
import org.scalatest.matchers.should.Matchers
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power}

import java.time.ZonedDateTime

class HpModelSpec extends UnitSpec with Matchers with HpInputTestData {
  implicit val powerTolerance: Power = Kilowatts(1e-10)
  implicit val energyTolerance: Energy = KilowattHours(1e-10)

  private val dateTime: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02T03:04:05Z")

  "StorageModel" should {

    "Determine the current state" in {
      val hpModel =
        HpModel.apply(hpInputModel, thermalGrid(thermalHouse(18, 22), None))

      val lastTick = 3600L

      //  val testCases = Table(
      // ("lastThermalGridState", "power", "duration", "expThermalGridState")

    }

    "Calculate flex options" in {}

    "Calculate flex options with target SOC" in {}

    "Handle controlled power change" in {}

    "Handle controlled power change with ref target SOC" in {}

    "Handle the edge case of discharging in tolerance margins" in {}

    "Handle the edge case of charging in tolerance margins" in {}

    "Handle the edge case of discharging in positive target margin" in {}

    "Handle the edge case of charging in negative target margin" in {}
  }
}
