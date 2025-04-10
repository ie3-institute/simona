/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.data

import edu.ie3.simona.service.Data.PrimaryData._
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.Kilovars
import squants.energy.Kilowatts

class DataSpec extends UnitSpec {

  "Meta functions for active power should work as expected" in {
    ActivePowerExtra.zero shouldBe ActivePower(zeroKW)

    ActivePowerExtra.scale(
      ActivePower(Kilowatts(5)),
      2.5,
    ) shouldBe ActivePower(Kilowatts(12.5))
  }

  "Meta functions for complex power should work as expected" in {
    ComplexPowerExtra.zero shouldBe ComplexPower(zeroKW, zeroKVAr)

    ComplexPowerExtra.scale(
      ComplexPower(Kilowatts(5), Kilovars(2)),
      1.5,
    ) shouldBe ComplexPower(Kilowatts(7.5), Kilovars(3))
  }

  "Meta functions for active power and heat should work as expected" in {
    ActivePowerAndHeatExtra.zero shouldBe ActivePowerAndHeat(zeroKW, zeroKW)

    ActivePowerAndHeatExtra.scale(
      ActivePowerAndHeat(Kilowatts(5), Kilowatts(2)),
      2,
    ) shouldBe ActivePowerAndHeat(Kilowatts(10), Kilowatts(4))
  }

  "Meta functions for complex power and heat should work as expected" in {
    ComplexPowerAndHeatExtra.zero shouldBe
      ComplexPowerAndHeat(zeroKW, zeroKVAr, zeroKW)

    ComplexPowerAndHeatExtra.scale(
      ComplexPowerAndHeat(Kilowatts(5), Kilovars(1), Kilowatts(2)),
      3,
    ) shouldBe ComplexPowerAndHeat(Kilowatts(15), Kilovars(3), Kilowatts(6))
  }

}
