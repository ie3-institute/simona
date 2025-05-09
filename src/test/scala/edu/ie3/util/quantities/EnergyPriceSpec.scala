/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities.EuroPerKilowatthour
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.energy.KilowattHours
import squants.market.EUR

class EnergyPriceSpec extends AnyFlatSpec with Matchers {

  behavior of "EnergyPrice and its Units of Measure"

  it should "create values using UOM factories" in {
    EuroPerKilowatthour(
      1
    ).toEuroPerKilowattHour should be(1)
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = EuroPerKilowatthour(1)

    x.toEuroPerKilowattHour should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    EuroPerKilowatthour(1).toString(
      EuroPerKilowatthour
    ) should be("1.0 €/kWh")
  }

  it should "return Euro when multiplied by Kilowatthour" in {
    EuroPerKilowatthour(1) * KilowattHours(10) should be(
      EUR(10)
    )
  }
}
