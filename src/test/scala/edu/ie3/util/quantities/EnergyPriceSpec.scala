/*
 * © 2023-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.simona.test.matchers.{DoubleMatchers, SquantsMatchers}
import edu.ie3.util.scala.quantities.{
  EuroPerKilowattHour,
  EuroPerMegawattHour,
  EuroPerWattHour,
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.energy.{KilowattHours, WattHours}
import squants.market.{EUR, Money}

class EnergyPriceSpec
    extends AnyFlatSpec
    with Matchers
    with DoubleMatchers
    with SquantsMatchers {

  // testing tolerances
  given Double = 1e-10
  given Money = EUR(1e-10)

  behavior of "EnergyPrice and its Units of Measure"

  it should "create values using UOM factories" in {
    EuroPerWattHour(
      1
    ).toEuroPerWattHour should be(1)
    EuroPerKilowattHour(
      1
    ).toEuroPerKilowattHour should be(1)
    EuroPerMegawattHour(
      1
    ).toEuroPerMegawattHour should be(1)
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = EuroPerKilowattHour(1)

    x.toEuroPerKilowattHour should be(1)
    x.toEuroPerWattHour should approximate(1e-3)
    x.toEuroPerMegawattHour should approximate(1e3)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    EuroPerWattHour(1).toString(
      EuroPerWattHour
    ) should be("1.0 €/Wh")

    EuroPerKilowattHour(1).toString(
      EuroPerKilowattHour
    ) should be("1.0 €/kWh")

    EuroPerMegawattHour(1).toString(
      EuroPerMegawattHour
    ) should be("1.0 €/MWh")
  }

  it should "return Euro when multiplied with power" in {
    EuroPerKilowattHour(1) * KilowattHours(10) should
      approximate(EUR(10))

    EuroPerMegawattHour(80) * KilowattHours(100) should
      approximate(EUR(8))

    EuroPerWattHour(1) * WattHours(100) should
      approximate(EUR(100))
  }
}
