/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities.{
  WattHoursPerSquareMeter,
  WattsPerSquareMeter,
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.energy.{WattHours, Watts}
import squants.radio.{BecquerelsPerSquareMeterSecond, SquareMeterSeconds}
import squants.space.SquareMeters
import squants.time.Seconds

class IrradianceSpec extends AnyFlatSpec with Matchers {

  behavior of "Irradiance and its Units of Measure"

  it should "create values using UOM factories" in {
    WattsPerSquareMeter(
      1
    ).toWattsPerSquareMeter should be(1)
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = WattsPerSquareMeter(1)

    x.toWattsPerSquareMeter should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    WattsPerSquareMeter(1).toString(
      WattsPerSquareMeter
    ) should be("1.0 W/m²")
  }

  it should "return Power when multiplied by Area" in {
    WattsPerSquareMeter(1) * SquareMeters(10) should be(
      Watts(10)
    )
  }

  it should "return Energy when multiplied by AreaTime" in {
    WattsPerSquareMeter(2) * SquareMeterSeconds(3600) should be(
      WattHours(2)
    )
  }

  it should "return Irradiation when multiplied by Time" in {
    WattsPerSquareMeter(2) * Seconds(7200) should be(
      WattHoursPerSquareMeter(4)
    )
  }

  it should "return ParticleFlux when divided by Energy" in {
    WattsPerSquareMeter(10800) / WattHours(3) should be(
      BecquerelsPerSquareMeterSecond(1)
    )
  }

  it should "return Energy when divided by ParticleFlux" in {
    WattsPerSquareMeter(10800) / BecquerelsPerSquareMeterSecond(10) should be(
      WattHours(0.3)
    )
  }
}
