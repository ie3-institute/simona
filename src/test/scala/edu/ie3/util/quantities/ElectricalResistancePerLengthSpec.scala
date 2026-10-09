/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities.{
  ElectricalResistancePerLength,
  MicroohmsPerMeter,
  MilliohmsPerMeter,
  OhmsPerKilometer,
  OhmsPerMeter,
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import edu.ie3.util.scala.quantities.ElectricalResistancePerLengthConversions.*
import squants.Meters
import squants.electro.Ohms
import squants.space.Kilometers

import scala.util.Success

class ElectricalResistancePerLengthSpec extends AnyFlatSpec with Matchers {

  behavior of "ElectricalResistancePerLength and its Units of Measure"

  it should "create values using UOM factories" in {
    OhmsPerMeter(1).toOhmsPerMeter should be(1d)
    OhmsPerKilometer(1).toOhmsPerKilometer should be(1d)
    MilliohmsPerMeter(1).toMilliohmsPerMeter should be(1d)
    MicroohmsPerMeter(1).toMicroohmsPerMeter should be(1d)
  }

  it should "properly convert between units" in {
    val rpl = OhmsPerMeter(1)

    rpl.toOhmsPerKilometer should be(1000d)
    rpl.toMilliohmsPerMeter should be(1000d)
    rpl.toMicroohmsPerMeter should be(1e6)

    OhmsPerKilometer(1000).toOhmsPerMeter should be(1d)
    MilliohmsPerMeter(500).toOhmsPerMeter should be(0.5d)
  }

  it should "parse strings correctly" in {
    ElectricalResistancePerLength("10.5 Ω/m") should be(
      Success(OhmsPerMeter(10.5))
    )
    ElectricalResistancePerLength("1.5 Ω/km") should be(
      Success(OhmsPerKilometer(1.5))
    )
    ElectricalResistancePerLength("2.5 mΩ/m") should be(
      Success(MilliohmsPerMeter(2.5))
    )
    ElectricalResistancePerLength("3.5 µΩ/m") should be(
      Success(MicroohmsPerMeter(3.5))
    )
  }

  it should "support implicit conversions from numbers (DSL)" in {
    1.ohmsPerMeter should be(OhmsPerMeter(1))
    1.5.ohmsPerKilometer should be(OhmsPerKilometer(1.5))
    10.milliohmsPerMeter should be(MilliohmsPerMeter(10))
    0.5.microohmsPerMeter should be(MicroohmsPerMeter(0.5))
  }

  it should "calculate absolute electrical resistance when multiplied by length" in {
    val resPerLength = OhmsPerMeter(0.5)
    val length = Meters(10)

    val absoluteResistance = resPerLength * length
    absoluteResistance should be(Ohms(5.0))

    val resPerLengthKm = OhmsPerKilometer(200)
    val lengthKm = Kilometers(2)

    val resPerLengthAbs = resPerLengthKm * lengthKm
    resPerLengthAbs should be(Ohms(400.0))
  }
}
