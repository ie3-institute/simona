/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities.Vars
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.Each
import squants.energy.Watts

class ReactivePowerSpec extends AnyFlatSpec with Matchers {

  behavior of "ReactivePower and its Units of Measure"

  it should "create values using UOM factories" in {
    Vars(
      1
    ).toVars should be(1)
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = Vars(1)
    x.toVars should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    Vars(1).toString(
      Vars
    ) should be("1.0 Var")
  }

  it should "return Dimensionless when divided by ActivePower" in {
    Vars(100) / Watts(10) should be(
      Each(10)
    )
  }

}
