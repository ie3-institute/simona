/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.{Dimensionless, Each}
import squants.electro.{Amperes, ElectricPotential, Kilovolts, Ohms}
import squants.energy.{Megawatts, Power, Kilowatts}

class RefSystemSpec extends AnyFlatSpec with Matchers {

  "A RefSystem with nominal power and nominal voltage" should "provide corresponding nominal current and nominal impedance" in {

    val nominalPower: Power = Kilowatts(600)
    val nominalVoltage: ElectricPotential = Kilovolts(10)

    val refSystem = RefSystem(nominalPower, nominalVoltage)

    refSystem.nominalPower should be(nominalPower)
    refSystem.nominalVoltage should be(nominalVoltage)
    refSystem.nominalCurrent should be(
      Amperes(34.64101615137754774109785366023500d)
    )
    refSystem.nominalImpedance should be(
      Ohms(166.6666666666666666666666666666666d)
    )
  }

  "A dimensionless impedance" should "be transferred correctly between reference systems" in {
    val from = RefSystem(Megawatts(60d), Kilovolts(110d))
    val to = RefSystem(Megawatts(40d), Kilovolts(110d))
    val impedance = Each(0.1d)
    val expected = Each(0.06666666666666667d)

    val actual: Dimensionless = RefSystem.transferImpedance(impedance, from, to)

    actual should be(expected)
  }

  "A dimensionless admittance" should "be transferred correctly between reference systems" in {
    val from = RefSystem(Megawatts(60d), Kilovolts(110d))
    val to = RefSystem(Megawatts(40d), Kilovolts(110d))
    val admittance = Each(0.1d)
    val expected = Each(0.15000000000000002d)

    val actual: Dimensionless =
      RefSystem.transferAdmittance(admittance, from, to)

    actual should be(expected)
  }
}
