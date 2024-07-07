/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.util.scala.quantities.Sq
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.{Dimensionless, Each}
import squants.electro.{Amperes, ElectricPotential, Kilovolts, Ohms, Volts}
import squants.energy.{Megawatts, Power, Watts}

class RefSystemSpec extends AnyFlatSpec with Matchers {

  "A RefSystem with nominal power and nominal voltage" should "provide corresponding nominal current and nominal impedance" in {

    val nominalPowerAsSquant: Power = Sq.create(600000, Watts)
    val nominalVoltageAsSquant: ElectricPotential = Sq.create(10000, Volts)

    val refSystem = RefSystem(nominalPowerAsSquant, nominalVoltageAsSquant)

    refSystem.nominalPower should be(nominalPowerAsSquant)
    refSystem.nominalVoltage should be(nominalVoltageAsSquant)
    refSystem.nominalCurrent should be(
      Sq.create(34.64101615137754774109785366023500d, Amperes)
    )
    refSystem.nominalImpedance should be(
      Sq.create(166.6666666666666666666666666666666d, Ohms)
    )
  }

  "A dimensionless impedance" should "be transferred correctly between reference systems" in {
    val from = RefSystem(Sq.create(60d, Megawatts), Sq.create(110d, Kilovolts))
    val to = RefSystem(Sq.create(40d, Megawatts), Sq.create(110d, Kilovolts))
    val impedance = Sq.create(0.1d, Each)
    val expected = Sq.create(0.06666666666666667d, Each)

    val actual: Dimensionless = RefSystem.transferImpedance(impedance, from, to)

    actual should be(expected)
  }

  "A dimensionless admittance" should "be transferred correctly between reference systems" in {
    val from = RefSystem(Sq.create(60d, Megawatts), Sq.create(110d, Kilovolts))
    val to = RefSystem(Sq.create(40d, Megawatts), Sq.create(110d, Kilovolts))
    val admittance = Sq.create(0.1d, Each)
    val expected = Sq.create(0.15000000000000002d, Each)

    val actual: Dimensionless =
      RefSystem.transferAdmittance(admittance, from, to)

    actual should be(expected)
  }
}
