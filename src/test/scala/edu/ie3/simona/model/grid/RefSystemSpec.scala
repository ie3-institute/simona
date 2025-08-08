/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.simona.test.common.UnitSpec
import org.scalatest.matchers.should.Matchers
import squants.{Dimensionless, Each, ElectricCurrent}
import squants.electro.*
import squants.energy.{Kilowatts, Megawatts, Power}

class RefSystemSpec extends UnitSpec with Matchers {

  protected given tolerance: Dimensionless = Each(1e-12)
  protected given currentTolerance: ElectricCurrent = Amperes(1e-9)
  protected given impedanceTolerance: ElectricalResistance = Ohms(1e-9)

  "A RefSystem with nominal power and nominal voltage" should {

    "provide corresponding nominal current and nominal impedance" in {

      val nominalPower: Power = Kilowatts(600)
      val nominalVoltage: ElectricPotential = Kilovolts(10)

      val refSystem = RefSystem(nominalPower, nominalVoltage)

      refSystem.nominalPower should be(nominalPower)
      refSystem.nominalVoltage should be(nominalVoltage)
      refSystem.nominalCurrent should approximate(
        Amperes(34.641016151)
      )
      refSystem.nominalImpedance should approximate(
        Ohms(166.666666666)
      )
    }
  }

  "transfer a dimensionless impedance correctly to another reference system" in {
    val from = RefSystem(Megawatts(60d), Kilovolts(110d))
    val to = RefSystem(Megawatts(40d), Kilovolts(110d))
    val impedance = Each(0.1d)
    val expected = Each(0.066666666666d)

    val actual: Dimensionless = RefSystem.transferImpedance(impedance, from, to)

    actual should approximate(expected)
  }

  "transfer a dimensionless admittance correctly to another reference system" in {
    val from = RefSystem(Megawatts(60d), Kilovolts(110d))
    val to = RefSystem(Megawatts(40d), Kilovolts(110d))
    val admittance = Each(0.1d)
    val expected = Each(0.15)

    val actual: Dimensionless =
      RefSystem.transferAdmittance(admittance, from, to)

    actual should approximate(expected)
  }
}
