/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid


import edu.ie3.util.scala.quantities.Sq
import spock.lang.Specification
import squants.Dimensionless
import squants.Each$
import squants.electro.Amperes$
import squants.electro.ElectricPotential
import squants.electro.Kilovolts$
import squants.electro.Ohms$
import squants.electro.Volts$
import squants.energy.Megawatts$
import squants.energy.Watts$
import squants.energy.Power


class RefSystemTest extends Specification {

  def "A RefSystem with nominal power and nominal voltage should provide corresponding nominal current and nominal impedance "() {

    given: "the nominal power and the nominal voltage"

    double nominalPower = 600000 // in voltampere
    double nominalVoltage = 10000 // in volt

    Power nominalPowerAsSquant = Sq.create(nominalPower, Watts$.MODULE$)
    ElectricPotential nominalVoltageAsSquant = Sq.create(nominalVoltage, Volts$.MODULE$)

    when:
    RefSystem refSystem = RefSystem.apply(nominalPowerAsSquant, nominalVoltageAsSquant)

    then: "the nominal current and the nominal power should be calculated accordingly"
    refSystem.nominalPower() == Sq.create(600000d, Watts$.MODULE$)
    refSystem.nominalVoltage() == Sq.create(10000d, Volts$.MODULE$)
    refSystem.nominalCurrent() =~ Sq.create(34.64101615137754774109785366023500d, Amperes$.MODULE$)
    refSystem.nominalImpedance() =~ Sq.create (166.6666666666666666666666666666666d, Ohms$.MODULE$)
  }

  def "An dimensionless impedance is transferred correctly between to reference system"() {
    given:
    RefSystem from = RefSystem.apply(Sq.create(60d, Megawatts$.MODULE$), Sq.create(110d, Kilovolts$.MODULE$))
    RefSystem to = RefSystem.apply(Sq.create(40d, Megawatts$.MODULE$), Sq.create(110d, Kilovolts$.MODULE$))
    Dimensionless impedance = Sq.create(0.1d, Each$.MODULE$)
    Dimensionless expected = Sq.create(0.0666d, Each$.MODULE$)

    when:
    Dimensionless actual = RefSystem.transferImpedance(impedance, from, to)

    then:
    actual =~ expected
  }

  def "An dimensionless admittance is transferred correctly between to reference system"() {
    given:
    RefSystem from = RefSystem.apply(Sq.create(60d, Megawatts$.MODULE$), Sq.create(110d, Kilovolts$.MODULE$))
    RefSystem to = RefSystem.apply(Sq.create(40d, Megawatts$.MODULE$), Sq.create(110d, Kilovolts$.MODULE$))
    Dimensionless admittance = Sq.create(0.1d, Each$.MODULE$)
    Dimensionless expected = Sq.create(0.15d, Each$.MODULE$)

    when:
    Dimensionless actual = RefSystem.transferAdmittance(admittance, from, to)

    then:

    actual =~ expected
  }
}
