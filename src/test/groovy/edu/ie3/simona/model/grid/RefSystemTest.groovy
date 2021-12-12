/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.util.quantities.QuantityUtil
import spock.lang.Specification
import tech.units.indriya.quantity.Quantities

import javax.measure.Quantity
import javax.measure.quantity.Dimensionless
import javax.measure.quantity.ElectricPotential
import javax.measure.quantity.Power

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static tech.units.indriya.unit.Units.*

class RefSystemTest extends Specification {

	def testTolerance = 1E-3

	def "A RefSystem with nominal power and nominal voltage should provide corresponding nominal current and nominal impedance "() {

		given: "the nominal power and the nominal voltage"

		double nominalPower = 600000 // in voltampere
		double nominalVoltage = 10000 // in volt

		Quantity<Power> nominalPowerAsQuant = Quantities.getQuantity(nominalPower, VOLTAMPERE)
		Quantity<ElectricPotential> nominalVoltageAsQuant = Quantities.getQuantity(nominalVoltage, VOLT)

		when:
		RefSystem refSystem = RefSystem.apply(nominalPowerAsQuant, nominalVoltageAsQuant)

		then: "the nominal current and the nominal power should be calculated accordingly"
		refSystem.nominalPower() == Quantities.getQuantity(600000d, VOLTAMPERE)
		refSystem.nominalVoltage() == Quantities.getQuantity(10000d, VOLT)
		QuantityUtil.isEquivalentAbs(refSystem.nominalCurrent(), Quantities.getQuantity(34.64101615137754774109785366023500d, AMPERE), 1E-10)
		QuantityUtil.isEquivalentAbs(refSystem.nominalImpedance(), Quantities.getQuantity(166.6666666666666666666666666666666d, OHM), 1E-10)
	}

	def "An dimensionless impedance is transferred correctly between to reference system"() {
		given:
		RefSystem from = RefSystem.apply(Quantities.getQuantity(60d, MEGAVOLTAMPERE), Quantities.getQuantity(110d, KILOVOLT))
		RefSystem to = RefSystem.apply(Quantities.getQuantity(40d, MEGAVOLTAMPERE), Quantities.getQuantity(110d, KILOVOLT))
		Quantity<Dimensionless> impedance = Quantities.getQuantity(0.1d, PU)
		Quantity<Dimensionless> expected = Quantities.getQuantity(0.15d, PU)

		when:
		Quantity<Dimensionless> actual = RefSystem.transferImpedance(impedance, from, to)

		then:
		actual.getUnit() == impedance.getUnit()
		testTolerance < Math.abs(actual.getValue().doubleValue() - expected.getValue().doubleValue())
	}

	def "An dimensionless admittance is transferred correctly between to reference system"() {
		given:
		RefSystem from = RefSystem.apply(Quantities.getQuantity(60d, MEGAVOLTAMPERE), Quantities.getQuantity(110d, KILOVOLT))
		RefSystem to = RefSystem.apply(Quantities.getQuantity(40d, MEGAVOLTAMPERE), Quantities.getQuantity(110d, KILOVOLT))
		Quantity<Dimensionless> admittance = Quantities.getQuantity(0.1d, PU)
		Quantity<Dimensionless> expected = Quantities.getQuantity(0.0666d, PU)

		when:
		Quantity<Dimensionless> actual = RefSystem.transferAdmittance(admittance, from, to)

		then:
		actual.getUnit() == admittance.getUnit()
		testTolerance < Math.abs(actual.getValue().doubleValue() - expected.getValue().doubleValue())
	}
}
