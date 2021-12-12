/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.system.type.HpTypeInput
import edu.ie3.datamodel.models.input.thermal.ThermalHouseInput
import edu.ie3.simona.model.participant.HpModel.HpData
import edu.ie3.simona.model.participant.HpModel.HpState
import edu.ie3.simona.model.thermal.ThermalHouse
import edu.ie3.util.quantities.QuantityUtil
import edu.ie3.util.scala.OperationInterval
import spock.lang.Shared
import spock.lang.Specification
import spock.lang.Unroll

import static edu.ie3.util.quantities.PowerSystemUnits.KILOVOLTAMPERE
import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATT
import static tech.units.indriya.quantity.Quantities.getQuantity
import static tech.units.indriya.unit.Units.CELSIUS

class HpModelTest extends Specification {

	@Shared
	static final Double TOLERANCE = 0.0001
	@Shared
	HpInput hpInput

	def setupSpec() {
		def hpTypeInput = new HpTypeInput(
				UUID.randomUUID(),
				"HpTypeInput",
				null,
				null,
				getQuantity(100, KILOVOLTAMPERE),
				0.95,
				getQuantity(15, KILOWATT)
				)

		hpInput = new HpInput(
				UUID.randomUUID(),
				"HpInput",
				OperatorInput.NO_OPERATOR_ASSIGNED,
				OperationTime.notLimited(),
				null,
				null,
				new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
				hpTypeInput
				)
	}

	static def buildStandardModel(ThermalHouse thermalHouse) {
		return new HpModel(
				UUID.randomUUID(),
				"HpModel",
				null,
				1.0,
				null,
				getQuantity(100, KILOWATT),
				0.95,
				getQuantity(15, KILOWATT),
				thermalHouse
				)
	}

	static def buildHpData(HpState hpState) {
		return new HpData(hpState, 7200, getQuantity(10, CELSIUS))
	}

	static def buildThermalHouse(Double lowerBoundaryTemperature, Double upperBoundaryTemperature) {
		def thermalHouseInput = new ThermalHouseInput(
				UUID.randomUUID(),
				"Thermal House",
				null,
				getQuantity(1.0, StandardUnits.THERMAL_TRANSMISSION),
				getQuantity(10.0, StandardUnits.HEAT_CAPACITY),
				getQuantity(0, CELSIUS), // stub
				getQuantity(upperBoundaryTemperature, CELSIUS),
				getQuantity(lowerBoundaryTemperature, CELSIUS)
				)
		def thermalHouse = ThermalHouse.apply(thermalHouseInput)
		return thermalHouse
	}


	@Unroll
	def "Check active power, time tick and running state after calculating next state with #hpState:"() {
		given:
		def hpData = buildHpData(hpState)
		def thermalHouse = buildThermalHouse(18, 22)
		def hpModel = buildStandardModel(thermalHouse)

		when:
		def nextState = hpModel.calculateNextState(hpData)

		then:
		nextState.lastTimeTick() == expectedTimeTick
		nextState.activePower().isEquivalentTo(getQuantity(expectedActivePower, KILOWATT))
		nextState.isRunning() == expectedRunningStatus

		where:
		hpState                                                                   || expectedTimeTick | expectedRunningStatus | expectedActivePower        // (isRunning, tooHigh, tooLow)
		new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(17, CELSIUS)) || 7200             | true                  | 95                            // tests case (false, false, true)
		new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(18, CELSIUS)) || 7200             | false                 | 0                            // tests case (false, false, false)
		new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(22, CELSIUS)) || 7200             | false                 | 0                            // tests case (false, false, false)
		new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(23, CELSIUS)) || 7200             | false                 | 0                            // tests case (false, true, false)

		new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(17, CELSIUS)) || 7200             | true                  | 95                            // tests case (true, false, true)
		new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(18, CELSIUS)) || 7200             | true                  | 95                            // tests case (true, false, false)
		new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(22, CELSIUS)) || 7200             | true                  | 95                            // tests case (true, false, false)
		new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(23, CELSIUS)) || 7200             | false                 | 0                            // tests case (true, true, false)

	}

	def "Check new inner temperature after calculating next state with #hpState:"() {
		given:
		def hpData = buildHpData(hpState)
		def thermalHouse = buildThermalHouse(18, 22)
		def hpModel = buildStandardModel(thermalHouse)

		when:
		def nextInnerTemperature = hpModel.calculateNextState(hpData).innerTemperature()

		then:
		QuantityUtil.equals(nextInnerTemperature, getQuantity(expectedNewInnerTemperature, CELSIUS), TOLERANCE)

		where:
		hpState                                                                   || expectedNewInnerTemperature                                            // (isRunning, tooHigh, tooLow)
		new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(17, CELSIUS)) || 18.6                            // tests case (false, false, true)
		new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(18, CELSIUS)) || 16.4                            // tests case (false, false, false)
		new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(20, CELSIUS)) || 18                            // tests case (false, false, false)
		new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(22, CELSIUS)) || 19.6                            // tests case (false, false, false)
		new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(23, CELSIUS)) || 20.4                            // tests case (false, true, false)

		new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(17, CELSIUS)) || 18.6                            // tests case (true, false, true)
		new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(18, CELSIUS)) || 19.4                            // tests case (true, false, false)
		new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(20, CELSIUS)) || 21                            // tests case (false, false, false)
		new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(22, CELSIUS)) || 22.6                            // tests case (true, false, false)
		new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(23, CELSIUS)) || 20.4                            // tests case (true, true, false)

	}


	def "Check build method:"() {
		when:
		def thermalHouse = buildThermalHouse(18, 22)
		def hpModelCaseClass = buildStandardModel(thermalHouse)
		def hpModelCaseObject = HpModel.apply(
				hpInput,
				OperationInterval.apply(0L, 86400L),
				null,
				thermalHouse)

		then:
		hpModelCaseClass.sRated().getValue() == hpModelCaseObject.sRated().getValue()
		hpModelCaseClass.cosPhiRated() == hpModelCaseObject.cosPhiRated()
		hpModelCaseClass.pThermal().getValue() == hpModelCaseObject.pThermal().getValue()
		hpModelCaseClass.thermalHouse() == hpModelCaseObject.thermalHouse()
	}
}
