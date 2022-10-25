/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.ThermalHouseInput
import edu.ie3.util.quantities.QuantityUtil
import spock.lang.Shared
import spock.lang.Specification

import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATT
import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR
import static tech.units.indriya.quantity.Quantities.getQuantity
import static tech.units.indriya.unit.Units.CELSIUS
import static tech.units.indriya.unit.Units.KELVIN
import static tech.units.indriya.unit.Units.SECOND

class ThermalHouseTest extends Specification {

    @Shared
    static final Double TOLERANCE = 0.0001

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
        return ThermalHouse.apply(thermalHouseInput)
    }


    def "Functions testing inner temperature work as expected"() {
        given:
        def thermalHouse = buildThermalHouse(18, 22)

        when:
        def isHigher = thermalHouse.isInnerTemperatureTooHigh(innerTemperature)
        def isLower = thermalHouse.isInnerTemperatureTooLow(innerTemperature)

        then:
        isHigher == isTooHigh
        isLower == isTooLow

        where:
        innerTemperature         || isTooHigh | isTooLow
        getQuantity(20, CELSIUS) || false     | false
        getQuantity(18, CELSIUS) || false     | true
        getQuantity(22, CELSIUS) || true      | false
        getQuantity(17, CELSIUS) || false     | true
        getQuantity(23, CELSIUS) || true      | false
    }

    def "Calculation of thermal energy change and new inner temperature is performed correctly"() {
        given:
        def thermalHouse = buildThermalHouse(18, 22)
        def innerTemperature = getQuantity(20, CELSIUS)

        when:
        def thermalEnergyGain = thermalHouse.calcThermalEnergyGain(getQuantity(100, KILOWATT), getQuantity(3600, SECOND)).to(KILOWATTHOUR)
        def thermalEnergyLoss = thermalHouse.calcThermalEnergyLoss(innerTemperature, getQuantity(10, CELSIUS), getQuantity(3600, SECOND)).to(KILOWATTHOUR)
        def thermalEnergyChange = thermalHouse.calcThermalEnergyChange(thermalEnergyGain, thermalEnergyLoss).to(KILOWATTHOUR)
        def innerTemperatureChange = thermalHouse.calcInnerTemperatureChange(thermalEnergyChange).to(KELVIN)
        def newInnerTemperature = thermalHouse.calcNewInnerTemperature(innerTemperature, innerTemperatureChange)

        then:
        QuantityUtil.equals(getQuantity(100, KILOWATTHOUR), thermalEnergyGain.to(KILOWATTHOUR), TOLERANCE)
        QuantityUtil.equals(getQuantity(10, KILOWATTHOUR), thermalEnergyLoss.to(KILOWATTHOUR), TOLERANCE)
        QuantityUtil.equals(getQuantity(90, KILOWATTHOUR), thermalEnergyChange.to(KILOWATTHOUR), TOLERANCE)
        QuantityUtil.equals(getQuantity(9, KELVIN), innerTemperatureChange.to(KELVIN), TOLERANCE)
        QuantityUtil.equals(getQuantity(29, CELSIUS), newInnerTemperature.to(CELSIUS), TOLERANCE)
    }

    def "Comprising function to calculate new inner temperature works as expected"() {
        given:
        def thermalHouse = buildThermalHouse(18, 22)
        def thermalPower = getQuantity(100, KILOWATT)
        def duration = getQuantity(3600, SECOND)
        def currentInnerTemperature = getQuantity(20, CELSIUS)
        def ambientTemperature = getQuantity(10, CELSIUS)

        when:
        def newInnerTemperature = thermalHouse.newInnerTemperature(thermalPower, duration, currentInnerTemperature, ambientTemperature)

        then:
        QuantityUtil.equals(getQuantity(29, CELSIUS), newInnerTemperature.to(CELSIUS), TOLERANCE)
    }

    def "Check build method:"() {
        given:
        def thermalHouseInput = new ThermalHouseInput(
                UUID.randomUUID(),
                "Thermal House",
                null,
                getQuantity(1.0, StandardUnits.THERMAL_TRANSMISSION),
                getQuantity(10.0, StandardUnits.HEAT_CAPACITY),
                getQuantity(0, CELSIUS), // stub
                getQuantity(18, CELSIUS),
                getQuantity(22, CELSIUS)
        )

        when:
        def thermalHouse = buildThermalHouse(18, 22)

        then:
        thermalHouse.id() == thermalHouseInput.id
        thermalHouse.operatorInput() == thermalHouseInput.operator
        thermalHouse.operationTime() == thermalHouseInput.operationTime
        thermalHouse.bus() == thermalHouseInput.thermalBus
        thermalHouse.ethLosses() == thermalHouseInput.ethLosses
        thermalHouse.ethCapa() == thermalHouseInput.ethCapa
        thermalHouse.lowerBoundaryTemperature() == getQuantity(18, CELSIUS)
        thermalHouse.upperBoundaryTemperature() == getQuantity(22, CELSIUS)
    }
}
