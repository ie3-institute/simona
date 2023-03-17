/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.ThermalHouseInput
import edu.ie3.util.scala.quantities.Sq
import spock.lang.Shared
import spock.lang.Specification
import squants.energy.*
import squants.thermal.*
import squants.time.*

import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR_PER_KELVIN
import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATT_PER_KELVIN
import static tech.units.indriya.quantity.Quantities.getQuantity
import static tech.units.indriya.unit.Units.CELSIUS

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
    Temperature innerTemp = Sq.create(innerTemperature, Celsius$.MODULE$)
    def isHigher = thermalHouse.isInnerTemperatureTooHigh(innerTemp)
    def isLower = thermalHouse.isInnerTemperatureTooLow(innerTemp, thermalHouse.lowerBoundaryTemperature())

    then:
    isHigher == isTooHigh
    isLower == isTooLow

    where:
    innerTemperature || isTooHigh | isTooLow
    20               || false     | false
    18               || false     | true
    22               || true      | false
    17               || false     | true
    23               || true      | false
  }

  def "Calculation of thermal energy change and new inner temperature is performed correctly"() {
    given:
    def thermalHouse = buildThermalHouse(18, 22)
    def innerTemperature = Sq.create(20, Celsius$.MODULE$)

    when:
    def thermalEnergyGain = thermalHouse.calcThermalEnergyGain(Sq.create(100, Kilowatts$.MODULE$), Sq.create(3600, Seconds$.MODULE$))
    def thermalEnergyLoss = thermalHouse.calcThermalEnergyLoss(innerTemperature, Sq.create(10, Celsius$.MODULE$), Sq.create(3600, Seconds$.MODULE$))
    def thermalEnergyChange = thermalHouse.calcThermalEnergyChange(thermalEnergyGain, thermalEnergyLoss)
    def innerTemperatureChange = thermalHouse.calcInnerTemperatureChange(thermalEnergyChange)
    def newInnerTemperature = thermalHouse.calcNewInnerTemperature(innerTemperature, innerTemperatureChange)

    then:
    Math.abs(100d - thermalEnergyGain.toKilowattHours()) < TOLERANCE
    Math.abs(10d - thermalEnergyLoss.toKilowattHours()) < TOLERANCE
    Math.abs(90d - thermalEnergyChange.toKilowattHours()) < TOLERANCE
    Math.abs(9d - innerTemperatureChange.toKelvinScale()) < TOLERANCE
    Math.abs(29d - newInnerTemperature.toCelsiusScale()) < TOLERANCE
  }

  def "Comprising function to calculate new inner temperature works as expected"() {
    given:
    def thermalHouse = buildThermalHouse(18, 22)
    def thermalPower = Sq.create(100, Kilowatts$.MODULE$)
    def duration = Sq.create(3600, Seconds$.MODULE$)
    def currentInnerTemperature = Sq.create(20, Celsius$.MODULE$)
    def ambientTemperature = Sq.create(10, Celsius$.MODULE$)

    when:
    def newInnerTemperature = thermalHouse.newInnerTemperature(thermalPower, duration, currentInnerTemperature, ambientTemperature)

    then:
    Math.abs(newInnerTemperature.toCelsiusScale() - 29d) < TOLERANCE
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
    thermalHouse.ethLosses().toKilowatts() == thermalHouseInput.ethLosses.to(KILOWATT_PER_KELVIN).getValue().doubleValue()
    (thermalHouse.ethCapa().$times(Sq.create(1d, Kelvin$.MODULE$))).toKilowattHours() == thermalHouseInput.ethCapa.to(KILOWATTHOUR_PER_KELVIN).getValue().doubleValue()
    thermalHouse.lowerBoundaryTemperature() == Sq.create(18, Celsius$.MODULE$)
    thermalHouse.upperBoundaryTemperature() == Sq.create(22, Celsius$.MODULE$)
  }
}
