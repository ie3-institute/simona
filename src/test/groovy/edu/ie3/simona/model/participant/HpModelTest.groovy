/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import squants.thermal.Celsius$

import static edu.ie3.util.quantities.PowerSystemUnits.KILOVOLTAMPERE
import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATT
import static tech.units.indriya.quantity.Quantities.getQuantity
import static tech.units.indriya.unit.Units.CELSIUS

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
import squants.energy.Kilowatts$
import edu.ie3.util.scala.quantities.Sq
import edu.ie3.util.TimeUtil
import spock.lang.Shared
import spock.lang.Specification
import spock.lang.Unroll



class HpModelTest extends Specification {

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
        Sq.create(100d, Kilowatts$.MODULE$),
        0.95,
        Sq.create(15d, Kilowatts$.MODULE$),
        thermalHouse
        )
  }

  static def buildHpData(HpState hpState) {
    return new HpData(hpState, 7200L,  Sq.create(10d, Celsius$.MODULE$))
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
    nextState.activePower() =~ expectedActivePower
    nextState.isRunning() == expectedRunningStatus

    where:
    hpState                                                                                          || expectedTimeTick | expectedRunningStatus | expectedActivePower        // (isRunning, tooHigh, tooLow)
    new HpState(false, 0L, Sq.create(0d, Kilowatts$.MODULE$), Sq.create(17d, Celsius$.MODULE$))      || 7200             | true                  | 95                            // tests case (false, false, true)
    new HpState(false, 0L, Sq.create(0d, Kilowatts$.MODULE$), Sq.create(18d, Celsius$.MODULE$))      || 7200             | false                 | 0                            // tests case (false, false, false)
    new HpState(false, 0L, Sq.create(0d, Kilowatts$.MODULE$), Sq.create(22d, Celsius$.MODULE$))      || 7200             | false                 | 0                            // tests case (false, false, false)
    new HpState(false, 0L, Sq.create(0d, Kilowatts$.MODULE$), Sq.create(23d, Celsius$.MODULE$))      || 7200             | false                 | 0                            // tests case (false, true, false)
    new HpState(true, 0L, Sq.create(95d, Kilowatts$.MODULE$), Sq.create(17d, Celsius$.MODULE$))      || 7200             | true                  | 95                            // tests case (true, false, true)
    new HpState(true, 0L, Sq.create(95d, Kilowatts$.MODULE$), Sq.create(18d, Celsius$.MODULE$))      || 7200             | true                  | 95                            // tests case (true, false, false)
    new HpState(true, 0L, Sq.create(95d, Kilowatts$.MODULE$), Sq.create(22d, Celsius$.MODULE$))      || 7200             | true                  | 95                            // tests case (true, false, false)
    new HpState(true, 0L, Sq.create(95d, Kilowatts$.MODULE$), Sq.create(23d, Celsius$.MODULE$))      || 7200             | false                 | 0                            // tests case (true, true, false)
  }

  def "Check new inner temperature after calculating next state with #hpState:"() {
    given:
    def hpData = buildHpData(hpState)
    def thermalHouse = buildThermalHouse(18, 22)
    def hpModel = buildStandardModel(thermalHouse)

    when:
    def nextInnerTemperature = hpModel.calculateNextState(hpData).innerTemperature()

    then:
    nextInnerTemperature =~ expectedNewInnerTemperature

    where:
    hpState                                                                                     || expectedNewInnerTemperature // (isRunning, tooHigh, tooLow)
    new HpState(false, 0L, Sq.create(0d, Kilowatts$.MODULE$), Sq.create(17d, Celsius$.MODULE$)) || 18.6  // tests case (false, false, true)
    new HpState(false, 0L, Sq.create(0d, Kilowatts$.MODULE$), Sq.create(18d, Celsius$.MODULE$)) || 16.4  // tests case (false, false, false)
    new HpState(false, 0L, Sq.create(0d, Kilowatts$.MODULE$), Sq.create(20d, Celsius$.MODULE$)) || 18    // tests case (false, false, false)
    new HpState(false, 0L, Sq.create(0d, Kilowatts$.MODULE$), Sq.create(22d, Celsius$.MODULE$)) || 19.6  // tests case (false, false, false)
    new HpState(false, 0L, Sq.create(0d, Kilowatts$.MODULE$), Sq.create(23d, Celsius$.MODULE$)) || 20.4  // tests case (false, true, false)

    new HpState(true, 0L, Sq.create(95d, Kilowatts$.MODULE$), Sq.create(17d, Celsius$.MODULE$)) || 18.6  // tests case (true, false, true)
    new HpState(true, 0L, Sq.create(95d, Kilowatts$.MODULE$), Sq.create(18d, Celsius$.MODULE$)) || 19.4  // tests case (true, false, false)
    new HpState(true, 0L, Sq.create(95d, Kilowatts$.MODULE$), Sq.create(20d, Celsius$.MODULE$)) || 21    // tests case (false, false, false)
    new HpState(true, 0L, Sq.create(95d, Kilowatts$.MODULE$), Sq.create(22d, Celsius$.MODULE$)) || 22.6  // tests case (true, false, false)
    new HpState(true, 0L, Sq.create(95d, Kilowatts$.MODULE$), Sq.create(23d, Celsius$.MODULE$)) || 20.4  // tests case (true, true, false)
  }


  def "Check build method:"() {
    when:
    def thermalHouse = buildThermalHouse(18, 22)
    def hpModelCaseClass = buildStandardModel(thermalHouse)
    def startDate = TimeUtil.withDefaults.toZonedDateTime("2021-01-01 00:00:00")
    def endDate = startDate.plusSeconds(86400L)
    def hpModelCaseObject = HpModel.apply(
        hpInput,
        startDate,
        endDate,
        null,
        1.0,
        thermalHouse)

    then:
    hpModelCaseClass.sRated() == hpModelCaseObject.sRated()
    hpModelCaseClass.cosPhiRated() == hpModelCaseObject.cosPhiRated()
    hpModelCaseClass.pThermal() == hpModelCaseObject.pThermal()
    hpModelCaseClass.thermalHouse() == hpModelCaseObject.thermalHouse()
  }
}
