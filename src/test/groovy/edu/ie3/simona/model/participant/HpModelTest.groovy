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
import edu.ie3.simona.model.participant.HpModel.HpRelevantData
import edu.ie3.simona.model.participant.HpModel.HpState
import edu.ie3.simona.model.thermal.ThermalGrid
import edu.ie3.simona.model.thermal.ThermalHouse
import edu.ie3.simona.model.thermal.ThermalModelState
import edu.ie3.simona.model.thermal.ThermalStorage
import edu.ie3.util.quantities.QuantityUtil
import edu.ie3.util.scala.OperationInterval
import scala.Tuple2
import scala.collection.immutable.Set
import scala.collection.immutable.HashSet
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
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

    static HpModel buildStandardModel(ThermalHouse thermalHouse) {
        Set<ThermalHouse> thermalHouses = new HashSet<ThermalHouse>() as Set<ThermalHouse>
        def thermalStorages = []
        def thermalGrid = new ThermalGrid(thermalHouses.$plus(thermalHouse) as Set<ThermalHouse>, thermalStorages as Set<ThermalStorage>)
        return new HpModel(
                UUID.randomUUID(),
                "HpModel",
                null,
                1.0,
                null,
                getQuantity(100, KILOWATT),
                0.95,
                getQuantity(15, KILOWATT),
                thermalGrid
        )
    }

    static ThermalGrid.ThermalGridState buildThermalState(double temperature) {
        Map<UUID, ThermalModelState> map = new HashMap<UUID, ThermalModelState>()
        def state = new ThermalHouse.ThermalHouseState(0L, getQuantity(temperature, StandardUnits.TEMPERATURE), getQuantity(0d, StandardUnits.ACTIVE_POWER_IN))
        return new ThermalGrid.ThermalGridState(map.$plus(new Tuple2<UUID, ThermalHouse.ThermalHouseState>(thermHouseUuid, state)) as scala.collection.immutable.Map<UUID, ThermalModelState>)
    }

    static def buildHpData(HpState hpState) {
        return new HpRelevantData(hpState, 7200, getQuantity(10, CELSIUS))
    }

    private static UUID thermHouseUuid = UUID.fromString("75a43a0f-7c20-45ca-9568-949b728804ca")

    static def buildThermalHouse(Double lowerBoundaryTemperature, Double upperBoundaryTemperature) {
        def thermalHouseInput = new ThermalHouseInput(
                thermHouseUuid,
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
        nextState.isRunning() == expectedRunningStatus
        nextState.activePower().isEquivalentTo(getQuantity(expectedActivePower, KILOWATT))

        where:
        hpState                                                                                           || expectedTimeTick | expectedRunningStatus | expectedActivePower
        new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(0, KILOWATT), buildThermalState(17))  || 7200             | true                  | 95
        new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(0, KILOWATT), buildThermalState(18))  || 7200             | true                  | 95
        new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(0, KILOWATT), buildThermalState(22))  || 7200             | false                 | 0
        new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(0, KILOWATT), buildThermalState(23))  || 7200             | false                 | 0

        new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(80, KILOWATT), buildThermalState(17)) || 7200             | true                  | 95
        new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(80, KILOWATT), buildThermalState(18)) || 7200             | true                  | 95
        new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(80, KILOWATT), buildThermalState(22)) || 7200             | false                 | 0
        new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(80, KILOWATT), buildThermalState(23)) || 7200             | false                 | 0

    }

    def "Check new inner temperature after calculating next state with #hpState:"() {
        given:
        def hpData = buildHpData(hpState)
        def thermalHouse = buildThermalHouse(18, 22)
        def hpModel = buildStandardModel(thermalHouse)

        when:
        def nextInnerTemperature = hpModel.calculateNextState(hpData).thermalGridState().partState().get(thermalHouse.uuid()).map({s -> (s as ThermalHouse.ThermalHouseState).innerTemperature() }).get()

        then:
        QuantityUtil.equals(nextInnerTemperature, getQuantity(expectedNewInnerTemperature, CELSIUS), TOLERANCE)

        where:
        hpState                                                                                           || expectedNewInnerTemperature
        new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(0, KILOWATT), buildThermalState(17))  || 15.6
        new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(0, KILOWATT), buildThermalState(18))  || 16.4
        new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(0, KILOWATT), buildThermalState(20))  || 18
        new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(0, KILOWATT), buildThermalState(22))  || 19.6
        new HpState(false, 0, getQuantity(0, KILOWATT), getQuantity(0, KILOWATT), buildThermalState(23))  || 20.4

        new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(80, KILOWATT), buildThermalState(17)) || 15.6
        new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(80, KILOWATT), buildThermalState(18)) || 16.4
        new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(80, KILOWATT), buildThermalState(20)) || 18
        new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(80, KILOWATT), buildThermalState(22)) || 19.6
        new HpState(true, 0, getQuantity(95, KILOWATT), getQuantity(80, KILOWATT), buildThermalState(23)) || 20.4

    }


    def "Check build method:"() {
        when:
        def thermalHouse = buildThermalHouse(18, 22)
        def hpModelCaseClass = buildStandardModel(thermalHouse)
        def thermalGrid = new ThermalGrid([thermalHouse] as Set<ThermalHouse>, [] as Set<ThermalStorage>)
        def hpModelCaseObject = HpModel.apply(
                hpInput,
                OperationInterval.apply(0L, 86400L),
                null,
                thermalGrid
        )

        then:
        hpModelCaseClass.sRated().getValue() == hpModelCaseObject.sRated().getValue()
        hpModelCaseClass.cosPhiRated() == hpModelCaseObject.cosPhiRated()
        hpModelCaseClass.pThermal().getValue() == hpModelCaseObject.pThermal().getValue()
        hpModelCaseClass.thermalGrid().houses().size() == 1
        hpModelCaseObject.thermalGrid().houses().size() == 1
        hpModelCaseClass.thermalGrid().houses().head() == hpModelCaseObject.thermalGrid().houses().head()
    }
}
