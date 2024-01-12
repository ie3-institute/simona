/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static tech.units.indriya.quantity.Quantities.getQuantity
import static tech.units.indriya.unit.Units.PERCENT

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.ChpInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.system.type.ChpTypeInput
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.participant.ChpModel.ChpState
import edu.ie3.simona.model.thermal.CylindricalThermalStorage
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.KilowattHoursPerKelvinCubicMeters$
import edu.ie3.util.scala.quantities.Sq
import spock.lang.Shared
import spock.lang.Specification
import spock.lang.Unroll
import squants.energy.*
import squants.space.CubicMeters$
import squants.thermal.Celsius$

class ChpModelTest extends Specification {

  @Shared
  static final Double TOLERANCE = 0.0001d
  @Shared
  ChpState chpStateNotRunning = new ChpState(false, 0, Sq.create(0, Kilowatts$.MODULE$), Sq.create(0, KilowattHours$.MODULE$))
  @Shared
  ChpState chpStateRunning = new ChpState(true, 0, Sq.create(0, Kilowatts$.MODULE$), Sq.create(0, KilowattHours$.MODULE$))
  @Shared
  CylindricalStorageInput storageInput
  @Shared
  ChpInput chpInput

  def setupSpec() {
    def thermalBus =  new ThermalBusInput(UUID.randomUUID(), "thermal bus")

    storageInput = new CylindricalStorageInput(
        UUID.randomUUID(),
        "ThermalStorage",
        thermalBus,
        getQuantity(100, StandardUnits.VOLUME),
        getQuantity(20, StandardUnits.VOLUME),
        getQuantity(30, StandardUnits.TEMPERATURE),
        getQuantity(40, StandardUnits.TEMPERATURE),
        getQuantity(1.15, StandardUnits.SPECIFIC_HEAT_CAPACITY))

    def chpTypeInput = new ChpTypeInput(
        UUID.randomUUID(),
        "ChpTypeInput",
        getQuantity(10000d, EURO),
        getQuantity(200d, EURO_PER_MEGAWATTHOUR),
        getQuantity(19, PERCENT),
        getQuantity(76, PERCENT),
        getQuantity(100, KILOVOLTAMPERE),
        0.95,
        getQuantity(50, KILOWATT),
        getQuantity(0, KILOWATT))

    chpInput = new ChpInput(
        UUID.randomUUID(),
        "ChpInput",
        OperatorInput.NO_OPERATOR_ASSIGNED,
        OperationTime.notLimited(),
        TestObjectFactory.buildNodeInput(false, GermanVoltageLevelUtils.MV_10KV, 0),
        thermalBus,
        new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
        chpTypeInput,
        null,
        false)
  }

  static def buildChpModel(CylindricalThermalStorage thermalStorage) {
    return new ChpModel(
        UUID.randomUUID(),
        "ChpModel",
        null,
        1.0,
        null,
        Sq.create(100, Kilowatts$.MODULE$),
        0.95,
        Sq.create(50, Kilowatts$.MODULE$),
        thermalStorage)
  }

  static def buildChpRelevantData(ChpState chpState, Double heatDemand) {
    return new ChpModel.ChpRelevantData(chpState, Sq.create(heatDemand, KilowattHours$.MODULE$), 7200)
  }

  static def buildThermalStorage(CylindricalStorageInput storageInput, Double storageLvl) {
    def storedEnergy = CylindricalThermalStorage.volumeToEnergy(
        Sq.create(storageLvl, CubicMeters$.MODULE$),
        Sq.create(storageInput.c.value.toDouble(), KilowattHoursPerKelvinCubicMeters$.MODULE$),
        Sq.create(storageInput.inletTemp.value.doubleValue(), Celsius$.MODULE$),
        Sq.create(storageInput.returnTemp.value.doubleValue(), Celsius$.MODULE$)
        )
    def thermalStorage = CylindricalThermalStorage.apply(storageInput, storedEnergy)
    return thermalStorage
  }

  @Unroll
  def "Check active power after calculating next state with #chpState and heat demand #heatDemand kWh:"() {
    given:
    def chpData = buildChpRelevantData(chpState, heatDemand)
    def thermalStorage = buildThermalStorage(storageInput, storageLvl)
    def chpModel = buildChpModel(thermalStorage)

    when:
    def activePower = chpModel.calculateNextState(chpData).activePower()

    then:
    activePower.toKilowatts() == expectedActivePower

    where:
    chpState           | storageLvl | heatDemand  || expectedActivePower
    chpStateNotRunning | 90         | 0           || 0                        // tests case (false, false, true)
    chpStateNotRunning | 90         | 8 * 115     || 95                        // tests case (false, true, false)
    chpStateNotRunning | 90         | 10          || 0                        // tests case (false, true, true)
    chpStateRunning    | 90         | 0           || 95                        // tests case (true, false, true)
    chpStateRunning    | 90         | 8 * 115     || 95                        // tests case (true, true, false)
    chpStateRunning    | 90         | 10          || 95                        // tests case (true, true, true)
    chpStateRunning    | 90         | 7 * 115 + 1 || 95                        // test case (_, true, false) and demand covered together with chp
    chpStateRunning    | 90         | 9 * 115     || 95                        // test case (_, true, false) and demand not covered together with chp
    chpStateRunning    | 92         | 1           || 95                        // test case (true, true, true) and storage volume exceeds maximum
    /* The following tests do not exist: (false, false, false), (true, false, false) */
  }

  @Unroll
  def "Check total energy after calculating next state with #chpState and heat demand #heatDemand kWh:"() {
    given:
    def chpData = buildChpRelevantData(chpState, heatDemand)
    def thermalStorage = buildThermalStorage(storageInput, storageLvl)
    def chpModel = buildChpModel(thermalStorage)

    when:
    def nextState = chpModel.calculateNextState(chpData)
    def thermalEnergy = nextState.thermalEnergy()

    then:
    Math.abs(thermalEnergy.toKilowattHours() - expectedTotalEnergy) < TOLERANCE

    where:
    chpState           | storageLvl | heatDemand  || expectedTotalEnergy
    chpStateNotRunning | 90         | 0           || 0            // tests case (false, false, true)
    chpStateNotRunning | 90         | 8 * 115     || 100    // tests case (false, true, false)
    chpStateNotRunning | 90         | 10          || 0            // tests case (false, true, true)
    chpStateRunning    | 90         | 0           || 100            // tests case (true, false, true)
    chpStateRunning    | 90         | 8 * 115     || 100        // tests case (true, true, false)
    chpStateRunning    | 90         | 10          || 100            // tests case (true, true, true)
    chpStateRunning    | 90         | 7 * 115 + 1 || 100    // test case (_, true, false) and demand covered together with chp
    chpStateRunning    | 90         | 9 * 115     || 100        // test case (_, true, false) and demand not covered together with chp
    chpStateRunning    | 92         | 1           || 93                // test case (true, true, true) and storage volume exceeds maximum
    /* The following tests do not exist: (false, false, false), (true, false, false) */
  }

  def "Check storage level after calculating next state with #chpState and heat demand #heatDemand kWh:"() {
    given:
    def chpData = buildChpRelevantData(chpState, heatDemand)
    def thermalStorage = buildThermalStorage(storageInput, storageLvl)
    def chpModel = buildChpModel(thermalStorage)

    when:
    chpModel.calculateNextState(chpData)
    def storageLevel = CylindricalThermalStorage.energyToVolume(thermalStorage._storedEnergy(), thermalStorage.c(), thermalStorage.inletTemp(), thermalStorage.returnTemp())
    def resStorageLvl = Sq.create(storageLevel.toCubicMeters(), CubicMeters$.MODULE$)

    then:
    storageLevel ~= resStorageLvl

    where:
    chpState           | storageLvl | heatDemand || expectedStorageLevel
    chpStateNotRunning | 90d         | 0         || 90d                    // tests case (false, false, true)
    chpStateNotRunning | 90d         | 8 * 115   || 20d                // tests case (false, true, false)
    chpStateNotRunning | 90d         | 10        || 89.1304d                // tests case (false, true, true)
    chpStateRunning    | 90d         | 0         || 98.6956d                    // tests case (true, false, true)
    chpStateRunning    | 90d         | 8 * 115   || 20d                // tests case (true, true, false)
    chpStateRunning    | 90d         | 10        || 97.8260d                // tests case (true, true, true)
    chpStateRunning    | 90d         | 806       || 28.6086d                // test case (_, true, false) and demand covered together with chp
    chpStateRunning    | 90d         | 9 * 115   || 20d                // test case (_, true, false) and demand not covered together with chp
    chpStateRunning    | 92d         | 1         || 100d                        // test case (true, true, true) and storage volume exceeds maximum
    /* The following tests do not exist: (false, false, false), (true, false, false) */
  }

  def "Check time tick and running status after calculating next state with #chpState and heat demand #heatDemand kWh:"() {
    given:
    def chpData = buildChpRelevantData(chpState, heatDemand)
    def thermalStorage = buildThermalStorage(storageInput, storageLvl)
    def chpModel = buildChpModel(thermalStorage)

    when:
    def nextState = chpModel.calculateNextState(chpData)

    then:
    nextState.lastTimeTick() == expectedTimeTick
    nextState.isRunning() == expectedRunningStatus

    where:
    chpState           | storageLvl | heatDemand | expectedTimeTick | expectedRunningStatus
    chpStateNotRunning | 90         | 0         || 7200             | false            // tests case (false, false, true)
    chpStateNotRunning | 90         | 8 * 115   || 7200             | true    // tests case (false, true, false)
    chpStateNotRunning | 90         | 10        || 7200             | false        // tests case (false, true, true)
    chpStateRunning    | 90         | 0         || 7200             | true                // tests case (true, false, true)
    chpStateRunning    | 90         | 8 * 115   || 7200             | true        // tests case (true, true, false)
    chpStateRunning    | 90         | 10        || 7200             | true            // tests case (true, true, true)
    chpStateRunning    | 90         | 806       || 7200             | true            // test case (_, true, false) and demand covered together with chp
    chpStateRunning    | 90         | 9 * 115   || 7200             | true        // test case (_, true, false) and demand not covered together with chp
    chpStateRunning    | 92         | 1         || 7200             | false            // test case (true, true, true) and storage volume exceeds maximum
    /* The following tests do not exist: (false, false, false), (true, false, false) */
  }

  def "Check apply, validation and build method:"() {
    when:
    def thermalStorage = buildThermalStorage(storageInput, 90)
    def chpModelCaseClass = buildChpModel(thermalStorage)
    def chpModelCaseObject = ChpModel.apply(
        chpInput,
        OperationInterval.apply(0L, 86400L),
        null,
        thermalStorage)

    then:
    chpModelCaseClass.sRated() == chpModelCaseObject.sRated()
    chpModelCaseClass.cosPhiRated() == chpModelCaseObject.cosPhiRated()
    chpModelCaseClass.pThermal() == chpModelCaseObject.pThermal()
    chpModelCaseClass.storage() == chpModelCaseObject.storage()
  }
}
