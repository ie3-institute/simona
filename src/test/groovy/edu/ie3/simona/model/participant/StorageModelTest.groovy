/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.system.type.StorageTypeInput
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.Sq
import scala.Option
import spock.lang.Shared
import spock.lang.Specification
import squants.energy.*

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static tech.units.indriya.quantity.Quantities.getQuantity

class StorageModelTest extends Specification {

  @Shared
  StorageInput inputModel
  @Shared
  static final Double TOLERANCE = 1e-10

  def setupSpec() {
    def nodeInput = new NodeInput(
    UUID.fromString("ad39d0b9-5ad6-4588-8d92-74c7d7de9ace"),
    "NodeInput",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    getQuantity(1d, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.LV,
    -1)

    def typeInput = new StorageTypeInput(
    UUID.fromString("fbee4995-24dd-45e4-9c85-7d986fe99ff3"),
    "Test_StorageTypeInput",
    getQuantity(10000d, EURO),
    getQuantity(0.05d, EURO_PER_MEGAWATTHOUR),
    getQuantity(100d, KILOWATTHOUR),
    getQuantity(13d, KILOVOLTAMPERE),
    0.997,
    getQuantity(10d, KILOWATT),
    getQuantity(0.03, PU_PER_HOUR),
    getQuantity(0.9, PU),
    )

    inputModel = new StorageInput(
    UUID.randomUUID(),
    "Test_StorageInput",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInput,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    typeInput
    )
  }

  def buildStorageModel(Option<Double> targetSoc = Option.empty()) {
    return StorageModel.apply(inputModel, 1,
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z"),
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T01:00:00Z"),
    0d,
    targetSoc)
  }

  def "Calculate flex options"() {
    given:
    def storageModel = buildStorageModel()
    def startTick = 3600L
    def data = new StorageModel.StorageRelevantData(startTick + timeDelta)
    def oldState = new StorageModel.StorageState(
    Sq.create(lastStored.doubleValue(), KilowattHours$.MODULE$),
    Sq.create(lastPower.doubleValue(), Kilowatts$.MODULE$),
    startTick
    )

    when:
    def result = (ProvideMinMaxFlexOptions) storageModel.determineFlexOptions(data, oldState)

    then:
    Math.abs(result.ref().toKilowatts() - pRef) < TOLERANCE
    Math.abs(result.min().toKilowatts() - pMin) < TOLERANCE
    Math.abs(result.max().toKilowatts() - pMax) < TOLERANCE

    where:
    lastStored   | lastPower | timeDelta || pRef | pMin | pMax
    // UNCHANGED STATE
    // completely empty
    0            | 0         | 1         || 0    | 0    | 10
    // at a tiny bit above empty
    0.011d       | 0         | 1         || 0    | -10  | 10
    // at mid-level charge
    60           | 0         | 1         || 0    | -10  | 10
    // almost fully charged
    99.989d      | 0         | 1         || 0    | -10  | 10
    // fully charged
    100          | 0         | 1         || 0    | -10  | 0
    // CHANGED STATE
    // discharged to empty
    10           | -9        | 3600      || 0    | 0    | 10
    // almost discharged to lowest allowed charge
    10           | -9        | 3590      || 0    | -10  | 10
    // charged to mid-level charge
    41           | 10        | 3600      || 0    | -10  | 10
    // discharged to mid-level charge
    60           | -9        | 3600      || 0    | -10  | 10
    // almost fully charged
    95.5         | 4.98      | 3600      || 0    | -10  | 10
    // fully charged
    95.5         | 5         | 3600      || 0    | -10  | 0
  }

  def "Calculate flex options with target SOC"() {
    given:
    def storageModel = buildStorageModel(Option.apply(0.5d))
    def startTick = 3600L
    def data = new StorageModel.StorageRelevantData(startTick + 1)
    def oldState = new StorageModel.StorageState(
    Sq.create(lastStored.doubleValue(), KilowattHours$.MODULE$),
    Sq.create(0d, Kilowatts$.MODULE$),
    startTick
    )

    when:
    def result = (ProvideMinMaxFlexOptions) storageModel.determineFlexOptions(data, oldState)

    then:
    Math.abs(result.ref().toKilowatts() - pRef) < TOLERANCE
    Math.abs(result.min().toKilowatts() - pMin) < TOLERANCE
    Math.abs(result.max().toKilowatts() - pMax) < TOLERANCE

    where:
    lastStored || pRef | pMin | pMax
    // completely empty
    0          || 10   | 0    | 10
    // below margin of ref power target
    49.9974    || 10   | -10  | 10
    // within margin below ref power target
    49.9976    || 0    | -10  | 10
    // exactly at ref power target
    50         || 0    | -10  | 10
    // within margin above ref power target
    50.0030    || 0    | -10  | 10
    // above margin of ref power target
    50.0031    || -10  | -10  | 10
    // at mid-level charge
    60         || -10  | -10  | 10
    // fully charged
    100        || -10  | -10  | 0
  }

  def "Handle controlled power change"() {
    given:
    def storageModel = buildStorageModel()
    def startTick = 3600L
    def data = new StorageModel.StorageRelevantData(startTick + 1)
    def oldState = new StorageModel.StorageState(
    Sq.create(lastStored.doubleValue(), KilowattHours$.MODULE$),
    Sq.create(0d, Kilowatts$.MODULE$),
    startTick
    )

    when:
    def result = storageModel.handleControlledPowerChange(
    data,
    oldState,
    Sq.create(setPower.doubleValue(), Kilowatts$.MODULE$)
    )

    then:
    Math.abs(result._1.chargingPower().toKilowatts() - expPower.doubleValue()) < TOLERANCE
    result._1.tick() == startTick + 1
    Math.abs(result._1.storedEnergy().toKilowattHours() - lastStored.doubleValue()) < TOLERANCE
    def flexChangeIndication = result._2
    flexChangeIndication.changesAtTick().defined == expScheduled
    flexChangeIndication.changesAtTick().map(x -> x == startTick + 1 + expDelta).getOrElse(_ -> true)
    flexChangeIndication.changesAtNextActivation() == expActiveNext

    where:
    lastStored | setPower || expPower | expActiveNext | expScheduled | expDelta
    // no power
    0          | 0        || 0        | false         | false        | 0
    50         | 0        || 0        | false         | false        | 0
    100        | 0        || 0        | false         | false        | 0
    // charging on empty
    0          | 1        || 1        | true          | true         | 100 * 3600 / 0.9
    0          | 2.5      || 2.5      | true          | true         | 40 * 3600 / 0.9
    0          | 5        || 5        | true          | true         | 20 * 3600 / 0.9
    0          | 10       || 10       | true          | true         | 10 * 3600 / 0.9
    // charging on half full
    50         | 5        || 5        | false         | true         | 10 * 3600 / 0.9
    50         | 10       || 10       | false         | true         | 5 * 3600 / 0.9
    // discharging on half full
    50         | -4.5     || -4.5     | false         | true         | 10 * 3600
    50         | -9       || -9       | false         | true         | 5 * 3600
    // discharging on full
    100        | -4.5     || -4.5     | true          | true         | 20 * 3600
    100        | -9       || -9       | true          | true         | 10 * 3600
  }

  def "Handle controlled power change with ref target SOC"() {
    given:
    def storageModel = buildStorageModel(Option.apply(0.5d))
    def startTick = 3600L
    def data = new StorageModel.StorageRelevantData(startTick + 1)
    def oldState = new StorageModel.StorageState(
    Sq.create(lastStored.doubleValue(), KilowattHours$.MODULE$),
    Sq.create(0d, Kilowatts$.MODULE$),
    startTick
    )

    when:
    def result = storageModel.handleControlledPowerChange(
    data,
    oldState,
    Sq.create(setPower.doubleValue(), Kilowatts$.MODULE$)
    )

    then:
    Math.abs(result._1.chargingPower().toKilowatts() - expPower.doubleValue()) < TOLERANCE
    result._1.tick() == startTick + 1
    Math.abs(result._1.storedEnergy().toKilowattHours() - lastStored.doubleValue()) < TOLERANCE
    def flexChangeIndication = result._2
    flexChangeIndication.changesAtTick().defined == expScheduled
    flexChangeIndication.changesAtTick().map(x -> x == startTick + 1 + expDelta).getOrElse(_ -> true)
    flexChangeIndication.changesAtNextActivation() == expActiveNext

    where:
    lastStored | setPower || expPower | expActiveNext | expScheduled | expDelta
    // no power
    0          | 0        || 0        | false         | false        | 0
    50         | 0        || 0        | false         | false        | 0
    100        | 0        || 0        | false         | false        | 0
    // charging on empty
    0          | 1        || 1        | true          | true         | 50 * 3600 / 0.9
    0          | 2.5      || 2.5      | true          | true         | 20 * 3600 / 0.9
    0          | 5        || 5        | true          | true         | 10 * 3600 / 0.9
    0          | 10       || 10       | true          | true         | 5 * 3600 / 0.9
    // charging on target ref
    50         | 5        || 5        | true          | true         | 10 * 3600 / 0.9
    50         | 10       || 10       | true          | true         | 5 * 3600 / 0.9
    // discharging on target ref
    50         | -4.5     || -4.5     | true          | true         | 10 * 3600
    50         | -9       || -9       | true          | true         | 5 * 3600
    // discharging on full
    100        | -4.5     || -4.5     | true          | true         | 10 * 3600
    100        | -9       || -9       | true          | true         | 5 * 3600
  }

  def "Handle the edge case of discharging in tolerance margins"() {
    given:
    def storageModel = buildStorageModel()
    def startTick = 1800L
    def data = new StorageModel.StorageRelevantData(startTick + 1)
    // margin is at ~ 0.0030864 kWh
    def oldState = new StorageModel.StorageState(
    Sq.create(0.002d, KilowattHours$.MODULE$),
    Sq.create(0d, Kilowatts$.MODULE$),
    startTick
    )

    when:
    def result = storageModel.handleControlledPowerChange(
    data,
    oldState,
    Sq.create(-5d, Kilowatts$.MODULE$)
    )

    then:
    Math.abs(result._1.chargingPower().toKilowatts()) < TOLERANCE
    result._1.tick() == startTick + 1
    Math.abs(result._1.storedEnergy().toKilowattHours() - oldState.storedEnergy().toKilowattHours()) < TOLERANCE
    def flexChangeIndication = result._2
    !flexChangeIndication.changesAtTick().defined
    flexChangeIndication.changesAtNextActivation()
  }

  def "Handle the edge case of charging in tolerance margins"() {
    given:
    def storageModel = buildStorageModel()
    def startTick = 1800L
    def data = new StorageModel.StorageRelevantData(startTick + 1)
    // margin is at ~ 99.9975 kWh
    def oldState = new StorageModel.StorageState(
    Sq.create(99.999d, KilowattHours$.MODULE$),
    Sq.create(0d, Kilowatts$.MODULE$),
    startTick
    )

    when:
    def result = storageModel.handleControlledPowerChange(
    data,
    oldState,
    Sq.create(9d, Kilowatts$.MODULE$)
    )

    then:
    Math.abs(result._1.chargingPower().toKilowatts()) < TOLERANCE
    result._1.tick() == startTick + 1
    Math.abs(result._1.storedEnergy().toKilowattHours() - oldState.storedEnergy().toKilowattHours()) < TOLERANCE
    def flexChangeIndication = result._2
    !flexChangeIndication.changesAtTick().defined
    flexChangeIndication.changesAtNextActivation()
  }

  def "Handle the edge case of discharging in positive target margin"() {
    given:
    def storageModel = buildStorageModel(Option.apply(0.3d))
    def startTick = 1800L
    def data = new StorageModel.StorageRelevantData(startTick + 1)
    // margin is at ~ 30.0025 kWh
    def oldState = new StorageModel.StorageState(
    Sq.create(30.0024d, KilowattHours$.MODULE$),
    Sq.create(0d, Kilowatts$.MODULE$),
    startTick
    )

    when:
    def result = storageModel.handleControlledPowerChange(
    data,
    oldState,
    Sq.create(-9d, Kilowatts$.MODULE$)
    )

    then:
    Math.abs(result._1.chargingPower().toKilowatts() - (-9d)) < TOLERANCE
    result._1.tick() == startTick + 1
    Math.abs(result._1.storedEnergy().toKilowattHours() - oldState.storedEnergy().toKilowattHours()) < TOLERANCE
    def flexChangeIndication = result._2
    flexChangeIndication.changesAtTick() == Option.apply(startTick + 1L + 10801L)
    flexChangeIndication.changesAtNextActivation()
  }

  def "Handle the edge case of charging in negative target margin"() {
    given:
    def storageModel = buildStorageModel(Option.apply(0.4d))
    def startTick = 1800L
    def data = new StorageModel.StorageRelevantData(startTick + 1)
    // margin is at ~ 39.9975 kWh
    def oldState = new StorageModel.StorageState(
    Sq.create(39.998d, KilowattHours$.MODULE$),
    Sq.create(0d, Kilowatts$.MODULE$),
    startTick
    )

    when:
    def result = storageModel.handleControlledPowerChange(
    data,
    oldState,
    Sq.create(5d, Kilowatts$.MODULE$)
    )

    then:
    Math.abs(result._1.chargingPower().toKilowatts() - (5d)) < TOLERANCE
    result._1.tick() == startTick + 1
    Math.abs(result._1.storedEnergy().toKilowattHours() - oldState.storedEnergy().toKilowattHours()) < TOLERANCE
    def flexChangeIndication = result._2
    flexChangeIndication.changesAtTick() == Option.apply(startTick + 1L + 48002L)
    flexChangeIndication.changesAtNextActivation()
  }
}
