/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.util.scala.quantities.Sq
import spock.lang.Shared
import spock.lang.Specification
import squants.energy.*

import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR
import static edu.ie3.util.quantities.QuantityUtil.isEquivalentAbs
import static tech.units.indriya.quantity.Quantities.getQuantity

class CylindricalThermalStorageTest extends Specification {

  static final double TESTING_TOLERANCE = 1e-10

  @Shared
  CylindricalStorageInput storageInput
  @Shared
  static final Double TOLERANCE = 0.0001

  def setupSpec() {
    storageInput = new CylindricalStorageInput(
        UUID.randomUUID(),
        "ThermalStorage",
        null,
        getQuantity(100, StandardUnits.VOLUME),
        getQuantity(20, StandardUnits.VOLUME),
        getQuantity(30, StandardUnits.TEMPERATURE),
        getQuantity(40, StandardUnits.TEMPERATURE),
        getQuantity(1.15, StandardUnits.SPECIFIC_HEAT_CAPACITY))
  }

  static def buildThermalStorage(CylindricalStorageInput storageInput, Double volume) {
    def storedEnergy =
        Sq.create(
        CylindricalThermalStorage.volumeToEnergy(getQuantity(volume, StandardUnits.VOLUME), storageInput.c, storageInput.inletTemp, storageInput.returnTemp)
        .to(KILOWATTHOUR)
        .getValue()
        .doubleValue(),
        KilowattHours$.MODULE$
        )
    def thermalStorage = CylindricalThermalStorage.apply(storageInput, storedEnergy)
    return thermalStorage
  }

  def vol2Energy(Double volume) {
    return CylindricalThermalStorage.volumeToEnergy(getQuantity(volume, StandardUnits.VOLUME), storageInput.c, storageInput.inletTemp, storageInput.returnTemp)
  }

  def "Check storage level operations:"() {
    given:
    def storage = buildThermalStorage(storageInput, 70)

    when:
    def initialLevel = getQuantity(storage._storedEnergy().toKilowattHours(), KILOWATTHOUR)
    storage._storedEnergy_$eq(
        Sq.create(vol2Energy(50).to(KILOWATTHOUR).getValue().doubleValue(),
        KilowattHours$.MODULE$)
        )
    def newLevel1 = getQuantity(storage._storedEnergy().toKilowattHours(), KILOWATTHOUR)
    def surplus = getQuantity(storage.tryToStoreAndReturnRemainder(
        Sq.create(vol2Energy(55).to(KILOWATTHOUR).getValue().doubleValue(),
        KilowattHours$.MODULE$)
        ).get().toKilowattHours(), KILOWATTHOUR)
    def newLevel2 = getQuantity(storage._storedEnergy().toKilowattHours(), KILOWATTHOUR)
    def isCovering = storage.isDemandCoveredByStorage(Sq.create(5, KilowattHours$.MODULE$))
    def lack = getQuantity(
        storage.tryToTakeAndReturnLack(
        Sq.create(vol2Energy(95).to(KILOWATTHOUR).getValue().doubleValue(),
        KilowattHours$.MODULE$)
        ).get().toKilowattHours(),
        KILOWATTHOUR
        )
    def newLevel3 = getQuantity(storage._storedEnergy().toKilowattHours(), KILOWATTHOUR)
    def notCovering = storage.isDemandCoveredByStorage(Sq.create(1, KilowattHours$.MODULE$))

    then:
    isEquivalentAbs(initialLevel, vol2Energy(70), TESTING_TOLERANCE)
    isEquivalentAbs(newLevel1, vol2Energy(50), TESTING_TOLERANCE)
    isEquivalentAbs(surplus, vol2Energy(5), TESTING_TOLERANCE)
    isEquivalentAbs(newLevel2, vol2Energy(100), TESTING_TOLERANCE)
    isEquivalentAbs(lack, vol2Energy(15), TESTING_TOLERANCE)
    isEquivalentAbs(newLevel3, vol2Energy(20), TESTING_TOLERANCE)
    isCovering
    !notCovering
  }

  def "Check converting methods:"() {
    given:
    def storage = buildThermalStorage(storageInput, 70)

    when:
    def usableThermalEnergy = storage.usableThermalEnergy()

    then:
    Math.abs(usableThermalEnergy.toKilowattHours() - 5 * 115) < TESTING_TOLERANCE
  }

  def "Check apply, validation and build method:"() {
    when:
    def storage = buildThermalStorage(storageInput, 70)

    then:
    storage.uuid() == storageInput.uuid
    storage.id() == storageInput.id
    storage.operatorInput() == storageInput.operator
    storage.operationTime() == storageInput.operationTime
    storage.bus() == storageInput.thermalBus
  }

  def "Check mutable state update:"() {
    when:
    def storage = buildThermalStorage(storageInput, 70)
    def lastState = new ThermalStorage.ThermalStorageState(tick, Sq.create(storedEnergy, KilowattHours$.MODULE$), Sq.create(qDot, Kilowatts$.MODULE$))
    def result = storage.updateState(newTick, Sq.create(newQDot, Kilowatts$.MODULE$), lastState)

    then:
    Math.abs(result._1().storedEnergy().toKilowattHours() - expectedStoredEnergy.doubleValue()) < TOLERANCE
    result._2.defined
    result._2.get() == expectedThreshold

    where:
    tick | storedEnergy | qDot  | newTick | newQDot || expectedStoredEnergy | expectedThreshold
    0L   | 250.0        | 10.0  | 3600L   | 42.0    || 260.0                | new ThermalStorage.ThermalStorageThreshold.StorageFull(79886L)
    0L   | 250.0        | 10.0  | 3600L   | -42.0   || 260.0                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(6171L)
    0L   | 250.0        | -10.0 | 3600L   | 42.0    || 240.0                | new ThermalStorage.ThermalStorageThreshold.StorageFull(81600L)
    0L   | 250.0        | -10.0 | 3600L   | -42.0   || 240.0                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(4457L)
    0L   | 250.0        | -10.0 | 3600L   | -42.0   || 240.0                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(4457L)
    0L   | 1000.0       | 149.0 | 3600L   | 5000.0  || 1149.0               | new ThermalStorage.ThermalStorageThreshold.StorageFull(3601L)
    0L   | 240.0        | -9.0  | 3600L   | -5000.0 || 231.0                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(3601L)
  }

  def "Check mutable state update, if no threshold is reached:"() {
    when:
    def storage = buildThermalStorage(storageInput, 70)
    def lastState = new ThermalStorage.ThermalStorageState(tick, Sq.create(storedEnergy, KilowattHours$.MODULE$), Sq.create(qDot, Kilowatts$.MODULE$))
    def result = storage.updateState(newTick, Sq.create(newQDot, Kilowatts$.MODULE$), lastState)

    then:
    Math.abs(result._1().storedEnergy().toKilowattHours() - expectedStoredEnergy.doubleValue()) < TOLERANCE
    result._2.empty

    where:
    tick | storedEnergy | qDot  | newTick | newQDot || expectedStoredEnergy
    0L   | 250.0        | 10.0  | 3600L   | 0.0     || 260.0
    0L   | 250.0        | -10.0 | 3600L   | 0.0     || 240.0
  }
}
