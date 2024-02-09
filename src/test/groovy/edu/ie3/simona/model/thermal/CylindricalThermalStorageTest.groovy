/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR
import static tech.units.indriya.quantity.Quantities.getQuantity

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.util.scala.quantities.KilowattHoursPerKelvinCubicMeters$
import edu.ie3.util.scala.quantities.Sq
import spock.lang.Shared
import spock.lang.Specification
import squants.energy.KilowattHours$
import squants.energy.Kilowatts$
import squants.space.CubicMeters$
import squants.thermal.Celsius$

class CylindricalThermalStorageTest extends Specification {

  static final double TESTING_TOLERANCE = 1e-10

  @Shared
  CylindricalStorageInput storageInput

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
    def storedEnergy = CylindricalThermalStorage.volumeToEnergy(Sq.create(volume, CubicMeters$.MODULE$),
        Sq.create(storageInput.c.value.doubleValue(), KilowattHoursPerKelvinCubicMeters$.MODULE$),
        Sq.create(storageInput.inletTemp.value.doubleValue(), Celsius$.MODULE$),
        Sq.create(storageInput.returnTemp.value.doubleValue(), Celsius$.MODULE$))
    def thermalStorage = CylindricalThermalStorage.apply(storageInput, storedEnergy)
    return thermalStorage
  }

  def vol2Energy(Double volume) {
    return CylindricalThermalStorage.volumeToEnergy( // FIXME below: get values in units with to..()
        Sq.create(volume, CubicMeters$.MODULE$),
        Sq.create(storageInput.c.value.doubleValue(), KilowattHoursPerKelvinCubicMeters$.MODULE$),
        Sq.create(storageInput.inletTemp.value.doubleValue(), Celsius$.MODULE$),
        Sq.create(storageInput.returnTemp.value.doubleValue(), Celsius$.MODULE$))
  }

  def "Check storage level operations:"() {
    given:
    def storage = buildThermalStorage(storageInput, 70)

    when:
    def initialLevel =
        getQuantity(storage._storedEnergy().toKilowattHours(), KILOWATTHOUR)
    storage._storedEnergy_$eq(vol2Energy(50d),)
    def newLevel1 = getQuantity(storage._storedEnergy().toKilowattHours(), KILOWATTHOUR)
    def surplus = storage.tryToStoreAndReturnRemainder(
        vol2Energy(55d))
    def newLevel2 = getQuantity(storage._storedEnergy().toKilowattHours(), KILOWATTHOUR)
    def isCovering = storage.isDemandCoveredByStorage(Sq.create(5, KilowattHours$.MODULE$))
    def lack =
        storage.tryToTakeAndReturnLack(
        vol2Energy(95d)
        )
    def newLevel3 = getQuantity(storage._storedEnergy().toKilowattHours(), KILOWATTHOUR)
    def notCovering = storage.isDemandCoveredByStorage(Sq.create(1, KilowattHours$.MODULE$))

    then:
    initialLevel.value.doubleValue() =~ vol2Energy(70d).toKilowattHours()
    newLevel1.value.doubleValue() =~ vol2Energy(50d).toKilowattHours()
    surplus =~ vol2Energy(5d)
    newLevel2.value.doubleValue() =~ vol2Energy(100d).toKilowattHours()
    lack =~ vol2Energy(15d)
    newLevel3.value.doubleValue() =~ vol2Energy(20d).toKilowattHours()
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
    Math.abs(result._1().storedEnergy().toKilowattHours() - expectedStoredEnergy.doubleValue()) < TESTING_TOLERANCE
    result._2.defined
    result._2.get() == expectedThreshold

    where:
    tick | storedEnergy  | qDot   | newTick | newQDot  || expectedStoredEnergy  | expectedThreshold
    0L   | 250.0d        | 10.0d  | 3600L   | 42.0d    || 260.0d                | new ThermalStorage.ThermalStorageThreshold.StorageFull(79886L)
    0L   | 250.0d        | 10.0d  | 3600L   | -42.0d   || 260.0d                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(6171L)
    0L   | 250.0d        | -10.0d | 3600L   | 42.0d    || 240.0d                | new ThermalStorage.ThermalStorageThreshold.StorageFull(81600L)
    0L   | 250.0d        | -10.0d | 3600L   | -42.0d   || 240.0d                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(4457L)
    0L   | 250.0d        | -10.0d | 3600L   | -42.0d   || 240.0d                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(4457L)
    0L   | 1000.0d       | 149.0d | 3600L   | 5000.0d  || 1149.0d               | new ThermalStorage.ThermalStorageThreshold.StorageFull(3601L)
    0L   | 240.0d        | -9.0d  | 3600L   | -5000.0d || 231.0d                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(3601L)
  }

  def "Check mutable state update, if no threshold is reached:"() {
    when:
    def storage = buildThermalStorage(storageInput, 70)
    def lastState = new ThermalStorage.ThermalStorageState(tick, Sq.create(storedEnergy, KilowattHours$.MODULE$), Sq.create(qDot, Kilowatts$.MODULE$))
    def result = storage.updateState(newTick, Sq.create(newQDot, Kilowatts$.MODULE$), lastState)

    then:
    Math.abs(result._1().storedEnergy().toKilowattHours() - expectedStoredEnergy.doubleValue()) < TESTING_TOLERANCE
    result._2.empty

    where:
    tick | storedEnergy  | qDot   | newTick | newQDot  || expectedStoredEnergy
    0L   | 250.0d        | 10.0d  | 3600L   | 0.0d     || 260.0d
    0L   | 250.0d        | -10.0d | 3600L   | 0.0d     || 240.0d
  }
}
