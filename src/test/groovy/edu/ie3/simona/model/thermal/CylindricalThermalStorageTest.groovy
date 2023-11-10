/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.KilowattHoursPerKelvinCubicMeters$
import edu.ie3.util.scala.quantities.Sq
import squants.energy.KilowattHours$
import squants.energy.Kilowatts$
import squants.space.CubicMeters$
import squants.thermal.Celsius$
import tech.units.indriya.unit.Units

import static tech.units.indriya.quantity.Quantities.getQuantity

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import spock.lang.Shared
import spock.lang.Specification

class CylindricalThermalStorageTest extends Specification {

  @Shared
  CylindricalStorageInput storageInput

  def setupSpec() {
    storageInput = new CylindricalStorageInput(
        UUID.randomUUID(),
        "ThermalStorage",
        null,
        getQuantity(100d, StandardUnits.VOLUME),
        getQuantity(20d, StandardUnits.VOLUME),
        getQuantity(30d, StandardUnits.TEMPERATURE),
        getQuantity(40d, StandardUnits.TEMPERATURE),
        getQuantity(1.15d, StandardUnits.SPECIFIC_HEAT_CAPACITY))
  }

  static def buildThermalStorage(CylindricalStorageInput storageInput, Double volume) {
    def storedEnergy =
        CylindricalThermalStorage.volumeToEnergy(
        Sq.create(volume, CubicMeters$.MODULE$),
        Sq.create(storageInput.c.value.doubleValue(), KilowattHoursPerKelvinCubicMeters$.MODULE$),
        Sq.create(storageInput.inletTemp.value.doubleValue(), Celsius$.MODULE$),
        Sq.create(storageInput.returnTemp.value.doubleValue(), Celsius$.MODULE$)
        )
    def thermalStorage = CylindricalThermalStorage.apply(storageInput, storedEnergy)
    return thermalStorage
  }

  def vol2Energy(Double volume) {
    return CylindricalThermalStorage.volumeToEnergy(
        Sq.create(volume, CubicMeters$.MODULE$),
        Sq.create(storageInput.c.to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE).value.doubleValue(), KilowattHoursPerKelvinCubicMeters$.MODULE$),
        Sq.create(storageInput.inletTemp.to(Units.CELSIUS).value.doubleValue(), Celsius$.MODULE$),
        Sq.create(storageInput.returnTemp.to(Units.CELSIUS).value.doubleValue(), Celsius$.MODULE$)
        )
  }

  def "Check storage level operations:"() {
    given:
    def storage = buildThermalStorage(storageInput, 70)

    when:
    def initialLevel = storage._storedEnergy()
    storage._storedEnergy_$eq(vol2Energy(50))
    def newLevel1 = storage._storedEnergy()
    def surplus = storage.tryToStoreAndReturnRemainder(vol2Energy(55)).get()
    def newLevel2 = storage._storedEnergy()
    def isCovering = storage.isDemandCoveredByStorage(Sq.create(5d, KilowattHours$.MODULE$))
    def lack = storage.tryToTakeAndReturnLack(vol2Energy(95)).get()
    def newLevel3 = storage._storedEnergy()
    def notCovering = storage.isDemandCoveredByStorage(Sq.create(1d, KilowattHours$.MODULE$))
    then:
    initialLevel =~ vol2Energy(70d)
    newLevel1=~ vol2Energy(50d)
    surplus=~ vol2Energy(5d)
    newLevel2=~ vol2Energy(100d)
    lack=~ vol2Energy(15d)
    newLevel3=~ vol2Energy(20d)
    isCovering
    !notCovering
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
    def storage = buildThermalStorage(storageInput, 70d)
    def lastState = new ThermalStorage.ThermalStorageState(tick, Sq.create(storedEnergy, KilowattHours$.MODULE$), Sq.create(qDot,Kilowatts$.MODULE$))
    def result = storage.updateState(newTick, Sq.create(newQDot, Kilowatts$.MODULE$), lastState)

    then:
    result._1().storedEnergy() =~ Sq.create(expectedStoredEnergy, KilowattHours$.MODULE$)
    result._2.defined
    result._2.get() == expectedThreshold

    where:
    tick | storedEnergy | qDot   | newTick | newQDot  || expectedStoredEnergy  | expectedThreshold
    0L   | 250.0d       | 10.0d  | 3600L   | 42.0d    || 260.0d                | new ThermalStorage.ThermalStorageThreshold.StorageFull(79885L)
    0L   | 250.0d       | 10.0d  | 3600L   | -42.0d   || 260.0d                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(6171L)
    0L   | 250.0d       | -10.0d | 3600L   | 42.0d    || 240.0d                | new ThermalStorage.ThermalStorageThreshold.StorageFull(81600L)
    0L   | 250.0d       | -10.0d | 3600L   | -42.0d   || 240.0d                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(4457L)
    0L   | 250.0d       | -10.0d | 3600L   | -42.0d   || 240.0d                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(4457L)
    0L   | 1000.0d      | 149.0d | 3600L   | 5000.0d  || 1149.0d               | new ThermalStorage.ThermalStorageThreshold.StorageFull(3600L)
    0L   | 240.0d       | -9.0d  | 3600L   | -5000.0d || 231.0d                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(3600L)
  }

  def "Check mutable state update, if no threshold is reached:"() {
    when:
    def storage = buildThermalStorage(storageInput, 70)
    def lastState = new ThermalStorage.ThermalStorageState(tick, Sq.create(storedEnergy, KilowattHours$.MODULE$), Sq.create(qDot, Kilowatts$.MODULE$))
    def result = storage.updateState(newTick, Sq.create(newQDot, Kilowatts$.MODULE$), lastState)

    then:
    result._1().storedEnergy() =~ Sq.create(expectedStoredEnergy, KilowattHours$.MODULE$)
    result._2.empty

    where:
    tick | storedEnergy  | qDot   | newTick | newQDot  || expectedStoredEnergy
    0L   | 250.0d        | 10.0d  | 3600L   | 0.0d     || 260.0d
    0L   | 250.0d        | -10.0d | 3600L   | 0.0d     || 240.0d
  }
}
