/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.util.scala.quantities.KilowattHoursPerKelvinCubicMeters$

import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR

import static tech.units.indriya.quantity.Quantities.getQuantity

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.util.scala.quantities.Sq

import spock.lang.Shared
import spock.lang.Specification
import squants.energy.*
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
    def volumeFromUsableEnergy = CylindricalThermalStorage.energyToVolume(usableThermalEnergy, storage.c(), storage.inletTemp(), storage.returnTemp())

    then:
    Math.abs(usableThermalEnergy.toKilowattHours()) - 50 * 10 * 1.15 < TESTING_TOLERANCE
    Math.abs(volumeFromUsableEnergy.toCubicMeters()) - 50 < TESTING_TOLERANCE
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
    storage.storageVolumeLvlMax() =~ Sq.create(storageInput.storageVolumeLvl.value.doubleValue(), CubicMeters$.MODULE$)
    storage.storageVolumeLvlMin() =~ Sq.create(storageInput.storageVolumeLvlMin.value.doubleValue(), CubicMeters$.MODULE$)
    storage.inletTemp() =~ Sq.create(storageInput.inletTemp.value.doubleValue(), Celsius$.MODULE$)
    storage.returnTemp() =~ Sq.create(storageInput.returnTemp.value.doubleValue(), Celsius$.MODULE$)
    storage.c().value().doubleValue() =~ storageInput.c.value.doubleValue()
  }
}
