/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.util.quantities.QuantityUtil
import spock.lang.Shared
import spock.lang.Specification

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static tech.units.indriya.quantity.Quantities.getQuantity

class CylindricalThermalStorageTest extends Specification {

  static double TESTING_TOLERANCE = 1e-10

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
    def storedEnergy = CylindricalThermalStorage.volumeToEnergy(getQuantity(volume, StandardUnits.VOLUME), storageInput.c, storageInput.inletTemp, storageInput.returnTemp)
    def thermalStorage = CylindricalThermalStorage.apply(storageInput, storedEnergy)
    return thermalStorage
  }

  static def vol2Energy(CylindricalThermalStorage storage, Double volume) {
    return CylindricalThermalStorage.volumeToEnergy(getQuantity(volume, StandardUnits.VOLUME),storage.c(), storage.inletTemp(), storage.returnTemp())
  }

  def "Check storage level operations:"() {
    given:
    def storage = buildThermalStorage(storageInput, 70)

    when:
    def initialLevel = storage._storedEnergy()
    storage._storedEnergy_$eq(vol2Energy(storage, 50))
    def newLevel1 = storage._storedEnergy()
    def surplus = storage.tryToStoreAndReturnRemainder(vol2Energy(storage, 55)).get()
    def newLevel2 = storage._storedEnergy()
    def isCovering = storage.isDemandCoveredByStorage(getQuantity(5, KILOWATTHOUR))
    def lack = storage.tryToTakeAndReturnLack(vol2Energy(storage, 95)).get()
    def newLevel3 = storage._storedEnergy()
    def notCovering = storage.isDemandCoveredByStorage(getQuantity(1, KILOWATTHOUR))

    then:
    QuantityUtil.isEquivalentAbs(initialLevel, vol2Energy(storage, 70), TESTING_TOLERANCE)
    QuantityUtil.isEquivalentAbs(newLevel1, vol2Energy(storage, 50), TESTING_TOLERANCE)
    QuantityUtil.isEquivalentAbs(surplus, vol2Energy(storage, 5), TESTING_TOLERANCE)
    QuantityUtil.isEquivalentAbs(newLevel2, vol2Energy(storage, 100), TESTING_TOLERANCE)
    QuantityUtil.isEquivalentAbs(lack, vol2Energy(storage, 15), TESTING_TOLERANCE)
    QuantityUtil.isEquivalentAbs(newLevel3, vol2Energy(storage, 20), TESTING_TOLERANCE)
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
    QuantityUtil.isEquivalentAbs(usableThermalEnergy, getQuantity(5 * 115, KILOWATTHOUR), TESTING_TOLERANCE)
    QuantityUtil.isEquivalentAbs(volumeFromUsableEnergy, getQuantity(50, StandardUnits.VOLUME), TESTING_TOLERANCE)
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
    storage.storageVolumeLvlMax() == storageInput.storageVolumeLvl
    storage.storageVolumeLvlMin() == storageInput.storageVolumeLvlMin
    storage.inletTemp() == storageInput.inletTemp
    storage.returnTemp() == storageInput.returnTemp
    storage.c() == storageInput.c
  }
}
