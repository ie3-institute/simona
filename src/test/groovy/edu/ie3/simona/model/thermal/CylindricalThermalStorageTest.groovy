/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR
import static edu.ie3.util.quantities.QuantityUtil.isEquivalentAbs
import static tech.units.indriya.quantity.Quantities.getQuantity

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.util.quantities.QuantityUtil
import spock.lang.Shared
import spock.lang.Specification
import tech.units.indriya.quantity.Quantities

class CylindricalThermalStorageTest extends Specification {

    static final double TESTING_TOLERANCE = 1e-10@Shared
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
        def storedEnergy = CylindricalThermalStorage.volumeToEnergy(getQuantity(volume, StandardUnits.VOLUME), storageInput.c, storageInput.inletTemp, storageInput.returnTemp)
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
        def initialLevel = storage._storedEnergy()
        storage._storedEnergy_$eq(vol2Energy(50))
        def newLevel1 = storage._storedEnergy()
        def surplus = storage.tryToStoreAndReturnRemainder(vol2Energy(55)).get()
        def newLevel2 = storage._storedEnergy()
        def isCovering = storage.isDemandCoveredByStorage(getQuantity(5, KILOWATTHOUR))
        def lack = storage.tryToTakeAndReturnLack(vol2Energy(95)).get()
        def newLevel3 = storage._storedEnergy()
        def notCovering = storage.isDemandCoveredByStorage(getQuantity(1, KILOWATTHOUR))

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
    isEquivalentAbs(usableThermalEnergy, getQuantity(5 * 115, KILOWATTHOUR), TESTING_TOLERANCE)
    isEquivalentAbs(volumeFromUsableEnergy, getQuantity(50, StandardUnits.VOLUME), TESTING_TOLERANCE)
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
        def lastState = new ThermalStorage.ThermalStorageState(tick, Quantities.getQuantity(storedEnergy, KILOWATTHOUR), Quantities.getQuantity(qDot, KILOWATT))
        def result = storage.updateState(newTick, Quantities.getQuantity(newQDot, KILOWATT), lastState)

        then:
        QuantityUtil.equals(result._1().storedEnergy(), Quantities.getQuantity(expectedStoredEnergy, KILOWATTHOUR), TOLERANCE)
        result._2.defined
        result._2.get() == expectedThreshold

        where:
        tick | storedEnergy | qDot  | newTick | newQDot || expectedStoredEnergy | expectedThreshold
        0L   | 250.0        | 10.0  | 3600L   | 42.0    || 260.0                | new ThermalStorage.ThermalStorageThreshold.StorageFull(79885L)
        0L   | 250.0        | 10.0  | 3600L   | -42.0   || 260.0                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(6171L)
        0L   | 250.0        | -10.0 | 3600L   | 42.0    || 240.0                | new ThermalStorage.ThermalStorageThreshold.StorageFull(81600L)
        0L   | 250.0        | -10.0 | 3600L   | -42.0   || 240.0                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(4457L)
        0L   | 250.0        | -10.0 | 3600L   | -42.0   || 240.0                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(4457L)
        0L   | 1000.0       | 149.0 | 3600L   | 5000.0  || 1149.0               | new ThermalStorage.ThermalStorageThreshold.StorageFull(3600L)
        0L   | 240.0        | -9.0  | 3600L   | -5000.0 || 231.0                | new ThermalStorage.ThermalStorageThreshold.StorageEmpty(3600L)
    }

    def "Check mutable state update, if no threshold is reached:"() {
        when:
        def storage = buildThermalStorage(storageInput, 70)
        def lastState = new ThermalStorage.ThermalStorageState(tick, Quantities.getQuantity(storedEnergy, KILOWATTHOUR), Quantities.getQuantity(qDot, KILOWATT))
        def result = storage.updateState(newTick, Quantities.getQuantity(newQDot, KILOWATT), lastState)

        then:
        QuantityUtil.equals(result._1().storedEnergy(), Quantities.getQuantity(expectedStoredEnergy, KILOWATTHOUR), TOLERANCE)
        result._2.empty

        where:
        tick | storedEnergy | qDot  | newTick | newQDot || expectedStoredEnergy
        0L   | 250.0        | 10.0  | 3600L   | 0.0     || 260.0
        0L   | 250.0        | -10.0 | 3600L   | 0.0     || 240.0
    }
}
