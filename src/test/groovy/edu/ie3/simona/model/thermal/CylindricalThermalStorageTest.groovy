/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.util.scala.quantities.Sq
import edu.ie3.util.scala.quantities.WattHoursPerKelvinCubicMeters$
import spock.lang.Shared
import spock.lang.Specification
import squants.energy.*
import squants.space.CubicMeters$
import squants.thermal.Celsius$

import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR
import static edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE
import static edu.ie3.util.quantities.QuantityUtil.isEquivalentAbs
import static tech.units.indriya.quantity.Quantities.getQuantity
import static tech.units.indriya.unit.Units.CELSIUS
import static tech.units.indriya.unit.Units.CUBIC_METRE

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
        CylindricalThermalStorage.volumeToEnergy(
        Sq.create(volume, CubicMeters$.MODULE$),
        Sq.create(storageInput.c.value.doubleValue(), WattHoursPerKelvinCubicMeters$.MODULE$),
        Sq.create(storageInput.inletTemp.value.doubleValue(), Celsius$.MODULE$),
        Sq.create(storageInput.returnTemp.value.doubleValue(), Celsius$.MODULE$)
        )
    def thermalStorage = CylindricalThermalStorage.apply(storageInput, storedEnergy)
    return thermalStorage
  }

  def vol2Energy(Double volume) {
    return CylindricalThermalStorage.volumeToEnergy(
        Sq.create(volume, CubicMeters$.MODULE$),
        Sq.create(storageInput.c.value.doubleValue(), WattHoursPerKelvinCubicMeters$.MODULE$),
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
    initialLevel =~ vol2Energy(70d)
    newLevel1 =~ vol2Energy(50d)
    surplus =~ vol2Energy(5d)
    newLevel2 =~ vol2Energy(100d)
    lack =~ vol2Energy(15d)
    newLevel3 =~ vol2Energy(20d)
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
    storage.storageVolumeLvlMax() =~ storageInput.storageVolumeLvl
    storage.storageVolumeLvlMin() =~ storageInput.storageVolumeLvlMin
    storage.inletTemp() =~ storageInput.inletTemp
    storage.returnTemp() =~ storageInput.returnTemp
    storage.c() =~ storageInput.c
  }
}
