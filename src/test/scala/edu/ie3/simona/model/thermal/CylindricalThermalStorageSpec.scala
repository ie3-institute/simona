/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.KilowattHoursPerKelvinCubicMeters
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import squants.Energy
import squants.energy.{KilowattHours, Kilowatts}
import squants.space.{CubicMeters, Volume}
import squants.thermal.Celsius
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.Units

import java.util.UUID

class CylindricalThermalStorageSpec
    extends UnitSpec
    with Matchers
    with BeforeAndAfterAll {

  implicit val tolerance: Energy = KilowattHours(1e-10)

  lazy val storageInput = new CylindricalStorageInput(
    UUID.randomUUID(),
    "ThermalStorage",
    null,
    getQuantity(100, StandardUnits.VOLUME),
    getQuantity(0, StandardUnits.VOLUME),
    getQuantity(30, StandardUnits.TEMPERATURE),
    getQuantity(40, StandardUnits.TEMPERATURE),
    getQuantity(1.15, StandardUnits.SPECIFIC_HEAT_CAPACITY),
  )

  def buildThermalStorage(
      storageInput: CylindricalStorageInput,
      volume: Volume,
  ): CylindricalThermalStorage = {
    val storedEnergy = CylindricalThermalStorage.volumeToEnergy(
      volume,
      KilowattHoursPerKelvinCubicMeters(
        storageInput.getC
          .to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE)
          .getValue
          .doubleValue
      ),
      Celsius(
        storageInput.getInletTemp.to(Units.CELSIUS).getValue.doubleValue()
      ),
      Celsius(
        storageInput.getReturnTemp.to(Units.CELSIUS).getValue.doubleValue()
      ),
    )
    CylindricalThermalStorage(storageInput, storedEnergy)
  }

  def vol2Energy(volume: Volume): Energy = {
    CylindricalThermalStorage.volumeToEnergy(
      volume,
      KilowattHoursPerKelvinCubicMeters(
        storageInput.getC
          .to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE)
          .getValue
          .doubleValue
      ),
      Celsius(
        storageInput.getInletTemp.to(Units.CELSIUS).getValue.doubleValue()
      ),
      Celsius(
        storageInput.getReturnTemp.to(Units.CELSIUS).getValue.doubleValue()
      ),
    )
  }

  "CylindricalThermalStorage Model" should {

    "Check storage level operations:" in {
      val storage = buildThermalStorage(storageInput, CubicMeters(70))

      val initialLevel = storage._storedEnergy
      storage._storedEnergy_=(vol2Energy(CubicMeters(50)))
      val newLevel1 = storage._storedEnergy
      val surplus =
        storage.tryToStoreAndReturnRemainder(vol2Energy(CubicMeters(55)))
      val newLevel2 = storage._storedEnergy
      val isCovering = storage.isDemandCoveredByStorage(KilowattHours(5))
      val lack = storage.tryToTakeAndReturnLack(vol2Energy(CubicMeters(115)))
      val newLevel3 = storage._storedEnergy
      val notCovering = storage.isDemandCoveredByStorage(KilowattHours(1))

      initialLevel should approximate(vol2Energy(CubicMeters(70)))
      newLevel1 should approximate(vol2Energy(CubicMeters(50)))
      surplus.value shouldBe vol2Energy(CubicMeters(5))
      newLevel2 should approximate(vol2Energy(CubicMeters(100)))
      lack.value shouldBe vol2Energy(CubicMeters(15))
      newLevel3 should approximate(vol2Energy(CubicMeters(0)))
      isCovering shouldBe true
      notCovering shouldBe false
    }

    "Converting methods work correctly" in {
      val storage = buildThermalStorage(storageInput, CubicMeters(70))

      val usableThermalEnergy = storage.usableThermalEnergy
      usableThermalEnergy should approximate(KilowattHours(805))
    }

    "Apply, validation, and build method work correctly" in {
      val storage = buildThermalStorage(storageInput, CubicMeters(70))

      storage.uuid shouldBe storageInput.getUuid
      storage.id shouldBe storageInput.getId
      storage.operatorInput shouldBe storageInput.getOperator
      storage.operationTime shouldBe storageInput.getOperationTime
      storage.bus shouldBe storageInput.getThermalBus
    }

    "Check mutable state update correctly update the state with thresholds" in {
      val cases = Table(
        (
          "tick",
          "storedEnergy",
          "qDot",
          "newTick",
          "newQDot",
          "expectedStoredEnergy",
          "expectedThreshold",
        ),
        (
          0L,
          250.0,
          10.0,
          3600L,
          42.0,
          260.0,
          ThermalStorage.ThermalStorageThreshold.StorageFull(79886L),
        ),
        (
          0L,
          250.0,
          10.0,
          3600L,
          -42.0,
          260.0,
          ThermalStorage.ThermalStorageThreshold.StorageEmpty(25886L),
        ),
        (
          0L,
          250.0,
          -10.0,
          3600L,
          42.0,
          240.0,
          ThermalStorage.ThermalStorageThreshold.StorageFull(81600L),
        ),
        (
          0L,
          250.0,
          -10.0,
          3600L,
          -42.0,
          240.0,
          ThermalStorage.ThermalStorageThreshold.StorageEmpty(24171L),
        ),
        (
          0L,
          1000.0,
          149.0,
          3600L,
          5000.0,
          1149.0,
          ThermalStorage.ThermalStorageThreshold.StorageFull(3601L),
        ),
        (
          0L,
          240.0,
          -9.0,
          3600L,
          -5000.0,
          231.0,
          ThermalStorage.ThermalStorageThreshold.StorageEmpty(3766L),
        ),
      )

      forAll(cases) {
        (
            tick,
            storedEnergy,
            qDot,
            newTick,
            newQDot,
            expectedStoredEnergy,
            expectedThreshold,
        ) =>
          val storage = buildThermalStorage(storageInput, CubicMeters(70))
          val lastState = ThermalStorage.ThermalStorageState(
            tick,
            KilowattHours(storedEnergy),
            Kilowatts(qDot),
          )
          val result =
            storage.updateState(newTick, Kilowatts(newQDot), lastState)

          result._1.storedEnergy should approximate(
            KilowattHours(expectedStoredEnergy)
          )

          result._2 match {
            case Some(threshold) => threshold shouldBe expectedThreshold
            case None            => fail("Expected a threshold but got None")
          }
      }

    }

    "Check mutable state update, if no threshold is reached update state without hitting a threshold" in {
      val cases = Table(
        (
          "tick",
          "storedEnergy",
          "qDot",
          "newTick",
          "newQDot",
          "expectedStoredEnergy",
        ),
        (0L, 250.0, 10.0, 3600L, 0.0, 260.0),
        (0L, 250.0, -10.0, 3600L, 0.0, 240.0),
      )

      forAll(cases) {
        (tick, storedEnergy, qDot, newTick, newQDot, expectedStoredEnergy) =>
          val storage = buildThermalStorage(storageInput, CubicMeters(70))
          val lastState = ThermalStorage.ThermalStorageState(
            tick,
            KilowattHours(storedEnergy),
            Kilowatts(qDot),
          )
          val result =
            storage.updateState(newTick, Kilowatts(newQDot), lastState)

          result._1.storedEnergy should approximate(
            KilowattHours(expectedStoredEnergy)
          )

          result._2 match {
            case Some(threshold) =>
              fail(s"Expected no threshold, but got: $threshold")
            case None => succeed
          }
      }
    }
  }
}
