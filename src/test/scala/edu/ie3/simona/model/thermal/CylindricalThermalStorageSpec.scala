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
import squants.space.CubicMeters
import squants.thermal.Celsius
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.Units

import java.util.UUID

class CylindricalThermalStorageSpec
    extends UnitSpec
    with Matchers
    with BeforeAndAfterAll {

  final val TESTING_TOLERANCE = 1e-10
  final val tolerance: Double = 1e-10

  var storageInput: CylindricalStorageInput = _

  override def beforeAll(): Unit = {
    storageInput = new CylindricalStorageInput(
      UUID.randomUUID(),
      "ThermalStorage",
      null,
      getQuantity(100, StandardUnits.VOLUME),
      getQuantity(20, StandardUnits.VOLUME),
      getQuantity(30, StandardUnits.TEMPERATURE),
      getQuantity(40, StandardUnits.TEMPERATURE),
      getQuantity(1.15, StandardUnits.SPECIFIC_HEAT_CAPACITY),
    )
  }

  def buildThermalStorage(
      storageInput: CylindricalStorageInput,
      volume: Double,
  ): CylindricalThermalStorage = {
    val storedEnergy = CylindricalThermalStorage.volumeToEnergy(
      CubicMeters(volume),
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

  def vol2Energy(volume: Double): Energy = {
    CylindricalThermalStorage.volumeToEnergy(
      CubicMeters(volume),
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
      val storage = buildThermalStorage(storageInput, 70)

      val initialLevel = storage._storedEnergy.toKilowattHours
      storage._storedEnergy_=(vol2Energy(50))
      val newLevel1 = storage._storedEnergy.toKilowattHours
      val surplus = storage.tryToStoreAndReturnRemainder(vol2Energy(55))
      val newLevel2 = storage._storedEnergy.toKilowattHours
      val isCovering = storage.isDemandCoveredByStorage(KilowattHours(5))
      val lack = storage.tryToTakeAndReturnLack(vol2Energy(95))
      val newLevel3 = storage._storedEnergy.toKilowattHours
      val notCovering = storage.isDemandCoveredByStorage(KilowattHours(1))

      initialLevel should approximate(vol2Energy(70).toKilowattHours)(tolerance)
      newLevel1 should approximate(vol2Energy(50).toKilowattHours)(tolerance)
      surplus.value.toKilowattHours shouldBe vol2Energy(5).toKilowattHours
      newLevel2 should approximate(vol2Energy(100).toKilowattHours)(tolerance)
      lack.value.toKilowattHours shouldBe vol2Energy(15).toKilowattHours
      newLevel3 should approximate(vol2Energy(20).toKilowattHours)(tolerance)
      isCovering shouldBe true
      notCovering shouldBe false
    }

    "Converting methods work correctly" in {
      val storage = buildThermalStorage(storageInput, 70)

      val usableThermalEnergy = storage.usableThermalEnergy

      Math.abs(usableThermalEnergy.toKilowattHours - 5 * 115) shouldBe <(
        TESTING_TOLERANCE
      )
    }

    "Apply, validation, and build method work correctly" in {
      val storage = buildThermalStorage(storageInput, 70)

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
          new ThermalStorage.ThermalStorageThreshold.StorageFull(79886L),
        ),
        (
          0L,
          250.0,
          10.0,
          3600L,
          -42.0,
          260.0,
          new ThermalStorage.ThermalStorageThreshold.StorageEmpty(6171L),
        ),
        (
          0L,
          250.0,
          -10.0,
          3600L,
          42.0,
          240.0,
          new ThermalStorage.ThermalStorageThreshold.StorageFull(81600L),
        ),
        (
          0L,
          250.0,
          -10.0,
          3600L,
          -42.0,
          240.0,
          new ThermalStorage.ThermalStorageThreshold.StorageEmpty(4457L),
        ),
        (
          0L,
          250.0,
          -10.0,
          3600L,
          -42.0,
          240.0,
          new ThermalStorage.ThermalStorageThreshold.StorageEmpty(4457L),
        ),
        (
          0L,
          1000.0,
          149.0,
          3600L,
          5000.0,
          1149.0,
          new ThermalStorage.ThermalStorageThreshold.StorageFull(3601L),
        ),
        (
          0L,
          240.0,
          -9.0,
          3600L,
          -5000.0,
          231.0,
          new ThermalStorage.ThermalStorageThreshold.StorageEmpty(3601L),
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
          val storage = buildThermalStorage(storageInput, 70)
          val lastState = ThermalStorage.ThermalStorageState(
            tick,
            KilowattHours(storedEnergy),
            Kilowatts(qDot),
          )
          val result =
            storage.updateState(newTick, Kilowatts(newQDot), lastState)

          Math.abs(
            result._1.storedEnergy.toKilowattHours - expectedStoredEnergy
          ) should be < TESTING_TOLERANCE

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
          val storage = buildThermalStorage(storageInput, 70)
          val lastState = ThermalStorage.ThermalStorageState(
            tick,
            KilowattHours(storedEnergy),
            Kilowatts(qDot),
          )
          val result =
            storage.updateState(newTick, Kilowatts(newQDot), lastState)

          Math.abs(
            result._1.storedEnergy.toKilowattHours - expectedStoredEnergy
          ) should be < TESTING_TOLERANCE

          result._2 match {
            case Some(threshold) =>
              fail(s"Expected no threshold, but got: $threshold")
            case None => succeed
          }
      }
    }
  }
}
