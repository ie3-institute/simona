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
    getQuantity(30, StandardUnits.TEMPERATURE),
    getQuantity(40, StandardUnits.TEMPERATURE),
    getQuantity(1.15, StandardUnits.SPECIFIC_HEAT_CAPACITY),
    getQuantity(50, PowerSystemUnits.KILOWATT),
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

    "Apply, validation, and build method work correctly" in {
      val storage = buildThermalStorage(storageInput, CubicMeters(70))

      storage.uuid shouldBe storageInput.getUuid
      storage.id shouldBe storageInput.getId
      storage.operatorInput shouldBe storageInput.getOperator
      storage.operationTime shouldBe storageInput.getOperationTime
      storage.bus shouldBe storageInput.getThermalBus
    }

    "Check mutable state update correctly update the state" in {
      val cases = Table(
        ("storedEnergy", "tick", "qDot", "expectedStoredEnergy"),
        (0.0, 3600L, 0.0, 0.0),
        (0.0, 0L, 10.0, 0.0),
        (10.0, 3600L, 10.0, 20.0),
        (20.0, 3600L, -10.0, 10.0),
        (20.0, 7200L, -10.0, 0.0),
        (10.0, 7200L, -10.0, 0.0),
      )

      forAll(cases) { (storedEnergy, tick, qDot, expectedStoredEnergy) =>
        val storage = buildThermalStorage(storageInput, CubicMeters(70))
        val lastState =
          ThermalStorage.ThermalStorageState(0L, KilowattHours(storedEnergy))
        val storageState =
          storage.determineState(tick, lastState, Kilowatts(qDot))

        storageState.storedEnergy should approximate(
          KilowattHours(expectedStoredEnergy)
        )
      }

    }

    "Check thresholds for mutable state update correctly" in {
      val cases = Table(
        (
          "storedEnergy",
          "qDot",
          "expectedThreshold",
        ),
        (
          1140.0,
          10.0,
          ThermalStorage.ThermalStorageThreshold.StorageFull(3600L),
        ),
        (
          10.0,
          -10.0,
          ThermalStorage.ThermalStorageThreshold.StorageEmpty(3600L),
        ),
      )

      forAll(cases) {
        (
            storedEnergy,
            qDot,
            expectedThreshold,
        ) =>
          val storage = buildThermalStorage(storageInput, CubicMeters(70))
          val state =
            ThermalStorage.ThermalStorageState(0L, KilowattHours(storedEnergy))

          val threshold =
            storage.determineNextThreshold(state, Kilowatts(qDot))

          threshold match {
            case Some(threshold) => threshold shouldBe expectedThreshold
            case None            => fail("Expected a threshold but got None")
          }
      }

    }
  }
}
