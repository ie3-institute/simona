/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.config.RuntimeConfig.StorageRuntimeConfig
import edu.ie3.simona.ontology.messages.flex.PowerLimitFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.StorageInputTestData
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power}
import tech.units.indriya.quantity.Quantities.getQuantity

class StoragePowerLimitFlexModelSpec
    extends UnitSpec
    with StorageInputTestData {

  // Testing tolerances
  given Power = Kilowatts(1e-10)
  given Energy = KilowattHours(1e-10)

  def createModel(
      targetSoc: Option[Double] = Option.empty
  ): StoragePowerLimitFlexModel = {
    val model = StorageModel
      .Factory(
        storageInput,
        StorageRuntimeConfig(targetSoc = targetSoc),
      )
      .create()

    StoragePowerLimitFlexModel(model)
  }

  "A Storage PowerLimitFlexModel" should {

    "Calculate flex options" in {
      val storageModel = createModel()
      val tick = 3600L

      val testCases = Table(
        ("storedEnergy", "pRef", "pMin", "pMax"),
        // completely empty
        (0.0, 0.0, 0.0, 10.0),
        // at a tiny bit above empty
        (0.011, 0.0, -10.0, 10.0),
        // at mid-level charge
        (60.0, 0.0, -10.0, 10.0),
        // almost fully charged
        (99.989, 0.0, -10.0, 10.0),
        // fully charged
        (100.0, 0.0, -10.0, 0.0),
      )

      forAll(testCases) {
        (storedEnergy: Double, pRef: Double, pMin: Double, pMax: Double) =>
          val state = StorageModel.StorageState(
            KilowattHours(storedEnergy),
            tick,
          )

          storageModel.determineFlexOptions(state) match {
            case result: PowerLimitFlexOptions =>
              result.ref should approximate(Kilowatts(pRef))
              result.min should approximate(Kilowatts(pMin))
              result.max should approximate(Kilowatts(pMax))
            case _ =>
              fail("Expected result of type PowerLimitFlexOptions")
          }
      }
    }

    "Calculate flex options with target SOC" in {
      val storageModel = createModel(Some(0.5d))
      val tick = 3600L

      val testCases = Table(
        ("storedEnergy", "pRef", "pMin", "pMax"),
        // completely empty
        (0.0, 10.0, 0.0, 10.0),
        // below margin of ref power target
        (49.9974, 10.0, -10.0, 10.0),
        // within margin below ref power target
        (49.9976, 0.0, -10.0, 10.0),
        // exactly at ref power target
        (50.0, 0.0, -10.0, 10.0),
        // within margin above ref power target
        (50.0030, 0.0, -10.0, 10.0),
        // above margin of ref power target
        (50.0031, -10.0, -10.0, 10.0),
        // at mid-level charge
        (60.0, -10.0, -10.0, 10.0),
        // fully charged
        (100.0, -10.0, -10.0, 0.0),
      )

      forAll(testCases) {
        (storedEnergy: Double, pRef: Double, pMin: Double, pMax: Double) =>
          val state = StorageModel.StorageState(
            KilowattHours(storedEnergy),
            tick,
          )

          storageModel.determineFlexOptions(state) match {
            case result: PowerLimitFlexOptions =>
              result.ref should approximate(Kilowatts(pRef))
              result.min should approximate(Kilowatts(pMin))
              result.max should approximate(Kilowatts(pMax))
            case _ =>
              fail("Expected result of type PowerLimitFlexOptions")
          }
      }
    }

  }
}
