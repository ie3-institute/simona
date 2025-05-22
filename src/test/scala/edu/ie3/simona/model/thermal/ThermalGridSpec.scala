/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.exceptions.InvalidParameterException
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalEnergyDemand
import edu.ie3.simona.test.common.UnitSpec
import squants.energy.{KilowattHours, MegawattHours, WattHours, Watts}
import squants.thermal.Celsius
import squants.{Energy, Power, Temperature}

import scala.jdk.CollectionConverters._

class ThermalGridSpec
    extends UnitSpec
    with ThermalHouseTestData
    with ThermalStorageTestData {

  implicit val tempTolerance: Temperature = Celsius(1e-3)
  implicit val powerTolerance: Power = Watts(1e-3)
  implicit val energyTolerance: Energy = WattHours(1e-3)

  "Testing the thermal energy demand" when {
    "instantiating it from given values" should {
      "throw exception for non-sensible input (positive)" in {
        val possible = MegawattHours(40d)
        val required = MegawattHours(42d)

        intercept[InvalidParameterException] {
          ThermalEnergyDemand(required, possible)
        }.getMessage shouldBe s"The possible amount of energy $possible is smaller than the required amount of energy $required. This is not supported."
      }

      "set the correct values, if they are sensible" in {
        val possible = MegawattHours(45d)
        val required = MegawattHours(42d)

        val energyDemand = ThermalEnergyDemand(required, possible)

        energyDemand.required should approximate(required)
        energyDemand.possible should approximate(possible)
      }
    }

    "defining no demand" should {
      "actually have no demand" in {
        val energyDemand = ThermalEnergyDemand.noDemand

        energyDemand.required should approximate(MegawattHours(0d))
        energyDemand.possible should approximate(MegawattHours(0d))
      }
    }

    "checking for required and possible demand" should {
      "return proper information, if no required and no possible demand is apparent" in {
        val required = MegawattHours(0d)
        val possible = MegawattHours(0d)

        val energyDemand = ThermalEnergyDemand(required, possible)
        energyDemand.hasRequiredDemand shouldBe false
        energyDemand.hasPossibleDemand shouldBe false
      }

      "return proper information, if no required but possible demand is apparent" in {
        val required = MegawattHours(0d)
        val possible = MegawattHours(45d)

        val energyDemand = ThermalEnergyDemand(required, possible)
        energyDemand.hasRequiredDemand shouldBe false
        energyDemand.hasPossibleDemand shouldBe true
      }

      "throw exception, if required demand is higher than possible demand" in {
        val required = MegawattHours(1d)
        val possible = MegawattHours(0d)
        intercept[InvalidParameterException] {
          ThermalEnergyDemand(required, possible)
        }.getMessage shouldBe s"The possible amount of energy $possible is smaller than the required amount of energy $required. This is not supported."
      }

      "throw exception, if required demand is smaller than zero" in {
        val required = MegawattHours(-2d)
        val possible = MegawattHours(5d)
        intercept[InvalidParameterException] {
          ThermalEnergyDemand(required, possible)
        }.getMessage shouldBe s"The possible $possible or required $required amount of energy cannot be negative. This is not supported."
      }

      "throw exception, if possible demand is smaller than zero" in {
        val required = MegawattHours(2d)
        val possible = MegawattHours(-5d)
        intercept[InvalidParameterException] {
          ThermalEnergyDemand(required, possible)
        }.getMessage shouldBe s"The possible $possible or required $required amount of energy cannot be negative. This is not supported."
      }

      "throw exception, if possible and required demand are smaller than zero" in {
        val required = MegawattHours(-2d)
        val possible = MegawattHours(-5d)
        intercept[InvalidParameterException] {
          ThermalEnergyDemand(required, possible)
        }.getMessage shouldBe s"The possible $possible or required $required amount of energy cannot be negative. This is not supported."
      }

      "return proper information, if required and additional demand is apparent" in {
        val required = MegawattHours(45d)
        val possible = MegawattHours(47d)

        val energyDemand = ThermalEnergyDemand(required, possible)
        energyDemand.hasRequiredDemand shouldBe true
        energyDemand.hasPossibleDemand shouldBe true
      }
    }

    "adding two demands" should {
      "deliver proper results" in {
        val energyDemand1 = ThermalEnergyDemand(
          MegawattHours(45d),
          MegawattHours(47d),
        )
        val energyDemand2 = ThermalEnergyDemand(
          MegawattHours(23d),
          MegawattHours(28d),
        )

        val totalDemand = energyDemand1 + energyDemand2

        totalDemand.required should approximate(MegawattHours(68d))
        totalDemand.possible should approximate(MegawattHours(75d))
      }
    }
  }
  "ThermalGridState" should {
    val thermalGridOnlyHouse = ThermalGrid(
      new edu.ie3.datamodel.models.input.container.ThermalGrid(
        thermalBusInput,
        Set(thermalHouseInput).asJava,
        Set.empty[ThermalStorageInput].asJava,
        Set.empty[ThermalStorageInput].asJava,
      )
    )

    "return true when there is no storage" in {
      val initialState = ThermalGrid.startingState(
        thermalGridOnlyHouse,
        testGridAmbientTemperature,
      )
      val result = initialState.isThermalStorageEmpty
      result shouldBe true
    }

    val thermalGrid = ThermalGrid(
      new edu.ie3.datamodel.models.input.container.ThermalGrid(
        thermalBusInput,
        Set(thermalHouseInput).asJava,
        Set[ThermalStorageInput](thermalStorageInput).asJava,
        Set.empty[ThermalStorageInput].asJava,
      )
    )

    "return true when all stored energy is effectively zero" in {
      val initialState =
        ThermalGrid.startingState(thermalGrid, testGridAmbientTemperature)
      val result = initialState.isThermalStorageEmpty
      result shouldBe true
    }

    "return false when storage is not empty" in {
      val initialState =
        ThermalGrid.startingState(thermalGrid, testGridAmbientTemperature)
      val gridState = initialState.copy(heatStorageState =
        initialState.heatStorageState.map(storageState =>
          storageState.copy(storedEnergy = KilowattHours(1))
        )
      )
      val result = gridState.isThermalStorageEmpty
      result shouldBe false
    }
  }
}
