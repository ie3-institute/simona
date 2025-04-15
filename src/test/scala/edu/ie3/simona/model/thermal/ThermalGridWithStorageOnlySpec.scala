/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.{
  ThermalHouseInput,
  ThermalStorageInput,
}
import edu.ie3.simona.model.participant2.HpModel.{
  HpOperatingPoint,
  HpState,
  ThermalGridOperatingPoint,
}
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.StorageFull
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import squants.energy._
import squants.thermal.Celsius
import squants.{Energy, Power, Temperature}

import scala.jdk.CollectionConverters._

class ThermalGridWithStorageOnlySpec
    extends UnitSpec
    with ThermalStorageTestData
    with DefaultTestData {

  implicit val tempTolerance: Temperature = Celsius(1e-3)
  implicit val powerTolerance: Power = Watts(1e-3)
  implicit val energyTolerance: Energy = WattHours(1e-3)
  "Testing thermal grid generation with only a storage" should {
    "instantiating correctly from input data" in new ThermalStorageTestData {
      val thermalGridInput =
        new edu.ie3.datamodel.models.input.container.ThermalGrid(
          thermalBusInput,
          Set.empty[ThermalHouseInput].asJava,
          Set[ThermalStorageInput](thermalStorageInput).asJava,
          Set.empty[ThermalStorageInput].asJava,
        )

      ThermalGrid(thermalGridInput) match {
        case ThermalGrid(None, Some(thermalStorageGenerated)) =>
          thermalStorageGenerated shouldBe thermalStorage
        case _ =>
          fail("Generation of thermal grid from thermal input grid failed.")
      }
    }
  }

  "Testing a thermal grid with only a storage" when {
    val thermalGrid: ThermalGrid = ThermalGrid(
      new edu.ie3.datamodel.models.input.container.ThermalGrid(
        thermalBusInput,
        Set.empty[ThermalHouseInput].asJava,
        Set[ThermalStorageInput](thermalStorageInput).asJava,
        Set.empty[ThermalStorageInput].asJava,
      )
    )
    val initialGridState =
      ThermalGrid.startingState(thermalGrid, testGridAmbientTemperature)
    val initialHpState = HpState(
      -1L,
      initialGridState,
      HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
      onlyThermalDemandOfHeatStorage,
    )

    "requesting the starting state" should {
      "deliver proper results" in {
        initialGridState match {
          case ThermalGridState(
                None,
                Some(
                  ThermalStorageState(
                    tick,
                    storedEnergy,
                  )
                ),
              ) =>
            tick shouldBe expectedStorageStartingState.tick
            storedEnergy should approximate(
              expectedStorageStartingState.storedEnergy
            )

          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "determining the energy demand" should {
      "deliver the capabilities of the storage" in {
        val tick = 10800L // after three hours

        val updatedThermalGridState =
          thermalGrid.determineThermalGridState(
            tick,
            initialHpState,
            HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
          )

        val thermalDemands =
          thermalGrid.determineEnergyDemand(updatedThermalGridState)

        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(zeroKWh)
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        updatedThermalGridState.houseState shouldBe None
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(
            10800,
            zeroKWh,
          )
        )
      }

      "deliver the capabilities of a half full storage" in {
        val initialLoading = KilowattHours(575d)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading)
          )
        )
        val state = HpState(
          10800, // after three hours
          gridState,
          HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
          onlyAdditionalDemandOfHeatStorage,
        )

        val updatedThermalGridState =
          thermalGrid.determineThermalGridState(
            state.tick,
            state,
            HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
          )

        val thermalDemands =
          thermalGrid.determineEnergyDemand(updatedThermalGridState)

        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(zeroKWh)
        storageDemand.required should approximate(zeroKWh)
        storageDemand.possible should approximate(KilowattHours(575d))
        updatedThermalGridState.houseState shouldBe None
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800L, KilowattHours(575d))
        )
      }
    }

    "handling thermal feed in into the grid" should {
      "properly put energy to storage" in {
        val (thermalGridOperatingPoint, reachedThreshold) =
          thermalGrid.handleFeedIn(
            initialHpState,
            testGridQDotInfeed,
          )

        reachedThreshold shouldBe Some(StorageFull(276000))
        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
          testGridQDotInfeed,
          zeroKW,
          testGridQDotInfeed,
        )
      }
    }

    "updating the grid state dependent on the given thermal feed in" should {
      "deliver proper result, if energy is fed into the grid" in {
        val (thermalGridOperatingPoint, nextThreshold) =
          thermalGrid.handleFeedIn(
            initialHpState,
            testGridQDotInfeed,
          )

        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
          testGridQDotInfeed,
          zeroKW,
          testGridQDotInfeed,
        )
        nextThreshold shouldBe Some(StorageFull(276000))
      }

      "do not consume energy from storage if there is no heat sink for this consumption" in {
        val initialLoading = KilowattHours(200d)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading, tick = 0)
          )
        )
        val state = initialHpState.copy(
          thermalGridState = gridState,
          thermalDemands = onlyAdditionalDemandOfHeatStorage,
        )

        val (thermalGridOperatingPoint, threshold) =
          thermalGrid.handleConsumption(state)

        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint.zero
        threshold shouldBe None
      }

      "deliver proper result, if energy is neither consumed from nor fed into the grid" in {
        val (thermalGridOperatingPoint, threshold) =
          thermalGrid.handleConsumption(initialHpState)

        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint.zero
        threshold shouldBe None

      }
    }
  }
}
