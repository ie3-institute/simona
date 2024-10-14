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
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull,
}
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWH}
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
        case ThermalGrid(None, Some(thermalStorageGenerated), None) =>
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

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
                None,
              ) =>
            tick shouldBe expectedCylindricalStorageStartingState.tick
            storedEnergy should approximate(
              expectedCylindricalStorageStartingState.storedEnergy
            )
            qDot should approximate(
              expectedCylindricalStorageStartingState.qDot
            )

          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "determining the energy demand" should {
      "deliver the capabilities of the storage" in {
        val tick = 10800 // after three hours

        val (
          houseDemand,
          storageDemand,
          waterStorageDemand,
          updatedThermalGridState,
        ) =
          thermalGrid.energyDemandAndUpdatedState(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            ThermalGrid.startingState(thermalGrid),
            defaultSimulationStart,
            houseInhabitants,
          )

        houseDemand.required should approximate(zeroKWH)
        houseDemand.possible should approximate(zeroKWH)
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        waterStorageDemand.required should approximate(zeroKWH)
        waterStorageDemand.possible should approximate(zeroKWH)
        updatedThermalGridState.houseState shouldBe None
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800, zeroKWH, zeroKW)
        )
        updatedThermalGridState.domesticHotWaterStorageState shouldBe None
      }

      "deliver the capabilities of a half full storage" in {
        val tick = 10800 // after three hours

        val (
          houseDemand,
          storageDemand,
          waterStorageDemand,
          updatedThermalGridState,
        ) =
          thermalGrid.energyDemandAndUpdatedState(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            ThermalGridState(
              None,
              Some(ThermalStorageState(0L, KilowattHours(575d), zeroKW)),
              None,
            ),
            defaultSimulationStart,
            houseInhabitants,
          )

        houseDemand.required should approximate(zeroKWH)
        houseDemand.possible should approximate(zeroKWH)
        storageDemand.required should approximate(zeroKWH)
        storageDemand.possible should approximate(KilowattHours(575d))
        waterStorageDemand.required should approximate(zeroKWH)
        waterStorageDemand.possible should approximate(zeroKWH)
        updatedThermalGridState.houseState shouldBe None
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800L, KilowattHours(575d), zeroKW)
        )
        updatedThermalGridState.domesticHotWaterStorageState shouldBe None
      }
    }

    "handling thermal energy consumption from grid" should {
      val handleConsumption =
        PrivateMethod[(ThermalGridState, Option[ThermalThreshold])](
          Symbol("handleConsumption")
        )

      "properly take the energy from storage" in {
        val tick = 0L
        val gridState = ThermalGrid
          .startingState(thermalGrid)
          .copy(storageState =
            Some(
              ThermalStorageState(
                0L,
                KilowattHours(200d),
                zeroKW,
              )
            )
          )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            gridState,
            testGridQDotConsumptionHigh,
            defaultSimulationStart,
            houseInhabitants,
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
                None,
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(KilowattHours(200d))
            qDot should approximate(testGridQDotConsumptionHigh)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageEmpty(3600L))
      }
    }

    "handling thermal infeed into the grid" should {
      val handleInfeed =
        PrivateMethod[(ThermalGridState, Option[ThermalThreshold])](
          Symbol("handleInfeed")
        )

      "properly put energy to storage" in {
        val tick = 0L
        val gridState = ThermalGrid.startingState(thermalGrid)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            gridState,
            false,
            testGridQDotInfeed,
            ThermalEnergyDemand(zeroKWH, zeroKWH),
            ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
            ThermalEnergyDemand(zeroKWH, zeroKWH),
            defaultSimulationStart,
            houseInhabitants,
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
                None,
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(zeroKWH)
            qDot should approximate(testGridQDotInfeed)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageFull(276000L))
      }
    }

    "updating the grid state dependent on the given thermal infeed" should {
      "deliver proper result, if energy is fed into the grid" in {
        val (updatedState, nextThreshold) = thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          testGridAmbientTemperature,
          isRunning,
          testGridQDotInfeed,
          ThermalEnergyDemand(zeroKWH, zeroKWH),
          ThermalEnergyDemand(KilowattHours(1150), KilowattHours(1150)),
          ThermalEnergyDemand(zeroKWH, zeroKWH),
          defaultSimulationStart,
          houseInhabitants,
        )

        nextThreshold shouldBe Some(StorageFull(276000L))

        updatedState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
                None,
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(zeroKWH)
            qDot should approximate(testGridQDotInfeed)
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is consumed from the grid" in {
        thermalGrid.updateState(
          0L,
          ThermalGrid
            .startingState(thermalGrid)
            .copy(storageState =
              Some(
                ThermalStorageState(
                  0L,
                  KilowattHours(200d),
                  zeroKW,
                )
              )
            ),
          testGridAmbientTemperature,
          testGridAmbientTemperature,
          isRunning,
          testGridQDotConsumptionHigh,
          ThermalEnergyDemand(zeroKWH, zeroKWH),
          ThermalEnergyDemand(zeroKWH, zeroKWH),
          ThermalEnergyDemand(zeroKWH, zeroKWH),
          defaultSimulationStart,
          houseInhabitants,
        ) match {
          case (
                ThermalGridState(
                  None,
                  Some(ThermalStorageState(tick, storedEnergy, qDot)),
                  None,
                ),
                Some(StorageEmpty(thresholdTick)),
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(KilowattHours(200d))
            qDot should approximate(testGridQDotConsumptionHigh)
            thresholdTick shouldBe 3600L
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is neither consumed from nor fed into the grid" in {
        val updatedState = thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          testGridAmbientTemperature,
          isRunning,
          zeroKW,
          ThermalEnergyDemand(zeroKWH, zeroKWH),
          ThermalEnergyDemand(zeroKWH, zeroKWH),
          ThermalEnergyDemand(zeroKWH, zeroKWH),
          defaultSimulationStart,
          houseInhabitants,
        )
        updatedState match {
          case (
                ThermalGridState(
                  None,
                  Some(ThermalStorageState(tick, storedEnergy, qDot)),
                  None,
                ),
                None,
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(zeroKWH)
            qDot should approximate(zeroKW)

          case _ => fail("Thermal grid state updated failed")
        }
      }
    }
  }
}
