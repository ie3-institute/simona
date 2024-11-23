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
import edu.ie3.simona.model.participant.HpModel.HpRelevantData
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalDemandWrapper,
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull,
}
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
          thermalDemands,
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
        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand
        val waterStorageDemand = thermalDemands.domesticHotWaterStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(zeroKWh)
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        waterStorageDemand.required should approximate(zeroKWh)
        waterStorageDemand.possible should approximate(zeroKWh)
        updatedThermalGridState.houseState shouldBe None
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800, zeroKWh, zeroKW)
        )
        updatedThermalGridState.domesticHotWaterStorageState shouldBe None
      }

      "deliver the capabilities of a half full storage" in {
        val tick = 10800 // after three hours

        val (
          thermalDemands,
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
        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand
        val waterStorageDemand = thermalDemands.domesticHotWaterStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(zeroKWh)
        storageDemand.required should approximate(zeroKWh)
        storageDemand.possible should approximate(KilowattHours(575d))
        waterStorageDemand.required should approximate(zeroKWh)
        waterStorageDemand.possible should approximate(zeroKWh)
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
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val gridState = ThermalGrid.startingState(thermalGrid)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            relevantData,
            testGridAmbientTemperature,
            gridState,
            false,
            testGridQDotInfeed,
            onlyThermalDemandOfHeatStorage,
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
                None,
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(zeroKWh)
            qDot should approximate(testGridQDotInfeed)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageFull(276000L))
      }

      "properly take energy from storage" in {
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val gridState = ThermalGrid
          .startingState(thermalGrid)
          .copy(storageState =
            Some(
              ThermalStorageState(
                0L,
                KilowattHours(150d),
                zeroKW,
              )
            )
          )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            relevantData,
            testGridAmbientTemperature,
            gridState,
            isNotRunning,
            testGridQDotInfeed,
            onlyThermalDemandOfHeatStorage,
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
                None,
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(KilowattHours(150d))
            qDot should approximate(testGridQDotInfeed * (-1))
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageEmpty(36000L))
      }

    }

    "updating the grid state dependent on the given thermal infeed" should {
      "deliver proper result, if energy is fed into the grid" in {
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val thermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(KilowattHours(1150), KilowattHours(1150)),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
        )
        val (updatedState, nextThreshold) = thermalGrid.updateState(
          relevantData,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          isRunning,
          testGridQDotInfeed,
          thermalDemands,
        )

        nextThreshold shouldBe Some(StorageFull(276000L))

        updatedState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
                None,
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(zeroKWh)
            qDot should approximate(testGridQDotInfeed)
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is consumed from the grid" in {
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val thermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
        )
        thermalGrid.updateState(
          relevantData,
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
          isRunning,
          testGridQDotConsumptionHigh,
          thermalDemands,
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
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val thermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
        )
        val updatedState = thermalGrid.updateState(
          relevantData,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          isRunning,
          zeroKW,
          thermalDemands,
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
            storedEnergy should approximate(zeroKWh)
            qDot should approximate(zeroKW)

          case _ => fail("Thermal grid state updated failed")
        }
      }
    }
  }
}
