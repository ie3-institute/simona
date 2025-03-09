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
import edu.ie3.simona.model.participant2.HpModel.HpState
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
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
    val initialGridState = ThermalGrid.startingState(thermalGrid)

    "requesting the starting state" should {
      "deliver proper results" in {
        initialGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
              ) =>
            tick shouldBe expectedStorageStartingState.tick
            storedEnergy should approximate(
              expectedStorageStartingState.storedEnergy
            )
            qDot should approximate(expectedStorageStartingState.qDot)

          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "determining the energy demand" should {
      "deliver the capabilities of the storage" in {
        val state = HpState(
          10800, // after three hours
          testGridAmbientTemperature,
          initialGridState,
          testGridAmbientTemperature,
          // FIXME?
          noThermalDemand,
        )
        val (thermalDemands, updatedThermalGridState) =
          thermalGrid.energyDemandAndUpdatedState(state.tick, state)
        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(zeroKWh)
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        updatedThermalGridState.houseState shouldBe None
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800, zeroKWh, zeroKW)
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
          testGridAmbientTemperature,
          gridState,
          testGridAmbientTemperature,
          // FIXME?
          noThermalDemand,
        )

        val (thermalDemands, updatedThermalGridState) =
          thermalGrid.energyDemandAndUpdatedState(state.tick, state)
        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(zeroKWh)
        storageDemand.required should approximate(zeroKWh)
        storageDemand.possible should approximate(KilowattHours(575d))
        updatedThermalGridState.houseState shouldBe None
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800L, KilowattHours(575d), zeroKW)
        )
      }
    }

    "handling thermal energy consumption from grid" should {
      val handleConsumption =
        PrivateMethod[(ThermalGridState, Option[ThermalThreshold])](
          Symbol("handleConsumption")
        )

      "properly take the energy from storage" in {
        val gridState = initialGridState
          .copy(storageState =
            Some(
              ThermalStorageState(
                0L,
                KilowattHours(200d),
                zeroKW,
              )
            )
          )
        val state = HpState(
          0,
          testGridAmbientTemperature,
          gridState,
          testGridAmbientTemperature,
          // FIXME?
          noThermalDemand,
        )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            state.tick,
            state,
            testGridQDotConsumptionHigh,
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
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
        val state = HpState(
          0,
          testGridAmbientTemperature,
          initialGridState,
          testGridAmbientTemperature,
          // FIXME?
          noThermalDemand,
        )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            state.tick,
            state,
            isRunning,
            testGridQDotInfeed,
            onlyThermalDemandOfHeatStorage,
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(zeroKWh)
            qDot should approximate(testGridQDotInfeed)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageFull(276000L))
      }

      "properly take energy from storage" in {
        val gridState = initialGridState
          .copy(storageState =
            Some(
              ThermalStorageState(
                0L,
                KilowattHours(150d),
                zeroKW,
              )
            )
          )
        val state = HpState(
          0,
          testGridAmbientTemperature,
          gridState,
          testGridAmbientTemperature,
          // FIXME?
          noThermalDemand,
        )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            state.tick,
            state,
            isNotRunning,
            testGridQDotInfeed,
            onlyThermalDemandOfHeatStorage,
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
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
        val state = HpState(
          0,
          testGridAmbientTemperature,
          initialGridState,
          testGridAmbientTemperature,
          // FIXME?
          noThermalDemand,
        )
        val (updatedState, nextThreshold) = thermalGrid.updateState(
          state.tick,
          state,
          isRunning,
          testGridQDotInfeed,
          onlyThermalDemandOfHeatStorage,
        )

        nextThreshold shouldBe Some(StorageFull(276000L))

        updatedState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(zeroKWh)
            qDot should approximate(testGridQDotInfeed)
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is consumed from the grid" in {
        val initialLoading = KilowattHours(200d)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading)
          )
        )
        val state = HpState(
          0,
          testGridAmbientTemperature,
          gridState,
          testGridAmbientTemperature,
          // FIXME?
          noThermalDemand,
        )

        thermalGrid.updateState(
          state.tick,
          state,
          isRunning,
          testGridQDotConsumptionHigh,
          onlyThermalDemandOfHouse,
        ) match {
          case (
                ThermalGridState(
                  None,
                  Some(ThermalStorageState(tick, storedEnergy, qDot)),
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
        val state = HpState(
          0,
          testGridAmbientTemperature,
          initialGridState,
          testGridAmbientTemperature,
          // FIXME?
          noThermalDemand,
        )
        val updatedState = thermalGrid.updateState(
          state.tick,
          state,
          isRunning,
          zeroKW,
          noThermalDemand,
        )
        updatedState match {
          case (
                ThermalGridState(
                  None,
                  Some(ThermalStorageState(tick, storedEnergy, qDot)),
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
