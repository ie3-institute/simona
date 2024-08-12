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
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull,
}
import edu.ie3.simona.test.common.UnitSpec
import squants.energy._
import squants.thermal.Celsius
import squants.{Energy, Power, Temperature}

import scala.jdk.CollectionConverters._

class ThermalGridWithStorageOnlySpec
    extends UnitSpec
    with ThermalStorageTestData {

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
      )
    )

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
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
        val tick = 10800 // after three hours

        val gridDemand = thermalGrid.energyDemand(
          tick,
          testGridAmbientTemperature,
          ThermalGrid.startingState(thermalGrid),
        )

        gridDemand.required should approximate(KilowattHours(0d))
        gridDemand.possible should approximate(KilowattHours(1150d))

      }

      "deliver the capabilities of a half full storage" in {
        val tick = 10800 // after three hours

        val gridDemand = thermalGrid.energyDemand(
          tick,
          testGridambientTemperature,
          ThermalGridState(
            None,
            Some(ThermalStorageState(0L, KilowattHours(575d), Kilowatts(0d))),
          ),
        )

        gridDemand.required should approximate(KilowattHours(0d))
        gridDemand.possible should approximate(KilowattHours(575d))
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
                Kilowatts(0d),
              )
            )
          )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            testGridAmbientTemperature,
            gridState,
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
        val tick = 0L
        val gridState = ThermalGrid.startingState(thermalGrid)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            tick,
            testGridAmbientTemperature,
            gridState,
            testGridQDotInfeed,
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(KilowattHours(0d))
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
          testGridQDotInfeed,
        )

        nextThreshold shouldBe Some(StorageFull(276000L))

        updatedState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot)),
              ) =>
            tick shouldBe 0L
            storedEnergy should approximate(KilowattHours(0d))
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
                  Kilowatts(0d),
                )
              )
            ),
          testGridAmbientTemperature,
          testGridQDotConsumptionHigh,
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
        val updatedState = thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          Kilowatts(0d),
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
            storedEnergy should approximate(KilowattHours(0d))
            qDot should approximate(Megawatts(0d))

          case _ => fail("Thermal grid state updated failed")
        }
      }
    }
  }
}
