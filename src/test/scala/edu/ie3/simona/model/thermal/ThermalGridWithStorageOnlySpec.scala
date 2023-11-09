/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.{
  ThermalHouseInput,
  ThermalStorageInput
}
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull
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
          Set[ThermalStorageInput](thermalStorageInput).asJava
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
        Set[ThermalStorageInput](thermalStorageInput).asJava
      )
    )
    val ambientTemperature = Celsius(12d)
    val qDotInfeed = Kilowatts(15d)
    val qDotConsumption = Kilowatts(-200d)

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot))
              ) =>
            tick shouldBe expectedStorageStartingState.tick
            storedEnergy =~ expectedStorageStartingState.storedEnergy
            qDot =~ expectedStorageStartingState.qDot

          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "determining the energy demand" should {
      "deliver the capabilities of the storage" in {
        val tick = 10800 // after three hours

        val gridDemand = thermalGrid.energyDemand(
          tick,
          ambientTemperature,
          ThermalGrid.startingState(thermalGrid)
        )

        gridDemand.required =~ MegawattHours(0d)
        gridDemand.possible =~ MegawattHours(0.92d)
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
                KilowattHours(430d),
                Kilowatts(0d)
              )
            )
          )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            ambientTemperature,
            gridState,
            qDotConsumption
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot))
              ) =>
            tick shouldBe 0L
            storedEnergy =~ KilowattHours(430d)

            qDot =~ qDotConsumption
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageEmpty(3599L))
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
            ambientTemperature,
            gridState,
            qDotInfeed
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot))
              ) =>
            tick shouldBe 0L
            storedEnergy =~ KilowattHours(230d)
            qDot =~ qDotInfeed
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageFull(220799L))
      }
    }

    "updating the grid state dependent on the given thermal infeed" should {
      "deliver proper result, if energy is fed into the grid" in {
        val (updatedState, nextThreshold) = thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          ambientTemperature,
          qDotInfeed
        )

        nextThreshold shouldBe Some(StorageFull(220799L))

        updatedState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot))
              ) =>
            tick shouldBe 0L
            storedEnergy =~ KilowattHours(230d)
            qDot =~ qDotInfeed
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
                  KilowattHours(430d),
                  Kilowatts(0d)
                )
              )
            ),
          ambientTemperature,
          qDotConsumption
        ) match {
          case (
                ThermalGridState(
                  None,
                  Some(ThermalStorageState(tick, storedEnergy, qDot))
                ),
                Some(StorageEmpty(thresholdTick))
              ) =>
            tick shouldBe 0L
            storedEnergy =~ KilowattHours(430d)

            qDot =~ qDotConsumption
            thresholdTick shouldBe 3599L
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is neither consumed from nor fed into the grid" in {
        val updatedState = thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          ambientTemperature,
          Kilowatts(0d)
        )
        updatedState match {
          case (
                ThermalGridState(
                  None,
                  Some(ThermalStorageState(tick, storedEnergy, qDot))
                ),
                None
              ) =>
            tick shouldBe 0L
            storedEnergy =~ KilowattHours(230d)

            qDot =~ Megawatts(0d)

          case _ => fail("Thermal grid state updated failed")
        }
      }
    }
  }
}
