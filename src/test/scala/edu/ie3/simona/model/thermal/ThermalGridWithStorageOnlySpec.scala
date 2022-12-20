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
import squants.Kelvin
import squants.energy.{KilowattHours, Kilowatts, MegawattHours, Megawatts}
import squants.thermal.Celsius

import scala.jdk.CollectionConverters._

class ThermalGridWithStorageOnlySpec
    extends UnitSpec
    with ThermalStorageTestData {

  implicit val energyTolerance: squants.Energy = KilowattHours(1e-10)
  implicit val powerTolerance: squants.Power = Kilowatts(1e-10)
  implicit val temperatureTolerance: squants.Temperature = Kelvin(1e-3)

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
    val ambientTemperature = Celsius(12.0)
    val qDotInfeed = Kilowatts(15.0)
    val qDotConsumption = Kilowatts(-200.0)

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot))
              ) =>
            tick shouldBe expectedStorageStartingState.tick
            (storedEnergy ~= expectedStorageStartingState.storedEnergy) shouldBe true
            (qDot ~= expectedStorageStartingState.qDot) shouldBe true
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

        (gridDemand.required ~= MegawattHours(0.0)) shouldBe true
        (gridDemand.possible ~= MegawattHours(0.92)) shouldBe true
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
                KilowattHours(430.0),
                Kilowatts(0.0)
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
            (storedEnergy ~= KilowattHours(430d)) shouldBe true
            (qDot ~= qDotConsumption) shouldBe true
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
            (storedEnergy ~= KilowattHours(230d)) shouldBe true
            (qDot ~= qDotInfeed) shouldBe true
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageFull(220800L))
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

        nextThreshold shouldBe Some(StorageFull(220800L))

        updatedState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot))
              ) =>
            tick shouldBe 0L
            (storedEnergy ~= KilowattHours(230d)) shouldBe true
            (qDot ~= qDotInfeed) shouldBe true
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
                  KilowattHours(430.0),
                  Kilowatts(0.0)
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
            (storedEnergy ~= KilowattHours(430d)) shouldBe true
            (qDot ~= qDotConsumption) shouldBe true
            thresholdTick shouldBe 3600L
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is neither consumed from nor fed into the grid" in {
        val updatedState = thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          ambientTemperature,
          Megawatts(0.0)
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
            (storedEnergy ~= KilowattHours(230.0)) shouldBe true
            (qDot ~= Megawatts(0.0)) shouldBe true
          case _ => fail("Thermal grid state updated failed")
        }
      }
    }
  }
}
