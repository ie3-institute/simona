/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.{
  ThermalHouseInput,
  ThermalStorageInput
}
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridThreshold.{
  ThermalGridEmpty,
  ThermalGridFilledUp
}
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalGridState,
  ThermalGridThreshold
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.StorageFull
import edu.ie3.simona.test.common.UnitSpec
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import scala.jdk.CollectionConverters._

class ThermalGridWithStorageOnlySpec
    extends UnitSpec
    with ThermalStorageTestData {
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

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot))
              ) =>
            tick shouldBe expectedStorageStartingState.tick
            storedEnergy should equalWithTolerance(
              expectedStorageStartingState.storedEnergy
            )
            qDot should equalWithTolerance(
              expectedStorageStartingState.qDot
            )
          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "trying to push to storage" should {
      "return correct result" in {
        val takeOrPushToStorage = PrivateMethod[Option[
          (ThermalStorageState, Option[ThermalStorage.ThermalStorageThreshold])
        ]](Symbol("takeOrPushToStorage"))

        val qDotExternal =
          Quantities.getQuantity(12d, StandardUnits.ACTIVE_POWER_IN)
        thermalGrid invokePrivate takeOrPushToStorage(
          42L,
          ThermalGrid.startingState(thermalGrid).storageState,
          qDotExternal
        ) match {
          case Some((ThermalStorageState(tick, storedEnergy, qDot), reason)) =>
            tick shouldBe 42L
            storedEnergy shouldBe Quantities.getQuantity(
              230d,
              StandardUnits.ENERGY_IN
            )
            qDot shouldBe qDotExternal
            reason shouldBe Some(StorageFull(276042L))
          case _ => fail("Expected an updated storage state")
        }
      }
    }

    "determining the energy demand" should {
      "deliver the capabilities of the storage" in {
        val tick = 10800 // after three hours
        val ambientTemperature = Quantities.getQuantity(12d, Units.CELSIUS)

        val gridDemand = thermalGrid.energyDemand(
          tick,
          ambientTemperature,
          ThermalGrid.startingState(thermalGrid)
        )

        gridDemand.required should equalWithTolerance(
          Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
        )
        gridDemand.possible should equalWithTolerance(
          Quantities.getQuantity(0.92d, StandardUnits.ENERGY_RESULT)
        )
      }
    }

    "handling thermal energy consumption from grid" should {
      val handleConsumption =
        PrivateMethod[(ThermalGridState, Option[ThermalGridThreshold])](
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
                Quantities.getQuantity(430d, StandardUnits.ENERGY_IN),
                Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
              )
            )
          )
        val ambientTemperature = Quantities.getQuantity(12d, Units.CELSIUS)
        val externalQDot =
          Quantities.getQuantity(-200d, StandardUnits.ACTIVE_POWER_IN)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            ambientTemperature,
            gridState,
            externalQDot
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot))
              ) =>
            tick shouldBe 0L
            storedEnergy should equalWithTolerance(
              Quantities.getQuantity(430d, StandardUnits.ENERGY_IN)
            )
            qDot should equalWithTolerance(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(ThermalGridEmpty(3600L))
      }
    }

    "handling thermal infeed into the grid" should {
      "properly put energy to storage" in {
        val tick = 0L
        val gridState = ThermalGrid.startingState(thermalGrid)
        val ambientTemperature = Quantities.getQuantity(12d, Units.CELSIUS)
        val externalQDot =
          Quantities.getQuantity(15d, StandardUnits.ACTIVE_POWER_IN)

        val (updatedGridState, reachedThreshold) =
          thermalGrid.handleInfeed(
            tick,
            ambientTemperature,
            gridState,
            externalQDot
          )

        updatedGridState match {
          case ThermalGridState(
                None,
                Some(ThermalStorageState(tick, storedEnergy, qDot))
              ) =>
            tick shouldBe 0L
            storedEnergy should equalWithTolerance(
              Quantities.getQuantity(230d, StandardUnits.ENERGY_IN)
            )
            qDot should equalWithTolerance(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(ThermalGridFilledUp(220800L))
      }
    }
  }
}
