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
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
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

//  "Testing a thermal grid with only a storage" when {
//    val thermalGrid: ThermalGrid = ThermalGrid(
//      new edu.ie3.datamodel.models.input.container.ThermalGrid(
//        thermalBusInput,
//        Set.empty[ThermalHouseInput].asJava,
//        Set[ThermalStorageInput](thermalStorageInput).asJava
//      )
//    )
//    val ambientTemperature = Quantities.getQuantity(12d, Units.CELSIUS)
//    val qDotInfeed =
//      Quantities.getQuantity(15d, StandardUnits.ACTIVE_POWER_IN)
//    val qDotConsumption =
//      Quantities.getQuantity(-200d, StandardUnits.ACTIVE_POWER_IN)
//
//    "requesting the starting state" should {
//      "deliver proper results" in {
//        ThermalGrid.startingState(thermalGrid) match {
//          case ThermalGridState(
//                None,
//                Some(ThermalStorageState(tick, storedEnergy, qDot))
//              ) =>
//            tick shouldBe expectedStorageStartingState.tick
//            storedEnergy should equalWithTolerance(
//              expectedStorageStartingState.storedEnergy
//            )
//            qDot should equalWithTolerance(
//              expectedStorageStartingState.qDot
//            )
//          case _ => fail("Determination of starting state failed")
//        }
//      }
//    }
//
//    "trying to push to storage" should {
//      "return correct result" in {
//        val takeOrPushToStorage = PrivateMethod[Option[
//          (ThermalStorageState, Option[ThermalThreshold])
//        ]](Symbol("takeOrPushToStorage"))
//
//        thermalGrid invokePrivate takeOrPushToStorage(
//          42L,
//          ThermalGrid.startingState(thermalGrid).storageState,
//          qDotInfeed
//        ) match {
//          case Some((ThermalStorageState(tick, storedEnergy, qDot), reason)) =>
//            tick shouldBe 42L
//            storedEnergy shouldBe Quantities.getQuantity(
//              230d,
//              StandardUnits.ENERGY_IN
//            )
//            qDot shouldBe qDotInfeed
//            reason shouldBe Some(StorageFull(220842L))
//          case _ => fail("Expected an updated storage state")
//        }
//      }
//    }
//
//    "determining the energy demand" should {
//      "deliver the capabilities of the storage" in {
//        val tick = 10800 // after three hours
//
//        val gridDemand = thermalGrid.energyDemand(
//          tick,
//          ambientTemperature,
//          ThermalGrid.startingState(thermalGrid)
//        )
//
//        gridDemand.required should equalWithTolerance(
//          Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
//        )
//        gridDemand.possible should equalWithTolerance(
//          Quantities.getQuantity(0.92d, StandardUnits.ENERGY_RESULT)
//        )
//      }
//    }
//
//    "handling thermal energy consumption from grid" should {
//      val handleConsumption =
//        PrivateMethod[(ThermalGridState, Option[ThermalThreshold])](
//          Symbol("handleConsumption")
//        )
//
//      "properly take the energy from storage" in {
//        val tick = 0L
//        val gridState = ThermalGrid
//          .startingState(thermalGrid)
//          .copy(storageState =
//            Some(
//              ThermalStorageState(
//                0L,
//                Quantities.getQuantity(430d, StandardUnits.ENERGY_IN),
//                Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
//              )
//            )
//          )
//
//        val (updatedGridState, reachedThreshold) =
//          thermalGrid invokePrivate handleConsumption(
//            tick,
//            ambientTemperature,
//            gridState,
//            qDotConsumption
//          )
//
//        updatedGridState match {
//          case ThermalGridState(
//                None,
//                Some(ThermalStorageState(tick, storedEnergy, qDot))
//              ) =>
//            tick shouldBe 0L
//            storedEnergy should equalWithTolerance(
//              Quantities.getQuantity(430d, StandardUnits.ENERGY_IN)
//            )
//            qDot should equalWithTolerance(qDotConsumption)
//          case _ => fail("Thermal grid state has been calculated wrong.")
//        }
//        reachedThreshold shouldBe Some(ThermalGridEmpty(3600L))
//      }
//    }
//
//    "handling thermal infeed into the grid" should {
//      val handleInfeed =
//        PrivateMethod[(ThermalGridState, Option[ThermalGridThreshold])](
//          Symbol("handleInfeed")
//        )
//
//      "properly put energy to storage" in {
//        val tick = 0L
//        val gridState = ThermalGrid.startingState(thermalGrid)
//
//        val (updatedGridState, reachedThreshold) =
//          thermalGrid invokePrivate handleInfeed(
//            tick,
//            ambientTemperature,
//            gridState,
//            qDotInfeed
//          )
//
//        updatedGridState match {
//          case ThermalGridState(
//                None,
//                Some(ThermalStorageState(tick, storedEnergy, qDot))
//              ) =>
//            tick shouldBe 0L
//            storedEnergy should equalWithTolerance(
//              Quantities.getQuantity(230d, StandardUnits.ENERGY_IN)
//            )
//            qDot should equalWithTolerance(qDotInfeed)
//          case _ => fail("Thermal grid state has been calculated wrong.")
//        }
//        reachedThreshold shouldBe Some(ThermalGridFilledUp(220800L))
//      }
//    }
//
//    "updating the grid state dependent on the given thermal infeed" should {
//      "deliver proper result, if energy is fed into the grid" in {
//        thermalGrid.updateState(
//          0L,
//          ThermalGrid.startingState(thermalGrid),
//          ambientTemperature,
//          qDotInfeed
//        ) match {
//          case (
//                ThermalGridState(
//                  None,
//                  Some(ThermalStorageState(tick, storedEnergy, qDot))
//                ),
//                Some(ThermalGridFilledUp(thresholdTick))
//              ) =>
//            tick shouldBe 0L
//            storedEnergy should equalWithTolerance(
//              Quantities.getQuantity(230d, StandardUnits.ENERGY_IN)
//            )
//            qDot should equalWithTolerance(qDotInfeed)
//            thresholdTick shouldBe 220800L
//          case _ => fail("Thermal grid state updated failed")
//        }
//      }
//
//      "deliver proper result, if energy is consumed from the grid" in {
//        thermalGrid.updateState(
//          0L,
//          ThermalGrid
//            .startingState(thermalGrid)
//            .copy(storageState =
//              Some(
//                ThermalStorageState(
//                  0L,
//                  Quantities.getQuantity(430d, StandardUnits.ENERGY_IN),
//                  Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
//                )
//              )
//            ),
//          ambientTemperature,
//          qDotConsumption
//        ) match {
//          case (
//                ThermalGridState(
//                  None,
//                  Some(ThermalStorageState(tick, storedEnergy, qDot))
//                ),
//                Some(ThermalGridEmpty(thresholdTick))
//              ) =>
//            tick shouldBe 0L
//            storedEnergy should equalWithTolerance(
//              Quantities.getQuantity(430d, StandardUnits.ENERGY_IN)
//            )
//            qDot should equalWithTolerance(qDotConsumption)
//            thresholdTick shouldBe 3600L
//          case _ => fail("Thermal grid state updated failed")
//        }
//      }
//
//      "deliver proper result, if energy is neither consumed from nor fed into the grid" in {
//        val updatedState = thermalGrid.updateState(
//          0L,
//          ThermalGrid.startingState(thermalGrid),
//          ambientTemperature,
//          Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
//        )
//        updatedState match {
//          case (
//                ThermalGridState(
//                  None,
//                  Some(ThermalStorageState(tick, storedEnergy, qDot))
//                ),
//                None
//              ) =>
//            tick shouldBe 0L
//            storedEnergy should equalWithTolerance(
//              Quantities.getQuantity(230d, StandardUnits.ENERGY_IN)
//            )
//            qDot should equalWithTolerance(
//              Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
//            )
//          case _ => fail("Thermal grid state updated failed")
//        }
//      }
//    }
//  }
}
