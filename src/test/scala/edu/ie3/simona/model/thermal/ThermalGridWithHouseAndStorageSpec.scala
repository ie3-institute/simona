/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
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

class ThermalGridWithHouseAndStorageSpec
    extends UnitSpec
    with ThermalHouseTestData
    with ThermalStorageTestData {
  "Testing thermal grid generation with only a house" should {
    "instantiating correctly from input data" in {
      val thermalGridInput =
        new edu.ie3.datamodel.models.input.container.ThermalGrid(
          thermalBusInput,
          Set(thermalHouseInput).asJava,
          Set[ThermalStorageInput](thermalStorageInput).asJava
        )

      ThermalGrid(thermalGridInput) match {
        case ThermalGrid(
              Some(thermalHouseGenerated),
              Some(thermalStorageGenerated)
            ) =>
          thermalHouseGenerated shouldBe thermalHouse
          thermalStorageGenerated shouldBe thermalStorage
        case _ =>
          fail("Generation of thermal grid from thermal input grid failed.")
      }
    }
  }

  "Testing a thermal grid with house and storage" when {
    val thermalGrid: ThermalGrid = ThermalGrid(
      new edu.ie3.datamodel.models.input.container.ThermalGrid(
        thermalBusInput,
        Set(thermalHouseInput).asJava,
        Set[ThermalStorageInput](thermalStorageInput).asJava
      )
    )
    val ambientTemperature = Quantities.getQuantity(12d, Units.CELSIUS)
    val qDotInfeed =
      Quantities.getQuantity(15d, StandardUnits.ACTIVE_POWER_IN)
    val qDotConsumption =
      Quantities.getQuantity(-42d, StandardUnits.ACTIVE_POWER_IN)

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                )
              ) =>
            houseTick shouldBe expectedHouseStartingState.tick
            storageTick shouldBe expectedHouseStartingState.tick
            innerTemperature should equalWithTolerance(
              expectedHouseStartingState.innerTemperature
            )
            storedEnergy should equalWithTolerance(
              expectedStorageStartingState.storedEnergy
            )
            qDotHouse should equalWithTolerance(
              expectedHouseStartingState.qDot
            )
            qDotStorage should equalWithTolerance(
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

        thermalGrid invokePrivate takeOrPushToStorage(
          42L,
          ThermalGrid.startingState(thermalGrid).storageState,
          qDotInfeed
        ) match {
          case Some((ThermalStorageState(tick, storedEnergy, qDot), reason)) =>
            tick shouldBe 42L
            storedEnergy shouldBe Quantities.getQuantity(
              230d,
              StandardUnits.ENERGY_IN
            )
            qDot shouldBe qDotInfeed
            reason shouldBe Some(StorageFull(220842L))
          case _ => fail("Expected an updated storage state")
        }
      }
    }

    "determining the energy demand" should {
      "deliver the house demand (no demand) with added flexibility by storage" in {
        val tick = 10800 // after three hours

        val gridDemand = thermalGrid.energyDemand(
          tick,
          ambientTemperature,
          ThermalGrid.startingState(thermalGrid)
        )

        gridDemand.required should equalWithTolerance(
          Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
        )
        gridDemand.possible should equalWithTolerance(
          Quantities.getQuantity(0.031050 + 0.920, StandardUnits.ENERGY_RESULT),
          1e-6
        )
      }

      "consider stored energy to reduce house demand" in {
        val tick = 10800 // after three hours

        val startingState = ThermalGrid.startingState(thermalGrid)
        val gridDemand = thermalGrid.energyDemand(
          tick,
          ambientTemperature,
          startingState.copy(houseState =
            startingState.houseState.map(
              _.copy(innerTemperature =
                Quantities.getQuantity(16d, Units.CELSIUS)
              )
            )
          )
        )

        gridDemand.required should equalWithTolerance(
          Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
        )
        gridDemand.possible should equalWithTolerance(
          Quantities.getQuantity(1.041200, StandardUnits.ENERGY_RESULT),
          1e-6
        )
      }

      "consider stored energy to reduce house demand if stored energy is not enough" in {
        val tick = 10800 // after three hours

        val startingState = ThermalGrid.startingState(thermalGrid)
        val gridDemand = thermalGrid.energyDemand(
          tick,
          ambientTemperature,
          startingState.copy(houseState =
            startingState.houseState.map(
              _.copy(innerTemperature =
                Quantities.getQuantity(3d, Units.CELSIUS)
              )
            )
          )
        )

        gridDemand.required should equalWithTolerance(
          Quantities.getQuantity(0.0086499, StandardUnits.ENERGY_RESULT),
          1e-6
        )
        gridDemand.possible should equalWithTolerance(
          Quantities.getQuantity(1.4186499, StandardUnits.ENERGY_RESULT),
          1e-6
        )
      }
    }

    "handling too low infeed" should {
      val handleTooLowInfeed =
        PrivateMethod[(ThermalGridState, Some[ThermalGridThreshold])](
          Symbol("handleTooLowInfeed")
        )
      val initialTick = 0L
      val coldHouseTick = 3600L
      val ambientTemperature = Quantities.getQuantity(12d, Units.CELSIUS)
      val initialHouseState = expectedHouseStartingState
      val initialStorageState = expectedStorageStartingState

      "deliver correct state, if there is no external infeed at all and no sufficient stored energy" in {
        val qDot = Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
        val coldHouseState = thermalHouse
          .updateState(
            coldHouseTick,
            initialHouseState,
            ambientTemperature,
            qDot
          )
          ._1

        thermalGrid.handleTooLowInfeed(
          initialTick,
          coldHouseTick,
          initialHouseState,
          Some(initialStorageState),
          coldHouseState,
          ambientTemperature,
          qDot
        ) match {
          case (
                ThermalGridState(
                  Some(
                    ThermalHouseState(houseTick, innerTemperature, qDotHouse)
                  ),
                  Some(
                    ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                  )
                ),
                Some(ThermalGridEmpty(gridEmptyTick))
              ) =>
            houseTick shouldBe initialHouseState.tick
            innerTemperature should equalWithTolerance(
              initialHouseState.innerTemperature
            )
            qDotHouse should equalWithTolerance(initialHouseState.qDot)

            storageTick shouldBe initialStorageState.tick
            storedEnergy shouldBe initialStorageState.storedEnergy
            qDotStorage shouldBe initialStorageState.qDot

            gridEmptyTick shouldBe coldHouseTick
        }
      }

      "deliver correct state, if there is no external infeed at all and sufficient stored energy" in {
        val qDot = Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
        val coldHouseState = thermalHouse
          .updateState(
            coldHouseTick,
            initialHouseState,
            ambientTemperature,
            qDot
          )
          ._1
        val loadedStorageState = initialStorageState.copy(storedEnergy =
          Quantities.getQuantity(430d, StandardUnits.ENERGY_IN)
        )

        thermalGrid.handleTooLowInfeed(
          initialTick,
          coldHouseTick,
          initialHouseState,
          Some(loadedStorageState),
          coldHouseState,
          ambientTemperature,
          qDot
        ) match {
          case (
                ThermalGridState(
                  Some(
                    ThermalHouseState(houseTick, innerTemperature, qDotHouse)
                  ),
                  Some(
                    ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                  )
                ),
                Some(ThermalGridEmpty(gridEmptyTick))
              ) =>
            houseTick shouldBe initialTick
            innerTemperature should equalWithTolerance(
              Quantities.getQuantity(18.9999935, Units.CELSIUS),
              1e-6
            )
            qDotHouse should equalWithTolerance(
              Quantities.getQuantity(0.4117649, StandardUnits.ACTIVE_POWER_IN),
              1e-6
            )

            storageTick shouldBe initialTick
            storedEnergy shouldBe loadedStorageState.storedEnergy
            qDotStorage should equalWithTolerance(
              Quantities.getQuantity(-0.4117649, StandardUnits.ACTIVE_POWER_IN),
              1e-6
            )

            gridEmptyTick shouldBe 2108560L
        }
      }

      "deliver correct state, if there is too less external infeed and sufficient stored energy" in {
        val qDot = Quantities.getQuantity(0.2d, StandardUnits.ACTIVE_POWER_IN)
        val coldHouseState = thermalHouse
          .updateState(
            coldHouseTick,
            initialHouseState,
            ambientTemperature,
            qDot
          )
          ._1
        val loadedStorageState = initialStorageState.copy(storedEnergy =
          Quantities.getQuantity(430d, StandardUnits.ENERGY_IN)
        )

        thermalGrid.handleTooLowInfeed(
          initialTick,
          coldHouseTick,
          initialHouseState,
          Some(loadedStorageState),
          coldHouseState,
          ambientTemperature,
          qDot
        ) match {
          case (
                ThermalGridState(_, _),
                Some(ThermalGridEmpty(gridEmptyTick))
              ) =>
            gridEmptyTick shouldBe 772583
        }
      }
    }

    "handling thermal energy consumption from grid" should {
      val handleConsumption =
        PrivateMethod[(ThermalGridState, Option[ThermalGridThreshold])](
          Symbol("handleConsumption")
        )

      "heat house from storage, if there is no infeed at all" in {
        val tick = 0L
        val gridState = ThermalGrid.startingState(thermalGrid)
        val externalQDot =
          Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            ambientTemperature,
            gridState,
            externalQDot
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(ThermalStorageState(tick, storedEnergy, qDotStorage))
              ) =>
            houseTick shouldBe 0L
            innerTemperature should equalWithTolerance(
              Quantities.getQuantity(18.9999d, Units.CELSIUS),
              1e-3
            )
            qDotHouse should equalWithTolerance(
              Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
            )
            qDotHouse should equalWithTolerance(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(ThermalGridEmpty(154284L))
      }

//      "deliver the house state by just letting it cool down, if just no infeed is given" in {
//        val tick = 0L
//        val gridState = ThermalGrid.startingState(thermalGrid)
//        val externalQDot =
//          Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
//
//        val (updatedGridState, reachedThreshold) =
//          thermalGrid invokePrivate handleConsumption(
//            tick,
//            ambientTemperature,
//            gridState,
//            externalQDot
//          )
//
//        updatedGridState match {
//          case ThermalGridState(
//                Some(ThermalHouseState(tick, innerTemperature, qDot)),
//                None
//              ) =>
//            tick shouldBe 0L
//            innerTemperature should equalWithTolerance(
//              Quantities.getQuantity(18.9999d, Units.CELSIUS),
//              1e-3
//            )
//            qDot should equalWithTolerance(externalQDot)
//          case _ => fail("Thermal grid state has been calculated wrong.")
//        }
//        reachedThreshold shouldBe Some(ThermalGridEmpty(154284L))
//      }
//
//      "not withdraw energy from the house, if actual consumption is given" in {
//        val tick = 0L // after three house
//        val gridState = ThermalGrid.startingState(thermalGrid)
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
//                Some(ThermalHouseState(tick, innerTemperature, qDot)),
//                None
//              ) =>
//            tick shouldBe 0L
//            innerTemperature should equalWithTolerance(
//              Quantities.getQuantity(18.9999d, Units.CELSIUS),
//              1e-3
//            )
//            qDot should equalWithTolerance(
//              Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
//            )
//          case _ => fail("Thermal grid state has been calculated wrong.")
//        }
//        reachedThreshold shouldBe Some(ThermalGridEmpty(154284L))
//      }
    }

//    "handling thermal infeed into the grid" should {
//      "solely heat up the house" in {
//        val tick = 0L
//        val gridState = ThermalGrid.startingState(thermalGrid)
//
//        val (updatedGridState, reachedThreshold) =
//          thermalGrid.handleInfeed(
//            tick,
//            ambientTemperature,
//            gridState,
//            qDotInfeed
//          )
//
//        updatedGridState match {
//          case ThermalGridState(
//                Some(ThermalHouseState(tick, innerTemperature, qDot)),
//                None
//              ) =>
//            tick shouldBe 0L
//            innerTemperature should equalWithTolerance(
//              Quantities.getQuantity(18.9999d, Units.CELSIUS),
//              1e-3
//            )
//            qDot should equalWithTolerance(qDotInfeed)
//          case _ => fail("Thermal grid state has been calculated wrong.")
//        }
//        reachedThreshold shouldBe Some(ThermalGridFilledUp(7372L))
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
//                  Some(ThermalHouseState(tick, innerTemperature, qDot)),
//                  None
//                ),
//                Some(ThermalGridFilledUp(thresholdTick))
//              ) =>
//            tick shouldBe 0L
//            innerTemperature should equalWithTolerance(
//              Quantities.getQuantity(18.9999d, Units.CELSIUS),
//              1e-3
//            )
//            qDot should equalWithTolerance(qDotInfeed)
//            thresholdTick shouldBe 7372L
//          case _ => fail("Thermal grid state updated failed")
//        }
//      }
//
//      "deliver proper result, if energy is consumed from the grid" in {
//        thermalGrid.updateState(
//          0L,
//          ThermalGrid.startingState(thermalGrid),
//          ambientTemperature,
//          qDotConsumption
//        ) match {
//          case (
//                ThermalGridState(
//                  Some(ThermalHouseState(tick, innerTemperature, qDot)),
//                  None
//                ),
//                Some(ThermalGridEmpty(thresholdTick))
//              ) =>
//            tick shouldBe 0L
//            innerTemperature should equalWithTolerance(
//              Quantities.getQuantity(18.9999d, Units.CELSIUS),
//              1e-3
//            )
//            qDot should equalWithTolerance(
//              Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
//            )
//            thresholdTick shouldBe 154284L
//          case _ => fail("Thermal grid state updated failed")
//        }
//      }
//
//      "deliver proper result, if energy is neither consumed from nor fed into the grid" in {
//        thermalGrid.updateState(
//          0L,
//          ThermalGrid.startingState(thermalGrid),
//          ambientTemperature,
//          Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
//        ) match {
//          case (
//                ThermalGridState(
//                  Some(ThermalHouseState(tick, innerTemperature, qDot)),
//                  None
//                ),
//                Some(ThermalGridEmpty(thresholdTick))
//              ) =>
//            tick shouldBe 0L
//            innerTemperature should equalWithTolerance(
//              Quantities.getQuantity(18.9999d, Units.CELSIUS),
//              1e-3
//            )
//            qDot should equalWithTolerance(
//              Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
//            )
//            thresholdTick shouldBe 154284L
//          case _ => fail("Thermal grid state updated failed")
//        }
//      }
//    }
  }
}
