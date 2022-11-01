/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureUpperBoundaryReached
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull
}
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

    "handling thermal energy consumption from grid" should {
      val handleConsumption =
        PrivateMethod[(ThermalGridState, Option[ThermalThreshold])](
          Symbol("handleConsumption")
        )

      "return house threshold, if storage is in balance" in {
        val tick = 0L
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val initialLoading =
          Quantities.getQuantity(430d, StandardUnits.ENERGY_IN)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading)
          )
        )
        val externalQDot =
          Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            ambientTemperature,
            gridState,
            externalQDot
          )

        updatedGridState match {
          case ThermalGridState(
                _,
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                )
              ) =>
            storageTick shouldBe 0L
            storedEnergy should equalWithTolerance(initialLoading)
            qDotStorage should equalWithTolerance(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154285L)
        )
      }

      "take energy from storage, if there is actual consumption" in {
        val tick = 0L
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val initialLoading =
          Quantities.getQuantity(430d, StandardUnits.ENERGY_IN)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading)
          )
        )
        val externalQDot = qDotConsumption

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
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                )
              ) =>
            houseTick shouldBe 0L
            innerTemperature should equalWithTolerance(
              Quantities.getQuantity(18.9999d, Units.CELSIUS),
              1e-3
            )
            qDotHouse should equalWithTolerance(
              Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
            )

            storageTick shouldBe 0L
            storedEnergy should equalWithTolerance(initialLoading)
            qDotStorage should equalWithTolerance(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageEmpty(17143L))
      }
    }

    "revising infeed from storage to house" should {
      val zeroInflux = Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
      val tick = 3600L
      val ambientTemperature =
        Quantities.getQuantity(14d, StandardUnits.TEMPERATURE)
      "hand back unaltered information if needed information is missing" in {
        val maybeHouseState = Some(
          (
            ThermalHouseState(
              tick,
              thermalHouseInput.getTargetTemperature,
              zeroInflux
            ),
            None
          )
        )
        val maybeStorageState = None

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          None,
          ambientTemperature,
          qDotConsumption
        ) match {
          case (maybeRevisedHouseState, maybeRevisedStorageState) =>
            maybeRevisedHouseState shouldBe maybeHouseState
            maybeRevisedStorageState shouldBe maybeStorageState
        }
      }

      "hand back unaltered information if house temperature is above lower boundary temperature" in {
        val maybeHouseState = Some(
          (
            ThermalHouseState(
              tick,
              thermalHouseInput.getTargetTemperature,
              zeroInflux
            ),
            None
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              Quantities.getQuantity(50d, StandardUnits.ENERGY_IN),
              zeroInflux
            ),
            None
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
          ambientTemperature,
          zeroInflux
        ) match {
          case (maybeRevisedHouseState, maybeRevisedStorageState) =>
            maybeRevisedHouseState shouldBe maybeHouseState
            maybeRevisedStorageState shouldBe maybeStorageState
        }
      }

      "hand back unaltered information if house temperature is above lower boundary temperature, but has influx" in {
        val maybeHouseState = Some(
          (
            ThermalHouseState(
              tick,
              thermalHouseInput.getTargetTemperature,
              qDotInfeed
            ),
            Some(HouseTemperatureUpperBoundaryReached(3600L))
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              Quantities.getQuantity(50d, StandardUnits.ENERGY_IN),
              zeroInflux
            ),
            None
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
          ambientTemperature,
          qDotInfeed
        ) match {
          case (maybeRevisedHouseState, maybeRevisedStorageState) =>
            maybeRevisedHouseState shouldBe maybeHouseState
            maybeRevisedStorageState shouldBe maybeStorageState
        }
      }

      "hand back unaltered information if house temperature is at lower boundary temperature, but storage is empty" in {
        val maybeHouseState = Some(
          (
            ThermalHouseState(
              tick,
              thermalHouseInput.getLowerTemperatureLimit,
              zeroInflux
            ),
            Some(HouseTemperatureLowerBoundaryReached(tick))
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              Quantities.getQuantity(50d, StandardUnits.ENERGY_IN),
              qDotInfeed
            ),
            Some(StorageEmpty(tick))
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
          ambientTemperature,
          zeroInflux
        ) match {
          case (maybeRevisedHouseState, maybeRevisedStorageState) =>
            maybeRevisedHouseState shouldBe maybeHouseState
            maybeRevisedStorageState shouldBe maybeStorageState
        }
      }

      "alter the given states as expected, when all conditions are met" in {
        val maybeHouseState = Some(
          (
            ThermalHouseState(
              tick,
              thermalHouseInput.getLowerTemperatureLimit,
              zeroInflux
            ),
            Some(HouseTemperatureLowerBoundaryReached(tick))
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              Quantities.getQuantity(250d, StandardUnits.ENERGY_IN),
              qDotInfeed
            ),
            None
          )
        )
        val formerHouseState = Some(
          ThermalHouseState(
            0L,
            thermalHouseInput.getTargetTemperature,
            zeroInflux
          )
        )
        val formerStorageState = Some(
          ThermalStorageState(
            0L,
            Quantities.getQuantity(300d, StandardUnits.ENERGY_IN),
            Quantities.getQuantity(-50d, StandardUnits.ACTIVE_POWER_IN)
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          formerHouseState,
          formerStorageState,
          ambientTemperature,
          zeroInflux
        ) match {
          case (
                Some(
                  (
                    ThermalHouseState(houseTick, _, revisedQDotHouse),
                    Some(HouseTemperatureUpperBoundaryReached(houseColdTick))
                  )
                ),
                Some(
                  (
                    ThermalStorageState(storageTick, _, revisedQDotStorage),
                    Some(StorageEmpty(storageEmptyTick))
                  )
                )
              ) =>
            houseTick shouldBe tick
            storageTick shouldBe tick

            revisedQDotHouse should equalWithTolerance(
              thermalStorage.chargingPower
            )
            revisedQDotStorage should equalWithTolerance(
              thermalStorage.chargingPower.multiply(-1)
            )

            houseColdTick shouldBe 3718L
            storageEmptyTick shouldBe 3678L
          case _ => fail("Revision of states failed")
        }
      }
    }

    "handling thermal infeed into the grid" should {
      val handleInfeed =
        PrivateMethod[(ThermalGridState, Option[ThermalThreshold])](
          Symbol("handleInfeed")
        )

      "heat the use, if the upper temperature in the house is not reached" in {
        val tick = 0L
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val externalQDot = qDotInfeed

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            tick,
            ambientTemperature,
            initialGridState,
            externalQDot
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                )
              ) =>
            houseTick shouldBe 0L
            innerTemperature should equalWithTolerance(
              Quantities.getQuantity(18.9999d, Units.CELSIUS),
              1e-3
            )
            qDotHouse should equalWithTolerance(externalQDot)

            storageTick shouldBe -1L
            storedEnergy should equalWithTolerance(
              initialGridState.storageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotStorage should equalWithTolerance(
              Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureUpperBoundaryReached(7372L)
        )
      }

      "load the storage, if the upper temperature in the house is reached" in {
        val tick = 0L
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val gridState = initialGridState.copy(houseState =
          initialGridState.houseState.map(
            _.copy(innerTemperature = thermalHouse.upperBoundaryTemperature)
          )
        )
        val externalQDot = qDotInfeed

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            tick,
            ambientTemperature,
            gridState,
            externalQDot
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                )
              ) =>
            houseTick shouldBe 0L
            innerTemperature should equalWithTolerance(
              Quantities.getQuantity(20.99999167d, Units.CELSIUS),
              1e-3
            )
            qDotHouse should equalWithTolerance(
              Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
            )

            storageTick shouldBe 0L
            storedEnergy should equalWithTolerance(
              gridState.storageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotStorage should equalWithTolerance(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          StorageFull(220800L)
        )
      }
    }
  }
}
