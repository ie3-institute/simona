/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureUpperBoundaryReached,
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull,
}
import edu.ie3.simona.test.common.UnitSpec
import squants.energy._
import squants.thermal.Celsius
import squants.{Energy, Kelvin, Power, Temperature}
import tech.units.indriya.unit.Units

import scala.jdk.CollectionConverters._

class ThermalGridWithHouseAndStorageSpec
    extends UnitSpec
    with ThermalHouseTestData
    with ThermalStorageTestData {

  implicit val tempTolerance: Temperature = Kelvin(1e-3)
  implicit val powerTolerance: Power = Watts(1e-3)
  implicit val energyTolerance: Energy = WattHours(1e-3)

  "Testing thermal grid generation with only a house" should {
    "instantiating correctly from input data" in {
      val thermalGridInput =
        new edu.ie3.datamodel.models.input.container.ThermalGrid(
          thermalBusInput,
          Set(thermalHouseInput).asJava,
          Set[ThermalStorageInput](thermalStorageInput).asJava,
        )

      ThermalGrid(thermalGridInput) match {
        case ThermalGrid(
              Some(thermalHouseGenerated),
              Some(thermalStorageGenerated),
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
        Set[ThermalStorageInput](thermalStorageInput).asJava,
      )
    )

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                ),
              ) =>
            houseTick shouldBe expectedHouseStartingState.tick
            storageTick shouldBe expectedHouseStartingState.tick

            innerTemperature should approximate(
              expectedHouseStartingState.innerTemperature
            )
            storedEnergy should approximate(
              expectedStorageStartingState.storedEnergy
            )
            qDotHouse should approximate(expectedHouseStartingState.qDot)
            qDotStorage should approximate(expectedStorageStartingState.qDot)

          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "determining the energy demand" should {
      "deliver the house demand (no demand) with added flexibility by storage" in {
        val tick = 10800 // after three hours

        val (houseDemand, storageDemand, updatedThermalGridState) =
          thermalGrid.energyDemandAndUpdatedState(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            ThermalGrid.startingState(thermalGrid),
          )
        houseDemand.required should approximate(KilowattHours(0d))
        houseDemand.possible should approximate(KilowattHours(31.05009722d))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Kelvin(292.0799935185185), Kilowatts(0d))
        )
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800, KilowattHours(0d), Kilowatts(0d))
        )
      }

      "deliver the correct house and storage demand" in {
        val tick = 10800 // after three hours

        val startingState = ThermalGrid.startingState(thermalGrid)
        val (houseDemand, storageDemand, updatedThermalGridState) =
          thermalGrid.energyDemandAndUpdatedState(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            startingState.copy(houseState =
              startingState.houseState.map(
                _.copy(innerTemperature = Celsius(16d))
              )
            ),
          )

        houseDemand.required should approximate(KilowattHours(45.6000555))
        houseDemand.possible should approximate(KilowattHours(75.600055555))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Celsius(15.959996296296296), Kilowatts(0d))
        )
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800, KilowattHours(0d), Kilowatts(0d))
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
        val initialLoading = KilowattHours(430d)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading)
          )
        )
        val externalQDot = Kilowatts(0d)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            gridState,
            externalQDot,
          )

        updatedGridState match {
          case ThermalGridState(
                _,
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                ),
              ) =>
            storageTick shouldBe 0L
            storedEnergy should approximate(initialLoading)
            qDotStorage should approximate(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154285L)
        )
      }

      "take energy from storage, if there is actual consumption" in {
        val tick = 0L
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val initialLoading = KilowattHours(200d)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading)
          )
        )
        val externalQDot = testGridQDotConsumption

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            gridState,
            externalQDot,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                ),
              ) =>
            houseTick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDotHouse should approximate(Kilowatts(0d))

            storageTick shouldBe 0L
            storedEnergy should approximate(initialLoading)
            qDotStorage should approximate(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageEmpty(17143L))
      }
    }

    "revising infeed from storage to house" should {
      val zeroInflux = Kilowatts(0d)
      val tick = 3600L
      val ambientTemperature = Celsius(14d)
      "hand back unaltered information if needed information is missing" in {
        val maybeHouseState = Some(
          (
            ThermalHouseState(
              tick,
              Celsius(
                thermalHouseInput.getTargetTemperature
                  .to(Units.CELSIUS)
                  .getValue
                  .doubleValue
              ),
              zeroInflux,
            ),
            None,
          )
        )
        val maybeStorageState = None

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          None,
          testGridAmbientTemperature,
          testGridAmbientTemperature,
          testGridQDotConsumption,
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
              Celsius(
                thermalHouseInput.getTargetTemperature
                  .to(Units.CELSIUS)
                  .getValue
                  .doubleValue
              ),
              zeroInflux,
            ),
            None,
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              KilowattHours(50d),
              zeroInflux,
            ),
            None,
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
          ambientTemperature,
          ambientTemperature,
          zeroInflux,
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
              Celsius(
                thermalHouseInput.getTargetTemperature
                  .to(Units.CELSIUS)
                  .getValue
                  .doubleValue
              ),
              testGridQDotInfeed,
            ),
            Some(HouseTemperatureUpperBoundaryReached(3600L)),
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              KilowattHours(50d),
              zeroInflux,
            ),
            None,
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
          ambientTemperature,
          ambientTemperature,
          testGridQDotInfeed,
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
              Celsius(
                thermalHouseInput.getLowerTemperatureLimit
                  .to(Units.CELSIUS)
                  .getValue
                  .doubleValue
              ),
              zeroInflux,
            ),
            Some(HouseTemperatureLowerBoundaryReached(tick)),
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              KilowattHours(0d),
              testGridQDotInfeed,
            ),
            Some(StorageEmpty(tick)),
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
          ambientTemperature,
          ambientTemperature,
          zeroInflux,
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
              Celsius(
                thermalHouseInput.getLowerTemperatureLimit
                  .to(Units.CELSIUS)
                  .getValue
                  .doubleValue
              ),
              zeroInflux,
            ),
            Some(HouseTemperatureLowerBoundaryReached(tick)),
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              KilowattHours(20d),
              testGridQDotInfeed,
            ),
            None,
          )
        )
        val formerHouseState = Some(
          ThermalHouseState(
            0L,
            Celsius(
              thermalHouseInput.getTargetTemperature
                .to(Units.CELSIUS)
                .getValue
                .doubleValue
            ),
            zeroInflux,
          )
        )
        val formerStorageState = Some(
          ThermalStorageState(
            0L,
            KilowattHours(70d),
            Kilowatts(-50d),
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          formerHouseState,
          formerStorageState,
          ambientTemperature,
          ambientTemperature,
          zeroInflux,
        ) match {
          case (
                Some(
                  (
                    ThermalHouseState(houseTick, _, revisedQDotHouse),
                    Some(HouseTemperatureUpperBoundaryReached(houseWarmTick)),
                  )
                ),
                Some(
                  (
                    ThermalStorageState(storageTick, _, revisedQDotStorage),
                    Some(StorageEmpty(storageEmptyTick)),
                  )
                ),
              ) =>
            houseTick shouldBe tick
            storageTick shouldBe tick

            revisedQDotHouse should approximate(thermalStorage.chargingPower)
            revisedQDotStorage should approximate(
              thermalStorage.chargingPower * (-1)
            )

            houseWarmTick shouldBe 3695L
            storageEmptyTick shouldBe 3663L
          case _ => fail("Revision of states failed")
        }
      }
    }

    "handling thermal infeed into the grid" should {
      val handleInfeed =
        PrivateMethod[(ThermalGridState, Option[ThermalThreshold])](
          Symbol("handleInfeed")
        )

      "heat the house, if the upper temperature in the house is not reached" in {
        val tick = 0L
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val externalQDot = testGridQDotInfeed

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            initialGridState,
            isNotRunning,
            externalQDot,
            thermalDemand,
            noThermalDemand,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                ),
              ) =>
            houseTick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDotHouse should approximate(externalQDot)

            storageTick shouldBe 0L
            storedEnergy should approximate(
              initialGridState.storageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )

            qDotStorage should approximate(Kilowatts(0d))

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
        val externalQDot = testGridQDotInfeed

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            gridState,
            isNotRunning,
            externalQDot,
            noThermalDemand,
            thermalDemand,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                ),
              ) =>
            houseTick shouldBe 0L
            innerTemperature should approximate(Celsius(20.99999167d))
            qDotHouse should approximate(Kilowatts(0d))

            storageTick shouldBe 0L
            storedEnergy should approximate(
              gridState.storageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotStorage should approximate(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          StorageFull(276000L)
        )
      }
    }
  }
}
