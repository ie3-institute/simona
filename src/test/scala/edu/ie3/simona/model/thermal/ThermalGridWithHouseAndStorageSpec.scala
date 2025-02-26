/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.model.participant.HpModel.{HpRelevantData, HpState}
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureTargetOrUpperBoundaryReached,
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
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
        val relevantData = HpRelevantData(
          10800, // after three hours
          testGridAmbientTemperature,
        )
        val lastHpState = HpState(
          true,
          relevantData.currentTick,
          Some(testGridAmbientTemperature),
          Kilowatts(42),
          Kilowatts(42),
          ThermalGrid.startingState(thermalGrid),
          None,
        )
        val (thermalDemands, updatedThermalGridState) =
          thermalGrid.energyDemandAndUpdatedState(
            relevantData,
            lastHpState,
          )
        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(KilowattHours(1.05009722d))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Kelvin(292.0799935185185), zeroKW)
        )
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800, zeroKWh, zeroKW)
        )
      }

      "deliver the correct house and storage demand" in {
        val relevantData = HpRelevantData(
          10800, // after three hours
          testGridAmbientTemperature,
        )
        val startingState = ThermalGrid.startingState(thermalGrid)
        val lastHpState = HpState(
          true,
          relevantData.currentTick,
          Some(testGridAmbientTemperature),
          Kilowatts(42),
          Kilowatts(42),
          startingState.copy(houseState =
            startingState.houseState.map(
              _.copy(innerTemperature = Celsius(16d))
            )
          ),
          None,
        )

        val (thermalDemands, updatedThermalGridState) =
          thermalGrid.energyDemandAndUpdatedState(
            relevantData,
            lastHpState,
          )
        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand

        houseDemand.required should approximate(KilowattHours(45.6000555))
        houseDemand.possible should approximate(KilowattHours(45.600055555))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Celsius(15.959996296296296), zeroKW)
        )
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800, zeroKWh, zeroKW)
        )
      }
    }

    "handling thermal energy consumption from grid" should {
      val handleConsumption =
        PrivateMethod[(ThermalGridState, Option[ThermalThreshold])](
          Symbol("handleConsumption")
        )

      "return house threshold, if storage is in balance" in {
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
        )
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val initialLoading = KilowattHours(430d)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading)
          )
        )
        val externalQDot = zeroKW

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            relevantData,
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
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
        )
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
            relevantData,
            testGridAmbientTemperature,
            gridState,
            externalQDot,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(
                  ThermalHouseState(houseTick, innerTemperature, qDotHouse)
                ),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                ),
              ) =>
            houseTick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDotHouse should approximate(zeroKW)

            storageTick shouldBe 0L
            storedEnergy should approximate(initialLoading)
            qDotStorage should approximate(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageEmpty(17143L))
      }
    }

    "revising infeed from storage to house" should {
      val zeroInflux = zeroKW
      val relevantData = HpRelevantData(
        3600,
        testGridAmbientTemperature,
      )
      val ambientTemperature = Celsius(14d)
      "hand back unaltered information if needed information is missing" in {
        val maybeHouseState = Some(
          (
            ThermalHouseState(
              relevantData.currentTick,
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
          relevantData,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          None,
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
              relevantData.currentTick,
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
              relevantData.currentTick,
              KilowattHours(50d),
              zeroInflux,
            ),
            None,
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          relevantData,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
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
              relevantData.currentTick,
              Celsius(
                thermalHouseInput.getTargetTemperature
                  .to(Units.CELSIUS)
                  .getValue
                  .doubleValue
              ),
              testGridQDotInfeed,
            ),
            Some(HouseTemperatureTargetOrUpperBoundaryReached(3600L)),
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              relevantData.currentTick,
              KilowattHours(50d),
              zeroInflux,
            ),
            None,
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          relevantData,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
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
              relevantData.currentTick,
              Celsius(
                thermalHouseInput.getLowerTemperatureLimit
                  .to(Units.CELSIUS)
                  .getValue
                  .doubleValue
              ),
              zeroInflux,
            ),
            Some(
              HouseTemperatureLowerBoundaryReached(relevantData.currentTick)
            ),
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              relevantData.currentTick,
              zeroKWh,
              testGridQDotInfeed,
            ),
            Some(StorageEmpty(relevantData.currentTick)),
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          relevantData,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
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
              relevantData.currentTick,
              Celsius(
                thermalHouseInput.getLowerTemperatureLimit
                  .to(Units.CELSIUS)
                  .getValue
                  .doubleValue
              ),
              zeroInflux,
            ),
            Some(
              HouseTemperatureLowerBoundaryReached(relevantData.currentTick)
            ),
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              relevantData.currentTick,
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
          relevantData,
          maybeHouseState,
          maybeStorageState,
          formerHouseState,
          formerStorageState,
          ambientTemperature,
          zeroInflux,
        ) match {
          case (
                Some(
                  (
                    ThermalHouseState(houseTick, _, revisedQDotHouse),
                    Some(
                      HouseTemperatureTargetOrUpperBoundaryReached(
                        houseWarmTick
                      )
                    ),
                  )
                ),
                Some(
                  (
                    ThermalStorageState(storageTick, _, revisedQDotStorage),
                    Some(StorageEmpty(storageEmptyTick)),
                  )
                ),
              ) =>
            houseTick shouldBe relevantData.currentTick
            storageTick shouldBe relevantData.currentTick

            revisedQDotHouse should approximate(thermalStorage.chargingPower)
            revisedQDotStorage should approximate(
              thermalStorage.chargingPower * (-1)
            )

            houseWarmTick shouldBe 3601
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

      "heat the house, if the target temperature in the house is not reached" in {
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
        )
        val initialGridState = ThermalGridState(
          Some(ThermalHouseState(-1, Celsius(17), zeroKW)),
          Some(expectedStorageStartingState),
        )
        val externalQDot = testGridQDotInfeed

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            relevantData,
            testGridAmbientTemperature,
            initialGridState,
            isRunning,
            externalQDot,
            onlyThermalDemandOfHouse,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(
                  ThermalHouseState(houseTick, innerTemperature, qDotHouse)
                ),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                ),
              ) =>
            houseTick shouldBe 0L
            innerTemperature should approximate(Celsius(16.9999d))
            qDotHouse should approximate(externalQDot)

            storageTick shouldBe 0L
            storedEnergy should approximate(
              initialGridState.storageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )

            qDotStorage should approximate(zeroKW)

          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureTargetOrUpperBoundaryReached(7322L)
        )
      }

      "load the storage, if the target temperature in the house is reached" in {
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
        )
        val initialGridState = ThermalGridState(
          Some(ThermalHouseState(-1, Celsius(17), zeroKW)),
          Some(expectedStorageStartingState),
        )
        val gridState = initialGridState.copy(houseState =
          initialGridState.houseState.map(
            _.copy(innerTemperature = thermalHouse.upperBoundaryTemperature)
          )
        )
        val externalQDot = testGridQDotInfeed

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            relevantData,
            testGridAmbientTemperature,
            gridState,
            isRunning,
            externalQDot,
            onlyThermalDemandOfHeatStorage,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(
                  ThermalHouseState(houseTick, innerTemperature, qDotHouse)
                ),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                ),
              ) =>
            houseTick shouldBe 0L
            innerTemperature should approximate(Celsius(20.99999167d))
            qDotHouse should approximate(zeroKW)

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
