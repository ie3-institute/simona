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
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKWH, zeroKW}
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
          Set[ThermalStorageInput](domesticHotWaterStorageInput).asJava,
        )

      ThermalGrid(thermalGridInput) match {
        case ThermalGrid(
              Some(thermalHouseGenerated),
              Some(thermalStorageGenerated),
              Some(domesticHotWaterStorageGenerated),
            ) =>
          thermalHouseGenerated shouldBe thermalHouse
          thermalStorageGenerated shouldBe thermalStorage
          domesticHotWaterStorageGenerated shouldBe domesticHotWaterStorage
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
        Set[ThermalStorageInput](domesticHotWaterStorageInput).asJava,
      )
    )

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(
                    heatStorageTick,
                    storedEnergyHeatStorage,
                    qDotHeatStorage,
                  )
                ),
                Some(
                  ThermalStorageState(
                    waterStorageTick,
                    storedEnergyWaterStorage,
                    qDotDomesticHotWaterStorage,
                  )
                ),
              ) =>
            houseTick shouldBe expectedHouseStartingState.tick
            heatStorageTick shouldBe expectedCylindricalStorageStartingState.tick
            waterStorageTick shouldBe expectedDomesticHotWaterStorageStartingState.tick

            innerTemperature should approximate(
              expectedHouseStartingState.innerTemperature
            )
            storedEnergyHeatStorage should approximate(
              expectedCylindricalStorageStartingState.storedEnergy
            )
            storedEnergyWaterStorage should approximate(
              expectedDomesticHotWaterStorageStartingState.storedEnergy
            )
            qDotHouse should approximate(expectedHouseStartingState.qDot)
            qDotHeatStorage should approximate(
              expectedCylindricalStorageStartingState.qDot
            )
            qDotDomesticHotWaterStorage should approximate(
              expectedDomesticHotWaterStorageStartingState.qDot
            )

          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "determining the energy demand" should {
      "deliver the heat demand of the house (no demand) with added flexibility by storage" in {
        // hot water demand will be excluded here for test reasons
        val tick = 10800 // after three hours

        val (
          houseDemand,
          storageDemand,
          waterStorageDemand,
          updatedThermalGridState,
        ) =
          thermalGrid.energyDemandAndUpdatedState(
            tick,
            testGridAmbientTemperature,
            ThermalGrid.startingState(thermalGrid),
            defaultSimulationStart,
            houseInhabitants,
          )
        houseDemand.required should approximate(zeroKWH)
        houseDemand.possible should approximate(KilowattHours(31.05009722d))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        waterStorageDemand.required should approximate(KilowattHours(0d))
        waterStorageDemand.possible should approximate(KilowattHours(0d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Kelvin(292.0799935185185), zeroKW)
        )
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800, zeroKWH, zeroKW)
        )
        updatedThermalGridState.domesticHotWaterStorageState shouldBe Some(
          ThermalStorageState(
            10800,
            // when simulating from tick 0 - 10800 the hourly demand
            // for hot water would normally be taken from domestic
            // hot water storage, resulting in a lower storedEnergy here
            expectedDomesticHotWaterStorageStartingState.storedEnergy,
            domesticHotWaterStorage.getChargingPower * (-1),
          )
        )
      }

      "cover the hot water demand from domestic hot water storage" in {
        val tick = 0

        val (
          houseDemand,
          storageDemand,
          waterStorageDemand,
          updatedThermalGridState,
        ) =
          thermalGrid.energyDemandAndUpdatedState(
            tick,
            testGridAmbientTemperature,
            ThermalGrid.startingState(thermalGrid),
            defaultSimulationStart,
            houseInhabitants,
          )
        houseDemand.required should approximate(zeroKWH)
        houseDemand.possible should approximate(KilowattHours(30.0000972d))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        waterStorageDemand.required should approximate(KilowattHours(0d))
        waterStorageDemand.possible should approximate(KilowattHours(0d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(0, Kelvin(292.1499935185185), zeroKW)
        )
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(0, zeroKWH, zeroKW)
        )
        updatedThermalGridState.domesticHotWaterStorageState shouldBe Some(
          ThermalStorageState(
            0,
            expectedDomesticHotWaterStorageStartingState.storedEnergy,
            domesticHotWaterStorage.getChargingPower * (-1),
          )
        )
      }

      "deliver the correct house and storage demand" in {
        val tick = 10800 // after three hours

        val startingState = ThermalGrid.startingState(thermalGrid)
        val (
          houseDemand,
          storageDemand,
          waterStorageDemand,
          updatedThermalGridState,
        ) =
          thermalGrid.energyDemandAndUpdatedState(
            tick,
            testGridAmbientTemperature,
            startingState.copy(houseState =
              startingState.houseState.map(
                _.copy(innerTemperature = Celsius(16d))
              )
            ),
            defaultSimulationStart,
            houseInhabitants,
          )

        houseDemand.required should approximate(KilowattHours(45.6000555))
        houseDemand.possible should approximate(KilowattHours(75.600055555))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        waterStorageDemand.required should approximate(KilowattHours(0d))
        waterStorageDemand.possible should approximate(KilowattHours(0d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Celsius(15.959996296296296), zeroKW)
        )
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800, zeroKWH, zeroKW)
        )
        updatedThermalGridState.domesticHotWaterStorageState shouldBe Some(
          ThermalStorageState(
            10800,
            // when simulating from tick 0 - 10800 the hourly demand
            // for hot water would normally be taken from domestic
            // hot water storage, resulting in a lower storedEnergy here
            expectedDomesticHotWaterStorageStartingState.storedEnergy,
            domesticHotWaterStorage.getChargingPower * (-1),
          )
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
        val initialLoadingHeatStorage = KilowattHours(430d)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoadingHeatStorage)
          )
        )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            testGridAmbientTemperature,
            gridState,
            zeroKW,
            defaultSimulationStart,
            houseInhabitants,
          )

        updatedGridState match {
          case ThermalGridState(
                _,
                Some(
                  ThermalStorageState(
                    thermalStorageTick,
                    storedEnergyThermalStorage,
                    qDotThermalStorage,
                  )
                ),
                Some(
                  ThermalStorageState(
                    domesticHotWaterStorageTick,
                    storedEnergyDomesticHotWaterStorage,
                    qDotStorageDomesticHotWaterStorage,
                  )
                ),
              ) =>
            thermalStorageTick shouldBe 0L
            storedEnergyThermalStorage should approximate(
              initialLoadingHeatStorage
            )
            qDotThermalStorage should approximate(zeroKW)

            domesticHotWaterStorageTick shouldBe 0L
            storedEnergyDomesticHotWaterStorage should approximate(
              expectedDomesticHotWaterStorageStartingState.storedEnergy
            )
            qDotStorageDomesticHotWaterStorage should approximate(
              Kilowatts(-11d)
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          SimpleThermalThreshold(22L)
        )

        val (nextUpdatedGridState, nextReachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            22L,
            testGridAmbientTemperature,
            updatedGridState,
            zeroKW,
            defaultSimulationStart,
            houseInhabitants,
          )

        nextUpdatedGridState match {
          case ThermalGridState(
                _,
                Some(
                  ThermalStorageState(
                    thermalStorageTick,
                    storedEnergyThermalStorage,
                    qDotThermalStorage,
                  )
                ),
                Some(
                  ThermalStorageState(
                    domesticHotWaterStorageTick,
                    storedEnergyDomesticHotWaterStorage,
                    qDotStorageDomesticHotWaterStorage,
                  )
                ),
              ) =>
            thermalStorageTick shouldBe 22L
            storedEnergyThermalStorage should approximate(
              initialLoadingHeatStorage
            )
            qDotThermalStorage should approximate(zeroKW)

            domesticHotWaterStorageTick shouldBe 22L
            storedEnergyDomesticHotWaterStorage should approximate(
              KilowattHours(12.112777)
            )
            qDotStorageDomesticHotWaterStorage should approximate(zeroKW)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        nextReachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154288L)
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
            gridState,
            externalQDot,
            defaultSimulationStart,
            houseInhabitants,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(
                    thermalStorageTick,
                    storedEnergyThermalStorage,
                    qDotThermalStorage,
                  )
                ),
                Some(
                  ThermalStorageState(
                    domesticHotWaterStorageTick,
                    storedEnergyDomesticHotWaterStorage,
                    qDotStorageDomesticHotWaterStorage,
                  )
                ),
              ) =>
            houseTick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDotHouse should approximate(zeroKW)

            thermalStorageTick shouldBe 0L
            storedEnergyThermalStorage should approximate(initialLoading)
            qDotThermalStorage should approximate(externalQDot)

            domesticHotWaterStorageTick shouldBe 0L
            storedEnergyDomesticHotWaterStorage should approximate(
              expectedDomesticHotWaterStorageStartingState.storedEnergy
            )
            qDotStorageDomesticHotWaterStorage should approximate(
              Kilowatts(-11d)
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(SimpleThermalThreshold(22L))
      }
    }

    "revising infeed from storage to house" should {
      val zeroInflux = zeroKW
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
              zeroKWH,
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

            houseWarmTick shouldBe 13729L
            storageEmptyTick shouldBe 10145L
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
            initialGridState,
            externalQDot,
            thermalDemand,
            noThermalDemand,
            thermalDemand,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(
                    thermalStorageTick,
                    storedEnergyThermalStorage,
                    qDotThermalStorage,
                  )
                ),
                Some(
                  ThermalStorageState(
                    domesticHotWaterStorageTick,
                    storedEnergyDomesticHotWaterStorage,
                    qDotStorageDomesticHotWaterStorage,
                  )
                ),
              ) =>
            houseTick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDotHouse should approximate(externalQDot / 2)

            thermalStorageTick shouldBe 0L
            storedEnergyThermalStorage should approximate(
              initialGridState.storageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotThermalStorage should approximate(zeroKW)

            domesticHotWaterStorageTick shouldBe 0L
            storedEnergyDomesticHotWaterStorage should approximate(
              initialGridState.domesticHotWaterStorageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotStorageDomesticHotWaterStorage should approximate(
              externalQDot / 2
            )

          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureUpperBoundaryReached(15105L)
        )
      }

      "load the heat storage, if the upper temperature in the house is reached and there is no heat demand of hot domestic water storage" in {
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
            gridState,
            externalQDot,
            noThermalDemand,
            thermalDemand,
            noThermalDemand,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(
                    thermalStorageTick,
                    storedEnergyThermalStorage,
                    qDotThermalStorage,
                  )
                ),
                Some(
                  ThermalStorageState(
                    domesticHotWaterStorageTick,
                    storedEnergyDomesticHotWaterStorage,
                    qDotStorageDomesticHotWaterStorage,
                  )
                ),
              ) =>
            houseTick shouldBe 0L
            innerTemperature should approximate(Celsius(20.99999167d))
            qDotHouse should approximate(zeroKW)

            thermalStorageTick shouldBe 0L
            storedEnergyThermalStorage should approximate(
              gridState.storageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotThermalStorage should approximate(externalQDot)

            domesticHotWaterStorageTick shouldBe 0L
            // when simulating from tick 0 - 10800 the hourly demand
            // for hot water would normally be taken from domestic
            // hot water storage, resulting in a lower storedEnergy here
            storedEnergyDomesticHotWaterStorage should approximate(
              gridState.domesticHotWaterStorageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotStorageDomesticHotWaterStorage should approximate(zeroKW)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          StorageFull(276000L)
        )
      }

      "don't load the heat storage, if the upper temperature in the house is reached but there the domestic hot water storage is empty" in {
        val tick = 0L
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val gridState = initialGridState.copy(
          houseState = initialGridState.houseState.map(
            _.copy(innerTemperature = thermalHouse.upperBoundaryTemperature)
          ),
          domesticHotWaterStorageState =
            initialGridState.domesticHotWaterStorageState.map(
              _.copy(storedEnergy = zeroKWH)
            ),
        )
        val externalQDot = testGridQDotInfeed

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            tick,
            testGridAmbientTemperature,
            gridState,
            externalQDot,
            noThermalDemand,
            thermalDemand,
            thermalDemand,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(
                    thermalStorageTick,
                    storedEnergyThermalStorage,
                    qDotThermalStorage,
                  )
                ),
                Some(
                  ThermalStorageState(
                    domesticHotWaterStorageTick,
                    storedEnergyDomesticHotWaterStorage,
                    qDotStorageDomesticHotWaterStorage,
                  )
                ),
              ) =>
            houseTick shouldBe 0L
            innerTemperature should approximate(Celsius(20.99999167d))
            qDotHouse should approximate(zeroKW)

            thermalStorageTick shouldBe 0L
            storedEnergyThermalStorage should approximate(
              gridState.storageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotThermalStorage should approximate(zeroKW)

            domesticHotWaterStorageTick shouldBe 0L
            // when simulating from tick 0 - 10800 the hourly demand
            // for hot water would normally be taken from domestic
            // hot water storage, resulting in a lower storedEnergy here
            storedEnergyDomesticHotWaterStorage should approximate(
              gridState.domesticHotWaterStorageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotStorageDomesticHotWaterStorage should approximate(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          StorageFull(2923L)
        )
      }
    }
  }
}
