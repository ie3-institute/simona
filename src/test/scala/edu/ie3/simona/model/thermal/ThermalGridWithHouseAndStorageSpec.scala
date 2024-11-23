/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.model.participant.HpModel.HpRelevantData
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalDemandWrapper,
  ThermalEnergyDemand,
  ThermalGridState,
}
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

        val (thermalDemands, updatedThermalGridState) =
          thermalGrid.energyDemandAndUpdatedState(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            ThermalGrid.startingState(thermalGrid),
            defaultSimulationStart,
            houseInhabitants,
          )
        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand
        val waterStorageDemand = thermalDemands.domesticHotWaterStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(KilowattHours(31.05009722d))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        waterStorageDemand.required should approximate(KilowattHours(0d))
        waterStorageDemand.possible should approximate(KilowattHours(0d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Kelvin(292.0799935185185), zeroKW)
        )
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800, zeroKWh, zeroKW)
        )
        updatedThermalGridState.domesticHotWaterStorageState shouldBe Some(
          ThermalStorageState(
            10800,
            // when simulating from tick 0 - 10800 the hourly demand
            // for hot water would normally be taken from domestic
            // hot water storage, resulting in a lower storedEnergy here
            expectedDomesticHotWaterStorageStartingState.storedEnergy,
            Kilowatts(-10.991079452054795),
          )
        )
      }

      "cover the hot water demand from domestic hot water storage" in {
        val tick = 0

        val (
          thermalDemands,
          updatedThermalGridState,
        ) =
          thermalGrid.energyDemandAndUpdatedState(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            ThermalGrid.startingState(thermalGrid),
            defaultSimulationStart,
            houseInhabitants,
          )
        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand
        val waterStorageDemand = thermalDemands.domesticHotWaterStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(KilowattHours(30.0000972d))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        waterStorageDemand.required should approximate(KilowattHours(0d))
        waterStorageDemand.possible should approximate(KilowattHours(0d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(0, Kelvin(292.1499935185185), zeroKW)
        )
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(0, zeroKWh, zeroKW)
        )
        updatedThermalGridState.domesticHotWaterStorageState shouldBe Some(
          ThermalStorageState(
            0,
            expectedDomesticHotWaterStorageStartingState.storedEnergy,
            Kilowatts(-10.556649434187017),
          )
        )
      }

      "deliver the correct house and storage demand" in {
        val tick = 10800 // after three hours

        val startingState = ThermalGrid.startingState(thermalGrid)
        val (thermalDemands, updatedThermalGridState) =
          thermalGrid.energyDemandAndUpdatedState(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            startingState.copy(houseState =
              startingState.houseState.map(
                _.copy(innerTemperature = Celsius(16d))
              )
            ),
            defaultSimulationStart,
            houseInhabitants,
          )
        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand
        val waterStorageDemand = thermalDemands.domesticHotWaterStorageDemand

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
          ThermalStorageState(10800, zeroKWh, zeroKW)
        )
        updatedThermalGridState.domesticHotWaterStorageState shouldBe Some(
          ThermalStorageState(
            10800,
            // when simulating from tick 0 - 10800 the hourly demand
            // for hot water would normally be taken from domestic
            // hot water storage, resulting in a lower storedEnergy here
            expectedDomesticHotWaterStorageStartingState.storedEnergy,
            Kilowatts(-10.991079452054795),
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
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val initialLoadingHeatStorage = KilowattHours(430d)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoadingHeatStorage)
          )
        )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            relevantData,
            testGridAmbientTemperature,
            gridState,
            zeroKW,
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
              Kilowatts(-10.556649434187017)
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          SimpleThermalThreshold(23L)
        )

        val updatedRelevantData = HpRelevantData(
          23,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )

        val (nextUpdatedGridState, nextReachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            updatedRelevantData,
            testGridAmbientTemperature,
            updatedGridState,
            zeroKW,
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
            thermalStorageTick shouldBe 23L
            storedEnergyThermalStorage should approximate(
              initialLoadingHeatStorage
            )
            qDotThermalStorage should approximate(zeroKW)

            domesticHotWaterStorageTick shouldBe 23L
            storedEnergyDomesticHotWaterStorage should approximate(
              KilowattHours(12.112554739726027)
            )
            qDotStorageDomesticHotWaterStorage should approximate(zeroKW)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        nextReachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154288L)
        )
      }

      "take energy from storage, if there is actual consumption" in {
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
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
              Kilowatts(-10.556649434187017)
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(SimpleThermalThreshold(23L))
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
              zeroKWh,
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
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val thermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(KilowattHours(10), KilowattHours(15)),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
        )
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val externalQDot = testGridQDotInfeed

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            relevantData,
            testGridAmbientTemperature,
            initialGridState,
            isRunning,
            externalQDot,
            thermalDemands,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(
                  ThermalHouseState(houseTick, innerTemperature, qDotHouse)
                ),
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
            qDotHouse should approximate(externalQDot)

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
              Kilowatts(-10.5566494341)
            )

          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(SimpleThermalThreshold(23))

        val secondRelevantData = HpRelevantData(
          23L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val secondThermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(KilowattHours(9.9), KilowattHours(14.9)),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh)
        )

        val (secondUpdatedGridState, secondReachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
    secondRelevantData,
            testGridAmbientTemperature,
            updatedGridState,
            isRunning,
            externalQDot,
            secondThermalDemands,
          )

        secondUpdatedGridState match {
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
            houseTick shouldBe 23L
            innerTemperature should approximate(Celsius(19.00596203))
            qDotHouse should approximate(externalQDot)

            thermalStorageTick shouldBe 23L
            storedEnergyThermalStorage should approximate(
              initialGridState.storageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotThermalStorage should approximate(zeroKW)

            domesticHotWaterStorageTick shouldBe 23L
            storedEnergyDomesticHotWaterStorage should approximate(
              KilowattHours(12.112554739726027)
            )
            qDotStorageDomesticHotWaterStorage should approximate(
              zeroKW
            )

          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        secondReachedThreshold shouldBe Some(
          HouseTemperatureUpperBoundaryReached(7372)
        )
      }

      "heat the house and recharge the domestic hot water storage, if the house has required heat demand and domestic hot water storage is empty" in {
        val firstRelevantData = HpRelevantData(
          0,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val firstThermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(KilowattHours(5), KilowattHours(15)),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(KilowattHours(12.18), KilowattHours(12.18))
        )
        val initialGridState = ThermalGrid.startingState(thermalGrid)

        val gridState = initialGridState.copy(domesticHotWaterStorageState =
          initialGridState.domesticHotWaterStorageState.map(
            _.copy(storedEnergy = zeroKWh)
          )
        )
        val externalQDot = testGridQDotInfeed

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
     firstRelevantData,
            testGridAmbientTemperature,
            gridState,
            isRunning,
            externalQDot,
            firstThermalDemands,
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
            innerTemperature should approximate(Celsius(18.9999))
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
              zeroKWh
            )
            qDotStorageDomesticHotWaterStorage should approximate(
              zeroKW
            )

          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          SimpleThermalThreshold(32L)
        )

        val secondRelevantData = HpRelevantData(
          32,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val secondThermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(KilowattHours(4.8), KilowattHours(15)),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(KilowattHours(12.18), KilowattHours(12.18)),
        )

        val (secondUpdatedGridState, secondReachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
       secondRelevantData,
            testGridAmbientTemperature,
            updatedGridState,
            isRunning,
            externalQDot,
            secondThermalDemands,
          )

        secondUpdatedGridState match {
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
            houseTick shouldBe 32L
            innerTemperature should approximate(Celsius(19.004230555))
            qDotHouse should approximate(externalQDot / 2)

            thermalStorageTick shouldBe 32L
            storedEnergyThermalStorage should approximate(
              initialGridState.storageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotThermalStorage should approximate(zeroKW)

            domesticHotWaterStorageTick shouldBe 32L
            storedEnergyDomesticHotWaterStorage should approximate(
              zeroKWh
            )
            qDotStorageDomesticHotWaterStorage should approximate(
              externalQDot / 2
            )

          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        secondReachedThreshold shouldBe Some(
          StorageFull(5878)
        )
      }

      "load the heat storage, if the upper temperature in the house is reached and there is no heat demand of hot domestic water storage" in {
        val firstRelevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val firstThermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(KilowattHours(1150), KilowattHours(1150)),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
        )
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val gridState = initialGridState.copy(houseState =
          initialGridState.houseState.map(
            _.copy(innerTemperature = thermalHouse.upperBoundaryTemperature)
          )
        )
        val externalQDot = testGridQDotInfeed

        val (firstUpdatedGridState, firstReachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            firstRelevantData,
            testGridAmbientTemperature,
            gridState,
            false,
            externalQDot,
            firstThermalDemands,
          )

        firstUpdatedGridState match {
          case ThermalGridState(
                Some(
                  ThermalHouseState(houseTick, innerTemperature, qDotHouse)
                ),
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
            storedEnergyDomesticHotWaterStorage should approximate(
              gridState.domesticHotWaterStorageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotStorageDomesticHotWaterStorage should approximate(
              Kilowatts(-10.5566494341)
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        firstReachedThreshold shouldBe Some(SimpleThermalThreshold(23))

        val secondRelevantData = HpRelevantData(
          23L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val secondThermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(
            KilowattHours(1149.9083),
            KilowattHours(1149.9083),
          ),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
        )

        val (secondUpdatedGridState, secondReachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            secondRelevantData,
            testGridAmbientTemperature,
            firstUpdatedGridState,
            false,
            externalQDot,
            secondThermalDemands,
          )

        secondUpdatedGridState match {
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
            houseTick shouldBe 23L
            innerTemperature should approximate(Celsius(20.99999167d))
            qDotHouse should approximate(zeroKW)

            thermalStorageTick shouldBe 23L
            storedEnergyThermalStorage should approximate(
              KilowattHours(0.09583333333333334)
            )
            qDotThermalStorage should approximate(externalQDot)

            domesticHotWaterStorageTick shouldBe 23L
            storedEnergyDomesticHotWaterStorage should approximate(
              KilowattHours(12.112554739726027)
            )
            qDotStorageDomesticHotWaterStorage should approximate(zeroKW)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        secondReachedThreshold shouldBe Some(
          StorageFull(276000L)
        )
      }

      "don't load the heat storage, use qDot directly to cover hot water demand before recharge domestic hot water storage if the upper temperature in the house is reached and the domestic hot water storage is empty" in {
        val firstRelevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val firstThermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(KilowattHours(1150), KilowattHours(1150)),
          ThermalEnergyDemand(KilowattHours(12.18), KilowattHours(12.18)),
        )
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val gridState = initialGridState.copy(
          houseState = initialGridState.houseState.map(
            _.copy(innerTemperature = thermalHouse.upperBoundaryTemperature)
          ),
          domesticHotWaterStorageState =
            initialGridState.domesticHotWaterStorageState.map(
              _.copy(storedEnergy = zeroKWh)
            ),
        )
        val externalQDot = testGridQDotInfeed

        val (firstUpdatedGridState, firstReachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            firstRelevantData,
            testGridAmbientTemperature,
            gridState,
            isNotRunning,
            externalQDot,
            firstThermalDemands,
          )

        firstUpdatedGridState match {
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
            storedEnergyDomesticHotWaterStorage should approximate(
              gridState.domesticHotWaterStorageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotStorageDomesticHotWaterStorage should approximate(zeroKW)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        firstReachedThreshold shouldBe Some(SimpleThermalThreshold(16))

        val secondRelevantData = HpRelevantData(
          16L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val secondThermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(KilowattHours(1150), KilowattHours(1150)),
          ThermalEnergyDemand(KilowattHours(12.18), KilowattHours(12.18)),
        )

        val (secondUpdatedGridState, secondReachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            secondRelevantData,
            testGridAmbientTemperature,
            firstUpdatedGridState,
            isNotRunning,
            externalQDot,
            secondThermalDemands,
          )

        secondUpdatedGridState match {
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
            houseTick shouldBe 16L
            innerTemperature should approximate(Celsius(20.99999167d))
            qDotHouse should approximate(zeroKW)

            thermalStorageTick shouldBe 16L
            storedEnergyThermalStorage should approximate(
              gridState.storageState
                .map(_.storedEnergy)
                .getOrElse(fail("No initial storage state found"))
            )
            qDotThermalStorage should approximate(zeroKW)

            domesticHotWaterStorageTick shouldBe 16L
            storedEnergyDomesticHotWaterStorage should approximate(
              zeroKWh
            )
            qDotStorageDomesticHotWaterStorage should approximate(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        secondReachedThreshold shouldBe Some(
          StorageFull(2939L)
        )
      }
    }
  }
}
