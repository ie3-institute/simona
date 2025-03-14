/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.model.participant.HpModel.{HpRelevantData, HpState}
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalDemandWrapper,
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTargetTemperatureReached,
  HouseTemperatureLowerBoundaryReached,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import squants.energy._
import squants.thermal.Celsius
import squants.{Energy, Kelvin, Power, Temperature}

import scala.jdk.CollectionConverters._

class ThermalGridWithHouseOnlySpec
    extends UnitSpec
    with ThermalHouseTestData
    with ThermalStorageTestData {

  implicit val tempTolerance: Temperature = Celsius(1e-3)
  implicit val powerTolerance: Power = Watts(1e-3)
  implicit val energyTolerance: Energy = WattHours(1e-3)

  "Testing thermal grid generation with only a house" should {
    "instantiating correctly from input data" in new ThermalHouseTestData {
      val thermalGridInput =
        new edu.ie3.datamodel.models.input.container.ThermalGrid(
          thermalBusInput,
          Set(thermalHouseInput).asJava,
          Set.empty[ThermalStorageInput].asJava,
          Set[ThermalStorageInput](domesticHotWaterStorageInput).asJava,
        )

      ThermalGrid(thermalGridInput) match {
        case ThermalGrid(
              Some(thermalHouseGenerated),
              None,
              Some(domesticHotWaterStorageGenerated),
            ) =>
          thermalHouseGenerated shouldBe thermalHouse
          domesticHotWaterStorageGenerated shouldBe domesticHotWaterStorage
        case _ =>
          fail("Generation of thermal grid from thermal input grid failed.")
      }
    }
  }

  "Testing a thermal grid with only a house" when {
    val thermalGrid: ThermalGrid = ThermalGrid(
      new edu.ie3.datamodel.models.input.container.ThermalGrid(
        thermalBusInput,
        Set(thermalHouseInput).asJava,
        Set.empty[ThermalStorageInput].asJava,
        Set[ThermalStorageInput](domesticHotWaterStorageInput).asJava,
      )
    )

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                Some(
                  ThermalHouseState(
                    houseTick,
                    innerTemperature,
                    thermalInfeedHouse,
                  )
                ),
                None,
                Some(
                  ThermalStorageState(
                    waterDomesticHotStorageTick,
                    storedEnergyDomesticHotWaterStorage,
                    qDotDomesticHotWaterStorage,
                  )
                ),
              ) =>
            houseTick shouldBe expectedHouseStartingState.tick
            waterDomesticHotStorageTick shouldBe expectedDomesticHotWaterStorageStartingState.tick
            innerTemperature should approximate(
              expectedHouseStartingState.innerTemperature
            )
            storedEnergyDomesticHotWaterStorage should approximate(
              expectedDomesticHotWaterStorageStartingState.storedEnergy
            )
            thermalInfeedHouse should approximate(
              expectedHouseStartingState.qDot
            )
            qDotDomesticHotWaterStorage should approximate(
              expectedDomesticHotWaterStorageStartingState.qDot
            )
          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "determining the energy demand for heating and domestic hot water" should {
      "exactly be the thermal demand for heating of the house" in {
        val relevantData = HpRelevantData(
          10800, // after three hours
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
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
        val expectedHouseDemand = thermalHouse.energyDemandHeating(
          relevantData,
          expectedHouseStartingState,
        )

        val (
          thermalDemands,
          updatedThermalGridState,
        ) =
          thermalGrid.energyDemandAndUpdatedState(
            relevantData,
            lastHpState,
          )

        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand
        val domesticHotWaterDemand =
          thermalDemands.domesticHotWaterStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(KilowattHours(1.050097))
        storageDemand.required should approximate(zeroKWh)
        storageDemand.possible should approximate(zeroKWh)
        domesticHotWaterDemand.required should approximate(zeroKWh)
        domesticHotWaterDemand.possible should approximate(zeroKWh)
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Kelvin(292.0799935185185), zeroKW)
        )
        updatedThermalGridState.storageState shouldBe None
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

    "determining the energy demand for domestic warm water supply" should {
      "exactly be the thermal demand for domestic water supply of the house" in {
        val relevantData = HpRelevantData(
          86399, // heat demand for one day
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val expectedEnergyDemandWater =
          ThermalEnergyDemand(
            KilowattHours(3.6795136),
            KilowattHours(3.6795136),
          )

        val energyDemandDomesticHotWater =
          thermalHouse.energyDemandDomesticHotWater(
            relevantData,
            Some(expectedHouseStartingState),
          )

        energyDemandDomesticHotWater.required should approximate(
          expectedEnergyDemandWater.required
        )
        energyDemandDomesticHotWater.possible should approximate(
          expectedEnergyDemandWater.possible
        )
      }
    }

    "handling thermal energy consumption from grid" should {
      val handleConsumption =
        PrivateMethod[(ThermalGridState, Option[ThermalThreshold])](
          Symbol("handleConsumption")
        )

      "deliver the house state by just letting it cool down, if just no infeed is given" in {
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val gridState = ThermalGrid.startingState(thermalGrid)
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
                Some(ThermalHouseState(tickHouse, innerTemperature, qDotHouse)),
                None,
                Some(
                  ThermalStorageState(
                    tickDomesticHotWaterStorage,
                    energyDomesticHotWaterStorage,
                    qDotDomesticHotWaterStorage,
                  )
                ),
              ) =>
            tickHouse shouldBe 0L
            tickDomesticHotWaterStorage shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              domesticHotWaterStorage.maxEnergyThreshold
            )
            qDotHouse should approximate(externalQDot)
            qDotDomesticHotWaterStorage should approximate(
              Kilowatts(-10.556649434187017)
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          SimpleThermalThreshold(23L)
        )

        val secondRelevantData = HpRelevantData(
          23L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )

        val (nextUpdatedGridState, nextReachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            secondRelevantData,
            testGridAmbientTemperature,
            updatedGridState,
            externalQDot,
          )

        nextUpdatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(tickHouse, innerTemperature, qDotHouse)),
                None,
                Some(
                  ThermalStorageState(
                    tickDomesticHotWaterStorage,
                    energyDomesticHotWaterStorage,
                    qDotDomesticHotWaterStorage,
                  )
                ),
              ) =>
            tickHouse shouldBe 23L
            tickDomesticHotWaterStorage shouldBe 23L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              KilowattHours(12.112554739726027)
            )
            qDotHouse should approximate(externalQDot)
            qDotDomesticHotWaterStorage should approximate(zeroKW)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        nextReachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154288)
        )
      }

      "not withdraw energy from the house, if actual consumption is given" in {
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val gridState = ThermalGrid.startingState(thermalGrid)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            relevantData,
            testGridAmbientTemperature,
            gridState,
            testGridQDotConsumption,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(tickHouse, innerTemperature, qDotHouse)),
                None,
                Some(
                  ThermalStorageState(
                    tickDomesticHotWaterStorage,
                    energyDomesticHotWaterStorage,
                    qDotDomesticHotWaterStorage,
                  )
                ),
              ) =>
            tickHouse shouldBe 0L
            tickDomesticHotWaterStorage shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              domesticHotWaterStorage.maxEnergyThreshold
            )
            qDotHouse should approximate(Megawatts(0d))
            qDotDomesticHotWaterStorage should approximate(
              Kilowatts(-10.5566494)
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          SimpleThermalThreshold(23L)
        )

        val secondRelevantData = HpRelevantData(
          23L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )

        val (nextUpdatedGridState, nextReachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            secondRelevantData,
            testGridAmbientTemperature,
            updatedGridState,
            testGridQDotConsumption,
          )

        nextUpdatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(tickHouse, innerTemperature, qDotHouse)),
                None,
                Some(
                  ThermalStorageState(
                    tickDomesticHotWaterStorage,
                    energyDomesticHotWaterStorage,
                    qDotDomesticHotWaterStorage,
                  )
                ),
              ) =>
            tickHouse shouldBe 23L
            tickDomesticHotWaterStorage shouldBe 23L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              KilowattHours(12.112554739726027)
            )
            qDotHouse should approximate(zeroKW)
            qDotDomesticHotWaterStorage should approximate(zeroKW)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        nextReachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154288)
        )
      }
    }

    "handling thermal infeed into the grid" should {
      val handleInfeed =
        PrivateMethod[(ThermalGridState, Option[ThermalThreshold])](
          Symbol("handleInfeed")
        )

      "heat up the house and domestic hot water storage parallel" in {
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
        val initialGridState = ThermalGridState(
          Some(ThermalHouseState(-1, Celsius(17), zeroKW)),
          None,
        )
        val gridState = initialGridState.copy(
          houseState =
            initialGridState.houseState.map(_.copy(qDot = testGridQDotInfeed)),
          domesticHotWaterStorageState =
            initialGridState.domesticHotWaterStorageState.map(
              _.copy(storedEnergy = KilowattHours(0.06744526))
            ),
        )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            relevantData,
            testGridAmbientTemperature,
            gridState,
            isNotRunning,
            testGridQDotInfeed,
            thermalDemands,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(tickHouse, innerTemperature, qDotHouse)),
                None,
                Some(
                  ThermalStorageState(
                    tickDomesticHotWaterStorage,
                    energyDomesticHotWaterStorage,
                    qDotDomesticHotWaterStorage,
                  )
                ),
              ) =>
            tickHouse shouldBe 0L
            tickDomesticHotWaterStorage shouldBe 0L
            innerTemperature should approximate(Celsius(16.9999d))
            energyDomesticHotWaterStorage should approximate(
              KilowattHours(0.06744526)
            )
            // FIXME There might be the edge case, where qDot gets splitted but neither house or storage use all of it
            qDotHouse should approximate(testGridQDotInfeed)
            qDotDomesticHotWaterStorage should approximate(
              Kilowatts(-10.5566494)
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          StorageEmpty(23)
        )

        val updatedRelevantData = HpRelevantData(
          23,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val updatedThermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(KilowattHours(9.9), KilowattHours(15)),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(KilowattHours(12.18), KilowattHours(12.18)),
        )

        val (nextGridState, nextReachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            updatedRelevantData,
            testGridAmbientTemperature,
            updatedGridState,
            isRunning,
            testGridQDotInfeed,
            updatedThermalDemands,
          )

        nextGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(tickHouse, innerTemperature, qDotHouse)),
                None,
                Some(
                  ThermalStorageState(
                    tickDomesticHotWaterStorage,
                    energyDomesticHotWaterStorage,
                    qDotDomesticHotWaterStorage,
                  )
                ),
              ) =>
            tickHouse shouldBe 23L
            tickDomesticHotWaterStorage shouldBe 23L
            innerTemperature should approximate(Celsius(19.005962d))
            energyDomesticHotWaterStorage should approximate(
              zeroKWh
            )
            qDotHouse should approximate(testGridQDotInfeed / 2)
            qDotDomesticHotWaterStorage should approximate(
              testGridQDotInfeed / 2
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        nextReachedThreshold shouldBe Some(
          StorageFull(5869)
        )
      }
    }

    "updating the grid state dependent on the given thermal infeed" should {
      "deliver proper result, if energy is fed into the grid, house and domestic hot water storage have demand at the same time" in {
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
        // Domestic Hot Water Storage has energy for the heat demand of hot water of the first hour
        val gridState = initialGridState.copy(
          houseState =
            initialGridState.houseState.map(_.copy(qDot = testGridQDotInfeed)),
          domesticHotWaterStorageState =
            initialGridState.domesticHotWaterStorageState.map(
              _.copy(storedEnergy = KilowattHours(0.06744526))
            ),
        )

        val (firstState, firstReachedThreshold) = thermalGrid.updateState(
          relevantData,
          gridState,
          testGridAmbientTemperature,
          isRunning,
          testGridQDotInfeed,
          thermalDemands,
        )

        firstState match {
          case ThermalGridState(
                Some(
                  ThermalHouseState(tickHouse, innerTemperature, qDotHouse)
                ),
                None,
                Some(
                  ThermalStorageState(
                    tickDomesticHotWaterStorage,
                    energyDomesticHotWaterStorage,
                    qDotDomesticHotWaterStorage,
                  )
                ),
              ) =>
            tickHouse shouldBe 0L
            tickDomesticHotWaterStorage shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              KilowattHours(0.06744526)
            )
            qDotHouse should approximate(testGridQDotInfeed)
            qDotDomesticHotWaterStorage should approximate(
              Kilowatts(-10.5566494)
            )

          case _ => fail("Thermal grid state updated failed")
        }
        firstReachedThreshold shouldBe Some(StorageEmpty(23))

        val updatedRelevantData = HpRelevantData(
          23,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val updatedThermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(KilowattHours(9.9), KilowattHours(14.9)),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(KilowattHours(12.18), KilowattHours(12.18)),
        )

        val (secondState, secondReachedThreshold) =
          thermalGrid.updateState(
            updatedRelevantData,
            firstState,
            testGridAmbientTemperature,
            isRunning,
            testGridQDotInfeed,
            updatedThermalDemands,
          )

        secondState match {
          case ThermalGridState(
                Some(
                  ThermalHouseState(tickHouse, innerTemperature, qDotHouse)
                ),
                None,
                Some(
                  ThermalStorageState(
                    tickDomesticHotWaterStorage,
                    energyDomesticHotWaterStorage,
                    qDotDomesticHotWaterStorage,
                  )
                ),
              ) =>
            tickHouse shouldBe 23L
            tickDomesticHotWaterStorage shouldBe 23L
            innerTemperature should approximate(Celsius(19.00596))
            energyDomesticHotWaterStorage should approximate(
              zeroKWh
            )
            qDotHouse should approximate(testGridQDotInfeed / 2)
            qDotDomesticHotWaterStorage should approximate(
              testGridQDotInfeed / 2
            )

          case _ => fail("Thermal grid state updated failed")
        }
        secondReachedThreshold shouldBe Some(StorageFull(5869))
      }

      "deliver proper result, if energy is consumed from the grid" in {
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val thermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(KilowattHours(0), KilowattHours(1)),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
        )

        val updatedState = thermalGrid.updateState(
          relevantData,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          isNotRunning,
          testGridQDotConsumption,
          thermalDemands,
        )

        updatedState match {
          case (
                ThermalGridState(
                  Some(
                    ThermalHouseState(tickHouse, innerTemperature, qDotHouse)
                  ),
                  None,
                  Some(
                    ThermalStorageState(
                      tickDomesticHotWaterStorage,
                      energyDomesticHotWaterStorage,
                      qDotDomesticHotWaterStorage,
                    )
                  ),
                ),
                Some(SimpleThermalThreshold(thresholdTick)),
              ) =>
            tickHouse shouldBe 0L
            tickDomesticHotWaterStorage shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              expectedDomesticHotWaterStorageStartingState.storedEnergy
            )
            qDotHouse should approximate(Megawatts(0d))
            qDotDomesticHotWaterStorage should approximate(
              Kilowatts(-10.5566494)
            )
            thresholdTick shouldBe 23
          case _ => fail("Thermal grid state updated failed")
        }

        val updatedRelevantData = HpRelevantData(
          22,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val updatedThermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(KilowattHours(0), KilowattHours(1)),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
        )

        val nextUpdatedState = thermalGrid.updateState(
          updatedRelevantData,
          updatedState._1,
          testGridAmbientTemperature,
          isNotRunning,
          testGridQDotConsumption,
          thermalDemands,
        )

        nextUpdatedState match {
          case (
                ThermalGridState(
                  Some(
                    ThermalHouseState(tickHouse, innerTemperature, qDotHouse)
                  ),
                  None,
                  Some(
                    ThermalStorageState(
                      tickDomesticHotWaterStorage,
                      energyDomesticHotWaterStorage,
                      qDotDomesticHotWaterStorage,
                    )
                  ),
                ),
                Some(HouseTemperatureLowerBoundaryReached(thresholdTick)),
              ) =>
            tickHouse shouldBe 22L
            tickDomesticHotWaterStorage shouldBe 22L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              KilowattHours(12.115487142346634)
            )
            qDotHouse should approximate(zeroKW)
            qDotDomesticHotWaterStorage should approximate(zeroKW)
            thresholdTick shouldBe 154288
          case _ => fail("Thermal grid state updated failed")
        }

      }

      "deliver proper result, if energy is neither consumed from nor fed into the grid" in {
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val thermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
        )
        val updatedState = thermalGrid.updateState(
          relevantData,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          isNotRunning,
          zeroKW,
          thermalDemands,
        )

        updatedState match {
          case (
                ThermalGridState(
                  Some(
                    ThermalHouseState(tickHouse, innerTemperature, qDotHouse)
                  ),
                  None,
                  Some(
                    ThermalStorageState(
                      tickDomesticHotWaterStorage,
                      energyDomesticHotWaterStorage,
                      qDotDomesticHotWaterStorage,
                    )
                  ),
                ),
                Some(SimpleThermalThreshold(thresholdTick)),
              ) =>
            tickHouse shouldBe 0L
            tickDomesticHotWaterStorage shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              expectedDomesticHotWaterStorageStartingState.storedEnergy
            )
            qDotHouse should approximate(zeroKW)
            qDotDomesticHotWaterStorage should approximate(
              Kilowatts(-10.5566494)
            )
            thresholdTick shouldBe 23
          case _ => fail("Thermal grid state updated failed")
        }

        val updatedRelevantData = HpRelevantData(
          22,
          testGridAmbientTemperature,
          defaultSimulationStart,
          houseInhabitants,
        )
        val updatedThermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
        )

        val nextUpdatedState = thermalGrid.updateState(
          updatedRelevantData,
          updatedState._1,
          testGridAmbientTemperature,
          isNotRunning,
          zeroKW,
          updatedThermalDemands,
        )

        nextUpdatedState match {
          case (
                ThermalGridState(
                  Some(
                    ThermalHouseState(tickHouse, innerTemperature, qDotHouse)
                  ),
                  None,
                  Some(
                    ThermalStorageState(
                      tickDomesticHotWaterStorage,
                      energyDomesticHotWaterStorage,
                      qDotDomesticHotWaterStorage,
                    )
                  ),
                ),
                Some(HouseTemperatureLowerBoundaryReached(thresholdTick)),
              ) =>
            tickHouse shouldBe 22L
            tickDomesticHotWaterStorage shouldBe 22L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              KilowattHours(12.115487)
            )
            qDotHouse should approximate(zeroKW)
            qDotDomesticHotWaterStorage should approximate(zeroKW)
            thresholdTick shouldBe 154288
          case _ => fail("Thermal grid state updated failed")
        }
      }
    }
  }
}
