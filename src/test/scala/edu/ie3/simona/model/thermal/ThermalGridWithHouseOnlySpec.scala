/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureUpperBoundaryReached,
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWH}
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
        val tick = 10800 // after three hours
        val expectedHouseDemand = thermalHouse.energyDemandHeating(
          tick,
          testGridAmbientTemperature,
          expectedHouseStartingState,
        )

        val (
          houseDemand,
          storageDemand,
          domesticHotWaterDemand,
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

        houseDemand.required should approximate(expectedHouseDemand.required)
        houseDemand.possible should approximate(expectedHouseDemand.possible)
        storageDemand.required should approximate(zeroKWH)
        storageDemand.possible should approximate(zeroKWH)
        domesticHotWaterDemand.required should approximate(KilowattHours(0d))
        domesticHotWaterDemand.possible should approximate(KilowattHours(0d))
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
            domesticHotWaterStorage.getChargingPower * (-1),
          )
        )
      }
    }

    "determining the energy demand for domestic warm water supply" should {
      "exactly be the thermal demand for domestic water supply of the house" in {
        val tick = 86399 // heat demand for one day
        val expectedEnergyDemandWater =
          ThermalEnergyDemand(
            KilowattHours(3.7469589),
            KilowattHours(3.7469589),
          )

        val energyDemandDomesticHotWater =
          thermalHouse.energyDemandDomesticHotWater(
            tick,
            Some(expectedHouseStartingState),
            defaultSimulationStart,
            houseInhabitants,
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
        val tick = 0L
        val gridState = ThermalGrid.startingState(thermalGrid)
        val externalQDot = Megawatts(0d)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            gridState,
            externalQDot,
            defaultSimulationStart,
            houseInhabitants,
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
              (-1) * domesticHotWaterStorage.getChargingPower
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
            testGridAmbientTemperature,
            updatedGridState,
            externalQDot,
            defaultSimulationStart,
            houseInhabitants,
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
            tickHouse shouldBe 22L
            tickDomesticHotWaterStorage shouldBe 22L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              KilowattHours(12.112777)
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
        val tick = 0L
        val gridState = ThermalGrid.startingState(thermalGrid)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            gridState,
            testGridQDotConsumption,
            defaultSimulationStart,
            houseInhabitants,
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
              domesticHotWaterStorage.getChargingPower * (-1)
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
            testGridAmbientTemperature,
            updatedGridState,
            testGridQDotConsumption,
            defaultSimulationStart,
            houseInhabitants,
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
            tickHouse shouldBe 22L
            tickDomesticHotWaterStorage shouldBe 22L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              KilowattHours(12.112777)
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

      "solely heat up the house" in {
        val tick = 0L
        val gridState = ThermalGrid.startingState(thermalGrid)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            gridState,
            false,
            testGridQDotInfeed,
            thermalDemand,
            noThermalDemand,
            thermalDemand,
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
            qDotHouse should approximate(testGridQDotInfeed / 2)
            qDotDomesticHotWaterStorage should approximate(
              testGridQDotInfeed / 2
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureUpperBoundaryReached(15105)
        )
      }
    }

    "updating the grid state dependent on the given thermal infeed" should {
      "deliver proper result, if energy is fed into the grid" in {
        thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          testGridAmbientTemperature,
          isRunning,
          testGridQDotInfeed,
          thermalDemand,
          noThermalDemand,
          thermalDemand,
          defaultSimulationStart,
          houseInhabitants,
        ) match {
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
                Some(HouseTemperatureUpperBoundaryReached(thresholdTick)),
              ) =>
            tickHouse shouldBe 0L
            tickDomesticHotWaterStorage shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            energyDomesticHotWaterStorage should approximate(
              expectedDomesticHotWaterStorageStartingState.storedEnergy
            )
            qDotHouse should approximate(testGridQDotInfeed / 2)
            qDotDomesticHotWaterStorage should approximate(
              testGridQDotInfeed / 2
            )
            thresholdTick shouldBe 15105L
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is consumed from the grid" in {
        val updatedState = thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          testGridAmbientTemperature,
          isNotRunning,
          testGridQDotConsumption,
          thermalDemand,
          noThermalDemand,
          thermalDemand,
          defaultSimulationStart,
          houseInhabitants,
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
            qDotDomesticHotWaterStorage should approximate(Kilowatts(-11d))
            thresholdTick shouldBe 22
          case _ => fail("Thermal grid state updated failed")
        }

        val nextUpdatedState = thermalGrid.updateState(
          22L,
          updatedState._1,
          testGridAmbientTemperature,
          testGridAmbientTemperature,
          isNotRunning,
          testGridQDotConsumption,
          thermalDemand,
          noThermalDemand,
          thermalDemand,
          defaultSimulationStart,
          houseInhabitants,
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
              KilowattHours(12.1127777)
            )
            qDotHouse should approximate(zeroKW)
            qDotDomesticHotWaterStorage should approximate(zeroKW)
            thresholdTick shouldBe 154288
          case _ => fail("Thermal grid state updated failed")
        }

      }

      "deliver proper result, if energy is neither consumed from nor fed into the grid" in {
        val updatedState = thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          testGridAmbientTemperature,
          isNotRunning,
          zeroKW,
          thermalDemand,
          noThermalDemand,
          thermalDemand,
          defaultSimulationStart,
          houseInhabitants,
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
              domesticHotWaterStorage.getChargingPower * (-1)
            )
            thresholdTick shouldBe 22
          case _ => fail("Thermal grid state updated failed")
        }

        val nextUpdatedState = thermalGrid.updateState(
          22L,
          updatedState._1,
          testGridAmbientTemperature,
          testGridAmbientTemperature,
          isNotRunning,
          zeroKW,
          thermalDemand,
          noThermalDemand,
          thermalDemand,
          defaultSimulationStart,
          houseInhabitants,
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
              KilowattHours(12.1127777)
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
