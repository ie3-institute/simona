/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.model.participant.HpModel.{
  HpOperatingPoint,
  HpState,
  ThermalGridOperatingPoint,
}
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.StorageFull
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import squants.energy.*
import squants.thermal.Celsius
import squants.{Energy, Power, Temperature}

import scala.jdk.CollectionConverters.*

class ThermalGridWithHouseOnlySpec
    extends UnitSpec
    with ThermalHouseTestData
    with DefaultTestData
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

  "Testing a thermal grid with only a house and a domestic hot water storage" when {
    val thermalGrid: ThermalGrid = ThermalGrid(
      new edu.ie3.datamodel.models.input.container.ThermalGrid(
        thermalBusInput,
        Set(thermalHouseInput).asJava,
        Set.empty[ThermalStorageInput].asJava,
        Set[ThermalStorageInput](domesticHotWaterStorageInput).asJava,
      )
    )
    val initialGridState: ThermalGridState =
      ThermalGrid.startingState(thermalGrid, testGridAmbientTemperature)

    val initialHpState = HpState(
      0L,
      defaultSimulationStart,
      initialGridState,
      HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
      noThermalDemand,
    )

    "requesting the starting state" should {
      "deliver proper results" in {
        initialGridState match {
          case ThermalGridState(
                Some(
                  ThermalHouseState(
                    houseTick,
                    _,
                    innerTemperature,
                  )
                ),
                None,
                Some(
                  ThermalStorageState(
                    waterDomesticHotStorageTick,
                    storedEnergyDomesticHotWaterStorage,
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

          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "updatedThermalGridState" should {
      "exactly calculate the state of the thermalGrid" in {
        val tick = 10800L // after three hours

        val updatedThermalGridState =
          thermalGrid.determineState(
            tick,
            initialGridState,
            HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
          )

        updatedThermalGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, _, innerTemperature)),
                None,
                Some(
                  ThermalStorageState(
                    waterStorageTick,
                    waterStorageStoredEnergy,
                  )
                ),
              ) =>
            houseTick shouldBe 10800
            waterStorageTick shouldBe 10800
            innerTemperature should approximate(Celsius(18.93))
            waterStorageStoredEnergy should approximate(KilowattHours(12.18))

          case _ => fail("Thermal grid state couldn't be matched.")
        }
      }
    }

    "determining the energy demand" should {
      "exactly be the demand of the house" in {
        val tick = 10800L // after three hours

        val hoursToDetermine =
          thermalHouse.checkIfNeedToDetermineDomesticHotWaterDemand(
            tick,
            defaultSimulationStart,
            initialHpState,
          )

        val updatedThermalGridState =
          thermalGrid.determineState(
            tick,
            initialGridState,
            HpOperatingPoint.zero,
          )

        val thermalDemands =
          thermalGrid.determineEnergyDemand(
            updatedThermalGridState,
            hoursToDetermine,
          )

        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand
        val domesticHotWaterDemand =
          thermalDemands.domesticHotWaterStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(KilowattHours(1.04476746))
        storageDemand.required should approximate(zeroKWh)
        storageDemand.possible should approximate(zeroKWh)
        domesticHotWaterDemand.required should approximate(zeroKWh)
        domesticHotWaterDemand.possible should approximate(zeroKWh)
        // houseState and heatStorageState are already tested sufficiently
        updatedThermalGridState.domesticHotWaterStorageState shouldBe Some(
          ThermalStorageState(
            10800,
            // when simulating from tick 0 - 10800 the hourly demand
            // for hot water would normally be taken from domestic
            // hot water storage, resulting in a lower storedEnergy here
            expectedDomesticHotWaterStorageStartingState.storedEnergy,
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

        val hoursToDetermine =
          thermalHouse.checkIfNeedToDetermineDomesticHotWaterDemand(
            tick,
            defaultSimulationStart,
            initialHpState,
          )

        val energyDemandDomesticHotWater =
          thermalHouse.energyDemandDomesticHotWater(
            hoursToDetermine,
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

      "deliver the house state by just letting it cool down, if just no feed in is given" in {
        val (thermalGridOperatingPoint, reachedThreshold) =
          thermalGrid.handleConsumption(initialHpState)

        reachedThreshold shouldBe Some(
          SimpleThermalThreshold(3600)
        )
        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint.zero
      }
    }

    "handling thermal feed in into the grid" should {
      "heat up the house and domestic hot water storage parallel" in {
        val gridState = initialGridState.copy(
          houseState = initialGridState.houseState.map(
            _.copy(innerTemperature = thermalHouse.lowerBoundaryTemperature)
          ),
          domesticHotWaterStorageState =
            initialGridState.domesticHotWaterStorageState.map(
              _.copy(storedEnergy = zeroKWh)
            ),
        )

        val state = HpState(
          0,
          defaultSimulationStart,
          gridState,
          HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
          thermalDemandOfHouseAndWaterStorage,
        )

        val (thermalGridOperatingPoint, reachedThreshold) =
          thermalGrid.handleFeedIn(
            state,
            testGridQDotInfeed,
          )

        reachedThreshold shouldBe Some(StorageFull(5846L))
        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
          testGridQDotInfeed,
          testGridQDotInfeed / 2,
          zeroKW,
          testGridQDotInfeed / 2,
        )
      }
    }

    "updating the grid state dependent on the given thermal feed in" should {
      "deliver proper result, if energy is fed into the grid" in {
        val gridState = ThermalGridState(
          Some(
            ThermalHouseState(
              -1,
              testGridAmbientTemperature,
              Celsius(18),
            )
          ),
          None,
          Some(
            ThermalStorageState(
              -1,
              KilowattHours(42),
            )
          ),
        )
        val initState = initialHpState.copy(
          thermalGridState = gridState,
          thermalDemands = onlyThermalDemandOfHouse,
        )

        thermalGrid.handleFeedIn(
          initState,
          testGridQDotInfeed,
        ) match {
          case (
                thermalGridOperatingPoint,
                Some(SimpleThermalThreshold(thresholdTick)),
              ) =>
            thresholdTick shouldBe 3600
            thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
              testGridQDotInfeed,
              testGridQDotInfeed,
              zeroKW,
              zeroKW,
            )
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is consumed from the grid" in {
        thermalGrid.handleConsumption(initialHpState) match {
          case (
                thermalGridOperatingPoint,
                Some(SimpleThermalThreshold(thresholdTick)),
              ) =>
            thresholdTick shouldBe 3600L
            thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint.zero
          case _ => fail("Thermal grid state updated failed")
        }
      }
    }
  }
}
