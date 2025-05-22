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
  ThermalDemandWrapper,
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTargetTemperatureReached,
  HouseTemperatureLowerBoundaryReached,
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull,
}
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import squants.energy._
import squants.thermal.Celsius
import squants.{Energy, Kelvin, Power, Temperature}
import tech.units.indriya.unit.Units

import scala.jdk.CollectionConverters._

class ThermalGridWithHouseAndStorageSpec
    extends UnitSpec
    with ThermalHouseTestData
    with ThermalStorageTestData
    with DefaultTestData {

  implicit val tempTolerance: Temperature = Kelvin(1e-3)
  implicit val powerTolerance: Power = Watts(1e-3)
  implicit val energyTolerance: Energy = WattHours(1e-3)

  val thermalGrid: ThermalGrid = ThermalGrid(
    new edu.ie3.datamodel.models.input.container.ThermalGrid(
      thermalBusInput,
      Set(thermalHouseInput).asJava,
      Set[ThermalStorageInput](thermalStorageInput).asJava,
      Set[ThermalStorageInput](domesticHotWaterStorageInput).asJava,
    )
  )

  val initialGridState: ThermalGridState =
    ThermalGrid.startingState(thermalGrid, testGridAmbientTemperature)

  val initialHpState: HpState = HpState(
    0L,
    defaultSimulationStart,
    initialGridState,
    HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
    onlyThermalDemandOfHeatStorage,
  )

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
              Some(thermalHeatStorageGenerated),
              Some(domesticHotWaterStorageGenerated),
            ) =>
          thermalHouseGenerated shouldBe thermalHouse
          thermalHeatStorageGenerated shouldBe thermalStorage
          domesticHotWaterStorageGenerated shouldBe domesticHotWaterStorage
        case _ =>
          fail("Generation of thermal grid from thermal input grid failed.")
      }
    }
  }

  "Testing a thermal grid with house and storage" when {

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
                Some(
                  ThermalStorageState(
                    tickHeatStorage,
                    storedEnergyHeatStorage,
                  )
                ),
                Some(
                  ThermalStorageState(
                    tickWaterStorage,
                    storedEnergyWaterStorage,
                  )
                ),
              ) =>
            houseTick shouldBe expectedHouseStartingState.tick
            tickHeatStorage shouldBe expectedCylindricalStorageStartingState.tick
            tickWaterStorage shouldBe expectedDomesticHotWaterStorageStartingState.tick

            innerTemperature should approximate(
              expectedHouseStartingState.innerTemperature
            )
            storedEnergyHeatStorage should approximate(
              expectedCylindricalStorageStartingState.storedEnergy
            )

            storedEnergyWaterStorage should approximate(
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
                Some(
                  ThermalStorageState(heatStorageTick, heatStorageStoredEnergy)
                ),
                Some(
                  ThermalStorageState(
                    waterStorageTick,
                    waterStorageStoredEnergy,
                  )
                ),
              ) =>
            houseTick shouldBe 10800
            heatStorageTick shouldBe houseTick
            waterStorageTick shouldBe houseTick
            innerTemperature should approximate(Celsius(18.93))
            heatStorageStoredEnergy shouldBe zeroKWh
            waterStorageStoredEnergy should approximate(KilowattHours(12.18))
          case _ => fail("Thermal grid state couldn't be matched.")
        }
      }
    }

    "determining the energy demand" should {
      "deliver the heat demand of the house (no demand) with added flexibility by storage" in {
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
            HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
          )

        val thermalDemands =
          thermalGrid.determineEnergyDemand(
            updatedThermalGridState,
            hoursToDetermine,
          )

        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand
        val waterStorageDemand = thermalDemands.domesticHotWaterStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(KilowattHours(1.04476746))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        waterStorageDemand.required should approximate(KilowattHours(0d))
        waterStorageDemand.possible should approximate(KilowattHours(0d))
      }

      "updatedThermalGridState" should {
        "exactly calculate the state of the thermalGrid with zero as operating point" in {
          val tick = 10800
          val gridState = initialGridState.copy(houseState =
            initialGridState.houseState.map(
              _.copy(innerTemperature = Celsius(16d))
            )
          )

          val updatedThermalGridState =
            thermalGrid.determineState(
              tick,
              gridState,
              HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
            )

          updatedThermalGridState match {
            case ThermalGridState(
                  Some(ThermalHouseState(houseTick, _, innerTemperature)),
                  Some(
                    ThermalStorageState(
                      heatStorageTick,
                      heatStorageStoredEnergy,
                    )
                  ),
                  Some(
                    ThermalStorageState(
                      waterStorageTick,
                      waterStorageStoredEnergy,
                    )
                  ),
                ) =>
              houseTick shouldBe 10800
              heatStorageTick shouldBe houseTick
              innerTemperature should approximate(Celsius(15.96))
              heatStorageStoredEnergy shouldBe zeroKWh
            case _ => fail("Thermal grid state couldn't be matched.")
          }

          // OperatingPoint zero for waterStorage
          updatedThermalGridState.domesticHotWaterStorageState shouldBe Some(
            ThermalStorageState(
              10800,
              expectedDomesticHotWaterStorageStartingState.storedEnergy,
            )
          )
        }
        "exactly calculate the state of the thermalGrid with non-zero OperatingPoint" in {
          val tick = 10800
          val gridState = initialGridState.copy(houseState =
            initialGridState.houseState.map(
              _.copy(innerTemperature = Celsius(16d))
            )
          )

          val thGridOperatingPoint = ThermalGridOperatingPoint(
            testGridQDotInfeed,
            zeroKW,
            testGridQDotInfeed,
            Kilowatts(-1d),
          )

          val updatedThermalGridState =
            thermalGrid.determineState(
              tick,
              gridState,
              HpOperatingPoint(zeroKW, thGridOperatingPoint),
            )

          updatedThermalGridState match {
            case ThermalGridState(
                  Some(ThermalHouseState(houseTick, _, innerTemperature)),
                  Some(ThermalStorageState(heatStorageTick, heatStoredEnergy)),
                  Some(ThermalStorageState(waterStorageTick, waterStoredEnergy)),
                ) =>
              houseTick shouldBe 10800
              heatStorageTick shouldBe houseTick
              waterStorageTick shouldBe houseTick
              innerTemperature should approximate(Celsius(15.9602))
              heatStoredEnergy shouldBe KilowattHours(45)
              waterStoredEnergy should approximate(KilowattHours(9.18))
            case _ => fail("Thermal grid state couldn't be matched.")
          }
        }
      }

      "determine the hot water demand" in {
        val tick = 0
        val startingState =
          ThermalGrid.startingState(thermalGrid, testGridAmbientTemperature)

        val hoursToDetermine =
          thermalHouse.checkIfNeedToDetermineDomesticHotWaterDemand(
            tick,
            defaultSimulationStart,
            initialHpState,
          )

        val thermalDemands =
          thermalGrid.determineEnergyDemand(
            startingState,
            hoursToDetermine,
          )

        thermalDemands.houseDemand.required should approximate(zeroKWh)
        thermalDemands.houseDemand.possible should approximate(zeroKWh)
        thermalDemands.heatStorageDemand.required should approximate(
          KilowattHours(1150d)
        )
        thermalDemands.heatStorageDemand.possible should approximate(
          KilowattHours(1150d)
        )
        thermalDemands.domesticHotWaterStorageDemand.required should approximate(
          KilowattHours(0d)
        )
        thermalDemands.domesticHotWaterStorageDemand.possible should approximate(
          KilowattHours(0d)
        )
      }

      "deliver the correct house and storage demand" in {
        val tick = 10800

        val hoursToDetermine =
          thermalHouse.checkIfNeedToDetermineDomesticHotWaterDemand(
            tick,
            defaultSimulationStart,
            initialHpState,
          )

        val gridState = initialGridState.copy(houseState =
          initialGridState.houseState.map(
            _.copy(innerTemperature = Celsius(16d))
          )
        )

        val updatedThermalGridState =
          thermalGrid.determineState(
            tick,
            gridState,
            HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
          )

        val thermalDemands =
          thermalGrid.determineEnergyDemand(
            updatedThermalGridState,
            hoursToDetermine,
          )

        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand
        val waterStorageDemand = thermalDemands.domesticHotWaterStorageDemand

        houseDemand.required should approximate(KilowattHours(45.59701))
        houseDemand.possible should approximate(KilowattHours(45.59701))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        waterStorageDemand.required should approximate(KilowattHours(0d))
        waterStorageDemand.possible should approximate(KilowattHours(0d))
      }
    }

    "handling thermal energy consumption from grid" should {
      "return house threshold, if storage is in balance" in {
        val initialLoading = KilowattHours(430d)
        val gridState = initialGridState.copy(heatStorageState =
          initialGridState.heatStorageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading)
          )
        )

        val state = initialHpState.copy(
          thermalGridState = gridState,
          thermalDemands = onlyPossibleDemandOfHeatStorage,
        )

        val (thermalGridOperatingPoint, reachedThreshold) =
          thermalGrid.handleConsumption(state)

        reachedThreshold shouldBe Some(
          SimpleThermalThreshold(3600L)
        )
        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint.zero
      }

      "take energy from storage, if there is actual consumption" in {
        val initialLoading = KilowattHours(10d)
        val gridState = initialGridState.copy(
          houseState = initialGridState.houseState.map(houseState =>
            houseState.copy(innerTemperature = Celsius(17))
          ),
          heatStorageState =
            initialGridState.heatStorageState.map(storageState =>
              storageState.copy(storedEnergy = initialLoading)
            ),
        )

        val state = initialHpState.copy(
          thermalGridState = gridState,
          // The exact amount doesn't matter
          thermalDemands = ThermalDemandWrapper(
            ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
            ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
            ThermalEnergyDemand.noDemand,
            ThermalEnergyDemand.noDemand,
          ),
        )

        val (thermalGridOperatingPoint, reachedThreshold) =
          thermalGrid.handleConsumption(state)

        reachedThreshold shouldBe Some(StorageEmpty(1800))
        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
          zeroKW,
          thermalStorage.pThermalMax,
          thermalStorage.pThermalMax * -1,
          zeroKW,
        )
      }

      "properly take energy from storage when not reached target temp and was heated last operating point from Hp" in {
        val gridState = initialGridState
          .copy(
            houseState = initialGridState.houseState.map(
              _.copy(innerTemperature = Celsius(18.5))
            ),
            heatStorageState = Some(
              ThermalStorageState(
                0L,
                KilowattHours(5d),
              )
            ),
          )
        val lastOperatingPoint = HpOperatingPoint(
          Kilowatts(1),
          ThermalGridOperatingPoint(Kilowatts(1), Kilowatts(1), zeroKW, zeroKW),
        )

        val state = initialHpState.copy(
          thermalGridState = gridState,
          lastHpOperatingPoint = lastOperatingPoint,
          // The exact amount doesn't matter
          thermalDemands = ThermalDemandWrapper(
            ThermalEnergyDemand(zeroKWh, KilowattHours(1)),
            ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
            ThermalEnergyDemand.noDemand,
            ThermalEnergyDemand.noDemand,
          ),
        )

        val (thermalGridOperatingPoint, reachedThreshold) =
          thermalGrid.handleConsumption(state)

        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
          zeroKW,
          thermalStorage.pThermalMax,
          thermalStorage.pThermalMax * -1,
          zeroKW,
        )

        reachedThreshold shouldBe Some(StorageEmpty(900))
      }
    }

    "revising feed in from storage to house" should {
      val state = initialHpState.copy(
        tick = 3600L,
        thermalDemands = onlyThermalDemandOfHeatStorage,
      )
      "hand back unaltered information if needed information is missing" in {
        val maybeHouseState = Some(
          ThermalHouseState(
            state.tick,
            testGridAmbientTemperature,
            Celsius(
              thermalHouseInput.getTargetTemperature
                .to(Units.CELSIUS)
                .getValue
                .doubleValue
            ),
          )
        )
        val maybeStorageState = None
        val maybeWaterStorageState =
          Some(ThermalStorageState(0L, KilowattHours(2d)))

        val maybeThermalGridState =
          ThermalGridState(
            maybeHouseState,
            maybeStorageState,
            maybeWaterStorageState,
          )

        val maybeThreshold = None

        val hpState = state.copy(thermalGridState = maybeThermalGridState)

        thermalGrid.reviseFeedInFromStorage(
          hpState,
          maybeThreshold,
        ) match {
          case (thermalGridOperatingPoint, nextThreshold) =>
            thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint.zero
            nextThreshold shouldBe None
        }
      }

      "hand back unaltered information if house temperature is above lower boundary temperature" in {
        val maybeHouseState =
          ThermalHouseState(
            state.tick,
            testGridAmbientTemperature,
            Celsius(
              thermalHouseInput.getTargetTemperature
                .to(Units.CELSIUS)
                .getValue
                .doubleValue
            ),
          )
        val maybeStorageState =
          ThermalStorageState(
            state.tick,
            KilowattHours(50d),
          )

        val maybeWaterStorageState = None

        val maybeThermalGridState =
          ThermalGridState(
            Some(maybeHouseState),
            Some(maybeStorageState),
            maybeWaterStorageState,
          )

        val hpState = state.copy(thermalGridState = maybeThermalGridState)

        val maybeThreshold =
          thermalHouse.determineNextThreshold(maybeHouseState, zeroKW)

        thermalGrid.reviseFeedInFromStorage(
          hpState,
          maybeThreshold,
        ) match {
          case (
                thermalGridOperatingPoint,
                nextThreshold,
              ) =>
            thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint.zero
            nextThreshold shouldBe Some(
              HouseTemperatureLowerBoundaryReached(170082L)
            )
        }
      }

      "heat house from storage if house temperature is at lower boundary temperature" in {
        val maybeHouseState =
          ThermalHouseState(
            state.tick,
            testGridAmbientTemperature,
            Celsius(
              thermalHouseInput.getLowerTemperatureLimit
                .to(Units.CELSIUS)
                .getValue
                .doubleValue
            ),
          )

        val maybeHouseThreshold =
          thermalHouse.determineNextThreshold(maybeHouseState, zeroKW)

        val maybeStorageState =
          Some(ThermalStorageState(state.tick, KilowattHours(10)))

        val maybeWaterStorageState = None

        val hpState = state.copy(
          thermalGridState = state.thermalGridState.copy(
            houseState = Some(maybeHouseState),
            heatStorageState = maybeStorageState,
            domesticHotWaterStorageState = maybeWaterStorageState,
          ),
          // The exact amount doesn't matter
          thermalDemands = ThermalDemandWrapper(
            ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
            ThermalEnergyDemand(zeroKWh, KilowattHours(1)),
            ThermalEnergyDemand.noDemand,
            ThermalEnergyDemand.noDemand,
          ),
        )

        thermalGrid.reviseFeedInFromStorage(
          hpState,
          maybeHouseThreshold,
        ) match {
          case (
                thermalGridOperatingPoint,
                nextThreshold,
              ) =>
            thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
              zeroKW,
              thermalStorage.pThermalMax,
              thermalStorage.getpThermalMax * -1,
              zeroKW,
            )
            nextThreshold shouldBe Some(StorageEmpty(5400))
        }
      }

      "heat house from storage if house temperature is at lower boundary temperature, return houseTick" in {
        val tick = 3600

        val maybeHouseState = Some(
          ThermalHouseState(
            tick,
            testGridAmbientTemperature,
            Celsius(
              thermalHouseInput.getLowerTemperatureLimit
                .to(Units.CELSIUS)
                .getValue
                .doubleValue
            ),
          )
        )
        val maybeStorageState = Some(
          ThermalStorageState(
            tick,
            KilowattHours(20d),
          )
        )

        val gridState = initialGridState.copy(
          houseState = maybeHouseState,
          heatStorageState = maybeStorageState,
        )

        val state = initialHpState.copy(
          tick = tick,
          thermalGridState = gridState,
          // The exact amount doesn't matter
          thermalDemands = ThermalDemandWrapper(
            ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
            ThermalEnergyDemand(zeroKWh, KilowattHours(1)),
            ThermalEnergyDemand.noDemand,
            ThermalEnergyDemand.noDemand,
          ),
        )

        val (thermalGridOperatingPoint, threshold) =
          thermalGrid.handleConsumption(
            state
          )

        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
          zeroKW,
          thermalStorage.pThermalMax,
          thermalStorage.pThermalMax * -1,
          zeroKW,
        )
        threshold shouldBe Some(HouseTargetTemperatureReached(6344L))
      }
    }
  }

  "handling thermal feed in into the grid" should {
    "heat the house, if the target temperature in the house is not reached" in {
      val gridState = initialGridState.copy(
        houseState = Some(
          ThermalHouseState(
            -1,
            testGridAmbientTemperature,
            Celsius(17),
          )
        ),
        storageState = Some(expectedCylindricalStorageStartingState),
        Some(expectedDomesticHotWaterStorageStartingState),
      )

      val state = initialHpState.copy(
        thermalGridState = gridState,
        // The exact amount doesn't matter
        thermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
          ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
          ThermalEnergyDemand.noDemand,
          ThermalEnergyDemand.noDemand,
        ),
      )

      val externalQDot = testGridQDotInfeed

      val (thermalGridOperatingPoint, reachedThreshold) =
        thermalGrid.handleFeedIn(
          state,
          externalQDot,
        )

      reachedThreshold shouldBe Some(SimpleThermalThreshold(3600L))
      thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
        externalQDot,
        externalQDot,
        zeroKW,
        zeroKW,
      )
    }

    "heat the house and recharge the domestic hot water storage, if the house has required heat demand and domestic hot water storage is empty" in {
      val gridState = initialGridState.copy(
        houseState = initialGridState.houseState.map(
          _.copy(innerTemperature = thermalHouse.lowerBoundaryTemperature)
        ),
        domesticHotWaterStorageState =
          initialGridState.domesticHotWaterStorageState.map(
            _.copy(storedEnergy = zeroKWh)
          ),
      )

      val state =
        HpState(
          0,
          defaultSimulationStart,
          gridState,
          HpOperatingPoint.zero,
          thermalDemandOfHouseAndWaterStorage,
        )

      val (operatingPoint, reachedThreshold) =
        thermalGrid.handleFeedIn(
          state,
          testGridQDotInfeed,
        )

      reachedThreshold shouldBe Some(StorageFull(5846))
      operatingPoint shouldBe ThermalGridOperatingPoint(
        testGridQDotInfeed,
        testGridQDotInfeed / 2,
        zeroKW,
        testGridQDotInfeed / 2,
      )
    }
  }

  "load the storage, if the target temperature in the house is reached" in {
    val externalQDot = testGridQDotInfeed * 10

    val (thermalGridOperatingPoint, reachedThreshold) =
      thermalGrid.handleFeedIn(
        initialHpState,
        externalQDot,
      )

    reachedThreshold shouldBe Some(SimpleThermalThreshold(3600))
    thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
      externalQDot,
      zeroKW,
      externalQDot,
      zeroKW,
    )
  }
  "don't load the heat storage, use qDot directly to cover hot water demand before recharge domestic hot water storage if the upper temperature in the house is reached and the domestic hot water storage is empty" in {
    val firstThermalDemands = ThermalDemandWrapper(
      ThermalEnergyDemand(zeroKWh, zeroKWh),
      ThermalEnergyDemand(KilowattHours(1150), KilowattHours(1150)),
      ThermalEnergyDemand.noDemand,
      ThermalEnergyDemand(KilowattHours(12.18), KilowattHours(12.18)),
    )

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

    val state =
      HpState(
        0,
        defaultSimulationStart,
        gridState,
        HpOperatingPoint.zero,
        firstThermalDemands,
      )

    val (firstOperatingPoint, firstReachedThreshold) =
      thermalGrid.handleFeedIn(
        state,
        externalQDot,
      )

    firstOperatingPoint shouldBe ThermalGridOperatingPoint(
      testGridQDotInfeed,
      zeroKW,
      zeroKW,
      testGridQDotInfeed,
    )
    firstReachedThreshold shouldBe Some(StorageFull(2923))
  }

}
