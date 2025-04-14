/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.model.participant2.HpModel.{
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

  val thermalGrid: ThermalGrid = ThermalGrid(
    new edu.ie3.datamodel.models.input.container.ThermalGrid(
      thermalBusInput,
      Set(thermalHouseInput).asJava,
      Set[ThermalStorageInput](thermalStorageInput).asJava,
      Set.empty[ThermalStorageInput].asJava,
    )
  )

  val initialGridState: ThermalGridState =
    ThermalGrid.startingState(thermalGrid, testGridAmbientTemperature)

  val initialHpState = HpState(
    -1L,
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
          Set.empty[ThermalStorageInput].asJava,
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
                    storageTick,
                    storedEnergy,
                  )
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
          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "updatedThermalGridState" should {
      "exactly calculate the state of the thermalGrid" in {
        val tick = 10800L // after three hours

        val updatedThermalGridState =
          thermalGrid.updateThermalGridState(
            tick,
            initialHpState,
            HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
          )

        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(
            10800,
            testGridAmbientTemperature,
            Kelvin(292.0799935185185),
          )
        )
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(
            10800,
            zeroKWh,
          )
        )

      }
    }

    "determining the energy demand" should {
      "deliver the house demand (no demand) with added flexibility by storage" in {
        val tick = 10800L // after three hours

        val updatedThermalGridState =
          thermalGrid.updateThermalGridState(
            tick,
            initialHpState,
            HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
          )

        val thermalDemands =
          thermalGrid.determineEnergyDemand(updatedThermalGridState)

        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(KilowattHours(1.05009722d))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
      }

      "updatedThermalGridState" should {
        "exactly calculate the state of the thermalGrid" in {
          val tick = 10800
          val gridState = initialGridState.copy(houseState =
            initialGridState.houseState.map(
              _.copy(innerTemperature = Celsius(16d))
            )
          )

          val state = initialHpState.copy(
            thermalGridState = gridState,
            thermalDemands = thermalDemandOfHouseAndHeatStorage,
          )

          val updatedThermalGridState =
            thermalGrid.updateThermalGridState(
              tick,
              state,
              HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
            )

          updatedThermalGridState.houseState shouldBe Some(
            ThermalHouseState(
              10800,
              testGridAmbientTemperature,
              Celsius(15.959996296296296),
            )
          )
          updatedThermalGridState.storageState shouldBe Some(
            ThermalStorageState(10800, zeroKWh)
          )
        }
      }

      "deliver the correct house and storage demand" in {
        val tick = 10800
        val gridState = initialGridState.copy(houseState =
          initialGridState.houseState.map(
            _.copy(innerTemperature = Celsius(16d))
          )
        )

        val state = initialHpState.copy(
          tick = tick,
          thermalGridState = gridState,
          thermalDemands = thermalDemandOfHouseAndHeatStorage,
        )

        val updatedThermalGridState =
          thermalGrid.updateThermalGridState(
            state.tick,
            state,
            HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
          )

        val thermalDemands =
          thermalGrid.determineEnergyDemand(updatedThermalGridState)

        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand

        houseDemand.required should approximate(KilowattHours(45.6000555))
        houseDemand.possible should approximate(KilowattHours(45.600055555))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
      }
    }

    "handling thermal energy consumption from grid" should {
      "return house threshold, if storage is in balance" in {
        val initialLoading = KilowattHours(430d)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading)
          )
        )

        val state = initialHpState.copy(
          thermalGridState = gridState,
          thermalDemands = onlyAdditionalDemandOfHeatStorage,
        )

        val (thermalGridOperatingPoint, reachedThreshold) =
          thermalGrid.handleConsumption(state)

        reachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154284L)
        )
        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint.zero
      }

      "take energy from storage, if there is actual consumption" in {
        val initialLoading = KilowattHours(10d)
        val gridState = initialGridState.copy(
          houseState = initialGridState.houseState.map(houseState =>
            houseState.copy(innerTemperature = Celsius(17))
          ),
          storageState = initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading)
          ),
        )

        val state = initialHpState.copy(
          thermalGridState = gridState,
          // The exact amount doesn't matter
          thermalDemands = ThermalDemandWrapper(
            ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
            ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
          ),
        )

        val (thermalGridOperatingPoint, reachedThreshold) =
          thermalGrid.handleConsumption(state)

        reachedThreshold shouldBe Some(StorageEmpty(1799))
        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
          zeroKW,
          thermalStorage.pThermalMax,
          thermalStorage.pThermalMax * -1,
        )
      }

      "properly take energy from storage when not reached target temp and was heated last operating point from Hp" in {
        val gridState = initialGridState
          .copy(
            houseState = initialGridState.houseState.map(
              _.copy(innerTemperature = Celsius(18.5))
            ),
            storageState = Some(
              ThermalStorageState(
                0L,
                KilowattHours(5d),
              )
            ),
          )
        val lastOperatingPoint = HpOperatingPoint(
          Kilowatts(1),
          ThermalGridOperatingPoint(Kilowatts(1), Kilowatts(1), zeroKW),
        )

        val state = initialHpState.copy(
          thermalGridState = gridState,
          lastHpOperatingPoint = lastOperatingPoint,
          // The exact amount doesn't matter
          thermalDemands = ThermalDemandWrapper(
            ThermalEnergyDemand(zeroKWh, KilowattHours(1)),
            ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
          ),
        )

        val (thermalGridOperatingPoint, reachedThreshold) =
          thermalGrid.handleConsumption(state)

        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
          zeroKW,
          thermalStorage.pThermalMax,
          thermalStorage.pThermalMax * -1,
        )

        reachedThreshold shouldBe Some(StorageEmpty(900))
      }
    }

    "revising infeed from storage to house" should {
      val state = initialHpState.copy(
        tick = 3600L,
        thermalDemands = onlyThermalDemandOfHeatStorage,
      )
      "hand back unaltered information if needed information is missing" in {
        val maybeHouseThreshold = thermalHouse.determineNextThreshold(
          state.thermalGridState.houseState.getOrElse(
            throw new IllegalStateException(
              "Could not get state of thermal house."
            )
          ),
          zeroKW,
        )

        thermalGrid.reviseFeedInFromStorage(
          state,
          maybeHouseThreshold,
        ) match {
          case (
                thermalGridOperatingPoint,
                nextThreshold,
              ) =>
            thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint.zero
            nextThreshold shouldBe Some(
              HouseTemperatureLowerBoundaryReached(154284)
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

        val hpState = state.copy(
          thermalGridState = state.thermalGridState.copy(
            houseState = Some(maybeHouseState),
            storageState = maybeStorageState,
          ),
          // The exact amount doesn't matter
          thermalDemands = ThermalDemandWrapper(
            ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
            ThermalEnergyDemand(zeroKWh, KilowattHours(1)),
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
          storageState = maybeStorageState,
        )

        val state = initialHpState.copy(
          tick = tick,
          thermalGridState = gridState,
          // The exact amount doesn't matter
          thermalDemands = ThermalDemandWrapper(
            ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
            ThermalEnergyDemand(zeroKWh, KilowattHours(1)),
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
        )

        threshold shouldBe Some(HouseTargetTemperatureReached(6341))
      }
    }
  }

  "handling thermal feed in into the grid" should {
    "heat the house, if the target temperature in the house is not reached" in {
      val initialGridState = ThermalGridState(
        Some(
          ThermalHouseState(
            -1,
            testGridAmbientTemperature,
            Celsius(17),
          )
        ),
        Some(expectedStorageStartingState),
      )

      val state = initialHpState.copy(
        thermalGridState = initialGridState,
        // The exact amount doesn't matter
        thermalDemands = ThermalDemandWrapper(
          ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
          ThermalEnergyDemand(KilowattHours(1), KilowattHours(1)),
        ),
      )

      val externalQDot = testGridQDotInfeed

      val (thermalGridOperatingPoint, reachedThreshold) =
        thermalGrid.handleFeedIn(
          state,
          externalQDot,
        )

      reachedThreshold shouldBe Some(HouseTargetTemperatureReached(7321L))
      thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
        externalQDot,
        externalQDot,
        zeroKW,
      )
    }

    "load the storage, if the target temperature in the house is reached" in {
      val externalQDot = testGridQDotInfeed * 10

      val (thermalGridOperatingPoint, reachedThreshold) =
        thermalGrid.handleFeedIn(
          initialHpState,
          externalQDot,
        )

      reachedThreshold shouldBe Some(
        StorageFull(27599)
      )
      thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
        externalQDot,
        zeroKW,
        externalQDot,
      )
    }
  }

}
