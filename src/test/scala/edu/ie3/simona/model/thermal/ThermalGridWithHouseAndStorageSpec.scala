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
  ThermalOpWrapper,
}
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
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
    val thermalGrid: ThermalGrid = ThermalGrid(
      new edu.ie3.datamodel.models.input.container.ThermalGrid(
        thermalBusInput,
        Set(thermalHouseInput).asJava,
        Set[ThermalStorageInput](thermalStorageInput).asJava,
        Set.empty[ThermalStorageInput].asJava,
      )
    )

    val initialGridState: ThermalGridState =
      ThermalGrid.startingState(thermalGrid)

    val initialHpState = HpState(
      0L,
      testGridAmbientTemperature,
      initialGridState,
      HpOperatingPoint(zeroKW, ThermalOpWrapper.zero),
      testGridAmbientTemperature,
      noThermalDemand,
    )

    "requesting the starting state" should {
      "deliver proper results" in {
        initialGridState match {
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
        val tick = 10800L // after three hours
        val state = initialHpState.copy(
          tick = tick,
          thermalDemands = onlyThermalDemandOfHeatStorage,
        )

        val updatedThermalGridState =
          thermalGrid.updatedThermalGridState(
            state.tick,
            state,
            HpOperatingPoint(zeroKW, ThermalOpWrapper.zero),
          )

        val thermalDemands =
          thermalGrid.determineEnergyDemand(updatedThermalGridState)

        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(KilowattHours(1.05))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Kelvin(292.08), zeroKW)
        )
        updatedThermalGridState.storageState shouldBe Some(
          ThermalStorageState(10800, zeroKWh, zeroKW)
        )
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
          thermalDemands = onlyThermalDemandOfHeatStorage,
        )

        val updatedThermalGridState =
          thermalGrid.updatedThermalGridState(
            state.tick,
            state,
            HpOperatingPoint(zeroKW, ThermalOpWrapper.zero),
          )

        val thermalDemands =
          thermalGrid.determineEnergyDemand(updatedThermalGridState)

        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand

        houseDemand.required should approximate(KilowattHours(45.6))
        houseDemand.possible should approximate(KilowattHours(45.6))
        storageDemand.required should approximate(KilowattHours(1150d))
        storageDemand.possible should approximate(KilowattHours(1150d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Celsius(15.96), zeroKW)
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

        val externalQDot = zeroKW

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(state)

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
          thermalDemands = onlyAdditionalDemandOfHeatStorage,
        )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(state)

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
            innerTemperature should approximate(Celsius(16.9999))
            qDotHouse should approximate(thermalStorage.pThermalMax)

            storageTick shouldBe 0L
            storedEnergy should approximate(initialLoading)
            qDotStorage should approximate(thermalStorage.pThermalMax * -1)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageEmpty(1800L))
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
                zeroKW,
              )
            ),
          )
        val lastOperatingPoint = HpOperatingPoint(
          Kilowatts(1),
          ThermalOpWrapper(Kilowatts(1), Kilowatts(1), zeroKW),
        )

        val state = initialHpState.copy(
          thermalGridState = gridState,
          lastHpOperatingPoint = lastOperatingPoint,
          thermalDemands = onlyThermalDemandOfHouse,
        )

        val (updatedGridState, reachedThreshold) =
          thermalGrid.handleConsumption(state)

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                ),
              ) =>
            houseTick shouldBe 0L
            storageTick shouldBe 0L
            innerTemperature should approximate(Celsius(18.5))
            storedEnergy should approximate(KilowattHours(5d))
            qDotHouse should approximate(thermalStorage.pThermalMax)
            qDotStorage should approximate(thermalStorage.pThermalMax * -1)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageEmpty(900))
      }
    }

    "revising infeed from storage to house" should {
      val zeroInflux = zeroKW

      val state = initialHpState.copy(
        tick = 3600L,
        thermalDemands = onlyThermalDemandOfHeatStorage,
      )
      "hand back unaltered information if needed information is missing" in {
        val maybeHouseState = Some(
          ThermalHouseState(
            state.tick,
            Celsius(
              thermalHouseInput.getTargetTemperature
                .to(Units.CELSIUS)
                .getValue
                .doubleValue
            ),
            zeroInflux,
          )
        )
        val maybeStorageState = None

        val maybeThermalGridState =
          ThermalGridState(maybeHouseState, maybeStorageState)

        val maybeThreshold = None

        thermalGrid.reviseFeedInFromStorage(
          state,
          maybeThermalGridState,
          maybeThreshold,
          zeroKW,
        ) match {
          case (revisedGridState, nextThreshold) =>
            revisedGridState shouldBe ThermalGridState(
              maybeHouseState,
              maybeStorageState,
            )
            nextThreshold shouldBe None
        }
      }

      "hand back unaltered information if house temperature is above lower boundary temperature" in {
        val maybeHouseState = Some(
          ThermalHouseState(
            state.tick,
            Celsius(
              thermalHouseInput.getTargetTemperature
                .to(Units.CELSIUS)
                .getValue
                .doubleValue
            ),
            zeroInflux,
          )
        )
        val maybeStorageState = Some(
          ThermalStorageState(
            state.tick,
            KilowattHours(50d),
            zeroInflux,
          )
        )

        val maybeThermalGridState =
          ThermalGridState(maybeHouseState, maybeStorageState)

        val maybeThreshold = None
        thermalGrid.reviseFeedInFromStorage(
          state,
          maybeThermalGridState,
          maybeThreshold,
          zeroInflux,
        ) match {
          case (revisedGridState, nextThreshold) =>
            revisedGridState shouldBe ThermalGridState(
              maybeHouseState,
              maybeStorageState,
            )
            nextThreshold shouldBe None
        }
      }

      "hand back unaltered information if house temperature is above lower boundary temperature, but has influx" in {
        val maybeHouseState = Some(
          ThermalHouseState(
            state.tick,
            Celsius(
              thermalHouseInput.getTargetTemperature
                .to(Units.CELSIUS)
                .getValue
                .doubleValue
            ),
            testGridQDotInfeed,
          )
        )
        val maybeStorageState = Some(
          ThermalStorageState(
            state.tick,
            KilowattHours(50d),
            zeroInflux,
          )
        )

        val maybeThermalGridState =
          ThermalGridState(maybeHouseState, maybeStorageState)
        val maybeThreshold = Some(HouseTargetTemperatureReached(3600L))

        thermalGrid.reviseFeedInFromStorage(
          state,
          maybeThermalGridState,
          maybeThreshold,
          testGridQDotInfeed,
        ) match {
          case (revisedGridState, nextThreshold) =>
            revisedGridState shouldBe ThermalGridState(
              maybeHouseState,
              maybeStorageState,
            )
            nextThreshold shouldBe maybeThreshold
        }
      }

      "hand back unaltered information if house temperature is at lower boundary temperature, but storage is empty" in {
        val maybeHouseState = Some(
          ThermalHouseState(
            state.tick,
            Celsius(
              thermalHouseInput.getLowerTemperatureLimit
                .to(Units.CELSIUS)
                .getValue
                .doubleValue
            ),
            zeroInflux,
          )
        )
        val maybeStorageState = Some(
          ThermalStorageState(
            state.tick,
            zeroKWh,
            testGridQDotInfeed,
          )
        )

        val maybeThermalGridState =
          ThermalGridState(maybeHouseState, maybeStorageState)
        val maybeThreshold = Some(StorageEmpty(state.tick))

        thermalGrid.reviseFeedInFromStorage(
          state,
          maybeThermalGridState,
          maybeThreshold,
          zeroInflux,
        ) match {
          case (revisedGridState, nextThreshold) =>
            revisedGridState shouldBe ThermalGridState(
              maybeHouseState,
              maybeStorageState,
            )
            nextThreshold shouldBe maybeThreshold
        }
      }

      "alter the given states as expected, when all conditions are met" in {
        val tick = 3600L
        val initialLoading = KilowattHours(70d)
        val dischargingRate = Kilowatts(-50d)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(
              storedEnergy = initialLoading,
              qDot = dischargingRate,
            )
          )
        )

        val state = initialHpState.copy(
          tick = tick,
          thermalGridState = gridState,
          lastStateAmbientTemperature = Celsius(14),
          thermalDemands = noThermalDemand,
        )

        val maybeHouseState = Some(
          ThermalHouseState(
            state.tick,
            Celsius(
              thermalHouseInput.getLowerTemperatureLimit
                .to(Units.CELSIUS)
                .getValue
                .doubleValue
            ),
            zeroInflux,
          )
        )
        val maybeStorageState = Some(
          ThermalStorageState(
            state.tick,
            KilowattHours(20d),
            // just any Power, the stored energy is what we need here.
            zeroKW,
          )
        )

        val maybeThermalGridState =
          ThermalGridState(maybeHouseState, maybeStorageState)

        val maybeThreshold =
          Some(HouseTemperatureLowerBoundaryReached(state.tick))

        thermalGrid.reviseFeedInFromStorage(
          state,
          maybeThermalGridState,
          maybeThreshold,
          zeroInflux,
        ) match {
          case (
                ThermalGridState(
                  Some(
                    ThermalHouseState(houseTick, _, revisedQDotHouse)
                  ),
                  Some(
                    ThermalStorageState(storageTick, _, revisedQDotStorage)
                  ),
                ),
                Some(HouseTargetTemperatureReached(threshold)),
              ) =>
            houseTick shouldBe state.tick
            storageTick shouldBe state.tick

            revisedQDotHouse should approximate(thermalStorage.pThermalMax)
            revisedQDotStorage should approximate(
              thermalStorage.pThermalMax * -1
            )

            threshold shouldBe 3645
          case _ => fail("Revision of states failed")
        }
      }
    }

    "handling thermal feed in into the grid" should {
      val handleFeedIn =
        PrivateMethod[(ThermalGridState, Option[ThermalThreshold])](
          Symbol("handleFeedIn")
        )

      "heat the house, if the target temperature in the house is not reached" in {
        val initialGridState = ThermalGridState(
          Some(ThermalHouseState(-1, Celsius(17), zeroKW)),
          Some(expectedStorageStartingState),
        )

        val state = initialHpState.copy(
          thermalGridState = initialGridState,
          thermalDemands = onlyThermalDemandOfHouse,
        )

        val externalQDot = testGridQDotInfeed

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleFeedIn(
            state,
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
        reachedThreshold shouldBe Some(HouseTargetTemperatureReached(7322L))
      }

      "load the storage, if the target temperature in the house is reached" in {
        val gridState = initialGridState.copy(houseState =
          initialGridState.houseState.map(
            _.copy(innerTemperature = thermalHouse.targetTemperature)
          )
        )
        val state = initialHpState.copy(
          thermalGridState = gridState,
          thermalDemands = onlyThermalDemandOfHeatStorage,
        )
        val externalQDot = testGridQDotInfeed * 10

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleFeedIn(
            state,
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
            innerTemperature should approximate(Celsius(19d))
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
          StorageFull(27600L)
        )
      }
    }
  }
}
