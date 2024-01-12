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
  HouseTemperatureUpperBoundaryReached
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull
}
import edu.ie3.simona.test.common.UnitSpec
import squants.energy._
import squants.thermal.Celsius
import squants.{Energy, Power, Temperature}
import tech.units.indriya.unit.Units

import scala.jdk.CollectionConverters._

class ThermalGridWithHouseAndStorageSpec
    extends UnitSpec
    with ThermalHouseTestData
    with ThermalStorageTestData {

  implicit val tempTolerance: Temperature = Celsius(1e-3)
  implicit val powerTolerance: Power = Watts(1e-3)
  implicit val energyTolerance: Energy = WattHours(1e-3)

  "Testing thermal grid generation with only a house" should {
    "instantiating correctly from input data" in {
      val thermalGridInput =
        new edu.ie3.datamodel.models.input.container.ThermalGrid(
          thermalBusInput,
          Set(thermalHouseInput).asJava,
          Set[ThermalStorageInput](thermalStorageInput).asJava
        )

      ThermalGrid(thermalGridInput) match {
        case ThermalGrid(
              Some(thermalHouseGenerated),
              Some(thermalStorageGenerated)
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
        Set[ThermalStorageInput](thermalStorageInput).asJava
      )
    )
    val ambientTemperature = Celsius(12d)
    val qDotInfeed = Kilowatts(15d)
    val qDotConsumption = Kilowatts(-42d)

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                )
              ) =>
            houseTick shouldBe expectedHouseStartingState.tick
            storageTick shouldBe expectedHouseStartingState.tick
            (innerTemperature =~ expectedHouseStartingState.innerTemperature) shouldBe true
            (storedEnergy =~ expectedStorageStartingState.storedEnergy) shouldBe true
            (qDotHouse =~ expectedHouseStartingState.qDot) shouldBe true
            (qDotStorage =~ expectedStorageStartingState.qDot) shouldBe true

          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "determining the energy demand" should {
      "deliver the house demand (no demand) with added flexibility by storage" in {
        val tick = 10800 // after three hours

        val gridDemand = thermalGrid.energyDemand(
          tick,
          testGridambientTemperature,
          ThermalGrid.startingState(thermalGrid)
        )

        (gridDemand.required ~= MegawattHours(0.0)) shouldBe true
        (gridDemand.possible ~= MegawattHours(0.031050 + 0.920)) shouldBe true
      }

      "consider stored energy to reduce house demand" in {
        val tick = 10800 // after three hours

        val startingState = ThermalGrid.startingState(thermalGrid)
        val gridDemand = thermalGrid.energyDemand(
          tick,
          testGridambientTemperature,
          startingState.copy(houseState =
            startingState.houseState.map(
              _.copy(innerTemperature = Celsius(16d))
            )
          )
        )

        (gridDemand.required ~= MegawattHours(0.0)) shouldBe true
        (gridDemand.possible ~= MegawattHours(1.0412001)) shouldBe true
      }

      "consider stored energy to reduce house demand if stored energy is not enough" in {
        val tick = 10800 // after three hours

        val startingState = ThermalGrid.startingState(thermalGrid)
        val gridDemand = thermalGrid.energyDemand(
          tick,
          testGridambientTemperature,
          startingState.copy(houseState =
            startingState.houseState.map(
              _.copy(innerTemperature = Celsius(3d))
            )
          )
        )

        (gridDemand.required ~= MegawattHours(0.0086499)) shouldBe true
        (gridDemand.possible ~= MegawattHours(1.4186499)) shouldBe true
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
            testGridambientTemperature,
            gridState,
            externalQDot
          )

        updatedGridState match {
          case ThermalGridState(
                _,
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                )
              ) =>
            storageTick shouldBe 0L
            (storedEnergy ~= initialLoading) shouldBe true
            (qDotStorage ~= externalQDot) shouldBe true
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154285L)
        )
      }

      "take energy from storage, if there is actual consumption" in {
        val tick = 0L
        val initialGridState = ThermalGrid.startingState(thermalGrid)
        val initialLoading = KilowattHours(430d)
        val gridState = initialGridState.copy(storageState =
          initialGridState.storageState.map(storageState =>
            storageState.copy(storedEnergy = initialLoading)
          )
        )
        val externalQDot = testGridQDotConsumption

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            testGridambientTemperature,
            gridState,
            externalQDot
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                )
              ) =>
            houseTick shouldBe 0L
            (innerTemperature =~ Celsius(18.9999d)) shouldBe true
            (qDotHouse =~ Kilowatts(0d)) shouldBe true

            storageTick shouldBe 0L
            (storedEnergy ~= initialLoading) shouldBe true
            (qDotStorage ~= externalQDot) shouldBe true
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(StorageEmpty(17143L))
      }
    }

    "revising infeed from storage to house" should {
      val zeroInflux = Kilowatts(0d)
      val tick = 3600L
      val testGridambientTemperature = Celsius(14d)
      "hand back unaltered information if needed information is missing" in {
        val maybeHouseState = Some(
          (
            ThermalHouseState(
              tick,
              Kelvin(
                thermalHouseInput.getTargetTemperature
                  .to(Units.KELVIN)
                  .getValue
                  .doubleValue
              ),
              zeroInflux
            ),
            None
          )
        )
        val maybeStorageState = None

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          None,
          testGridambientTemperature,
          testGridQDotConsumption
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
              Kelvin(
                thermalHouseInput.getTargetTemperature
                  .to(Units.KELVIN)
                  .getValue
                  .doubleValue
              ),
              zeroInflux
            ),
            None
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              KilowattHours(50d),
              zeroInflux
            ),
            None
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
          testGridambientTemperature,
          zeroInflux
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
              Kelvin(
                thermalHouseInput.getTargetTemperature
                  .to(Units.KELVIN)
                  .getValue
                  .doubleValue
              ),
              qDotInfeed
            ),
            Some(HouseTemperatureUpperBoundaryReached(3600L))
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              KilowattHours(50d),
              zeroInflux
            ),
            None
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
          testGridambientTemperature,
          testGridQDotInfeed
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
              Kelvin(
                thermalHouseInput.getLowerTemperatureLimit
                  .to(Units.KELVIN)
                  .getValue
                  .doubleValue
              ),
              zeroInflux
            ),
            Some(HouseTemperatureLowerBoundaryReached(tick))
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              KilowattHours(50d),
              qDotInfeed
            ),
            Some(StorageEmpty(tick))
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          maybeHouseState.map(_._1),
          maybeStorageState.map(_._1),
          testGridambientTemperature,
          zeroInflux
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
              Kelvin(
                thermalHouseInput.getLowerTemperatureLimit
                  .to(Units.KELVIN)
                  .getValue
                  .doubleValue
              ),
              zeroInflux
            ),
            Some(HouseTemperatureLowerBoundaryReached(tick))
          )
        )
        val maybeStorageState = Some(
          (
            ThermalStorageState(
              tick,
              KilowattHours(250d),
              qDotInfeed
            ),
            None
          )
        )
        val formerHouseState = Some(
          ThermalHouseState(
            0L,
            Kelvin(
              thermalHouseInput.getTargetTemperature
                .to(Units.KELVIN)
                .getValue
                .doubleValue
            ),
            zeroInflux
          )
        )
        val formerStorageState = Some(
          ThermalStorageState(
            0L,
            KilowattHours(300d),
            Kilowatts(-50d)
          )
        )

        thermalGrid.reviseInfeedFromStorage(
          tick,
          maybeHouseState,
          maybeStorageState,
          formerHouseState,
          formerStorageState,
          testGridambientTemperature,
          zeroInflux
        ) match {
          case (
                Some(
                  (
                    ThermalHouseState(houseTick, _, revisedQDotHouse),
                    Some(HouseTemperatureUpperBoundaryReached(houseColdTick))
                  )
                ),
                Some(
                  (
                    ThermalStorageState(storageTick, _, revisedQDotStorage),
                    Some(StorageEmpty(storageEmptyTick))
                  )
                )
              ) =>
            houseTick shouldBe tick
            storageTick shouldBe tick

            (revisedQDotHouse ~= thermalStorage.chargingPower) shouldBe true
            (revisedQDotStorage ~= thermalStorage.chargingPower * -1) shouldBe true

            houseColdTick shouldBe 3718L
            storageEmptyTick shouldBe 3678L
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
            testGridambientTemperature,
            initialGridState,
            externalQDot
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                )
              ) =>
            houseTick shouldBe 0L
            (innerTemperature ~= Celsius(18.9999d)) shouldBe true
            (qDotHouse ~= externalQDot) shouldBe true

            storageTick shouldBe -1L
            (storedEnergy ~= initialGridState.storageState
              .map(_.storedEnergy)
              .getOrElse(fail("No initial storage state found"))) shouldBe true
            (qDotStorage ~= Kilowatts(0d)) shouldBe true
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
            testGridambientTemperature,
            gridState,
            externalQDot
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(houseTick, innerTemperature, qDotHouse)),
                Some(
                  ThermalStorageState(storageTick, storedEnergy, qDotStorage)
                )
              ) =>
            houseTick shouldBe 0L
            (innerTemperature ~= Celsius(20.99999167d)) shouldBe true
            (qDotHouse ~= Megawatts(0.0)) shouldBe true

            storageTick shouldBe 0L
            (storedEnergy ~= initialGridState.storageState
              .map(_.storedEnergy)
              .getOrElse(fail("No initial storage state found"))) shouldBe true
            (qDotStorage ~= externalQDot) shouldBe true
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          StorageFull(220800L)
        )
      }
    }
  }
}
