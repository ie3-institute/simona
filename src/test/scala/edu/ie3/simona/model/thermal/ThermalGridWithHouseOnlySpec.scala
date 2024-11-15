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
import edu.ie3.simona.test.common.UnitSpec
import squants.energy._
import squants.thermal.Celsius
import squants.{Energy, Kelvin, Power, Temperature}

import scala.jdk.CollectionConverters._

class ThermalGridWithHouseOnlySpec extends UnitSpec with ThermalHouseTestData {

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
        )

      ThermalGrid(thermalGridInput) match {
        case ThermalGrid(Some(thermalHouseGenerated), None) =>
          thermalHouseGenerated shouldBe thermalHouse
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
      )
    )

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                Some(ThermalHouseState(tick, innerTemperature, thermalInfeed)),
                None,
              ) =>
            tick shouldBe expectedHouseStartingState.tick
            innerTemperature should approximate(
              expectedHouseStartingState.innerTemperature
            )
            thermalInfeed should approximate(expectedHouseStartingState.qDot)

          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "determining the energy demand" should {
      "exactly be the demand of the house" in {
        val tick = 10800 // after three hours
        val expectedHouseDemand = thermalHouse.energyDemand(
          tick,
          testGridAmbientTemperature,
          expectedHouseStartingState,
        )

        val (houseDemand, storageDemand, updatedThermalGridState) =
          thermalGrid.energyDemandAndUpdatedState(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            ThermalGrid.startingState(thermalGrid),
          )

        houseDemand.required should approximate(expectedHouseDemand.required)
        houseDemand.possible should approximate(expectedHouseDemand.possible)
        storageDemand.required should approximate(KilowattHours(0d))
        storageDemand.possible should approximate(KilowattHours(0d))
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Kelvin(292.0799935185185), Kilowatts(0d))
        )
        updatedThermalGridState.storageState shouldBe None
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
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(tick, innerTemperature, qDot)),
                None,
              ) =>
            tick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDot should approximate(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154285L)
        )
      }

      "not withdraw energy from the house, if actual consumption is given" in {
        val tick = 0L // after three hours
        val gridState = ThermalGrid.startingState(thermalGrid)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            testGridAmbientTemperature,
            testGridAmbientTemperature,
            gridState,
            testGridQDotConsumption,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(tick, innerTemperature, qDot)),
                None,
              ) =>
            tick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDot should approximate(Megawatts(0d))
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154285L)
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
            isNotRunning,
            testGridQDotInfeed,
            onlyThermalDemandOfHouse,
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(tick, innerTemperature, qDot)),
                None,
              ) =>
            tick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDot should approximate(testGridQDotInfeed)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureUpperBoundaryReached(7372L)
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
          onlyThermalDemandOfHouse,
        ) match {
          case (
                ThermalGridState(
                  Some(ThermalHouseState(tick, innerTemperature, qDot)),
                  None,
                ),
                Some(HouseTemperatureUpperBoundaryReached(thresholdTick)),
              ) =>
            tick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDot should approximate(testGridQDotInfeed)
            thresholdTick shouldBe 7372L
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is consumed from the grid" in {
        thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          testGridAmbientTemperature,
          isNotRunning,
          testGridQDotConsumption,
          onlyThermalDemandOfHouse,
        ) match {
          case (
                ThermalGridState(
                  Some(ThermalHouseState(tick, innerTemperature, qDot)),
                  None,
                ),
                Some(HouseTemperatureLowerBoundaryReached(thresholdTick)),
              ) =>
            tick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDot should approximate(Megawatts(0d))
            thresholdTick shouldBe 154285L
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is neither consumed from nor fed into the grid" in {
        thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          testGridAmbientTemperature,
          isNotRunning,
          Megawatts(0d),
          onlyThermalDemandOfHouse,
        ) match {
          case (
                ThermalGridState(
                  Some(ThermalHouseState(tick, innerTemperature, qDot)),
                  None,
                ),
                Some(HouseTemperatureLowerBoundaryReached(thresholdTick)),
              ) =>
            tick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDot should approximate(Kilowatts(0d))
            thresholdTick shouldBe 154285L
          case _ => fail("Thermal grid state updated failed")
        }
      }
    }
  }
}
