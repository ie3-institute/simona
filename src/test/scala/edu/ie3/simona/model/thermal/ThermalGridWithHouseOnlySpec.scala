/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.model.participant.HpModel.{HpRelevantData, HpState}
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureTargetOrUpperBoundaryReached,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
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
        val relevantData = HpRelevantData(
          10800, // after three hours
          testGridAmbientTemperature,
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

        val (thermalDemands, updatedThermalGridState) =
          thermalGrid.energyDemandAndUpdatedState(
            relevantData,
            lastHpState,
          )

        val houseDemand = thermalDemands.houseDemand
        val storageDemand = thermalDemands.heatStorageDemand

        houseDemand.required should approximate(zeroKWh)
        houseDemand.possible should approximate(KilowattHours(1.050097))
        storageDemand.required should approximate(zeroKWh)
        storageDemand.possible should approximate(zeroKWh)
        updatedThermalGridState.houseState shouldBe Some(
          ThermalHouseState(10800, Kelvin(292.0799935185185), zeroKW)
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
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
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
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
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
                Some(ThermalHouseState(tick, innerTemperature, qDot)),
                None,
              ) =>
            tick shouldBe 0L
            innerTemperature should approximate(Celsius(18.9999d))
            qDot should approximate(zeroKW)
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
        val relevantData = HpRelevantData(
          0L,
          testGridAmbientTemperature,
        )
        val gridState = ThermalGridState(
          Some(ThermalHouseState(-1, Celsius(17), zeroKW)),
          None,
        )

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleInfeed(
            relevantData,
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
            innerTemperature should approximate(Celsius(16.9999d))
            qDot should approximate(testGridQDotInfeed)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureTargetOrUpperBoundaryReached(7322L)
        )
      }
    }

    "updating the grid state dependent on the given thermal infeed" should {
      val relevantData = HpRelevantData(0, testGridAmbientTemperature)
      "deliver proper result, if energy is fed into the grid" in {
        val gridState = ThermalGridState(
          Some(ThermalHouseState(-1, Celsius(17), zeroKW)),
          None,
        )

        thermalGrid.updateState(
          relevantData,
          gridState,
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
                Some(
                  HouseTemperatureTargetOrUpperBoundaryReached(thresholdTick)
                ),
              ) =>
            tick shouldBe 0L
            innerTemperature should approximate(Celsius(16.9999d))
            qDot should approximate(testGridQDotInfeed)
            thresholdTick shouldBe 7322L
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is consumed from the grid" in {
        thermalGrid.updateState(
          relevantData,
          ThermalGrid.startingState(thermalGrid),
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
            qDot should approximate(zeroKW)
            thresholdTick shouldBe 154285L
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is neither consumed from nor fed into the grid" in {
        thermalGrid.updateState(
          relevantData,
          ThermalGrid.startingState(thermalGrid),
          testGridAmbientTemperature,
          isNotRunning,
          zeroKW,
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
            qDot should approximate(zeroKW)
            thresholdTick shouldBe 154285L
          case _ => fail("Thermal grid state updated failed")
        }
      }
    }
  }
}
