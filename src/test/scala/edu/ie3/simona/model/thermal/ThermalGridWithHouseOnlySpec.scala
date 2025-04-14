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
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
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
        Set.empty[ThermalStorageInput].asJava,
      )
    )
    val initialGridState: ThermalGridState =
      ThermalGrid.startingState(thermalGrid, testGridAmbientTemperature)

    val initialHpState = HpState(
      -1L,
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
                    tick,
                    _,
                    innerTemperature,
                  )
                ),
                None,
              ) =>
            tick shouldBe expectedHouseStartingState.tick
            innerTemperature should approximate(
              expectedHouseStartingState.innerTemperature
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
            Kelvin(292.08),
          )
        )
        updatedThermalGridState.storageState shouldBe None

      }
    }

    "determining the energy demand" should {
      "exactly be the demand of the house" in {
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
        houseDemand.possible should approximate(KilowattHours(1.0499999999))
        storageDemand.required should approximate(zeroKWh)
        storageDemand.possible should approximate(zeroKWh)
      }
    }

    "handling thermal energy consumption from grid" should {

      "deliver the house state by just letting it cool down, if just no feed in is given" in {
        val (thermalGridOperatingPoint, reachedThreshold) =
          thermalGrid.handleConsumption(initialHpState)

        reachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154285L)
        )
        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint.zero
      }
    }

    "handling thermal feed in into the grid" should {
      "solely heat up the house" in {
        val gridState = ThermalGridState(
          Some(
            ThermalHouseState(
              -1,
              testGridAmbientTemperature,
              Celsius(17),
            )
          ),
          None,
        )

        val state = HpState(
          0,
          gridState,
          HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
          onlyThermalDemandOfHouse,
        )

        val (thermalGridOperatingPoint, reachedThreshold) =
          thermalGrid.handleFeedIn(
            state,
            testGridQDotInfeed,
          )

        reachedThreshold shouldBe Some(HouseTargetTemperatureReached(7321L))
        thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
          testGridQDotInfeed,
          testGridQDotInfeed,
          zeroKW,
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
              Celsius(17),
            )
          ),
          None,
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
                Some(HouseTargetTemperatureReached(thresholdTick)),
              ) =>
            thresholdTick shouldBe 7321L
            thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint(
              testGridQDotInfeed,
              testGridQDotInfeed,
              zeroKW,
            )
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is consumed from the grid" in {
        thermalGrid.handleConsumption(initialHpState) match {
          case (
                thermalGridOperatingPoint,
                Some(HouseTemperatureLowerBoundaryReached(thresholdTick)),
              ) =>
            thresholdTick shouldBe 154285L
            thermalGridOperatingPoint shouldBe ThermalGridOperatingPoint.zero
          case _ => fail("Thermal grid state updated failed")
        }
      }
    }
  }
}
