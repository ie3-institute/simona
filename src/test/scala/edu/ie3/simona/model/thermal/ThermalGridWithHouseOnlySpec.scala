/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{HouseTemperatureLowerBoundaryReached, HouseTemperatureUpperBoundaryReached}
import edu.ie3.simona.test.common.UnitSpec
import squants.energy.{Kilowatts, Megawatts, WattHours, Watts}
import squants.thermal.Celsius
import squants.{Energy, Power, Temperature}

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
          Set.empty[ThermalStorageInput].asJava
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
        Set.empty[ThermalStorageInput].asJava
      )
    )
    val ambientTemperature = Celsius(12d)
    val qDotInfeed = Kilowatts(15d)
    val qDotConsumption = Kilowatts(-42d)

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                Some(ThermalHouseState(tick, innerTemperature, thermalInfeed)),
                None
              ) =>
            tick shouldBe expectedHouseStartingState.tick
            (innerTemperature =~ expectedHouseStartingState.innerTemperature) shouldBe true
            (thermalInfeed =~ expectedHouseStartingState.qDot) shouldBe true

          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "determining the energy demand" should {
      "exactly be the demand of the house" in {
        val tick = 10800 // after three house
        val houseDemand = thermalHouse.energyDemand(
          tick,
          ambientTemperature,
          expectedHouseStartingState
        )

        val gridDemand = thermalGrid.energyDemand(
          tick,
          ambientTemperature,
          ThermalGrid.startingState(thermalGrid)
        )

        (gridDemand.required =~ houseDemand.required) shouldBe true
        (gridDemand.possible =~ houseDemand.possible) shouldBe true
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
            ambientTemperature,
            gridState,
            externalQDot
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(tick, innerTemperature, qDot)),
                None
              ) =>
            tick shouldBe 0L
            (innerTemperature =~ Celsius(18.9999d)) shouldBe true
            (qDot =~ externalQDot) shouldBe true
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154284L)
        )
      }

      "not withdraw energy from the house, if actual consumption is given" in {
        val tick = 0L // after three house
        val gridState = ThermalGrid.startingState(thermalGrid)

        val (updatedGridState, reachedThreshold) =
          thermalGrid invokePrivate handleConsumption(
            tick,
            ambientTemperature,
            gridState,
            qDotConsumption
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(tick, innerTemperature, qDot)),
                None
              ) =>
            tick shouldBe 0L
            (innerTemperature =~ Celsius(18.9999d)) shouldBe true
            (qDot =~ Megawatts(0d)) shouldBe true
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(
          HouseTemperatureLowerBoundaryReached(154284L)
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
            ambientTemperature,
            gridState,
            qDotInfeed
          )

        updatedGridState match {
          case ThermalGridState(
                Some(ThermalHouseState(tick, innerTemperature, qDot)),
                None
              ) =>
            tick shouldBe 0L
            (innerTemperature =~ Celsius(18.9999d)) shouldBe true
            (qDot =~ qDotInfeed) shouldBe true
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
          ambientTemperature,
          qDotInfeed
        ) match {
          case (
                ThermalGridState(
                  Some(ThermalHouseState(tick, innerTemperature, qDot)),
                  None
                ),
                Some(HouseTemperatureUpperBoundaryReached(thresholdTick))
              ) =>
            tick shouldBe 0L
            (innerTemperature =~ Celsius(18.9999d)) shouldBe true
            (qDot =~ qDotInfeed) shouldBe true
            thresholdTick shouldBe 7372L
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is consumed from the grid" in {
        thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          ambientTemperature,
          qDotConsumption
        ) match {
          case (
                ThermalGridState(
                  Some(ThermalHouseState(tick, innerTemperature, qDot)),
                  None
                ),
                Some(HouseTemperatureLowerBoundaryReached(thresholdTick))
              ) =>
            tick shouldBe 0L
            (innerTemperature =~ Celsius(18.9999d)) shouldBe true
            (qDot =~ Megawatts(0d)) shouldBe true
            thresholdTick shouldBe 154284L
          case _ => fail("Thermal grid state updated failed")
        }
      }

      "deliver proper result, if energy is neither consumed from nor fed into the grid" in {
        thermalGrid.updateState(
          0L,
          ThermalGrid.startingState(thermalGrid),
          ambientTemperature,
          Megawatts(0d)
        ) match {
          case (
                ThermalGridState(
                  Some(ThermalHouseState(tick, innerTemperature, qDot)),
                  None
                ),
                Some(HouseTemperatureLowerBoundaryReached(thresholdTick))
              ) =>
            tick shouldBe 0L
            (innerTemperature =~ Celsius(18.9999d)) shouldBe true
            (qDot =~ Kilowatts(0d)) shouldBe true
            thresholdTick shouldBe 154284L
          case _ => fail("Thermal grid state updated failed")
        }
      }
    }
  }
}
