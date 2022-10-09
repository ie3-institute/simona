/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.ThermalStorageInput
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridThreshold.{
  ThermalGridEmpty,
  ThermalGridFilledUp
}
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalGridState,
  ThermalGridThreshold
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.test.common.UnitSpec
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import scala.jdk.CollectionConverters._

class ThermalGridWithHouseOnlySpec extends UnitSpec with ThermalHouseTestData {
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

    "requesting the starting state" should {
      "deliver proper results" in {
        ThermalGrid.startingState(thermalGrid) match {
          case ThermalGridState(
                Some(ThermalHouseState(tick, innerTemperature, thermalInfeed)),
                None
              ) =>
            tick shouldBe expectedHouseStartingState.tick
            innerTemperature should equalWithTolerance(
              expectedHouseStartingState.innerTemperature
            )
            thermalInfeed should equalWithTolerance(
              expectedHouseStartingState.qDot
            )
          case _ => fail("Determination of starting state failed")
        }
      }
    }

    "trying to push to storage" should {
      "return empty result" in {
        val takeOrPushToStorage = PrivateMethod[Option[
          (ThermalStorageState, Option[ThermalStorage.ThermalStorageThreshold])
        ]](Symbol("takeOrPushToStorage"))

        thermalGrid invokePrivate takeOrPushToStorage(
          42L,
          ThermalGrid.startingState(thermalGrid).storageState,
          Quantities.getQuantity(12d, StandardUnits.ACTIVE_POWER_RESULT)
        ) shouldBe None
      }
    }

    "determining the energy demand" should {
      "exactly be the demand of the house" in {
        val tick = 10800 // after three house
        val ambientTemperature = Quantities.getQuantity(12d, Units.CELSIUS)
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

        gridDemand.required should equalWithTolerance(houseDemand.required)
        gridDemand.possible should equalWithTolerance(houseDemand.possible)
      }
    }

    "handling thermal energy consumption from grid" should {
      val handleConsumption =
        PrivateMethod[(ThermalGridState, Option[ThermalGridThreshold])](
          Symbol("handleConsumption")
        )

      "deliver the house state by just letting it cool down, if just no infeed is given" in {
        val tick = 0L
        val gridState = ThermalGrid.startingState(thermalGrid)
        val ambientTemperature = Quantities.getQuantity(12d, Units.CELSIUS)
        val externalQDot =
          Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)

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
            innerTemperature should equalWithTolerance(
              Quantities.getQuantity(18.9999d, Units.CELSIUS),
              1e-3
            )
            qDot should equalWithTolerance(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(ThermalGridEmpty(154284L))
      }

      "not withdraw energy from the house, if actual consumption is given" in {
        val tick = 0L // after three house
        val gridState = ThermalGrid.startingState(thermalGrid)
        val ambientTemperature = Quantities.getQuantity(12d, Units.CELSIUS)
        val externalQDot =
          Quantities.getQuantity(-42d, StandardUnits.ACTIVE_POWER_RESULT)

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
            innerTemperature should equalWithTolerance(
              Quantities.getQuantity(18.9999d, Units.CELSIUS),
              1e-3
            )
            qDot should equalWithTolerance(
              Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
            )
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(ThermalGridEmpty(154284L))
      }
    }

    "handling thermal infeed into the grid" should {
      "solely heat up the house" in {
        val tick = 0L
        val gridState = ThermalGrid.startingState(thermalGrid)
        val ambientTemperature = Quantities.getQuantity(12d, Units.CELSIUS)
        val externalQDot =
          Quantities.getQuantity(15d, StandardUnits.ACTIVE_POWER_IN)

        val (updatedGridState, reachedThreshold) =
          thermalGrid.handleInfeed(
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
            innerTemperature should equalWithTolerance(
              Quantities.getQuantity(18.9999d, Units.CELSIUS),
              1e-3
            )
            qDot should equalWithTolerance(externalQDot)
          case _ => fail("Thermal grid state has been calculated wrong.")
        }
        reachedThreshold shouldBe Some(ThermalGridFilledUp(7372L))
      }
    }
  }
}
