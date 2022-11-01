/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.model.participant.HpModel.HpState
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureUpperBoundaryReached
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

class HpModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with HpModelTestData {
  "Testing the heat pump model" when {
    val testingTolerance = 0.0001

    "calculating the next state with different states" should {
      "deliver correct tick, power and running state" in {
        val cases = Table(
          (
            "state",
            "expectedTick",
            "expectedRunningState",
            "expectedActivePower",
            "expectedInnerTemperature",
            "expectedNextThreshold"
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              thermalState(17),
              None
            ),
            7200,
            true,
            95,
            15.6,
            Some(HouseTemperatureUpperBoundaryReached(31710L))
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              thermalState(18),
              None
            ),
            7200,
            true,
            95,
            16.4,
            Some(HouseTemperatureUpperBoundaryReached(30641L))
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              thermalState(20),
              None
            ),
            7200,
            true,
            95,
            18.0,
            Some(HouseTemperatureUpperBoundaryReached(27771L))
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              thermalState(22),
              None
            ),
            7200,
            false,
            0,
            19.6,
            Some(HouseTemperatureLowerBoundaryReached(13200L))
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              thermalState(23),
              None
            ),
            7200,
            false,
            0,
            20.4,
            Some(HouseTemperatureLowerBoundaryReached(15507L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Quantities.getQuantity(95, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(80, PowerSystemUnits.KILOWATT),
              thermalState(17),
              None
            ),
            7200,
            true,
            95,
            15.6,
            Some(HouseTemperatureUpperBoundaryReached(31710L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Quantities.getQuantity(95, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(80, PowerSystemUnits.KILOWATT),
              thermalState(18),
              None
            ),
            7200,
            true,
            95,
            16.4,
            Some(HouseTemperatureUpperBoundaryReached(30641L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Quantities.getQuantity(95, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(80, PowerSystemUnits.KILOWATT),
              thermalState(20),
              None
            ),
            7200,
            true,
            95,
            18.0,
            Some(HouseTemperatureUpperBoundaryReached(27771L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Quantities.getQuantity(95, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(80, PowerSystemUnits.KILOWATT),
              thermalState(22),
              None
            ),
            7200,
            true,
            95,
            19.6,
            Some(HouseTemperatureUpperBoundaryReached(23200L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Quantities.getQuantity(95, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(80, PowerSystemUnits.KILOWATT),
              thermalState(25),
              None
            ),
            7200,
            false,
            0,
            22.0,
            Some(HouseTemperatureLowerBoundaryReached(19200L))
          )
        )

        forAll(cases) {
          (
              state,
              expectedTick,
              expectedRunningState,
              expectedActivePower,
              expectedInnerTemperature,
              expectedNextThreshold
          ) =>
            val data = hpData
            val house = thermalHouse(18, 22)
            val grid = thermalGrid(house)
            val hp = hpModel(grid)

            hp.calculateNextState(state, data) match {
              case HpState(
                    isRunning,
                    lastTimeTick,
                    _,
                    activePower,
                    _,
                    ThermalGridState(Some(thermalHouseState), _),
                    maybeThreshold
                  ) =>
                isRunning shouldBe expectedRunningState
                lastTimeTick shouldBe expectedTick
                activePower should equalWithTolerance(
                  Quantities.getQuantity(
                    expectedActivePower,
                    PowerSystemUnits.KILOWATT
                  )
                )

                thermalHouseState.innerTemperature should equalWithTolerance(
                  Quantities.getQuantity(
                    expectedInnerTemperature,
                    Units.CELSIUS
                  )
                )

                maybeThreshold shouldBe expectedNextThreshold
            }
        }
      }
    }
  }
}
