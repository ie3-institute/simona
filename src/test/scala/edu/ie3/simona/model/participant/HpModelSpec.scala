/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.model.participant.HpModel.HpState
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
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
            "expectedInnerTemperature"
          ),
          (
            HpState(
              isRunning = false,
              0,
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              thermalState(17),
              None
            ),
            7200,
            true,
            95,
            15.6
          ),
          (
            HpState(
              isRunning = false,
              0,
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              thermalState(18),
              None
            ),
            7200,
            true,
            95,
            16.4
          ),
          (
            HpState(
              isRunning = false,
              0,
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              thermalState(20),
              None
            ),
            7200,
            true,
            95,
            18.0
          ),
          (
            HpState(
              isRunning = false,
              0,
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              thermalState(22),
              None
            ),
            7200,
            false,
            0,
            19.6
          ),
          (
            HpState(
              isRunning = false,
              0,
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(0, PowerSystemUnits.KILOWATT),
              thermalState(23),
              None
            ),
            7200,
            false,
            0,
            20.4
          ),
          (
            HpState(
              isRunning = true,
              0,
              Quantities.getQuantity(95, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(80, PowerSystemUnits.KILOWATT),
              thermalState(17),
              None
            ),
            7200,
            true,
            95,
            15.6
          ),
          (
            HpState(
              isRunning = true,
              0,
              Quantities.getQuantity(95, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(80, PowerSystemUnits.KILOWATT),
              thermalState(18),
              None
            ),
            7200,
            true,
            95,
            16.4
          ),
          (
            HpState(
              isRunning = true,
              0,
              Quantities.getQuantity(95, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(80, PowerSystemUnits.KILOWATT),
              thermalState(20),
              None
            ),
            7200,
            true,
            95,
            18.0
          ),
          (
            HpState(
              isRunning = true,
              0,
              Quantities.getQuantity(95, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(80, PowerSystemUnits.KILOWATT),
              thermalState(22),
              None
            ),
            7200,
            true,
            95,
            19.6
          ),
          (
            HpState(
              isRunning = true,
              0,
              Quantities.getQuantity(95, PowerSystemUnits.KILOWATT),
              Quantities.getQuantity(80, PowerSystemUnits.KILOWATT),
              thermalState(25),
              None
            ),
            7200,
            false,
            0,
            22.0
          )
        )

        forAll(cases) {
          (
              state,
              expectedTick,
              expectedRunningState,
              expectedActivePower,
              expectedInnerTemperature
          ) =>
            val data = hpData
            val house = thermalHouse(18, 22)
            val grid = thermalGrid(house)
            val hp = hpModel(grid)

            hp.calculateNextState(state, data) match {
              case HpState(
                    isRunning,
                    lastTimeTick,
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

                maybeThreshold shouldBe None
            }
        }
      }
    }
  }
}
