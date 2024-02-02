/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.model.participant.HpModel.HpState
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.test.common.UnitSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{Kilowatts, Watts}
import squants.thermal.Celsius
import squants.{Power, Temperature}

class HpModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with HpModelTestData {

  implicit val tempTolerance: Temperature = Celsius(1e-3)
  implicit val powerTolerance: Power = Watts(1e-3)

  "Testing the heat pump model" when {

    "calculating the next state with different states" should {
      "deliver correct tick, power and running state" in {
        val cases = Table(
          (
            "state",
            "expectedRunningState",
            "expectedActivePower",
            "expectedInnerTemperature"
          ),
          (
            HpState(
              isRunning = false,
              0,
              Kilowatts(0d),
              Kilowatts(0d),
              thermalState(Celsius(17d))
            ),
            true,
            95,
            15.6
          ),
          (
            HpState(
              isRunning = false,
              0,
              Kilowatts(0d),
              Kilowatts(0d),
              thermalState(Celsius(18))
            ),
            true,
            95,
            16.4
          ),
          (
            HpState(
              isRunning = false,
              0,
              Kilowatts(0d),
              Kilowatts(0d),
              thermalState(Celsius(20))
            ),
            true,
            95,
            18.0
          ),
          (
            HpState(
              isRunning = false,
              0,
              Kilowatts(0d),
              Kilowatts(0d),
              thermalState(Celsius(22))
            ),
            false,
            0,
            19.6
          ),
          (
            HpState(
              isRunning = false,
              0,
              Kilowatts(0d),
              Kilowatts(0d),
              thermalState(Celsius(23))
            ),
            false,
            0,
            20.4
          ),
          (
            HpState(
              isRunning = true,
              0,
              Kilowatts(95d),
              Kilowatts(80d),
              thermalState(Celsius(17))
            ),
            true,
            95,
            15.6
          ),
          (
            HpState(
              isRunning = true,
              0,
              Kilowatts(95d),
              Kilowatts(80d),
              thermalState(Celsius(18))
            ),
            true,
            95,
            16.4
          ),
          (
            HpState(
              isRunning = true,
              0,
              Kilowatts(95d),
              Kilowatts(80d),
              thermalState(Celsius(20))
            ),
            true,
            95,
            18.0
          ),
          (
            HpState(
              isRunning = true,
              0,
              Kilowatts(95d),
              Kilowatts(80d),
              thermalState(Celsius(22))
            ),
            true,
            95,
            19.6
          ),
          (
            HpState(
              isRunning = true,
              0,
              Kilowatts(95d),
              Kilowatts(80d),
              thermalState(Celsius(25))
            ),
            false,
            0,
            22.0
          )
        )

        forAll(cases) {
          (
              state,
              expectedRunningState,
              expectedActivePower,
              expectedInnerTemperature
          ) =>
            val data = hpData(state)
            val house = thermalHouse(18, 22)
            val grid = thermalGrid(house)
            val hp = hpModel(grid)

            hp.calculateNextState(data) match {
              case HpState(
                    isRunning,
                    lastTimeTick,
                    activePower,
                    _,
                    ThermalGridState(Some(thermalHouseState), _)
                  ) =>
                isRunning shouldBe expectedRunningState
                activePower should approximate(Kilowatts(expectedActivePower))
                thermalHouseState.innerTemperature should approximate(
                  Celsius(
                    expectedInnerTemperature
                  )
                )
            }
        }
      }
    }
  }
}
