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
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.WattsPerKelvin
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.Kelvin
import squants.energy.{Kilowatts, Watts}
import squants.thermal.Celsius

class HpModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with HpModelTestData {

  private implicit val powerTolerance: squants.Power = Watts(0.1)
  private implicit val temperatureTolerance: squants.Temperature = Kelvin(1e-3)

  "Testing the heat pump model" when {
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
              Kilowatts(0.0),
              Kilowatts(0.0),
              thermalState(17),
              None
            ),
            7200,
            true,
            95,
            15.6,
            Some(HouseTemperatureUpperBoundaryReached(31711L))
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Kilowatts(0.0),
              Kilowatts(0.0),
              thermalState(18),
              None
            ),
            7200,
            true,
            95,
            16.4,
            Some(HouseTemperatureUpperBoundaryReached(30642L))
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Kilowatts(0.0),
              Kilowatts(0.0),
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
              Kilowatts(0.0),
              Kilowatts(0.0),
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
              Kilowatts(0.0),
              Kilowatts(0.0),
              thermalState(23),
              None
            ),
            7200,
            false,
            0,
            20.4,
            Some(HouseTemperatureLowerBoundaryReached(15508L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Kilowatts(95.0),
              Kilowatts(80.0),
              thermalState(17),
              None
            ),
            7200,
            true,
            95,
            15.6,
            Some(HouseTemperatureUpperBoundaryReached(31711L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Kilowatts(95.0),
              Kilowatts(80.0),
              thermalState(18),
              None
            ),
            7200,
            true,
            95,
            16.4,
            Some(HouseTemperatureUpperBoundaryReached(30642L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Kilowatts(95.0),
              Kilowatts(80.0),
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
              Kilowatts(95.0),
              Kilowatts(80.0),
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
              Kilowatts(95.0),
              Kilowatts(80.0),
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
                (activePower ~= Kilowatts(expectedActivePower)) shouldBe true

                (thermalHouseState.innerTemperature ~= Celsius(
                  expectedInnerTemperature
                )) shouldBe true

                maybeThreshold shouldBe expectedNextThreshold
            }
        }
      }
    }

    "determining the flexibility options" when {
      "the house is heated up and storage has space" should {
        "deliver positive flexibility" in {
          val house = thermalHouse(18, 22)
            .copy(ethLosses = WattsPerKelvin(200))
          val grid = thermalGrid(house, Some(thermalStorage))
          val hp = hpModel(grid)
          val relevantData = hpData.copy(currentTimeTick =
            2763L
          ) // Tick, at which the house is heated up
          val lastState = HpState(
            isRunning = true,
            0,
            hpData.ambientTemperature,
            Kilowatts(95.0),
            Kilowatts(80.0),
            thermalState(21, 80, 20, 0),
            Some(HouseTemperatureUpperBoundaryReached(7995L))
          )

          hp.determineFlexOptions(relevantData, lastState) match {
            case ProvideMinMaxFlexOptions(
                  modelUuid,
                  referencePower,
                  minPower,
                  maxPower
                ) =>
              modelUuid shouldBe hp.uuid
              (referencePower ~= Kilowatts(95.0)) shouldBe true
              (minPower ~= Kilowatts(0.0)) shouldBe true
              (maxPower ~= Kilowatts(95.0)) shouldBe true
          }
        }
      }
    }
  }
}
