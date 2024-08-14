/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.model.participant.HpModel.HpState
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureUpperBoundaryReached,
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.HpInputTestData
import edu.ie3.util.scala.quantities.WattsPerKelvin
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{KilowattHours, Kilowatts, Watts}
import squants.thermal.Celsius
import squants.{Kelvin, Power, Temperature}

class HpModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with HpInputTestData {

  implicit val tempTolerance: Temperature = Kelvin(1e-3)
  implicit val powerTolerance: Power = Watts(1e-3)

  "Testing the heat pump model" when {
    "calculating the next state with different states" should {
      "deliver correct tick, power and running state" in {
        val cases = Table(
          (
            "state",
            "expectedRunningState",
            "expectedActivePower",
            "expectedInnerTemperature",
            "expectedNextThreshold",
          ),
          (
            HpState(
              isRunning = false,
              0,
              Some(hpData.ambientTemperature),
              Kilowatts(0d),
              Kilowatts(0d),
              thermalState(Celsius(17d)),
              None,
            ),
            true,
            95,
            15.6,
            Some(HouseTemperatureUpperBoundaryReached(31711L)),
          ),
          (
            HpState(
              isRunning = false,
              0,
              Some(hpData.ambientTemperature),
              Kilowatts(0d),
              Kilowatts(0d),
              thermalState(Celsius(18)),
              None,
            ),
            true,
            95,
            16.4,
            Some(HouseTemperatureUpperBoundaryReached(30642L)),
          ),
          (
            HpState(
              isRunning = false,
              0,
              Some(hpData.ambientTemperature),
              Kilowatts(0d),
              Kilowatts(0d),
              thermalState(Celsius(20)),
              None,
            ),
            true,
            95,
            18.0,
            Some(HouseTemperatureUpperBoundaryReached(27771L)),
          ),
          (
            HpState(
              isRunning = false,
              0,
              Some(hpData.ambientTemperature),
              Kilowatts(0d),
              Kilowatts(0d),
              thermalState(Celsius(22)),
              None,
            ),
            false,
            0,
            19.6,
            Some(HouseTemperatureLowerBoundaryReached(13200L)),
          ),
          (
            HpState(
              isRunning = false,
              0,
              Some(hpData.ambientTemperature),
              Kilowatts(0d),
              Kilowatts(0d),
              thermalState(Celsius(23)),
              None,
            ),
            false,
            0,
            20.4,
            Some(HouseTemperatureLowerBoundaryReached(15508L)),
          ),
          (
            HpState(
              isRunning = true,
              0,
              Some(hpData.ambientTemperature),
              Kilowatts(95d),
              Kilowatts(80d),
              thermalState(Celsius(17)),
              None,
            ),
            true,
            95,
            15.6,
            Some(HouseTemperatureUpperBoundaryReached(31711L)),
          ),
          (
            HpState(
              isRunning = true,
              0,
              Some(hpData.ambientTemperature),
              Kilowatts(95d),
              Kilowatts(80d),
              thermalState(Celsius(18)),
              None,
            ),
            true,
            95,
            16.4,
            Some(HouseTemperatureUpperBoundaryReached(30642L)),
          ),
          (
            HpState(
              isRunning = true,
              0,
              Some(hpData.ambientTemperature),
              Kilowatts(95d),
              Kilowatts(80d),
              thermalState(Celsius(20)),
              None,
            ),
            true,
            95,
            18.0,
            Some(HouseTemperatureUpperBoundaryReached(27771L)),
          ),
          (
            HpState(
              isRunning = true,
              0,
              Some(hpData.ambientTemperature),
              Kilowatts(95d),
              Kilowatts(80d),
              thermalState(Celsius(22)),
              None,
            ),
            true,
            95,
            19.6,
            Some(HouseTemperatureUpperBoundaryReached(23200L)),
          ),
          (
            HpState(
              isRunning = true,
              0,
              Some(hpData.ambientTemperature),
              Kilowatts(95d),
              Kilowatts(80d),
              thermalState(Celsius(25)),
              None,
            ),
            false,
            0,
            22.0,
            Some(HouseTemperatureLowerBoundaryReached(19200L)),
          ),
        )

        forAll(cases) {
          (
              state,
              expectedRunningState,
              expectedActivePower,
              expectedInnerTemperature,
              expectedNextThreshold,
          ) =>
            val data = hpData
            val house = thermalHouse(18, 22)
            val grid = thermalGrid(house, None, Some(domesticHotWaterStorage))
            val hp = hpModel(grid)

            hp.determineState(state, data) match {
              case HpState(
                    isRunning,
                    _,
                    _,
                    activePower,
                    _,
                    ThermalGridState(Some(thermalHouseState), _, _),
                    maybeThreshold,
                  ) =>
                isRunning shouldBe expectedRunningState
                activePower should approximate(Kilowatts(expectedActivePower))
                thermalHouseState.innerTemperature should approximate(
                  Celsius(
                    expectedInnerTemperature
                  )
                )
                maybeThreshold shouldBe expectedNextThreshold
              case unexpected =>
                fail(s"Expected a hp state but got none $unexpected.")
            }
        }
      }
    }

    "determining the flexibility options" when {
      "the house is heated up and storage has space" should {
        "deliver positive flexibility" in {
          val house = thermalHouse(18, 22)
            .copy(ethLosses = WattsPerKelvin(200))
          val grid = thermalGrid(house, Some(thermalStorage), None)
          val hp = hpModel(grid)
          // Tick, at which the house is heated up
          val relevantData = hpData.copy(currentTick = 2763L)
          val thermalState = ThermalGridState(
            Some(
              ThermalHouseState(
                0L,
                Celsius(21),
                Kilowatts(80),
              )
            ),
            Some(
              ThermalStorageState(
                0L,
                KilowattHours(250),
                Kilowatts(0),
              )
            ),
            None,
          )
          val lastState = HpState(
            isRunning = true,
            0,
            Some(hpData.ambientTemperature),
            Kilowatts(95.0),
            Kilowatts(80.0),
            thermalState,
            Some(HouseTemperatureUpperBoundaryReached(7995L)),
          )

          hp.determineFlexOptions(relevantData, lastState) match {
            case ProvideMinMaxFlexOptions(
                  modelUuid,
                  referencePower,
                  minPower,
                  maxPower,
                ) =>
              modelUuid shouldBe hp.uuid
              referencePower should approximate(Kilowatts(95.0))
              minPower should approximate(Kilowatts(0.0))
              maxPower should approximate(Kilowatts(95.0))
          }
        }
      }
    }
  }
}
