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
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor3}
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
            val grid = thermalGrid(house)
            val hp = hpModel(grid)

            hp.determineState(state, data) match {
              case HpState(
                    isRunning,
                    _,
                    _,
                    activePower,
                    _,
                    ThermalGridState(Some(thermalHouseState), _),
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

    "determining the flexibility options for different states" should {
      "deliver correct flexibility options" in {
        val testCases
            : TableFor3[ThermalGridState, HpState, (Double, Double, Double)] =
          Table(
            ("thermalState", "lastState", "expectedValues"),
            // House is below lower temperature boundary
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
              ),
              HpState(
                isRunning = false,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(0.0),
                Kilowatts(0.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (95.0, 95.0, 95.0),
            ),
            // House is between target temperature and lower temperature boundary, Hp actually running
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(19), Kilowatts(80))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (95.0, 0.0, 95.0),
            ),

            // House is between target temperature and lower temperature boundary, Hp actually not running
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
              ),
              HpState(
                isRunning = false,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(0.0),
                Kilowatts(0.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (0.0, 0.0, 95.0),
            ),
            // Storage and house have remaining capacity
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(80))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(21), Kilowatts(80))),
                  Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (95.0, 0.0, 95.0),
            ),

            // Storage is full, House has capacity till upper boundary
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(80))),
                Some(ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))),
              ),
              HpState(
                isRunning = false,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(0.0),
                Kilowatts(0.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(21), Kilowatts(80))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (0.0, 0.0, 95.0),
            ),

            // No capacity for flexibility at all
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(22), Kilowatts(80))),
                Some(ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(22), Kilowatts(80))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (0.0, 0.0, 0.0),
            ),

            // No capacity for flexibility at all when storage is full and house has been (externally) heated up above upperTemperatureBoundary
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(25), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))),
              ),
              HpState(
                isRunning = false,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(25), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (0.0, 0.0, 0.0),
            ),
          )

        // Run the test cases
        forAll(testCases) {
          (
              thermalState: ThermalGridState,
              lastState: HpState,
              expectedValues: (Double, Double, Double),
          ) =>
            val (expectedReferencePower, expectedMinPower, expectedMaxPower) =
              expectedValues

            // Initialize the house and grid models
            val house =
              thermalHouse(18, 22).copy(ethLosses = WattsPerKelvin(200))
            val grid = thermalGrid(house, Some(thermalStorage))
            val hp = hpModel(grid)

            // Create relevant data for the current test
            val relevantData = hpData.copy(currentTick =
              thermalState.houseState.map(_.tick).getOrElse(0L)
            )

            // Invoke determineFlexOptions and match the results
            hp.determineFlexOptions(relevantData, lastState) match {
              case ProvideMinMaxFlexOptions(
                    modelUuid,
                    referencePower,
                    minPower,
                    maxPower,
                  ) =>
                modelUuid shouldBe hp.uuid
                referencePower should approximate(
                  Kilowatts(expectedReferencePower)
                )
                minPower should approximate(Kilowatts(expectedMinPower))
                maxPower should approximate(Kilowatts(expectedMaxPower))
            }
        }
      }
    }

  }
}
