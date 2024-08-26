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
              case (
                    _,
                    _,
                    HpState(
                      isRunning,
                      _,
                      _,
                      activePower,
                      _,
                      ThermalGridState(Some(thermalHouseState), _, _),
                      maybeThreshold,
                    ),
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
            // 1. Hp actually not running
            // House is below lower temperature boundary
            // Heat storage is empty
            // Domestic hot water storage is full
            // HP must be turned on
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = false,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(0.0),
                Kilowatts(0.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                  Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (95.0, 95.0, 95.0),
            ),
            // 2. Same as before but heat storage is NOT empty
            // should be possible to keep hp off
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
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
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (0.0, 0.0, 95.0),
            ),
            // 3. Same as before but domestic hot water storage is NOT full
            // HP must be turned on
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
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
                  Some(
                    ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (95.0, 95.0, 95.0),
            ),
            // 4. Hp actually running
            // House is below lower temperature boundary
            // Heat storage is empty
            // Domestic hot water storage is full
            // Hp must run because of house and storage
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(0.0),
                Kilowatts(0.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (95.0, 95.0, 95.0),
            ),
            // 5. Same as before but heat storage is NOT empty
            // Hp should not run because of storage but can be turned on
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
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
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (0.0, 0.0, 95.0),
            ),
            // 6. Same as before but domestic hot water storage is NOT full
            // HP must be turned on
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
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
                  Some(
                    ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (95.0, 95.0, 95.0),
            ),
            // 7. Hp actually running
            // House is between target temperature and lower temperature boundary
            // Heat storage is empty
            // Domestic hot water storage is full
            // Hp runs but can be turned off
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (95.0, 0.0, 95.0),
            ),
            // 6. Same as before but heat storage is NOT empty
            // should be possible to keep hp off
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (95.0, 0.0, 95.0),
            ),
            // 9. Hp actually NOT running
            // House is between target temperature and lower temperature boundary
            // Heat storage is empty
            // Domestic hot water storage is full
            // Hp should run because of storage but can be turned off
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = false,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (95.0, 0.0, 95.0),
            ),
            // 10. Same as before but heat storage is NOT empty
            // Hp should be off but able to turn on
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = false,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                None,
              ),
              (0.0, 0.0, 95.0),
            ),
            // 11. Hp actually running
            // House is between target temperature and upper temperature boundary
            // Heat storage is empty
            // Domestic hot water storage is full
            // Hp will run because of storage but can be turned off
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                  Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (95.0, 0.0, 95.0),
            ),
            // 12. Same as before but storage is NOT empty
            // Hp should run but can be turned off
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (95.0, 0.0, 95.0),
            ),
            // 13. Hp actually not running
            // House is between target temperature and upper temperature boundary
            // Heat storage is empty
            // Domestic hot water storage is full
            // Hp should run because of storage but can be turned off
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = false,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                  Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (95.0, 0.0, 95.0),
            ),
            // 14. Same as before but storage is NOT empty
            // Hp should not run but can be turned on for storage or house
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = false,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (0.0, 0.0, 95.0),
            ),

            // 15. Hp actually running
            // House is at upper temperature boundary
            // Heat storage is empty
            // Domestic hot water storage is full
            // Hp should run because of storage but can be turned off
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(22), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                  Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (95.0, 0.0, 95.0),
            ),
            // 16. Same as before but storage is NOT empty
            // Hp should run but can be turned off
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(22), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (95.0, 0.0, 95.0),
            ),
            // 17. Hp actually not running
            // House is at upper temperature boundary
            // Heat storage is empty
            // Domestic hot water storage is full
            // Hp should run because of storage but can be turned off
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(22), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = false,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                  Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (95.0, 0.0, 95.0),
            ),

            // 18. Same as before but storage is NOT empty
            // Hp should not run but can be turned on for storage
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(22), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = false,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (0.0, 0.0, 95.0),
            ),

            // 19. Hp actually not running
            // House has capacity till upper boundary
            // Heat storage is full
            // Domestic hot water storage is full
            // Hp should not run but can be turned on
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
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
                    ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (0.0, 0.0, 95.0),
            ),

            // 20. Same as before but HP is running
            // Hp should run but can be turned off
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(0.0),
                Kilowatts(0.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (95.0, 0.0, 95.0),
            ),
            // 21. No capacity for flexibility at all because house is
            // at upperTempBoundary and storage is at max capacity
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(22), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
              ),
              HpState(
                isRunning = true,
                0,
                Some(hpData.ambientTemperature),
                Kilowatts(95.0),
                Kilowatts(80.0),
                ThermalGridState(
                  Some(ThermalHouseState(0L, Celsius(22), Kilowatts(0))),
                  Some(
                    ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))
                  ),
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
                  ),
                ),
                Some(HouseTemperatureUpperBoundaryReached(0L)),
              ),
              (0.0, 0.0, 0.0),
            ),

            // 22. No capacity for flexibility at all when storage is full and house has been (externally) heated up above upperTemperatureBoundary
            (
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(25), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))),
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
                  Some(
                    ThermalStorageState(0L, KilowattHours(250), Kilowatts(0))
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
            val grid = thermalGrid(
              house,
              Some(thermalStorage),
              Some(domesticHotWaterStorage),
            )
            val hp = hpModel(grid)

            // Create relevant data for the current test
            // As we are only testing flexOptions here, we can use tick 0
            // which is also the tick of the lastState.
            // This should not happen in the simulation!
            // This can be simplified once the transitoryData is introduced
            val relevantData = hpData.copy(currentTick =
              thermalState.houseState.map(_.tick).getOrElse(0L)
            )

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
