/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.simona.model.participant2.HpModel.HpState
import edu.ie3.simona.model.participant2.ParticipantModel.ActivePowerAndHeatOperatingPoint
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouseTestData
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.HpInputTestData
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import org.scalatest.matchers.should.Matchers
import squants.energy.{KilowattHours, Kilowatts}
import squants.thermal.Celsius
import squants.{Energy, Kelvin, Power, Temperature}

class HpModelSpec
    extends UnitSpec
    with Matchers
    with HpInputTestData
    with ThermalHouseTestData {
  implicit val powerTolerance: Power = Kilowatts(1e-10)
  implicit val energyTolerance: Energy = KilowattHours(1e-10)
  implicit val tempTolerance: Temperature = Kelvin(1e-3)

  // build the HpModel
  val hpModel: HpModel = HpModel(hpInputModel, hpModelSpecThermalGrid)

  "StorageModel" should {

    "Determine the current state" in {
      val cases = Table(
        (
          "state",
          "expectedInnerTemperature",
          "exptHouseDemand",
          "exptHeatStorageDemand",
        ),
        (
          HpState(
            0,
            Celsius(10),
            thermalState(Celsius(17d)),
            Celsius(10),
            noThermalDemand,
          ),
          15.6,
          KilowattHours(44),
          KilowattHours(64),
        ),
           (
          HpState(
            0,
            Celsius(10),
            thermalState(Celsius(18)),
            Celsius(10),
            noThermalDemand,
          ),
          16.4,
             zeroKWh,
             zeroKWh,
        ),
        (
          HpState(

            0,
            Celsius(10),
            thermalState(Celsius(20)),
            Celsius(10),
            noThermalDemand,
          ),
          18.0,
          zeroKWh,
          zeroKWh,
        ),
        (
          HpState(
            0,
            Celsius(10),
            thermalState(Celsius(22)),
            Celsius(10),
            noThermalDemand,
          ),
          19.6,
          zeroKWh,
          zeroKWh,
        ),
        (
          HpState(
            0,
            Celsius(10),
            thermalState(Celsius(23)),
            Celsius(10),
            noThermalDemand,
          ),
          20.4,
          zeroKWh,
          zeroKWh,
        ),
        (
          HpState(
            0,
            Celsius(10),
            thermalState(Celsius(17), Kilowatts(80d)),
            Celsius(10),
            noThermalDemand,
          ),
          31.6,
          zeroKWh,
          zeroKWh,
        ),
        (
          HpState(
            0,
            Celsius(10),
            thermalState(Celsius(18), Kilowatts(80d)),
            Celsius(10),
            noThermalDemand,
          ),
          32.4,
          zeroKWh,
          zeroKWh,
        ),
        (
          HpState(
            0,
            Celsius(10),
            thermalState(Celsius(20), Kilowatts(80d)),
            Celsius(10),
            noThermalDemand,
          ),
          34.0,
          zeroKWh,
          zeroKWh,

        ),
        (
          HpState(
            0,
            Celsius(10),
            thermalState(Celsius(22), Kilowatts(80d)),
            Celsius(10),
            noThermalDemand,
          ),
          35.6,
          zeroKWh,
          zeroKWh,
        ),
        (
          HpState(
            0,
            Celsius(10),
            thermalState(Celsius(25), Kilowatts(80d)),
            Celsius(10),
            noThermalDemand,
          ),
          38.0,
          zeroKWh,
          zeroKWh,
        )
      )

      forAll(cases) {
        (
            state,
            expectedInnerTemperature,
            exptHouseDemand,
            exptHeatStorageDemand,
        ) =>
          val expectedTick = 7200
          val date = defaultSimulationStart
          val operatingPoint = ActivePowerAndHeatOperatingPoint(zeroKW, None)

          hpModel.determineState(
            state,
            operatingPoint,
            expectedTick,
            date,
          ) match {
            case HpState(
                  tick,
                  _,
                  ThermalGridState(Some(thermalHouseState), _),
                  _,
                  thermalDemands,
                ) => {
              tick shouldBe expectedTick
              thermalHouseState.innerTemperature should approximate(
                Celsius(
                  expectedInnerTemperature
                )
              )

              thermalDemands.houseDemand shouldBe exptHouseDemand
                thermalDemands.heatStorageDemand shouldBe exptHeatStorageDemand
            }
            case unexpected =>
              fail(s"Expected a hp state but got none $unexpected.")
          }
      }
    }

    "Calculate flex options" in {
      val testCases =
        Table(
          ("state", "expectedValues"),
          // 1. Hp actually not running
          // House is below lower temperature boundary
          // Heat storage is empty
          // hp must be turned on(
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (95.0, 95.0, 95.0),
          ),
          // 2. Same as before but heat storage is NOT empty
          // should be possible to turn hp on
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                Some(
                  ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                ),
              ),
              Celsius(10d),
              // FIXME?
              noThermalDemand,
            ),
            (0.0, 0.0, 95.0),
          ),

          // 3. Hp actually running
          // House is below lower temperature boundary
          // Heat storage is empty
          // Hp must run because of house and storage
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                Some(
                  ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))
                ),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (95.0, 95.0, 95.0),
          ),
          // 4. Same as before but heat storage is NOT empty
          // Hp should not run because of storage but can be turned on
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(0))),
                Some(
                  ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                ),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (0.0, 0.0, 95.0),
          ),
          // 5. Hp actually running
          // House is between target temperature and lower temperature boundary
          // Heat storage is empty
          // Hp runs but can be turned off
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                Some(
                  ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))
                ),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (95.0, 0.0, 95.0),
          ),
          // 6. Same as before but heat storage is NOT empty
          // should be possible to keep hp off
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                Some(
                  ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                ),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (95.0, 0.0, 95.0),
          ),
          // 7. Hp actually NOT running
          // House is between target temperature and lower temperature boundary
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                Some(
                  ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))
                ),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (95.0, 0.0, 95.0),
          ),
          // 8. Same as before but heat storage is NOT empty
          // Hp should be off but able to turn on
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                Some(
                  ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))
                ),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (0.0, 0.0, 95.0),
          ),
          // 9. Hp actually running
          // House is between target temperature and upper temperature boundary
          // Heat storage is empty
          // Hp will run because of storage but can be turned off
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (95.0, 0.0, 95.0),
          ),
          // 10. Same as before but storage is NOT empty
          // Hp should run but can be turned off
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (95.0, 0.0, 95.0),
          ),
          // 11. Hp actually not running
          // House is between target temperature and upper temperature boundary
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (95.0, 0.0, 95.0),
          ),
          // 12. Same as before but storage is NOT empty
          // Hp should not run but can be turned on for storage or house
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (0.0, 0.0, 95.0),
          ),

          // 13. Hp actually running
          // House is at upper temperature boundary
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (95.0, 0.0, 95.0),
          ),
          // 14. Same as before but storage is NOT empty
          // Hp should run but can be turned off
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (95.0, 0.0, 95.0),
          ),
          // 15. Hp actually not running
          // House is at upper temperature boundary
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(0), Kilowatts(0))),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (95.0, 0.0, 95.0),
          ),

          // 16. Same as before but storage is NOT empty
          // Hp should not run but can be turned on for storage
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(0))),
                Some(ThermalStorageState(0L, KilowattHours(20), Kilowatts(0))),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (0.0, 0.0, 95.0),
          ),
          // Storage is full, House has capacity till upper boundary
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(0))),
                Some(
                  ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))
                ),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (0.0, 0.0, 95.0),
          ),
          // No capacity for flexibility at all because house is
          // at upperTempBoundary and storage is at max capacity
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(22), Kilowatts(0))),
                Some(
                  ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))
                ),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (0.0, 0.0, 0.0),
          ),
          // No capacity for flexibility at all when storage is full and house has been (externally) heated up above upperTemperatureBoundary
          (
            HpState(
              0,
              Celsius(10d),
              ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(25), Kilowatts(0))),
                Some(
                  ThermalStorageState(0L, KilowattHours(500), Kilowatts(0))
                ),
              ),
              Celsius(10d), // FIXME?
              noThermalDemand,
            ),
            (0.0, 0.0, 0.0),
          ),
        )

      // Run the test cases
      forAll(testCases) {
        (
            state,
            expectedValues: (Double, Double, Double),
        ) =>
          val (expectedReferencePower, expectedMinPower, expectedMaxPower) =
            expectedValues

          // Create relevant data for the current test
          // As we are only testing flexOptions here, we can use tick 0
          // which is also the tick of the lastState.
          // This should not happen in the simulation!
          // This can be simplified once the transitoryData is introduced

          // Invoke determineFlexOptions and match the results
          hpModel.determineFlexOptions(state) match {
            case ProvideMinMaxFlexOptions(
                  modelUuid,
                  referencePower,
                  minPower,
                  maxPower,
                ) =>
              modelUuid shouldBe hpModel.uuid
              referencePower should approximate(
                Kilowatts(expectedReferencePower)
              )
              minPower should approximate(Kilowatts(expectedMinPower))
              maxPower should approximate(Kilowatts(expectedMaxPower))
          }

      }
    }

    "Handle controlled power change" in {}
  }
}
