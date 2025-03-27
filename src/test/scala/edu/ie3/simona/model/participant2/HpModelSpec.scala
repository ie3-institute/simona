/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.simona.model.participant2.HpModel.{
  HpOperatingPoint,
  HpState,
  ThermalOpWrapper,
}
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalDemandWrapper,
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouseTestData
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
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

  "HpModel" should {

    "Determine the current state" in {
      val defaultState = HpState(
        0,
        Celsius(10),
        thermalState(Celsius(17d)),
        HpOperatingPoint(zeroKW, None),
        Celsius(10),
        noThermalDemand,
      )

      val cases = Table(
        (
          "state",
          "expectedInnerTemperature",
          "exptHouseDemand",
          "exptHeatStorageDemand",
        ),
        (
          defaultState.copy(thermalGridState = thermalState(Celsius(17))),
          15.6,
          (44.0, 44.0),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState = thermalState(Celsius(18))),
          16.4,
          (36.0, 36.0),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState = thermalState(Celsius(20))),
          18.0,
          (20.0, 20.0),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState = thermalState(Celsius(22))),
          19.6,
          (0.0, 4.0),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState = thermalState(Celsius(23))),
          20.4,
          (0.0, 0.0),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState =
            thermalState(Celsius(0), Kilowatts(80))
          ),
          18.0,
          (20.0, 20.0),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState =
            thermalState(Celsius(2), Kilowatts(80))
          ),
          19.6,
          (0.0, 4.0),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState =
            thermalState(Celsius(17), Kilowatts(80))
          ),
          31.6,
          (0.0, 0.0),
          (0.0, 0.0),
        ),
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
          val operatingPoint = state.lastHpOperatingPoint.copy(thermalOps =
            Some(
              ThermalOpWrapper(
                zeroKW,
                state.thermalGridState.houseState.map(_.qDot).getOrElse(zeroKW),
                zeroKW,
              )
            )
          )
          val expectedDemand = ThermalDemandWrapper(
            ThermalEnergyDemand(
              KilowattHours(exptHouseDemand._1),
              KilowattHours(exptHouseDemand._2),
            ),
            ThermalEnergyDemand(
              KilowattHours(exptHeatStorageDemand._1),
              KilowattHours(exptHeatStorageDemand._2),
            ),
          )

          val updatedState = hpModel.determineState(
            state,
            operatingPoint,
            expectedTick,
            date,
          )

          updatedState match {
            case HpState(
                  tick,
                  _,
                  ThermalGridState(Some(thermalHouseState), _),
                  _,
                  _,
                  thermalDemands,
                ) => {
              tick shouldBe expectedTick
              thermalHouseState.tick shouldBe expectedTick
              thermalHouseState.innerTemperature should approximate(
                Celsius(
                  expectedInnerTemperature
                )
              )

              thermalDemands.houseDemand.possible should approximate(
                expectedDemand.houseDemand.possible
              )
              thermalDemands.houseDemand.required should approximate(
                expectedDemand.houseDemand.required
              )
              thermalDemands.heatStorageDemand.possible should approximate(
                expectedDemand.heatStorageDemand.possible
              )
              thermalDemands.heatStorageDemand.required should approximate(
                expectedDemand.heatStorageDemand.required
              )
            }
            case unexpected =>
              fail(s"Expected a hp state but got none $unexpected.")
          }
      }
    }

    "Calculate flex options" in {

      // the exact demand doesn't matter
      val noDemand = ThermalEnergyDemand(zeroKWh, zeroKWh)
      val onlyAddDemand = ThermalEnergyDemand(zeroKWh, KilowattHours(1))
      val demand = ThermalEnergyDemand(KilowattHours(1), KilowattHours(1))

      val defaultState = HpState(
        0,
        Celsius(10),
        thermalState(Celsius(17d)),
        HpOperatingPoint(zeroKW, None),
        Celsius(10),
        noThermalDemand,
      )

      val testCases =
        Table(
          ("state", "expectedValues"),
          // 1. Hp actually not running
          // House is below lower temperature boundary
          // Heat storage is empty
          // hp must be turned on(
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), zeroKW)),
                Some(ThermalStorageState(0L, zeroKWh, zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(demand, demand),
            ),
            (95.0, 95.0, 95.0),
          ),
          // 2. Same as before but heat storage is NOT empty
          // should be possible to turn hp on
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), zeroKW)),
                Some(ThermalStorageState(0L, KilowattHours(20), zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(demand, onlyAddDemand),
            ),
            (0.0, 0.0, 95.0),
          ),

          // 3. Hp actually running
          // House is below lower temperature boundary
          // Heat storage is empty
          // Hp must run because of house and storage
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(1))),
                Some(ThermalStorageState(0L, zeroKWh, zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(demand, demand),
            ),
            (95.0, 95.0, 95.0),
          ),
          // 4. Same as before but heat storage is NOT empty
          // Hp should not run because of storage but can be turned on
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(15), Kilowatts(1))),
                Some(ThermalStorageState(0L, KilowattHours(20), zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(demand, onlyAddDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 5. Hp actually running
          // House is between target temperature and lower temperature boundary
          // Heat storage is empty
          // Hp runs but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(1))),
                Some(ThermalStorageState(0L, zeroKWh, zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(onlyAddDemand, demand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 6. Same as before but heat storage is NOT empty
          // should be possible to keep hp off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(1))),
                Some(ThermalStorageState(0L, KilowattHours(20), zeroKW)),
              ),
              thermalDemands =
                ThermalDemandWrapper(onlyAddDemand, onlyAddDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 7. Hp actually NOT running
          // House is between target temperature and lower temperature boundary
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), zeroKW)),
                Some(ThermalStorageState(0L, zeroKWh, zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(onlyAddDemand, demand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 8. Same as before but heat storage is NOT empty
          // Hp should be off but able to turn on
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), zeroKW)),
                Some(ThermalStorageState(0L, KilowattHours(20), zeroKW)),
              ),
              thermalDemands =
                ThermalDemandWrapper(onlyAddDemand, onlyAddDemand),
            ),
            (0.0, 0.0, 95.0),
          ),
          // 9. Hp actually running
          // House is at target temperature boundary
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(20), Kilowatts(1))),
                Some(ThermalStorageState(0L, zeroKWh, zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(noDemand, demand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 10. Same as before but storage is NOT empty
          // Hp should run but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(20), Kilowatts(1))),
                Some(ThermalStorageState(0L, KilowattHours(20), zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(noDemand, onlyAddDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 11. Hp actually not running
          // House is at target temperature boundary
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(20), zeroKW)),
                Some(ThermalStorageState(0L, zeroKWh, zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(noDemand, demand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 12. Same as before but storage is NOT empty
          // Hp should not run but can be turned on for storage
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(20), zeroKW)),
                Some(ThermalStorageState(0L, KilowattHours(20), zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(noDemand, onlyAddDemand),
            ),
            (0.0, 0.0, 95.0),
          ),
          // 13. Hp actually running
          // House is above target temperature
          // Heat storage is empty
          // Hp will run because of storage but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(1))),
                Some(ThermalStorageState(0L, zeroKWh, zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(noDemand, demand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 14. Same as before but storage is NOT empty
          // Hp should run but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), Kilowatts(1))),
                Some(ThermalStorageState(0L, KilowattHours(20), zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(noDemand, onlyAddDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 15. Hp actually not running
          // House is above target temperature
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), zeroKW)),
                Some(ThermalStorageState(0L, zeroKWh, zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(noDemand, demand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 16. Same as before but storage is NOT empty
          // Hp should not run but can be turned on for storage
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(21), zeroKW)),
                Some(ThermalStorageState(0L, KilowattHours(20), zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(noDemand, onlyAddDemand),
            ),
            (0.0, 0.0, 95.0),
          ),
          // Storage is full, House has capacity till upper boundary, Hp not running
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), zeroKW)),
                Some(ThermalStorageState(0L, KilowattHours(500), zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(onlyAddDemand, noDemand),
            ),
            (0.0, 0.0, 95.0),
          ),
          // Storage is full, House has capacity till upper boundary, Hp is running
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(19), Kilowatts(1))),
                Some(ThermalStorageState(0L, KilowattHours(500), zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(onlyAddDemand, noDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // No capacity for flexibility at all because house is
          // at target temperature and storage is at max capacity
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(20), zeroKW)),
                Some(ThermalStorageState(0L, KilowattHours(500), zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(noDemand, noDemand),
            ),
            (0.0, 0.0, 0.0),
          ),
          // No capacity for flexibility at all when storage is full and house has been (externally) heated up above target temperature
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(ThermalHouseState(0L, Celsius(25), zeroKW)),
                Some(ThermalStorageState(0L, KilowattHours(500), zeroKW)),
              ),
              thermalDemands = ThermalDemandWrapper(noDemand, noDemand),
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
            case MinMaxFlexOptions(
                  referencePower,
                  minPower,
                  maxPower,
                ) =>
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
