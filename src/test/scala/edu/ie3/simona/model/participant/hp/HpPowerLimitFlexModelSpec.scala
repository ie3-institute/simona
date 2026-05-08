/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.hp

import edu.ie3.simona.model.participant.hp.HpModel.{
  HpOperatingPoint,
  HpState,
  ThermalGridOperatingPoint,
}
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalDemandWrapper,
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.ontology.messages.flex.PowerLimitFlexOptions
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.HpInputTestData
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import squants.energy.{KilowattHours, Kilowatts}
import squants.thermal.Celsius
import squants.{Energy, Kelvin, Power, Temperature}

class HpPowerLimitFlexModelSpec extends UnitSpec with HpInputTestData {

  // Testing tolerances
  given Power = Kilowatts(1e-10)
  given Energy = KilowattHours(1e-5)
  given Temperature = Kelvin(1e-3)

  val hpModel: HpModel =
    HpModel.Factory(hpInputModel, hpModelSpecThermalGrid).create()

  val flexModel = HpPowerLimitFlexModel(hpModel)

  "A HP PowerLimitFlexModel" should {

    "Calculate flex options" in {

      // the exact demand doesn't matter
      val noDemand = ThermalEnergyDemand(zeroKWh, zeroKWh)
      val onlyAddDemand = ThermalEnergyDemand(zeroKWh, KilowattHours(1))
      val demand = ThermalEnergyDemand(KilowattHours(1), KilowattHours(1))
      val ambientTemperature = Celsius(10)

      val defaultState = HpState(
        0,
        defaultSimulationStart,
        thermalState(Celsius(17d), ambientTemperature),
        HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
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
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(15),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    zeroKWh,
                  )
                ),
                None,
              ),
              thermalDemands =
                ThermalDemandWrapper(demand, demand, demand, noDemand),
            ),
            (95.0, 95.0, 95.0),
          ),
          // 2. Same as before but heat storage is NOT empty
          // should be possible to turn hp on
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(15),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(20),
                  )
                ),
                None,
              ),
              thermalDemands =
                ThermalDemandWrapper(demand, onlyAddDemand, demand, noDemand),
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
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(15),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    zeroKWh,
                  )
                ),
                None,
              ),
              lastHpOperatingPoint = HpOperatingPoint(
                Kilowatts(1),
                ThermalGridOperatingPoint(
                  Kilowatts(1),
                  Kilowatts(1),
                  zeroKW,
                  zeroKW,
                ),
              ),
              thermalDemands =
                ThermalDemandWrapper(demand, demand, demand, noDemand),
            ),
            (95.0, 95.0, 95.0),
          ),
          // 4. Same as before but heat storage is NOT empty
          // Hp should run but could be turned off because of storage
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(15),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(20),
                  )
                ),
                None,
              ),
              lastHpOperatingPoint = HpOperatingPoint(
                Kilowatts(1),
                ThermalGridOperatingPoint(
                  Kilowatts(1),
                  Kilowatts(1),
                  zeroKW,
                  zeroKW,
                ),
              ),
              thermalDemands =
                ThermalDemandWrapper(demand, onlyAddDemand, demand, noDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 5. Hp actually running
          // House is between target temperature and lower temperature boundary
          // Heat storage is empty
          // Hp should run, since it was running in the last state
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(19),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    zeroKWh,
                  )
                ),
                None,
              ),
              lastHpOperatingPoint = HpOperatingPoint(
                Kilowatts(1),
                ThermalGridOperatingPoint(
                  Kilowatts(1),
                  Kilowatts(1),
                  zeroKW,
                  zeroKW,
                ),
              ),
              thermalDemands =
                ThermalDemandWrapper(onlyAddDemand, demand, noDemand, noDemand),
            ),
            (95.0, 95.0, 95.0),
          ),
          // 6. Same as before but the last operating point is now zero
          // Hp runs but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(19),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    zeroKWh,
                  )
                ),
                None,
              ),
              lastHpOperatingPoint = HpOperatingPoint(
                zeroKW,
                ThermalGridOperatingPoint.zero,
              ),
              thermalDemands =
                ThermalDemandWrapper(onlyAddDemand, demand, demand, noDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 7. Same as before but heat storage is NOT empty
          // should be possible to keep hp off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(19),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(20),
                  )
                ),
                None,
              ),
              lastHpOperatingPoint = HpOperatingPoint(
                Kilowatts(1),
                ThermalGridOperatingPoint(
                  Kilowatts(1),
                  Kilowatts(1),
                  zeroKW,
                  zeroKW,
                ),
              ),
              thermalDemands = ThermalDemandWrapper(
                onlyAddDemand,
                onlyAddDemand,
                demand,
                noDemand,
              ),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 8. Hp actually NOT running
          // House is between target temperature and lower temperature boundary
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(19),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    zeroKWh,
                  )
                ),
                None,
              ),
              thermalDemands =
                ThermalDemandWrapper(onlyAddDemand, demand, demand, noDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 9. Same as before but heat storage is NOT empty
          // Hp should be off but able to turn on
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(19),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(20),
                  )
                ),
                None,
              ),
              thermalDemands = ThermalDemandWrapper(
                onlyAddDemand,
                onlyAddDemand,
                demand,
                noDemand,
              ),
            ),
            (0.0, 0.0, 95.0),
          ),
          // 10. Hp actually running
          // House is at target temperature boundary
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(20),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    zeroKWh,
                  )
                ),
                None,
              ),
              lastHpOperatingPoint = HpOperatingPoint(
                Kilowatts(1),
                ThermalGridOperatingPoint(
                  Kilowatts(1),
                  Kilowatts(1),
                  zeroKW,
                  zeroKW,
                ),
              ),
              thermalDemands =
                ThermalDemandWrapper(noDemand, demand, demand, noDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 11. Same as before but storage is NOT empty
          // Hp should run but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(20),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(20),
                  )
                ),
                None,
              ),
              lastHpOperatingPoint = HpOperatingPoint(
                Kilowatts(1),
                ThermalGridOperatingPoint(
                  Kilowatts(1),
                  Kilowatts(1),
                  zeroKW,
                  zeroKW,
                ),
              ),
              thermalDemands =
                ThermalDemandWrapper(noDemand, onlyAddDemand, demand, noDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 12. Hp actually not running
          // House is at target temperature boundary
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(20),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    zeroKWh,
                  )
                ),
                None,
              ),
              thermalDemands =
                ThermalDemandWrapper(noDemand, demand, demand, noDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 13. Same as before but storage is NOT empty
          // Hp should not run but can be turned on for storage
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(20),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(20),
                  )
                ),
                None,
              ),
              thermalDemands =
                ThermalDemandWrapper(noDemand, onlyAddDemand, demand, noDemand),
            ),
            (0.0, 0.0, 95.0),
          ),
          // 14. Hp actually running
          // House is above target temperature
          // Heat storage is empty
          // Hp will run because of storage but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(21),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    zeroKWh,
                  )
                ),
                None,
              ),
              lastHpOperatingPoint = HpOperatingPoint(
                Kilowatts(1),
                ThermalGridOperatingPoint(
                  Kilowatts(1),
                  Kilowatts(1),
                  zeroKW,
                  zeroKW,
                ),
              ),
              thermalDemands =
                ThermalDemandWrapper(noDemand, demand, demand, noDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 15. Same as before but storage is NOT empty
          // Hp should run but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(21),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(20),
                  )
                ),
                None,
              ),
              lastHpOperatingPoint = HpOperatingPoint(
                Kilowatts(1),
                ThermalGridOperatingPoint(
                  Kilowatts(1),
                  Kilowatts(1),
                  zeroKW,
                  zeroKW,
                ),
              ),
              thermalDemands = ThermalDemandWrapper(
                noDemand,
                onlyAddDemand,
                noDemand,
                noDemand,
              ),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 16. Hp actually not running
          // House is above target temperature
          // Heat storage is empty
          // Hp should run because of storage but can be turned off
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(21),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    zeroKWh,
                  )
                ),
                None,
              ),
              thermalDemands =
                ThermalDemandWrapper(noDemand, demand, demand, noDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 17. Same as before but storage is NOT empty
          // Hp should not run but can be turned on for storage
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(21),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(20),
                  )
                ),
                None,
              ),
              thermalDemands =
                ThermalDemandWrapper(noDemand, onlyAddDemand, demand, noDemand),
            ),
            (0.0, 0.0, 95.0),
          ),
          // 18. Storage is full, House has capacity till upper boundary, Hp not running
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(19),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(500),
                  )
                ),
                None,
              ),
              thermalDemands = ThermalDemandWrapper(
                onlyAddDemand,
                noDemand,
                noDemand,
                noDemand,
              ),
            ),
            (0.0, 0.0, 95.0),
          ),
          // 19. Storage is full, House has capacity till upper boundary, Hp is running
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(19),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(500),
                  )
                ),
                None,
              ),
              lastHpOperatingPoint = HpOperatingPoint(
                Kilowatts(1),
                ThermalGridOperatingPoint(
                  Kilowatts(1),
                  Kilowatts(1),
                  zeroKW,
                  zeroKW,
                ),
              ),
              thermalDemands = ThermalDemandWrapper(
                onlyAddDemand,
                noDemand,
                noDemand,
                noDemand,
              ),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 20. No capacity for flexibility at all because house is
          // at target temperature and storage is at max capacity
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(20),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(500),
                  )
                ),
                None,
              ),
              thermalDemands =
                ThermalDemandWrapper(noDemand, noDemand, noDemand, noDemand),
            ),
            (0.0, 0.0, 0.0),
          ),
          // 21. No capacity for flexibility at all when storage is full and house has been (externally) heated up above target temperature
          (
            defaultState.copy(
              thermalGridState = ThermalGridState(
                Some(
                  ThermalHouseState(
                    0L,
                    ambientTemperature,
                    Celsius(25),
                  )
                ),
                Some(
                  ThermalStorageState(
                    0L,
                    KilowattHours(500),
                  )
                ),
                None,
              ),
              thermalDemands =
                ThermalDemandWrapper(noDemand, noDemand, noDemand, noDemand),
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
          flexModel.determineFlexOptions(state, DataTimeType.Current) match {
            case PowerLimitFlexOptions(
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
  }
}
