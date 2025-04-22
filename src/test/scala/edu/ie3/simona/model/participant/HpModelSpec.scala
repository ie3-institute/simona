/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.model.participant.HpModel.{
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
  val hpModel: HpModel =
    HpModel.Factory(hpInputModel, hpModelSpecThermalGrid).create()

  "HpModel" should {

    "Determine the current state" in {
      val ambientTemperature = Celsius(10)
      val defaultState = HpState(
        0,
        defaultSimulationStart,
        thermalState(Celsius(17d), ambientTemperature),
        HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
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
          defaultState.copy(thermalGridState =
            thermalState(Celsius(17), ambientTemperature)
          ),
          15.7309,
          (42.6911468252153, 42.6911468252153),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState =
            thermalState(Celsius(18), ambientTemperature)
          ),
          16.54958,
          (34.5041678002461, 34.5041678002461),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState =
            thermalState(Celsius(20), ambientTemperature)
          ),
          18.186979,
          (0.0, 18.1302097503072),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState =
            thermalState(Celsius(22), ambientTemperature)
          ),
          19.82437,
          (0.0, 1.75625170036824),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState =
            thermalState(Celsius(23), ambientTemperature)
          ),
          20.64307273246,
          (0.0, 0.0),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(
            thermalGridState = thermalState(Celsius(0), ambientTemperature),
            lastHpOperatingPoint = HpOperatingPoint(
              Kilowatts(80),
              ThermalGridOperatingPoint(
                Kilowatts(80),
                Kilowatts(80),
                zeroKW,
                zeroKW,
              ),
            ),
          ),
          16.3171887, // TODO CHECK THIS?!
          (36.8281122472325, 36.8281122472325),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(
            thermalGridState = thermalState(Celsius(2), ambientTemperature),
            lastHpOperatingPoint = HpOperatingPoint(
              Kilowatts(80),
              ThermalGridOperatingPoint(
                Kilowatts(80),
                Kilowatts(80),
                zeroKW,
                zeroKW,
              ),
            ),
          ),
          17.9545845802706, // TODO CHECK THIS?!
          (20.4541541972941, 20.4541541972941),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(
            thermalGridState = thermalState(Celsius(17), ambientTemperature),
            lastHpOperatingPoint = HpOperatingPoint(
              Kilowatts(80),
              ThermalGridOperatingPoint(
                Kilowatts(80),
                Kilowatts(80),
                zeroKW,
                zeroKW,
              ),
            ),
          ),
          30.2350531177, // TODO CHECK THIS?!
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
            ThermalGridOperatingPoint(
              zeroKW,
              state.lastHpOperatingPoint.thermalOps.qDotHouse,
              zeroKW,
              zeroKW,
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
            ThermalEnergyDemand.noDemand,
            ThermalEnergyDemand(
              zeroKWh, //   KilowattHours(exptWaterStorageDemand._1),
              zeroKWh, // KilowattHours(exptWatertorageDemand._2),
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
                  ThermalGridState(Some(thermalHouseState), _, _),
                  _,
                  thermalDemands,
                ) =>
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
                Kilowatts(1),
                ThermalGridOperatingPoint(
                  Kilowatts(1),
                  Kilowatts(1),
                  zeroKW,
                  zeroKW,
                ),
              ),
              thermalDemands =
                ThermalDemandWrapper(onlyAddDemand, demand, demand, noDemand),
            ),
            (95.0, 0.0, 95.0),
          ),
          // 6. Same as before but heat storage is NOT empty
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
          // 7. Hp actually NOT running
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
          // 8. Same as before but heat storage is NOT empty
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
          // 9. Hp actually running
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
          // 10. Same as before but storage is NOT empty
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
          // 11. Hp actually not running
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
          // 12. Same as before but storage is NOT empty
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
          // 13. Hp actually running
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
          // 14. Same as before but storage is NOT empty
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
          // 15. Hp actually not running
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
          // 16. Same as before but storage is NOT empty
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
          // Storage is full, House has capacity till upper boundary, Hp not running
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
          // Storage is full, House has capacity till upper boundary, Hp is running
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
          // No capacity for flexibility at all because house is
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
          // No capacity for flexibility at all when storage is full and house has been (externally) heated up above target temperature
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

    "determine operating point without flex control correctly" in {
      val ambientTemperature = Celsius(10)

      val cases = Table(
        (
          "tick",
          "requiredDemandHouse",
          "expectedHpQDot",
          "expectedTick",
        ),
        (0, 0d, 0d, Some(4196)),
        (5000, 1d, 95d, Some(11492)),
      )

      forAll(cases) {
        (
            tick,
            requiredDemandHouse,
            expectedHpQDot,
            expectedTick,
        ) =>
          val state = HpState(
            tick,
            defaultSimulationStart,
            ThermalGridState(
              Some(ThermalHouseState(tick, ambientTemperature, Celsius(19))),
              None,
              None,
            ),
            HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
            ThermalDemandWrapper(
              ThermalEnergyDemand(
                KilowattHours(requiredDemandHouse),
                KilowattHours(requiredDemandHouse),
              ),
              ThermalEnergyDemand(zeroKWh, zeroKWh),
              ThermalEnergyDemand(zeroKWh, zeroKWh),
              ThermalEnergyDemand.noDemand,
            ),
          )

          val (op, threshold) = hpModel.determineOperatingPoint(state)

          op.activePower shouldBe Kilowatts(expectedHpQDot)
          threshold shouldBe expectedTick

      }
    }

    "determine operating point with flex control correctly" in {
      val ambientTemperature = Celsius(10)

      val cases = Table(
        (
          "tick",
          "setPower",
          "requiredDemandHouse",
          "expectedHpQDot",
          "expectedTick",
        ),
        (0L, 0d, 0d, 0d, Some(4196)),
        (5000L, 95d, 1d, 95d, Some(11492)),
        (0L, 80d, 0d, 95d, Some(4196)),
        (5000L, 80d, 1d, 95d, Some(11492)),
      )

      forAll(cases) {
        (
            tick,
            setPwr,
            requiredDemandHouse,
            expectedHpQDot,
            expectedTick,
        ) =>
          val state = HpState(
            tick,
            defaultSimulationStart,
            ThermalGridState(
              Some(ThermalHouseState(tick, ambientTemperature, Celsius(19))),
              None,
              None,
            ),
            HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero),
            ThermalDemandWrapper(
              ThermalEnergyDemand(
                KilowattHours(requiredDemandHouse),
                KilowattHours(requiredDemandHouse),
              ),
              ThermalEnergyDemand(zeroKWh, zeroKWh),
              ThermalEnergyDemand(zeroKWh, zeroKWh),
              ThermalEnergyDemand.noDemand,
            ),
          )
          val setPower = Kilowatts(setPwr)

          val (op, threshold) = hpModel.determineOperatingPoint(state, setPower)

          op.activePower shouldBe Kilowatts(expectedHpQDot)
          threshold.changesAtTick shouldBe expectedTick
      }
    }
  }
}
