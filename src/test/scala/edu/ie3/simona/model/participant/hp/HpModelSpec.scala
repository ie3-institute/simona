/*
 * © 2022. TU Dortmund University,
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
import edu.ie3.simona.model.thermal.ThermalHouseTestData
import edu.ie3.simona.ontology.messages.flex.FlexType
import edu.ie3.simona.service.DataTimeType
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

  // Testing tolerances
  given Power = Kilowatts(1e-10)
  given Energy = KilowattHours(1e-5)
  given Temperature = Kelvin(1e-3)

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
          (42.6888473, 42.6888473),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState =
            thermalState(Celsius(18), ambientTemperature)
          ),
          16.54958,
          (34.501539, 34.501539),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState =
            thermalState(Celsius(20), ambientTemperature)
          ),
          18.186979,
          (0.0, 18.12692469),
          (0.0, 0.0),
        ),
        (
          defaultState.copy(thermalGridState =
            thermalState(Celsius(22), ambientTemperature)
          ),
          19.82437,
          (0.0, 1.7523096),
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
          16.3142322,
          (36.8576777, 36.8576777),
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
          17.9516937,
          (20.4830627, 20.4830627),
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
          30.232655,
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
            ThermalEnergyDemand(zeroKWh, zeroKWh),
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

    "determine operating point without flex control correctly" in {
      val ambientTemperature = Celsius(10)

      val cases = Table(
        (
          "tick",
          "requiredDemandHouse",
          "expectedHpQDot",
          "expectedTick",
        ),
        (0, 0d, 0d, Some(4240)),
        (5000, 1d, 95d, Some(11563)),
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
              ThermalEnergyDemand.noDemand,
              ThermalEnergyDemand.noDemand,
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

      val flexModel = hpModel.flexModels(FlexType.PowerLimit)

      val cases = Table(
        (
          "tick",
          "setPower",
          "requiredDemandHouse",
          "expectedHpQDot",
          "expectedTick",
        ),
        (0L, 0d, 0d, 0d, Some(4240)),
        (5000L, 95d, 1d, 95d, Some(11563)),
        (0L, 80d, 0d, 95d, Some(4240)),
        (5000L, 80d, 1d, 95d, Some(11563)),
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
              ThermalEnergyDemand.noDemand,
              ThermalEnergyDemand.noDemand,
              ThermalEnergyDemand.noDemand,
            ),
          )
          val setPower = Kilowatts(setPwr)

          val op = hpModel.determineOperatingPoint(state, setPower)
          val threshold = flexModel.determineNextActivation(
            state,
            op,
            setPower,
            DataTimeType.Current,
          )

          op.activePower shouldBe Kilowatts(expectedHpQDot)
          threshold.changesAtTick shouldBe expectedTick

      }
    }
  }
}
