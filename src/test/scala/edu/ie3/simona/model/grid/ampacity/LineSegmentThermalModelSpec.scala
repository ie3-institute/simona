/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.LineState
import edu.ie3.simona.model.thermal.ThermalHouseTestData
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.{
  HpInputTestData,
  LineSegmentThermalModelInputData,
}
import org.scalatest.matchers.should.Matchers
import squants.energy.{KilowattHours, Kilowatts}
import squants.thermal.Celsius
import squants.{Amperes, Energy, Kelvin, Power, Temperature}

class LineSegmentThermalModelSpec
    extends UnitSpec
    with LineSegmentThermalModelInputData
    with Matchers
    with HpInputTestData
    with ThermalHouseTestData {

  // Testing tolerances
  given Power = Kilowatts(1e-10)
  given Energy = KilowattHours(1e-5)
  given Temperature = Kelvin(1e-3)

  "LineSegmentThermalModel" should {

    "Determine the current state" in {
      val groundTemperatureValue = Celsius(20)

      val startingState: LineState =
        LineSegmentThermalModel.startingState(
          groundTemperatureValue,
          cableSetup,
        )

      val currentModel: LineSegmentThermalModel =
        startingState.currentLineSegmentThermalModel

      val cases = Table(
        (
          "state",
          "lineCurrent",
          "expectedLineTemperature",
        ),
        (startingState, 537, 89.0),
      )

      forAll(cases) {
        (
            state,
            lineCurrent,
            exptLineTemperature,
        ) =>

          val tick = 720000
          val lineState = startingState
          val current = Amperes(lineCurrent)

          val expectedLineTemp = Celsius(exptLineTemperature)

          val updatedState: LineState = currentModel.determineState(
            tick,
            lineState,
            current,
          )

          updatedState match {
            case LineState(
                  tick,
                  lastTick,
                  _,
                  _,
                  _,
                  lineTemperatures,
                ) =>
              lineTemperatures.currentLineTemp1 should approximate(
                expectedLineTemp
              )

            case unexpected =>
              fail(
                s"Expected a thermal line model state but got none $unexpected."
              )
          }
      }
    }

  }
}
