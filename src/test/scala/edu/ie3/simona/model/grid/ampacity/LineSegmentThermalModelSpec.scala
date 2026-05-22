/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.LineState
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.LineSegmentThermalModelInputData
import org.scalatest.matchers.should.Matchers
import squants.energy.{KilowattHours, Kilowatts}
import squants.thermal.Celsius
import squants.{Amperes, Energy, Kelvin, Power, Temperature}

class LineSegmentThermalModelSpec
    extends UnitSpec
    with LineSegmentThermalModelInputData
    with Matchers {

  // Testing tolerances
  given Power = Kilowatts(1e-10)
  given Energy = KilowattHours(1e-5)
  given Temperature = Kelvin(1e-3)

  "LineSegmentThermalModel" should {

    "Determine the current state" in {
      val cases = Table(
        (
          "tick",
          "cableSetup",
          "groundTemp",
          "lineCurrent",
          "expectedLineTemperature",
        ),
        (
          72000L,
          cigreT880LandCable33kV,
          20d,
          537d,
          87.51797273323201,
        ), // CIGRE TB880 S. 205
        (3600L, cigreT880LandCable33kV, 20d, 537d, 69.50004461804613),
        (72000L, cigreT880LandCable33kV, 5d, 537d, 72.51797273323201),
        (3600L, andersSingleCore10kV, 15d, 629d, 90.0),
      )

      forAll(cases) {
        (
            tick,
            cableSetup,
            groundTemp,
            lineCurrent,
            exptLineTemperature,
        ) =>
          val groundTemperature = Celsius(groundTemp)

          val startingState: LineState =
            LineSegmentThermalModel.startingState(
              groundTemperature,
              cableSetup,
            )

          val currentModel: LineSegmentThermalModel =
            startingState.currentLineSegmentThermalModel

          val current = Amperes(lineCurrent)

          val expectedLineTemp = Celsius(exptLineTemperature)

          val updatedState: LineState = currentModel.determineState(
            tick,
            startingState,
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
