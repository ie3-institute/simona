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
import squants.{Amperes, Energy, Kelvin, Meters, Power, Temperature}

class LineSegmentThermalModelSpec
    extends UnitSpec
    with LineSegmentThermalModelInputData
    with Matchers {

  // Testing tolerances
  given Power = Kilowatts(1e-10)
  given Energy = KilowattHours(1e-5)
  given Temperature = Kelvin(1e-3)
  given Double = 1e-10

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
          89.879674069971, // approx 90°C
        ), // CIGRE TB880 S. 205
        (3600L, cigreT880LandCable33kV, 20d, 537d, 70.54034259413268),
        (72000L, cigreT880LandCable33kV, 5d, 537d, 74.87967406997072),
        (
          72000L,
          andersSingleCore10kV,
          15d,
          629d,
          91.27098,
        ), // a bit too much because of overestimated screen ac resistance
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

          val startingState: LineState = LineSegmentThermalModel.startingState(
            groundTemperature,
            cableSetup,
            lineSegmentThermalModel,
            defaultSimulationStart,
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

    "determine temperature level weights based on cable depth" in {
      val cases = Table(
        ("depth", "expectedW3", "expectedW4"),
        (0.3, 1.0, 0.0), // below lower bound
        (0.64, 1.0, 0.0), // exactly lower bound
        (1.0, 0.7251908397, 0.274809160305), // typical cable depth
        (1.95, 0.0, 1.0), // exactly upper bound
        (2.5, 0.0, 1.0), // above upper bound
        ((0.64 + 1.95) / 2.0, 0.5, 0.5), // midpoint interpolation
      )

      forAll(cases) { (depth, expectedW3, expectedW4) =>
        val d = Meters(depth)
        val (w3, w4) =
          LineSegmentThermalModel.determineWeightsGroundTemperatures(d)

        w3 should approximate(expectedW3)
        w4 should approximate(expectedW4)
        (w3 + w4) should approximate(1.0)
      }
    }

  }
}
