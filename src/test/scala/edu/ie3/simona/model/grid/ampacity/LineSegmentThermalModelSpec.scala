/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.LineState
import edu.ie3.simona.service.Data.SecondaryData.{CurrentVoltage, WeatherData}
import edu.ie3.simona.test.common.input.LineSegmentThermalModelInputData
import edu.ie3.simona.test.common.{UnitSpec, WeatherTestData}
import org.scalatest.matchers.should.Matchers
import squants.electro.Kilovolts
import squants.energy.{KilowattHours, Kilowatts}
import squants.thermal.Celsius
import squants.{Amperes, Energy, Kelvin, Meters, Power, Temperature}

class LineSegmentThermalModelSpec
    extends UnitSpec
    with LineSegmentThermalModelInputData
    with WeatherTestData
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
            initGroundTemp,
            lineCurrent,
            exptLineTemperature,
        ) =>

          val initialGroundTemperature = Celsius(initGroundTemp)

          val startingState: LineState = LineSegmentThermalModel.initState(
            cableSetup,
            lineSegmentThermalModel,
            initialGroundTemperature,
          )

          val currentModel: LineSegmentThermalModel =
            startingState.currentLineSegmentThermalModel

          val current = Amperes(lineCurrent)

          val expectedLineTemp = Celsius(exptLineTemperature)

          val updatedState: LineState = currentModel.determineState(
            tick,
            startingState,
            current,
            defaultSimulationStart,
          )

          updatedState.lineTemperatures.currentLineTemp1 should approximate(
            expectedLineTemp
          )
      }
    }

    "throw an exception for positive cable depth" in {
      an[IllegalArgumentException] should be thrownBy {
        LineSegmentThermalModel.determineWeightsGroundTemperatures(
          Meters(0.5)
        )
      }
    }

    "determine temperature level weights based on cable depth" in {
      val cases = Table(
        ("depth", "expectedW3", "expectedW4"),
        (-0.3, 1.0, 0.0), // below lower bound
        (-0.64, 1.0, 0.0), // exactly lower bound
        (-1.0, 0.7251908397, 0.274809160305), // typical cable depth
        (-1.95, 0.0, 1.0), // exactly upper bound
        (-2.5, 0.0, 1.0), // above upper bound
        ((-0.64 - 1.95) / 2.0, 0.5, 0.5), // midpoint interpolation
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

    "handle mixed weather and voltage data" in {
      val initialState = LineSegmentThermalModel.initState(
        cigreT880LandCable33kV,
        lineSegmentThermalModel,
        Celsius(20),
      )
      val receivedVoltage = Kilovolts(20)
      val updatedState = lineSegmentThermalModel.handleInput(
        initialState,
        Seq(
          weatherData,
          CurrentVoltage(lineSegmentThermalModel.uuid, receivedVoltage),
        ),
      )
      val (weightTempLvl3, weightTempLvl4) =
        LineSegmentThermalModel.determineWeightsGroundTemperatures(
          Meters(initialState.cableSetup.pointA.height)
        )
      val groundTempLvl3 = weatherData.groundTempLvl3.getOrElse(
        fail("Test weather data must provide ground temperature level 3.")
      )
      val groundTempLvl4 = weatherData.groundTempLvl4.getOrElse(
        fail("Test weather data must provide ground temperature level 4.")
      )
      val expectedGroundTemperature =
        groundTempLvl3 * weightTempLvl3 + groundTempLvl4 * weightTempLvl4

      updatedState.currentVoltage.shouldBe(receivedVoltage)
      updatedState.groundTemperature.should(
        approximate(expectedGroundTemperature)
      )
    }

    "derive the ground temperature at cable depth from weather data" in {
      val (weightTempLvl3, weightTempLvl4) =
        LineSegmentThermalModel.determineWeightsGroundTemperatures(
          Meters(cigreT880LandCable33kV.pointA.height)
        )
      val groundTempLvl3 = weatherData.groundTempLvl3.getOrElse(
        fail("Test weather data must provide ground temperature level 3.")
      )
      val groundTempLvl4 = weatherData.groundTempLvl4.getOrElse(
        fail("Test weather data must provide ground temperature level 4.")
      )
      val expectedGroundTemperature =
        groundTempLvl3 * weightTempLvl3 + groundTempLvl4 * weightTempLvl4

      LineSegmentThermalModel
        .groundTemperatureFromWeather(
          cigreT880LandCable33kV,
          weatherData,
        )
        .should(approximate(expectedGroundTemperature))
    }

    "throw an exception if a ground temperature level is missing" in {
      an[IllegalArgumentException] should be thrownBy {
        LineSegmentThermalModel.groundTemperatureFromWeather(
          cigreT880LandCable33kV,
          weatherData.copy(groundTempLvl3 = None),
        )
      }

      an[IllegalArgumentException] should be thrownBy {
        LineSegmentThermalModel.groundTemperatureFromWeather(
          cigreT880LandCable33kV,
          weatherData.copy(groundTempLvl4 = None),
        )
      }
    }

  }
}
