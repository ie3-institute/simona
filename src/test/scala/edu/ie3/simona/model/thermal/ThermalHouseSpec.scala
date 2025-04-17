/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTargetTemperatureReached,
  HouseTemperatureLowerBoundaryReached,
}
import edu.ie3.simona.model.thermal.ThermalHouse.{
  ThermalHouseState,
  startingState,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.HpInputTestData
import edu.ie3.util.scala.quantities.WattsPerKelvin
import org.scalatest.prop.{TableFor2, TableFor3, TableFor6}
import squants.energy._
import squants.thermal._
import squants.time._
import squants.{Energy, Temperature}

class ThermalHouseSpec extends UnitSpec with HpInputTestData {

  implicit val tolerance: Temperature = Celsius(1e-4)
  implicit val energyTolerance: Energy = KilowattHours(1e-4)

  "ThermalHouse" should {
    "Functions testing inner temperature work as expected" in {

      val thermalHouseTest = thermalHouse(18, 22)

      val testCases: TableFor3[Double, Boolean, Boolean] = Table(
        ("Inner Temperature (C)", "Is Too High", "Is Too Low"),
        (17d, false, true),
        (17.98d, false, true),
        (18d, false, true),
        (19.98d, false, false),
        (20d, true, false),
        (22d, true, false),
        (22.02d, true, false),
        (23d, true, false),
      )

      forAll(testCases) {
        (innerTemperature: Double, isTooHigh: Boolean, isTooLow: Boolean) =>
          val innerTemp = Temperature(innerTemperature, Celsius)
          val isHigher = thermalHouseTest.isInnerTemperatureTooHigh(innerTemp)
          val isLower = thermalHouseTest.isInnerTemperatureTooLow(innerTemp)

          isHigher shouldBe isTooHigh
          isLower shouldBe isTooLow
      }
    }

    "Comprising function to calculate new inner temperature works as expected" in {
      val thermalHouseTest = thermalHouse(18, 22)
      val thermalPower = Kilowatts(100)
      val duration = Seconds(3600)
      val currentInnerTemperature = Temperature(20, Celsius)
      val ambientTemperature = Temperature(10, Celsius)

      val newInnerTemperature = thermalHouseTest.newInnerTemperature(
        thermalPower,
        duration,
        currentInnerTemperature,
        ambientTemperature,
      )

      newInnerTemperature should approximate(Temperature(29, Celsius))
    }

    "Check for the correct state of house" in {
      val house = thermalHouse(18, 22)
      val tick = 3600
      val ambientTemperature = Temperature(15, Celsius)
      val initialHouseState = startingState(house, ambientTemperature)

      val testCases: TableFor2[Double, Double] = Table(
        ("qDotHouse", "expectedTemperature(K)"),
        // loss is higher than gain
        (0d, 292.65),
        (1d, 292.75),
        (2d, 292.85),
        // Loss and gain should be equal resulting no temperature change
        (5d, 293.15),
        // gain is higher than loss
        (6d, 293.25),
        (10d, 293.65),
      )

      forAll(testCases) {
        (lastOperatingPoint: Double, expectedTemperature: Double) =>
          val thermalHouseState =
            house.determineState(
              tick,
              initialHouseState,
              Kilowatts(lastOperatingPoint),
            )

          thermalHouseState match {
            case ThermalHouseState(
                  tick,
                  _,
                  temperature,
                ) =>
              tick shouldBe 3600L
              temperature should approximate(Kelvin(expectedTemperature))
            case unexpected =>
              fail(s"Expected a thermalHouseState but got none $unexpected.")
          }
      }
    }

    "Check for the correct state over multiple simulation steps" in {
      val house = thermalHouse(18, 22)
      val ambientTemperature = Celsius(5)
      val initialHouseState = startingState(house, ambientTemperature)

      val testCases: TableFor6[Double, Long, Double, Double, Long, Double] =
        Table(
          (
            "qDotFirstPeriod",
            "firstTick",
            "expectedTemperatureFirstPeriod",
            "qDotSecondPeriod",
            "secondTick",
            "expectedTemperatureSecondPeriod",
          ),
          (30d, 36000, 302.63, 30d, 72000, 306.12),
          (30d, 18000, 299.05, 30d, 72000, 306.12),
          (30d, 7200, 295.87, 30d, 72000, 306.12),
        )

      forAll(testCases) {
        (
            qDotFirstPeriod: Double,
            firstTick: Long,
            expectedTemperatureFirstPeriod: Double,
            qDotSecondPeriod: Double,
            secondTick: Long,
            expectedTemperatureSecondPeriod: Double,
        ) =>
          val thermalHouseState = house.determineState(
            firstTick,
            initialHouseState,
            Kilowatts(qDotFirstPeriod),
          )

          thermalHouseState match {
            case ThermalHouseState(
                  tick,
                  temperature,
                  _,
                ) =>
              tick shouldBe firstTick
              temperature should approximate(
                Kelvin(expectedTemperatureFirstPeriod)
              )
            case unexpected =>
              fail(s"Expected a thermalHouseState but got none $unexpected.")
          }

          val finalThermalHouseState = house.determineState(
            secondTick,
            thermalHouseState,
            Kilowatts(qDotSecondPeriod),
          )

          finalThermalHouseState match {
            case ThermalHouseState(
                  tick,
                  temperature,
                  _,
                ) =>
              tick shouldBe secondTick
              temperature should approximate(
                Kelvin(expectedTemperatureSecondPeriod)
              )
            case unexpected =>
              fail(s"Expected a thermalHouseState but got none $unexpected.")
          }
      }
    }

    "Check if the same state is reached by different ways of simulation steps" in {
      val house = thermalHouse(18, 22)
      val ambientTemperature = Celsius(5)
      val initialHouseState = startingState(house, ambientTemperature)

      val qDot = Kilowatts(30)
      val firstTickCaseA = 18000
      val firstTickCaseB = 7200

      val finalTick = 72000

      val thermalHouseStateCaseA = house.determineState(
        firstTickCaseA,
        initialHouseState,
        qDot,
      )

      val thermalHouseStateCaseB = house.determineState(
        firstTickCaseB,
        initialHouseState,
        qDot,
      )

      val finalThermalHouseStateCaseA = house.determineState(
        finalTick,
        thermalHouseStateCaseA,
        qDot,
      )
      val finalThermalHouseStateCaseB = house.determineState(
        finalTick,
        thermalHouseStateCaseB,
        qDot,
      )

      val finalThermalHouseStateCaseC = house.determineState(
        finalTick,
        initialHouseState,
        qDot,
      )
      finalThermalHouseStateCaseA.innerTemperature should approximate(
        finalThermalHouseStateCaseC.innerTemperature
      )
      finalThermalHouseStateCaseA.innerTemperature should approximate(
        finalThermalHouseStateCaseB.innerTemperature
      )
    }

    "Check for the correct next threshold of house with thermal feed in" in {
      val tick = 3600
      val house = thermalHouse(18, 22)
      val ambientTemperature = Temperature(15, Celsius)
      val initialHouseState = startingState(house, ambientTemperature)

      val testCases: TableFor3[Double, Double, Option[ThermalThreshold]] =
        Table(
          ("lastOperatingPoint", "newOperatingPoint", "expectedThreshold"),
          // some OperatingPoints not capable to heat the house sufficient
          (0d, 0d, Some(HouseTemperatureLowerBoundaryReached(15600))),
          (1d, 1d, Some(HouseTemperatureLowerBoundaryReached(19600))),
          (2d, 2d, Some(HouseTemperatureLowerBoundaryReached(26266))),
          // OperatingPoint that keeps the house in perfect balance
          (5d, 5d, None),
          // some OperatingPoints that increase the house inner temperature after some cooling down first
          (0d, 6d, Some(HouseTargetTemperatureReached(15600))),
          (0d, 10d, Some(HouseTargetTemperatureReached(6872))),
        )

      forAll(testCases) {
        (
            lastOp: Double,
            newOp: Double,
            expectedThreshold: Option[ThermalThreshold],
        ) =>
          val lastOperatingPoint = Kilowatts(lastOp)
          val newOperatingPoint = Kilowatts(newOp)

          val state = house.determineState(
            tick,
            initialHouseState,
            lastOperatingPoint,
          )
          val threshold = house.determineNextThreshold(
            state,
            newOperatingPoint,
          )

          threshold match {
            case threshold => threshold shouldBe expectedThreshold
            case unexpected =>
              fail(
                s"Expected a thermalHouseThreshold but got none $unexpected."
              )
          }
      }
    }

    "Check build method" in {

      val thermalTestHouse = thermalHouse(18, 22)
      val thermalHouseInput = defaultThermalHouseInput

      thermalTestHouse.id shouldBe thermalHouseInput.getId
      thermalTestHouse.operatorInput shouldBe thermalHouseInput.getOperator
      thermalTestHouse.operationTime shouldBe thermalHouseInput.getOperationTime
      thermalTestHouse.bus shouldBe null
      thermalTestHouse.ethLosses shouldBe WattsPerKelvin(1000.0)
      (thermalTestHouse.ethCapa * Temperature(
        1,
        Kelvin,
      )) shouldBe KilowattHours(10.0)
      thermalTestHouse.lowerBoundaryTemperature should approximate(
        Temperature(
          18,
          Celsius,
        )
      )
      thermalTestHouse.upperBoundaryTemperature should approximate(
        Temperature(
          22,
          Celsius,
        )
      )
    }
  }
}
