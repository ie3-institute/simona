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
import org.scalatest.prop.{TableFor2, TableFor3, TableFor7}
import squants.energy._
import squants.thermal._
import squants.time._
import squants.{Energy, Temperature}

class ThermalHouseSpec extends UnitSpec with HpInputTestData {

  implicit val temperaturTolerance: Temperature = Celsius(1e-4)
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

      newInnerTemperature should approximate(Temperature(28.5646, Celsius))
    }

    "Check for the correct state of house" in {
      val house = thermalHouse(18, 22)
      val tick = 3600
      val ambientTemperature = Temperature(15, Celsius)
      val initialHouseState = startingState(house, ambientTemperature)

      val testCases: TableFor2[Double, Double] = Table(
        ("qDotHouse", "expectedTemperature(K)"),
        // loss is higher than gain
        (0d, 292.67418),
        (1d, 292.76935),
        (2d, 292.86451),
        // Loss and gain should be equal resulting no temperature change
        (5d, 293.15),
        // gain is higher than loss
        (6d, 293.2451),
        (10d, 293.6258),
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

      val testCases
          : TableFor7[Double, Double, Long, Double, Double, Long, Double] =
        Table(
          (
            "startingTemp",
            "qDotFirstPeriod",
            "firstTick",
            "expectedTemperatureFirstPeriod",
            "qDotSecondPeriod",
            "secondTick",
            "expectedTemperatureSecondPeriod",
          ),
          (20d, 30d, 36000L, 29.4818, 30d, 72000L, 32.97),
          (20d, 30d, 18000L, 25.9020, 30d, 72000L, 32.97),
          (20d, 30d, 7200L, 22.719, 30d, 72000L, 32.97),
          (20d, 0d, 5151L, 18.0002, 30d, 72000L, 32.3454),
          (20d, 1d, 5549L, 18.0001, 20d, 72000L, 23.8948),
          (20d, 2d, 6013L, 18.0002, 10d, 72000L, 15.4799),
          (19d, 14d, 99999L, 19.0, 14d, 72000L, 19.00),
          (20d, 15d, 99999L, 20.0, 15d, 72000L, 20.00),
          (18d, 16d, 39550L, 20.0, 10d, 72000L, 17.03),
          (18d, 20d, 12113L, 20.0, 5d, 72000L, 11.8947),
          (18d, 25d, 6563L, 19.9998, 0d, 72000L, 7.436),
        )

      forAll(testCases) {
        (
            startingTemp: Double,
            qDotFirstPeriod: Double,
            firstTick: Long,
            expectedTemperatureFirstPeriod: Double,
            qDotSecondPeriod: Double,
            secondTick: Long,
            expectedTemperatureSecondPeriod: Double,
        ) =>
          val initialHouseState = startingState(house, ambientTemperature)
            .copy(innerTemperature = Celsius(startingTemp))
          val thermalHouseState = house.determineState(
            firstTick,
            initialHouseState,
            Kilowatts(qDotFirstPeriod),
          )

          thermalHouseState match {
            case ThermalHouseState(
                  tick,
                  _,
                  temperature,
                ) =>
              tick shouldBe firstTick
              temperature should approximate(
                Celsius(expectedTemperatureFirstPeriod)
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
                  _,
                  temperature,
                ) =>
              tick shouldBe secondTick
              temperature should approximate(
                Celsius(expectedTemperatureSecondPeriod)
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

    "Check if the same threshold is determined by different ways of simulation steps" in {
      val house = thermalHouse(18, 22)
      val ambientTemperature = Celsius(5)
      val initialHouseState = startingState(house, ambientTemperature)
        .copy(innerTemperature = Celsius(6))
      val qDot = Kilowatts(30)

      val inBetweenStateCaseA =
        house.determineState(3600, initialHouseState, qDot)
      val inBetweenStateCaseB =
        house.determineState(18000, initialHouseState, qDot)

      val finalThresholdCaseA =
        house.determineNextThreshold(inBetweenStateCaseA, qDot)
      val finalThresholdCaseB =
        house.determineNextThreshold(inBetweenStateCaseB, qDot)

      val finalThresholdCaseC = house.determineNextThreshold(
        initialHouseState,
        qDot,
      )

      val tolerance = 1d
      (finalThresholdCaseA, finalThresholdCaseB, finalThresholdCaseC) match {
        case (Some(thresholdA), Some(thresholdB), Some(thresholdC)) =>
          thresholdA.tick.doubleValue should approximate(
            thresholdB.tick.doubleValue
          )(tolerance)
          thresholdB.tick.doubleValue should approximate(
            thresholdC.tick.doubleValue
          )(tolerance)
          thresholdC shouldBe HouseTargetTemperatureReached(23732)
        case _ => fail("Could not match thresholds.")
      }
    }

    "Check for the correct next threshold of house with thermal feed in" in {
      val house = thermalHouse(18, 22)
      val ambientTemperature = Celsius(5d)
      val initialHouseState = startingState(house, ambientTemperature)

      val testCases: TableFor3[Double, Double, Option[ThermalThreshold]] =
        Table(
          ("currentInnerTemp", "newOperatingPoint", "expectedThreshold"),
          // some OperatingPoints not capable to heat the house sufficient
          (20d, 0d, Some(HouseTemperatureLowerBoundaryReached(5151))),
          (20d, 1d, Some(HouseTemperatureLowerBoundaryReached(5549))),
          (20d, 2d, Some(HouseTemperatureLowerBoundaryReached(6013))),
          (20d, 10d, Some(HouseTemperatureLowerBoundaryReached(18389))),
          // OperatingPoint that keeps the house in perfect balance
          (19d, 14d, None),
          (20d, 15d, None),
          // some OperatingPoints that increase the house inner temperature after some cooling down first
          (18d, 16d, Some(HouseTargetTemperatureReached(39550))),
          (18d, 20d, Some(HouseTargetTemperatureReached(12113))),
          (18d, 25d, Some(HouseTargetTemperatureReached(6563))),
        )

      forAll(testCases) {
        (
            currentInnerTemp: Double,
            newOp: Double,
            expectedThreshold: Option[ThermalThreshold],
        ) =>
          val newOperatingPoint = Kilowatts(newOp)
          val state =
            initialHouseState.copy(innerTemperature = Celsius(currentInnerTemp))

          val thresholdOption = house.determineNextThreshold(
            state,
            newOperatingPoint,
          )

          thresholdOption match {
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
