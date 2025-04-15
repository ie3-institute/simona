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
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import edu.ie3.util.scala.quantities.WattsPerKelvin
import org.scalatest.prop.TableFor3
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

    "Check for the correct state of house when ambient temperature changes" in {
      val house = thermalHouse(18, 22)
      val tick = 3600
      val ambientTemperature = Temperature(-20, Celsius)
      val initialHouseState = startingState(house)
      val lastAmbientTemperature = Temperature(15, Celsius)

      val (thermalHouseState, threshold) =
        house.updateState(
          tick,
          initialHouseState,
          ambientTemperature,
          lastAmbientTemperature,
          zeroKW,
        )

      thermalHouseState match {
        case ThermalHouseState(tick, temperature, qDot) =>
          tick shouldBe 3600L
          temperature should approximate(Kelvin(292.65))
          qDot shouldBe zeroKW
        case unexpected =>
          fail(s"Expected a thermalHouseState but got none $unexpected.")
      }
      threshold shouldBe Some(HouseTemperatureLowerBoundaryReached(4967))
    }

    "Check for the correct state of house when thermal in feed changes" in {
      val house = thermalHouse(18, 22)
      val tick = 3600
      val ambientTemperature = Temperature(10, Celsius)
      val initQDot = Kilowatts(5) // won't be sufficient to increase inner temp
      val initialHouseState = ThermalHouseState(0, Celsius(18.5), initQDot)
      val newQDot = Kilowatts(100) // should increase inner temp

      val (thermalHouseState, threshold) =
        house.updateState(
          tick,
          initialHouseState,
          ambientTemperature,
          ambientTemperature,
          newQDot,
        )

      thermalHouseState match {
        case ThermalHouseState(tick, temperature, qDot) =>
          tick shouldBe 3600L
          temperature should approximate(Celsius(18.15))
          qDot shouldBe newQDot
        case unexpected =>
          fail(s"Expected a thermalHouseState but got none $unexpected.")
      }
      threshold shouldBe Some(HouseTargetTemperatureReached(4325))
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
