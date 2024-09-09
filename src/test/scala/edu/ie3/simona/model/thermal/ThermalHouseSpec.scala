/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.HouseTemperatureLowerBoundaryReached
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
        (20d, false, false),
        (22d, true, false),
        (22.02d, true, false),
        (23d, true, false),
      )

      testCases.foreach { case (innerTemperature, isTooHigh, isTooLow) =>
        val innerTemp = Temperature(innerTemperature, Celsius)
        val isHigher = thermalHouseTest.isInnerTemperatureTooHigh(innerTemp)
        val isLower = thermalHouseTest.isInnerTemperatureTooLow(innerTemp)

        isHigher shouldBe isTooHigh
        isLower shouldBe isTooLow
      }
    }

    "Calculation of thermal energy change and new inner temperature is performed correctly" in {
      val thermalHouseTest = thermalHouse(18, 22)
      val innerTemperature = Temperature(20, Celsius)

      val thermalEnergyGain = KilowattHours(100)
      val thermalEnergyLoss = thermalHouseTest.ethLosses.calcThermalEnergyChange(
        innerTemperature,
        Temperature(10, Celsius),
        Seconds(3600),
      )
      val thermalEnergyChange = thermalEnergyGain - thermalEnergyLoss

      val innerTemperatureChange = thermalEnergyChange/thermalHouseTest.ethCapa

      val newInnerTemperature = innerTemperature + innerTemperatureChange


      thermalEnergyGain should approximate(KilowattHours(100))
      thermalEnergyLoss should approximate(KilowattHours(10))
      thermalEnergyChange should approximate(KilowattHours(90))
      innerTemperatureChange should approximate(Kelvin(9.0))
      newInnerTemperature should approximate(Celsius(29.0))
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
      val initialHousestate = startingState(house)
      val lastAmbientTemperature = Temperature(15, Celsius)
      val ambientTemperature = Temperature(-20, Celsius)

      val (thermalHouseState, threshold) = house.determineState(
        3600L,
        initialHousestate,
        lastAmbientTemperature,
        ambientTemperature,
        zeroKW,
      )

      thermalHouseState match {
        case ThermalHouseState(tick, temperature, qDot) =>
          tick shouldBe 3600L
          temperature should approximate(Kelvin(292.64986111))
          qDot shouldBe zeroKW
        case unexpected =>
          fail(s"Expected a thermalHouseState but got none $unexpected.")
      }
      threshold shouldBe Some(HouseTemperatureLowerBoundaryReached(4967))
    }

    "Check build method" in {

      val thermalTestHouse = thermalHouse(18, 22)
      val thermalHouseInput = defaultThermalHouse

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
