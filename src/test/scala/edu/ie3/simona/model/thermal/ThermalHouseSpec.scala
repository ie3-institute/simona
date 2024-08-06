/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.HpInputTestData
import org.scalatest.prop.TableFor3
import squants.energy._
import squants.thermal._
import squants.time._
import squants.{Energy, Temperature}

class ThermalHouseSpec extends UnitSpec with HpInputTestData {

  implicit val tolerance: Temperature = Celsius(1e-4)
  implicit val energyTolerance: Energy = KilowattHours(1e-4)

  def buildThermalHouse(
      lowerBoundaryTemperature: Double,
      upperBoundaryTemperature: Double,
  ): ThermalHouse = {
    thermalHouse(lowerBoundaryTemperature, upperBoundaryTemperature)
  }

  "ThermalHouse" should {
    "Functions testing inner temperature work as expected" in {
      val thermalHouse = buildThermalHouse(18, 22)

      val testCases: TableFor3[Double, Boolean, Boolean] = Table(
        ("Inner Temperature (°C)", "Is Too High", "Is Too Low"),
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
        val isHigher = thermalHouse.isInnerTemperatureTooHigh(innerTemp)
        val isLower = thermalHouse.isInnerTemperatureTooLow(innerTemp)

        isHigher shouldBe isTooHigh
        isLower shouldBe isTooLow
      }
    }

    "Calculation of thermal energy change and new inner temperature is performed correctly" in {
      val thermalHouse = buildThermalHouse(18, 22)
      val innerTemperature = Temperature(20, Celsius)

      val thermalEnergyGain =
        thermalHouse.calcThermalEnergyGain(Kilowatts(100), Seconds(3600))
      val thermalEnergyLoss = thermalHouse.calcThermalEnergyLoss(
        innerTemperature,
        Temperature(10, Celsius),
        Seconds(3600),
      )
      val thermalEnergyChange = thermalHouse.calcThermalEnergyChange(
        thermalEnergyGain,
        thermalEnergyLoss,
      )
      val innerTemperatureChange =
        thermalHouse.calcInnerTemperatureChange(thermalEnergyChange)
      val newInnerTemperature = thermalHouse.calcNewInnerTemperature(
        innerTemperature,
        innerTemperatureChange,
      )

      thermalEnergyGain should approximate(KilowattHours(100))
      thermalEnergyLoss should approximate(KilowattHours(10))
      thermalEnergyChange should approximate(KilowattHours(90))
      innerTemperatureChange should approximate(Kelvin(9.0))
      newInnerTemperature should approximate(Celsius(29.0))
    }

    "Comprising function to calculate new inner temperature works as expected" in {
      val thermalHouse = buildThermalHouse(18, 22)
      val thermalPower = Kilowatts(100)
      val duration = Seconds(3600)
      val currentInnerTemperature = Temperature(20, Celsius)
      val ambientTemperature = Temperature(10, Celsius)

      val newInnerTemperature = thermalHouse.newInnerTemperature(
        thermalPower,
        duration,
        currentInnerTemperature,
        ambientTemperature,
      )

      newInnerTemperature should approximate(Temperature(29, Celsius))
    }

    "Check build method" in {

      val thermalHouse = buildThermalHouse(18, 22)
      val thermalHouseInput = defaultThermalHouse

      thermalHouse.id shouldBe thermalHouseInput.getId
      thermalHouse.operatorInput shouldBe thermalHouseInput.getOperator
      thermalHouse.operationTime shouldBe thermalHouseInput.getOperationTime
      thermalHouse.bus shouldBe thermalHouseInput.getThermalBus
      thermalHouse.ethLosses.toWattsPerKelvin shouldBe (thermalHouseInput.getEthLosses
        .to(StandardUnits.THERMAL_TRANSMISSION)
        .getValue
        .doubleValue * 1000)
      (thermalHouse.ethCapa * Temperature(
        1,
        Celsius,
      )).toKilowattHours shouldBe thermalHouseInput.getEthCapa
        .to(StandardUnits.HEAT_CAPACITY)
        .getValue
        .doubleValue
      thermalHouse.lowerBoundaryTemperature shouldBe Temperature(18, Celsius)
      thermalHouse.upperBoundaryTemperature shouldBe Temperature(22, Celsius)
    }
  }
}
