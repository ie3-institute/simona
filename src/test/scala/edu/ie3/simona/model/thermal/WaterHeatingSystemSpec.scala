/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.simona.test.common.UnitSpec
import squants.{Energy, Volume}
import squants.energy.KilowattHours
import squants.space.Litres
import squants.thermal.Celsius

import java.time.ZonedDateTime

class WaterHeatingSystemSpec extends UnitSpec with ThermalHouseTestData {
  implicit val energyTolerance: Energy = KilowattHours(1e-6)
  implicit val volumeTolerance: Volume = Litres(0.01)

  "Testing the thermal house " when {

    "calculating thermal energy demand for heating water" should {

      val thermalEnergyDemandWater =
        PrivateMethod[Energy](
          Symbol("thermalEnergyDemandWater")
        )

      "calculate the thermal energy demand to heat up water correctly" in {

        val cases = Table(
          ("waterDemand", "startTemp", "endTemp", "expectedEnergy"),
          (1d, 1d, 2d, 0.00116),
          (1000d, -5d, 55d, 69.6),
          (20d, 20d, 30d, 0.232),
          (55d, 100d, 100d, 0d),
          (2500d, 30d, 65d, 101.5),
        )

        forAll(cases) { (waterDemand, startTemp, endTemp, expectedResult) =>
          val result =
            thermalHouse invokePrivate thermalEnergyDemandWater(
              Litres(waterDemand),
              Celsius(startTemp),
              Celsius(endTemp),
            )

          result should approximate(KilowattHours(expectedResult))
        }

      }

      "throw an exception if end temperature is lower than start temperature" in {
        val waterDemand = Litres(100)
        val startTemp = Celsius(60)
        val endTemp = Celsius(20)

        intercept[RuntimeException] {
          thermalHouse invokePrivate thermalEnergyDemandWater(
            waterDemand,
            startTemp,
            endTemp,
          )
        }.getMessage shouldBe "End temperature of 20.0°C is lower than the start temperature 60.0°C for the water heating system."
      }

      "waterDemandOfHour" should {
        val waterDemandOfHour =
          PrivateMethod[Volume](
            Symbol("waterDemandOfHour")
          )

        "calculate the water demand correctly for a given hour" in {
          val simulationStart = ZonedDateTime.parse("2024-01-01T00:00:00Z")
          val cases = Table(
            ("tick", "housingType", "noPersons", "expectedVolume"),
            (0L, "house", 0d, 0d),
            (0L, "house", 1d, 0.64602),
            (1800L, "house", 1d, 0.64602),
            (1800L, "house", 2d, 1.29205),
            (3600L, "house", 1d, 0.358904),
            (10800L, "house", 4d, 0.43068),
            (28800L, "house", 4d, 9.76219),
            (86400L, "house", 2d, 1.29204),
            (0L, "flat", 0d, 0d),
            (0L, "flat", 1d, 0.3589),
            (1800L, "flat", 1d, 0.3589),
            (1800L, "flat", 2d, 0.7178),
            (3600L, "flat", 1d, 0.3589),
            (10800L, "flat", 4d, 0d),
            (28800L, "flat", 4d, 11.48493),
            (86400L, "flat", 2d, 0.7178),
          )

          forAll(cases) { (tick, housingType, noPersons, expectedResult) =>
            val demand = thermalHouse invokePrivate waterDemandOfHour(
              tick,
              simulationStart,
              noPersons,
              housingType,
            )
            val expected = Litres(expectedResult)
            demand should approximate(expected)
          }
        }

      }
    }
  }
}
