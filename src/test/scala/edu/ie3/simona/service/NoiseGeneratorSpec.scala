/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.simona.service.Data.SecondaryData.WeatherData
import edu.ie3.simona.service.DataTimeType.CurrentAndForecast
import edu.ie3.simona.test.common.UnitSpec
import squants.motion.KilometersPerHour
import squants.radio.WattsPerSquareMeter
import squants.thermal.Celsius
import squants.time.Hours

import scala.collection.mutable
import scala.collection.immutable

class NoiseGeneratorSpec extends UnitSpec {

  "A NoiseGenerator" should {

    "generate noise values and apply them to data" in {

      val currentTick = 7200L

      val forecastParams = CurrentAndForecast(Hours(6), Hours(1))
      val gen = NoiseGenerator(
        forecastParams = forecastParams,
        autoregression = 0.8d,
        seed = 1234L,
      ).update(currentTick)

      gen.noiseMap shouldBe mutable.SortedMap(
        7200L -> 0.14115907833078006,
        10800L -> 0.37372259414733083,
        14400L -> 0.9820612840170815,
        18000L -> 0.7058642153381252,
        21600L -> 0.201540442983696,
        25200L -> -0.4942978074998051,
        28800L -> -1.3250773424633955,
      )

      val sampleData = WeatherData(
        dirIrr = WattsPerSquareMeter(100),
        diffIrr = WattsPerSquareMeter(0),
        temp = Celsius(0),
        windVel = KilometersPerHour(0),
        groundTempLvl3 = Some(Celsius(0)),
        groundTempLvl4 = Some(Celsius(0)),
      )

      val series =
        forecastParams
          .ticks(currentTick)
          .map(_ -> sampleData)
          .to(immutable.SortedMap)

      val applyToData = (data: WeatherData, noise: Double) =>
        data.copy(dirIrr = data.dirIrr + WattsPerSquareMeter(noise * 10d))

      val result = gen.applyToSeries(series, applyToData)

      result shouldBe immutable.SortedMap(
        7200L -> sampleData.copy(dirIrr = WattsPerSquareMeter(100.0)),
        10800L -> sampleData.copy(dirIrr =
          WattsPerSquareMeter(100.62287099024554)
        ),
        14400L -> sampleData.copy(dirIrr =
          WattsPerSquareMeter(103.27353761339027)
        ),
        18000L -> sampleData.copy(dirIrr =
          WattsPerSquareMeter(103.52932107669062)
        ),
        21600L -> sampleData.copy(dirIrr =
          WattsPerSquareMeter(101.34360295322465)
        ),
        25200L -> sampleData.copy(dirIrr =
          WattsPerSquareMeter(95.88085160416829)
        ),
        28800L -> sampleData.copy(dirIrr =
          WattsPerSquareMeter(86.74922657536604)
        ),
      )

    }

  }

}
