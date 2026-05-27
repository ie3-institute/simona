/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import edu.ie3.simona.service.Data.SecondaryData.{
  WeatherData,
  SecondarySeriesData,
}
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import squants.motion.KilometersPerHour
import squants.thermal.Celsius

import scala.collection.immutable.SortedMap

trait WeatherTestData {

  protected val weatherData = WeatherData(
    dirIrr = WattsPerSquareMeter(500),
    diffIrr = WattsPerSquareMeter(50),
    temp = Celsius(20),
    windVel = KilometersPerHour(10),
    groundTempLvl1 = Some(Celsius(8)),
    groundTempLvl2 = Some(Celsius(-3)),
  )

  protected val weatherSeriesData = SecondarySeriesData(
    series = SortedMap(
      0L -> weatherData,
      3600L -> WeatherData(
        dirIrr = WattsPerSquareMeter(400),
        diffIrr = WattsPerSquareMeter(40),
        temp = Celsius(18),
        windVel = KilometersPerHour(15),
        groundTempLvl1 = Some(Celsius(15)),
        groundTempLvl2 = Some(Celsius(10)),
      ),
      7200L -> WeatherData(
        dirIrr = WattsPerSquareMeter(300),
        diffIrr = WattsPerSquareMeter(30),
        temp = Celsius(16),
        windVel = KilometersPerHour(12),
        groundTempLvl1 = Some(Celsius(5)),
        groundTempLvl2 = Some(Celsius(-5)),
      ),
    )
  )

}
