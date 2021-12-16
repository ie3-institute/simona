/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.marketprice

import edu.ie3.util.TimeUtil
import tech.units.indriya.ComparableQuantity

import java.io.File
import java.time.{ZoneId, ZonedDateTime}
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import tech.units.indriya.quantity.Quantities.getQuantity
import edu.ie3.util.quantities.PowerSystemUnits.{
  EURO_PER_KILOWATTHOUR,
  EURO_PER_MEGAWATTHOUR
}
import edu.ie3.util.quantities.interfaces.EnergyPrice

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object StaticMarketSource {

  // TODO: NSteffan: Of course this is horrible implementation, just for testing in master thesis :D
  //  In the long run, to use market based strategies for EvcsAgent, a market environment service must replace this.
  private val marketPriceFilePath =
    s"${Paths.get("").toAbsolutePath.toString}${File.separator}inputData${File.separator}market_price${File.separator}market_2025_30.csv"

  private val Regex: Regex =
    """(\d\d)/(\d\d)/(\d\d\d\d) (\d\d):(\d\d):(\d\d);(-?\d+(?:[.]\d+)?)""".r.unanchored

  private val bufferedSource: BufferedSource =
    Source.fromFile(marketPriceFilePath)

  private val marketPrices
      : Map[ZonedDateTime, ComparableQuantity[EnergyPrice]] =
    bufferedSource
      .getLines()
      .drop(1)
      .foldLeft(Map.empty[ZonedDateTime, ComparableQuantity[EnergyPrice]])(
        (
            priceMap: Map[ZonedDateTime, ComparableQuantity[EnergyPrice]],
            entry
        ) => {

          entry match {
            case Regex(day, month, year, hour, minute, second, price) =>
              val time = s"$year-$month-$day $hour:$minute:$second"
              Try {
                (
                  TimeUtil.withDefaults
                    .toZonedDateTime(time)
                    .withZoneSameInstant(ZoneId.of("UTC")),
                  getQuantity(
                    price.toDouble,
                    EURO_PER_MEGAWATTHOUR
                  ).to(EURO_PER_KILOWATTHOUR)
                )
              } match {
                case Failure(ex) =>
                  priceMap
                case Success((time, price)) =>
                  priceMap + (time -> price)
              }

            case _ =>
              throw new IllegalArgumentException(
                s"Invalid input string does not match regex: $entry"
              )
          }

        }
      )

  def getMarketPrices: Map[ZonedDateTime, ComparableQuantity[EnergyPrice]] =
    marketPrices

  def getPrice(time: ZonedDateTime): ComparableQuantity[EnergyPrice] = {
    val roundedTime =
      time
        .minusMinutes(time.getMinute)
        .minusSeconds(time.getSecond)
        .plusYears(
          9
        ) // to get prices for 2025 when simulating in 2016 (SimBench time series are for 2016)
    marketPrices.getOrElse(
      roundedTime,
      throw new RuntimeException(
        s"Error, no price for this time found: $roundedTime"
      )
    )
  }

}
