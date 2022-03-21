/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.marketoriented

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.simona.exceptions.{
  InitializationException,
  InvalidParameterException
}
import edu.ie3.simona.model.participant.evcs.PredictionAndSchedulingUtils.TimeStamp
import edu.ie3.simona.service.market.StaticMarketSource
import edu.ie3.util.quantities.interfaces.EnergyPrice
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.temporal.ChronoUnit
import java.time.{DayOfWeek, ZonedDateTime}

object MarketPricePrediction {

  val priceTimeTable: Vector[PriceTimeTableEntry] =
    calculateReferencePriceTimeTable(StaticMarketSource.prices)

  /** Entry in the price time table, which includes the history of price values
    * for a whole week.
    *
    * @param fromTimeStamp
    *   start of time frame
    * @param untilTimeStamp
    *   end of time frame
    * @param price
    *   price for this time frame
    */
  case class PriceTimeTableEntry(
      fromTimeStamp: TimeStamp,
      untilTimeStamp: TimeStamp,
      price: ComparableQuantity[EnergyPrice]
  )

  /** Predicted price for a specific time in the future.
    *
    * @param start
    *   start of time window
    * @param end
    *   end of time window
    * @param price
    *   predicted price in this time window
    */
  case class PredictedPrice(
      start: Long,
      end: Long,
      price: ComparableQuantity[EnergyPrice]
  )

  /** Calculate a time table for a reference week based on the known prices from
    * memory. Each known price value for a time window is weighted equally.
    *
    * @param prices
    *   known prices from history
    * @return
    *   vector of time table entries including the price value and the timeframe
    */
  private def calculateReferencePriceTimeTable(
      prices: Map[ZonedDateTime, ComparableQuantity[EnergyPrice]]
  ): Vector[PriceTimeTableEntry] = {
    val orderedPriceReferences = prices
      .map { case (zdt, price) =>
        val day = zdt.getDayOfWeek
        val hour = zdt.getHour

        TimeStamp(day, hour, 0) -> price
      }
      .groupBy(_._1)
      .map { case (timestamp, timeStampToPrice) =>
        val prices = timeStampToPrice.values
        val averagePrice =
          if (prices.isEmpty)
            throw new InitializationException(
              s"Unable to build reference prices for time stamp '$timestamp', as no prices are available."
            )
          else
            prices
              .foldLeft(
                Quantities.getQuantity(0d, StandardUnits.ENERGY_PRICE)
              ) { case (priceSum, price) =>
                priceSum.add(price)
              }
              .divide(prices.size)

        timestamp -> averagePrice
      }
      .toVector
      .sortBy(_._1)

    buildPriceTimeTable(orderedPriceReferences).toVector
  }

  private def buildPriceTimeTable(
      orderedPriceReferenceMap: Seq[
        (TimeStamp, ComparableQuantity[EnergyPrice])
      ]
  ): Seq[PriceTimeTableEntry] = {
    val priceTimeTable = orderedPriceReferenceMap.headOption
      .map { head =>
        /* Replicate the first element of the price map at the end */
        (orderedPriceReferenceMap :+ head).slice(
          1,
          orderedPriceReferenceMap.size + 1
        )
      }
      .map { rhs =>
        /* Build a sequence of sliding windows of the current and the next entry */
        orderedPriceReferenceMap.zip(rhs).map {
          case ((firstTimeStamp, price), (secondTimeStamp, _)) =>
            PriceTimeTableEntry(
              firstTimeStamp,
              secondTimeStamp,
              price
            )
        }
      }
      .getOrElse(Seq.empty[PriceTimeTableEntry])

    if (priceTimeTable.size != orderedPriceReferenceMap.size) {
      throw new InitializationException(
        "Error during building of price time table for prediction. Some values got lost."
      )
    } else {
      priceTimeTable
    }
  }

  /** Get time windows with predicted price values for the relevant time until
    * departure of the last EV based on the reference prices in the price time
    * table.
    *
    * @param currentTime
    *   current time to know where to start in the price time table
    * @param endTime
    *   the end time (e.g. departure time of the last ev) until which time the
    *   prices are required (can be max 7 days ahead of current time)
    * @param priceTimeTable
    *   the price time table with reference values for a whole week
    * @return
    *   vector of predicted prices with value and timeframe, ordered in time
    */
  def getPredictedPricesForRelevantTimeWindowBasedOnReferencePrices(
      currentTime: ZonedDateTime,
      endTime: ZonedDateTime,
      priceTimeTable: Vector[PriceTimeTableEntry],
      startTime: ZonedDateTime
  ): Vector[PredictedPrice] = {

    if (endTime.isAfter(currentTime.plusDays(7)))
      throw new InvalidParameterException(
        "Predicted prices can currently only be created up to one week ahead of the current time"
      )

    val currentTimeStamp =
      TimeStamp(
        currentTime.getDayOfWeek,
        currentTime.getHour,
        currentTime.getMinute
      )

    val currentPrice = getPriceTimeTableEntryThisTimeStampBelongsTo(
      currentTimeStamp,
      priceTimeTable
    )

    /* The price prediction for the current time frame the current time belongs to has to be added first,
     * afterwards the predictions until one week form current time are created based on the price time table.
     */

    PredictedPrice(
      startTime.until(
        currentTime,
        ChronoUnit.SECONDS
      ), // .plusMinutes(currentTimeStamp.minutesUntil(currentPrice.fromTimeStamp)),
      startTime.until(
        currentTime
          .plusMinutes(
            currentTimeStamp.minutesUntil(currentPrice.untilTimeStamp)
          )
          .minusSeconds(currentTime.getSecond),
        ChronoUnit.SECONDS
      ),
      currentPrice.price
    ) +: priceTimeTable
      /* entry that belongs to current time is added manually before, must be excluded here */
      .filter(currentPrice.fromTimeStamp != _.fromTimeStamp)
      .map(entry => {
        PredictedPrice(
          startTime.until(
            currentTime
              .plusMinutes(
                currentTimeStamp.minutesUntil(entry.fromTimeStamp)
              )
              .minusSeconds(currentTime.getSecond),
            ChronoUnit.SECONDS
          ),
          startTime.until(
            currentTime
              .plusMinutes(
                currentTimeStamp.minutesUntil(entry.untilTimeStamp)
              )
              .minusSeconds(currentTime.getSecond),
            ChronoUnit.SECONDS
          ),
          entry.price
        )
      })
      .filter(_.start < startTime.until(endTime, ChronoUnit.SECONDS))
      .sortBy { case PredictedPrice(start, _, _) =>
        start
      }
  }

  /** Find and return the price time table entry that belongs to a specific time
    * stamp. The time stamp must therefore be between the start and end time
    * stamp of the price time table entry.
    *
    * @param timeStamp
    *   the time stamp the time table entry should be returned
    * @param priceTimeTable
    *   the price time table
    * @return
    *   the price time table entry the time stamp belongs to
    */
  private def getPriceTimeTableEntryThisTimeStampBelongsTo(
      timeStamp: TimeStamp,
      priceTimeTable: Vector[PriceTimeTableEntry]
  ): PriceTimeTableEntry = priceTimeTable
    .find { case PriceTimeTableEntry(from, until, _) =>
      timeStamp.isBetween(from, until)
    }
    .getOrElse {
      throw new InvalidParameterException(
        s"Unable to get price for time stamp '$timeStamp'"
      )
    }
}
