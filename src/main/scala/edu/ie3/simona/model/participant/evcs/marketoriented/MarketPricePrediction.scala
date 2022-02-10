/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.marketoriented

import edu.ie3.simona.exceptions.InvalidParameterException
import edu.ie3.simona.model.participant.evcs.PredictionAndSchedulingUtils.TimeStamp
import edu.ie3.simona.service.market.StaticMarketSource
import edu.ie3.util.quantities.interfaces.EnergyPrice
import tech.units.indriya.ComparableQuantity

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
      start: ZonedDateTime,
      end: ZonedDateTime,
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

    val priceReferenceMap: Map[
      TimeStamp,
      (ComparableQuantity[EnergyPrice], Int)
    ] = prices.foldLeft(
      Map.empty: Map[
        TimeStamp,
        (ComparableQuantity[EnergyPrice], Int)
      ]
    )(
      (
          referencePrices: Map[
            TimeStamp,
            (ComparableQuantity[EnergyPrice], Int)
          ],
          price
      ) => {

        val day: DayOfWeek = price._1.getDayOfWeek
        val hour: Int = price._1.getHour
        val minute: Int = 0 // -> round to hour

        referencePrices.get(TimeStamp(day, hour, minute)) match {

          case Some(entry) =>
            /* There already is an entry. Update the entry with equal weighting */
            val updatedNumberOfValues = entry._2 + 1
            val updatedPrediction = entry._1
              .multiply(entry._2)
              .add(price._2)
              .divide(updatedNumberOfValues)

            referencePrices + (TimeStamp(
              day,
              hour,
              minute
            ) -> (updatedPrediction, updatedNumberOfValues))

          case None =>
            /* There is no entry yet. Add new entry */
            referencePrices + (TimeStamp(
              day,
              hour,
              minute
            ) -> (price._2, 1))
        }
      }
    )

    /* Order price entries based on time stamp */
    type OrderedPriceEntry = (TimeStamp, (ComparableQuantity[EnergyPrice], Int))
    val orderedPriceReferenceMap = priceReferenceMap.toVector.sortBy(_._1)

    buildPriceTimeTable(orderedPriceReferenceMap).toVector
  }

  private def buildPriceTimeTable(
      orderedPriceReferenceMap: Seq[
        (TimeStamp, (ComparableQuantity[EnergyPrice], Int))
      ]
  ): Seq[PriceTimeTableEntry] = {
    orderedPriceReferenceMap.headOption
      .map { head =>
        /* Replicate the first element of the price map at the end */
        orderedPriceReferenceMap :+ head
      }
      .map { priceMap =>
        /* Build a sequence of sliding windows of the current and the next entry */
        val slidingWindows = priceMap
          .slice(0, priceMap.length)
          .zip(priceMap.slice(1, priceMap.length - 1))
        slidingWindows.map {
          case ((firstTimeStamp, (price, _)), (secondTimeStamp, _)) =>
            PriceTimeTableEntry(
              firstTimeStamp,
              secondTimeStamp,
              price
            )
        }
      }
      .getOrElse(Seq.empty[PriceTimeTableEntry])
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
      priceTimeTable: Vector[PriceTimeTableEntry]
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
      currentTime, // .plusMinutes(currentTimeStamp.minutesUntil(currentPrice.fromTimeStamp)),
      currentTime
        .plusMinutes(
          currentTimeStamp.minutesUntil(currentPrice.untilTimeStamp)
        )
        .minusSeconds(currentTime.getSecond),
      currentPrice.price
    ) +: priceTimeTable
      /* entry that belongs to current time is added manually before, must be excluded here */
      .filter(currentPrice.fromTimeStamp != _.fromTimeStamp)
      .foldLeft(Vector.empty[PredictedPrice])(
        (timeTable: Vector[PredictedPrice], entry: PriceTimeTableEntry) => {
          timeTable :+ PredictedPrice(
            currentTime
              .plusMinutes(
                currentTimeStamp.minutesUntil(entry.fromTimeStamp)
              )
              .minusSeconds(currentTime.getSecond),
            currentTime
              .plusMinutes(
                currentTimeStamp.minutesUntil(entry.untilTimeStamp)
              )
              .minusSeconds(currentTime.getSecond),
            entry.price
          )
        }
      )
      .filter(_.start.isBefore(endTime))
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
  ): PriceTimeTableEntry = {

    val it = priceTimeTable.iterator
    while (it.hasNext) {
      val entry: PriceTimeTableEntry = it.next()
      if (timeStamp.isBetween(entry.fromTimeStamp, entry.untilTimeStamp))
        return entry
    }
    throw new InvalidParameterException(
      "This shouldn't happen, the price time table must " +
        "cover all possible time stamps."
    )

  }

}
