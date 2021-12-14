/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.marketoriented

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.model.participant.evcs.marketoriented.MarketPricePrediction.{
  PredictedPrice,
  getPredictedPricesForRelevantTimeWindowBasedOnReferencePrices,
  priceTimeTable
}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.interfaces.EnergyPrice
import edu.ie3.util.quantities.PowerSystemUnits.EURO_PER_KILOWATTHOUR
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.math.Ordering.Implicits.infixOrderingOps

object MarketOrientedCurrentPrice extends LazyLogging {

  /** Calculate price signal between 0 and 1 based on the average energy price
    * from current time in time length and the maximum and minimum prices in a
    * reference time (currently: next 24 hours).
    * @param currentTick
    *   the current tick the price signal is requested
    * @param startTime
    *   start time of the simulation to convert ticks to real times
    * @param timeLengthInSeconds
    *   time length of the interval to calculate the price signal for
    * @return
    *   optional price signal between 0 and 1
    */
  def calculateCurrentPriceMarketOriented(
      currentTick: Long,
      startTime: ZonedDateTime,
      timeLengthInSeconds: Int
  ): Option[Double] = {

    val currentTime = currentTick.toDateTime(startTime)
    val endTime = currentTime.plusSeconds(timeLengthInSeconds)

    /* Get time windows with predicted energy prices for the relevant time */
    val predictedPricesForTimeLength: Vector[PredictedPrice] =
      getPredictedPricesForRelevantTimeWindowBasedOnReferencePrices(
        currentTime,
        currentTime.plusSeconds(timeLengthInSeconds),
        priceTimeTable
      )

    /* Get time windows with predicted energy prices for next days as reference value */
    val predictedPricesForReferenceTimeFrame: Vector[PredictedPrice] =
      getPredictedPricesForRelevantTimeWindowBasedOnReferencePrices(
        currentTime,
        currentTime.plusDays(1), // must be <(=?) 7 days
        priceTimeTable
      )

    val maxPriceOfReferenceTimeFrame: Option[PredictedPrice] =
      predictedPricesForReferenceTimeFrame.maxByOption(predictedPrice =>
        predictedPrice.price
      )
    val minPriceOfReferenceTimeFrame: Option[PredictedPrice] =
      predictedPricesForReferenceTimeFrame.minByOption(predictedPrice =>
        predictedPrice.price
      )

    /* Calculate average predicted price in time length */

    val averagePriceOfTimeLength: ComparableQuantity[EnergyPrice] =
      predictedPricesForTimeLength
        .foldLeft(Quantities.getQuantity(0, EURO_PER_KILOWATTHOUR))(
          (priceSum: ComparableQuantity[EnergyPrice], entry: PredictedPrice) =>
            {
              priceSum.add(
                entry.price.multiply(
                  entry.start
                    .until(entry.end.min(endTime), ChronoUnit.SECONDS)
                    .longValue()
                )
              )
            }
        )
        .divide(timeLengthInSeconds)

    (maxPriceOfReferenceTimeFrame, minPriceOfReferenceTimeFrame) match {
      case (Some(max), Some(min)) =>
        if (max.price.isGreaterThan(min.price)) {
          /* Calculate average price relative from min to max price in reference time frame */
          Some(
            averagePriceOfTimeLength
              .subtract(min.price)
              .divide(max.price.subtract(min.price))
              .getValue
              .doubleValue()
          )
        } else {
          /* All prices are the same in this period */
          None
        }
      case _ =>
        None
    }

  }

}
