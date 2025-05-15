/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import edu.ie3.simona.exceptions.QuantityException
import squants.time.{Hours, TimeDerivative, TimeIntegral}
import squants.{Quantity, Seconds, UnitOfMeasure}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.function.Calculus
import tech.units.indriya.quantity.Quantities

import scala.collection.immutable.SortedMap
import scala.util.{Failure, Try}

object QuantityUtil {

  extension (d: Double) {

    /** Method to enable multiplying a [[Double]] with a [[Quantity]].
      * @param quantity
      *   The second factor.
      * @tparam Q
      *   Type of the quantity.
      * @return
      *   The resulting [[Quantity]].
      */
    def *[Q <: Quantity[Q]](quantity: Quantity[Q]): Q = quantity * d
  }

  /** The [[tech.units.indriya.function.DefaultNumberSystem]] is only covering
    * java [[Number]] children. As [[BigDecimal]] is not related to
    * [[java.math.BigDecimal]], this causes issues, why the
    * [[tech.units.indriya.spi.NumberSystem]] has to be used has to be specified
    * to something, that actually is able to handle the scala number system.
    */
  def adjustNumberSystem(): Unit =
    Calculus.setCurrentNumberSystem(
      Calculus.getNumberSystem(
        "edu.ie3.util.scala.quantities.ScalaNumberSystem"
      )
    )

  def zeroCompQuantity[Q <: javax.measure.Quantity[Q]](
      unit: javax.measure.Unit[Q]
  ): ComparableQuantity[Q] = Quantities.getQuantity(0, unit)

  def zero[Q <: Quantity[Q]](
      unit: UnitOfMeasure[Q]
  ): Q = unit(0d)

  /** Average given values over given tick window
    *
    * @param values
    *   Mapping from tick to respective value
    * @param windowStart
    *   First tick, that shall be included in the integral
    * @param windowEnd
    *   Last tick, that shall be included in the integral
    * @tparam Q
    *   Type of [[squants.Quantity]] that should be integrated
    * @tparam QI
    *   Type of [[squants.Quantity]] that will be the integral
    * @return
    *   Averaged quantity
    */
  def average[Q <: Quantity[Q] & TimeDerivative[QI], QI <: Quantity[
    QI
  ] & TimeIntegral[Q]](
      values: Map[Long, Q],
      windowStart: Long,
      windowEnd: Long,
  ): Try[Q] = {
    if windowStart == windowEnd then
      Failure(
        new IllegalArgumentException("Cannot average over trivial time window.")
      )
    else if windowStart > windowEnd then
      Failure(
        new IllegalArgumentException("Window end is before window start.")
      )
    else
      Try {
        integrate[Q, QI](
          values,
          windowStart,
          windowEnd,
        ) / Seconds(windowEnd - windowStart)
      }
  }

  /** Calculate the integration over provided values from window start until
    * window end
    *
    * @param values
    *   Mapping from tick to respective value
    * @param windowStart
    *   First tick, that shall be included in the integral
    * @param windowEnd
    *   Last tick, that shall be included in the integral
    * @tparam Q
    *   Type of [[Quantity]] that should be integrated
    * @tparam QI
    *   Type of [[Quantity]] that will be the integral
    * @return
    *   Integration over given values from window start to window end
    */
  def integrate[Q <: Quantity[Q] & TimeDerivative[QI], QI <: Quantity[
    QI
  ] & TimeIntegral[Q]](
      values: Map[Long, Q],
      windowStart: Long,
      windowEnd: Long,
  ): QI = {

    /** Case class to hold current state of integration
      *
      * @param currentIntegral
      *   Current state of the integral
      * @param lastTick
      *   Last tick, that has been visited
      * @param lastValue
      *   Value, that has been seen at the last tick
      */
    final case class IntegrationState(
        currentIntegral: QI,
        lastTick: Long,
        lastValue: Q,
    )

    val sortedValues = SortedMap.from(values)

    /* Determine the unit from the first best value */
    val unit = sortedValues.values.headOption
      .map(_.unit)
      .getOrElse(
        throw new QuantityException(
          "Unable to determine unit for dummy starting value."
        )
      )
    val zeroValue = unit(0d)

    /* the first relevant value for integration is placed before or at windowStart */
    val startValue = sortedValues
      .rangeUntil(windowStart + 1)
      .lastOption
      .map { case (_, value) =>
        value
      }
      .getOrElse(zeroValue)

    /* Filtering out values outside the specified time window.
       Excluding the value at first tick because the fold below starts with it.
       At the end, we add a dummy value (we only care about the ending tick).
     */
    val valuesWithinWindow = sortedValues.range(windowStart + 1, windowEnd) +
      (windowEnd -> zeroValue)

    /* Actually determining the integral, but sweeping over values and summing up everything */
    valuesWithinWindow
      .foldLeft(
        IntegrationState(
          zeroValue * Hours(0),
          windowStart,
          startValue,
        )
      ) {
        case (
              IntegrationState(currentIntegral, lastTick, lastValue),
              (tick, value),
            ) =>
          /* Calculate the partial integral over the last know value since it's occurrence and the instance when the newest value comes in */
          val duration = Seconds(tick - lastTick)
          val partialIntegral = lastValue * duration
          val updatedIntegral = currentIntegral + partialIntegral
          IntegrationState(updatedIntegral, tick, value)
      }
      .currentIntegral
  }

}
