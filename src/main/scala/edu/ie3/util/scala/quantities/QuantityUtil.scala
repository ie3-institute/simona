/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import edu.ie3.simona.exceptions.QuantityException
import edu.ie3.util.quantities.{QuantityUtil => PSQuantityUtil}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.function.Calculus
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import javax.measure.Quantity
import scala.collection.mutable
import scala.util.{Failure, Try}

object QuantityUtil {

  /** The [[tech.units.indriya.function.DefaultNumberSystem]] is only covering
    * java [[Number]] children. As [[BigDecimal]] is not related to
    * [[java.math.BigDecimal]], this causes issues, why the
    * [[tech.units.indriya.spi.NumberSystem]] has to be to be used has to be
    * specified to something, that actually is able to handle the scala number
    * system.
    */
  def adjustNumberSystem(): Unit =
    Calculus.setCurrentNumberSystem(
      Calculus.getNumberSystem(
        "edu.ie3.util.scala.quantities.ScalaNumberSystem"
      )
    )

  def zero[Q <: Quantity[Q]](
      unit: javax.measure.Unit[Q]
  ): ComparableQuantity[Q] = Quantities.getQuantity(0, unit)

  def add[Q <: Quantity[Q]](
      quantities: Iterable[Quantity[Q]]
  ): Option[Quantity[Q]] = {
    if (quantities.nonEmpty) {
      val iterator = quantities.iterator
      val sum = iterator.next()
      while (iterator.hasNext) {
        val next = iterator.next()
        if (!PSQuantityUtil.isEmpty(next)) sum.add(next)
      }
      Some(sum)
    } else None
  }

  implicit class ConvertibleQuantity[Q <: Quantity[Q]](
      private val q: Quantity[Q]
  ) extends AnyVal {

    /** Converts the quantity to an instance of [[ComparableQuantity]]
      *
      * @return
      *   the provided quantity as comparable quantity
      */
    def asComparable: ComparableQuantity[Q] = PSQuantityUtil.asComparable(q)
  }

  /** Average given values over given tick window
    *
    * @param values
    *   Mapping from tick to respective value
    * @param windowStart
    *   First tick, that shall be included in the integral
    * @param windowEnd
    *   Last tick, that shall be included in the integral
    * @param integrationQuantityClass
    *   Class of [[ComparableQuantity]] that will evolve from integration
    * @param integrationUnit
    *   Unit to use for the integral
    * @param averagingQuantityClass
    *   Class of [[ComparableQuantity]] that will evolve from averaging
    * @param averagingUnit
    *   Target unit of averaged quantities
    * @tparam Q
    *   Type of [[ComparableQuantity]] that should be integrated
    * @tparam QI
    *   Type of [[ComparableQuantity]] that will be the integral
    * @return
    *   Averaged quantity
    */
  def average[Q <: Quantity[Q], QI <: Quantity[QI]](
      values: Map[Long, ComparableQuantity[Q]],
      windowStart: Long,
      windowEnd: Long,
      integrationQuantityClass: Class[QI],
      integrationUnit: javax.measure.Unit[QI],
      averagingQuantityClass: Class[Q],
      averagingUnit: javax.measure.Unit[Q]
  ): Try[ComparableQuantity[Q]] = {
    if (windowStart == windowEnd)
      Failure(
        new IllegalArgumentException("Cannot average over trivial time window.")
      )
    else if (windowStart > windowEnd)
      Failure(
        new IllegalArgumentException("Window end is before window start.")
      )
    else
      Try {
        integrate(
          values,
          windowStart,
          windowEnd,
          integrationQuantityClass,
          integrationUnit
        ).divide(Quantities.getQuantity(windowEnd - windowStart, Units.SECOND))
          .asType(averagingQuantityClass)
          .to(averagingUnit)
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
    * @param integrationQuantityClass
    *   Class of [[ComparableQuantity]] that will evolve from integration
    * @param integrationUnit
    *   Unit to use for the integral
    * @tparam Q
    *   Type of [[ComparableQuantity]] that should be integrated
    * @tparam QI
    *   Type of [[ComparableQuantity]] that will be the integral
    * @return
    *   Integration over given values from window start to window end
    */
  def integrate[Q <: Quantity[Q], QI <: Quantity[QI]](
      values: Map[Long, ComparableQuantity[Q]],
      windowStart: Long,
      windowEnd: Long,
      integrationQuantityClass: Class[QI],
      integrationUnit: javax.measure.Unit[QI]
  ): ComparableQuantity[QI] = {

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
        currentIntegral: ComparableQuantity[QI],
        lastTick: Long,
        lastValue: ComparableQuantity[Q]
    )

    /* Determine the starting and ending value for the integral */
    val startValue = startingValue(values, windowStart)
    val (lastTick, lastValue) = endingValue(values, windowEnd)
    val valuesWithinWindow = mutable.LinkedHashMap.newBuilder
      .addAll(
        (values filter { case (tick, _) =>
          tick >= windowStart && tick <= windowEnd
        }).toSeq
          .sortBy(_._1)
      )
      .result()

    /* We need a value at the window end, so if the last value is not exactly there, replicate it at that point */
    if (windowEnd > lastTick)
      valuesWithinWindow.addOne(windowEnd -> lastValue)

    /* Actually determining the integral, but sweeping over values and summing up everything */
    valuesWithinWindow
      .foldLeft(
        IntegrationState(
          Quantities.getQuantity(0d, integrationUnit),
          windowStart,
          startValue
        )
      ) {
        case (
              IntegrationState(currentIntegral, lastTick, lastValue),
              (tick, value)
            ) =>
          /* Calculate the partial integral over the last know value since it's occurrence and the instance when the newest value comes in */
          val duration = Quantities.getQuantity(tick - lastTick, Units.SECOND)
          val partialIntegral =
            lastValue.multiply(duration).asType(integrationQuantityClass)
          val updatedIntegral = currentIntegral.add(partialIntegral)

          IntegrationState(updatedIntegral, tick, value)
      }
      .currentIntegral
      .to(integrationUnit)
  }

  /** Determine the starting value for the integration
    *
    * @param values
    *   Mapping of ticks to values
    * @param windowStart
    *   Tick, where the integration window starts
    * @tparam Q
    *   Type of quantity to account for
    * @return
    *   Either the first value <b>before</b> the window starts or 0, if not
    *   apparent
    */
  private def startingValue[Q <: Quantity[Q]](
      values: Map[Long, ComparableQuantity[Q]],
      windowStart: Long
  ): ComparableQuantity[Q] = {
    values
      .filter { case (tick, _) =>
        tick <= windowStart
      }
      .maxOption[(Long, ComparableQuantity[Q])](Ordering.by(_._1)) match {
      case Some((_, value)) => value
      case None =>
        val unit = values.headOption
          .map(_._2.getUnit)
          .getOrElse(
            throw new QuantityException(
              "Unable to determine unit for dummy starting value."
            )
          )
        Quantities.getQuantity(0d, unit)
    }
  }

  /** Determine the last value for the integration
    *
    * @param values
    *   Mapping of ticks to values
    * @param windowEnd
    *   Tick, where the integration window ends
    * @tparam Q
    *   Type of quantity to account for
    * @return
    *   Last entry before the integration window ends and it's corresponding
    *   tick
    */
  private def endingValue[Q <: Quantity[Q]](
      values: Map[Long, ComparableQuantity[Q]],
      windowEnd: Long
  ): (Long, ComparableQuantity[Q]) = {
    values
      .filter { case (tick, _) =>
        tick <= windowEnd
      }
      .maxOption[(Long, ComparableQuantity[Q])](Ordering.by(_._1)) match {
      case Some(tickToValue) => tickToValue
      case None =>
        throw new QuantityException(
          "Cannot integrate over an empty set of values."
        )
    }
  }

}
