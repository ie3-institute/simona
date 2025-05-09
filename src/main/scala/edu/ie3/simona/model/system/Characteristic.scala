/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.system

import edu.ie3.simona.exceptions.CharacteristicsException
import edu.ie3.simona.model.system.Characteristic.XYPair
import edu.ie3.simona.util.CollectionUtils._
import squants.Quantity

import scala.collection.SortedSet
import scala.reflect.ClassTag

/** Describes a mapping of an x-y-pairs with possibility to interpolate the y
  * values based on the provided x value
  */
trait Characteristic[A <: Quantity[A], O <: Quantity[O]] {

  protected val xyCoordinates: SortedSet[XYPair[A, O]]

  /** Interpolate values of a provided sequence of (x,y) tuple using the
    * provided x-y pairs sequence and the requested abscissa quantity
    *
    * @return
    *   the interpolated value (requestedX, interpolatedY)
    */
  def interpolateXy(
      requestedAbscissaQuantity: A
  )(implicit
      tag: ClassTag[O]
  ): (A, O) = {

    val xyCoords: Seq[(A, O)] =
      closestKeyValuePairs[A, O](
        xyCoordinates.toSeq
          .map(xyPair => xyPair.x -> xyPair.y)
          .toMap,
        requestedAbscissaQuantity,
      )

    xyCoords.foldLeft(
      (None: Option[A], None: Option[O])
    )({
      case ((None, None), (x, y)) =>
        /* We found the latest entry left of the requested abscissa element. Remember that one */
        (Some(x), Some(y))
      case ((Some(leftX), Some(leftY)), (rightX, rightY)) =>
        /* We found the next entry right to the requested abscissa element and have the left one at hand as well. */
        val m = (rightY - leftY).value / (rightX - leftX).value
        val b = leftY
        val deltaX = requestedAbscissaQuantity - leftX
        (
          Some(requestedAbscissaQuantity),
          Some(
            b.map(_ + (m * deltaX).value)
          ),
        )
      case _ =>
        throw new CharacteristicsException(
          s"Unable to interpolate for given xyCoordinates: $xyCoords"
        )
    }) match {
      case (Some(resX), Some(resY)) => (resX, resY)
      case (_, _) =>
        throw new CharacteristicsException(
          s"Unable to interpolate for given xyCoordinates: $xyCoords"
        )
    }
  }
}

object Characteristic {
  final case class XYPair[A <: Quantity[A], O <: Quantity[O]](
      x: A,
      y: O,
  ) extends Ordered[XYPair[A, O]] {

    /** The pairs are ordered by their x value first. If two pairs have the same
      * x value, the y values are compared.
      *
      * @param that
      *   pair to compare
      * @return
      */
    override def compare(that: XYPair[A, O]): Int = {
      val xCompare = x.compare(that.x)
      if (xCompare != 0)
        xCompare
      else
        y.compare(that.y)
    }
  }

}
