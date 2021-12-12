/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.system

import edu.ie3.datamodel.models.input.system.characteristic.CharacteristicPoint
import edu.ie3.simona.exceptions.CharacteristicsException
import edu.ie3.simona.model.system.Characteristic.XYPair
import edu.ie3.simona.util.CollectionUtils._
import tech.units.indriya.ComparableQuantity

import javax.measure.Quantity
import scala.collection.SortedSet
import scala.math.Ordered
import scala.math.Ordered.orderingToOrdered
import scala.reflect.ClassTag

/** Describes a mapping of a x-y-pairs with possibility to interpolate the y
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
      requestedAbscissaQuantity: ComparableQuantity[A]
  )(implicit
      tag: ClassTag[O]
  ): (ComparableQuantity[A], ComparableQuantity[O]) = {

    val xyCoords: Seq[(ComparableQuantity[A], ComparableQuantity[O])] =
      closestKeyValuePairs[A, O](
        xyCoordinates.toSeq
          .map(xyPair => xyPair.x -> xyPair.y)
          .toMap,
        requestedAbscissaQuantity
      )

    xyCoords.foldLeft(
      (None: Option[ComparableQuantity[A]], None: Option[ComparableQuantity[O]])
    )({
      case ((None, None), (x, y)) =>
        /* We found the latest entry left of the requested abscissa element. Remember that one */
        (Some(x), Some(y))
      case ((Some(leftX), Some(leftY)), (rightX, rightY)) =>
        /* We found the next entry right to the requested abscissa element and have the left one at hand as well. */
        val m = rightY
          .subtract(leftY)
          .divide(rightX.subtract(leftX))
        val b = leftY
        val deltaX = requestedAbscissaQuantity.subtract(leftX)
        (
          Some(requestedAbscissaQuantity),
          Some(
            m.multiply(deltaX)
              .asType(tag.runtimeClass.asInstanceOf[Class[O]])
              .add(b)
          )
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
      x: ComparableQuantity[A],
      y: ComparableQuantity[O]
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

  case object XYPair {
    def xYPairFromCharacteristicPoint[A <: Quantity[A], O <: Quantity[O]](
        point: CharacteristicPoint[A, O]
    ): XYPair[A, O] = new XYPair[A, O](point.getX, point.getY)
  }
}
