/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import edu.ie3.datamodel.models.result.connector.Transformer3WResult
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import tech.units.indriya.quantity.Quantities
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Success, Try}

private[listener] trait Transformer3wResultSupport {

  /** Case class to serve as a map key for unfulfilled three winding results
    *
    * @param model
    *   Unique identifier of the model
    * @param zdt
    *   Zoned date time, the result does belong to
    */
  final case class Transformer3wKey(
      model: UUID,
      zdt: ZonedDateTime,
  )

  /** Holding the result values of all three ports of a transformer
    *
    * @param a
    *   Port A
    * @param b
    *   Port B
    * @param c
    *   Port C
    */
  final case class AggregatedTransformer3wResult(
      a: Option[PartialTransformer3wResult.PortA],
      b: Option[PartialTransformer3wResult.PortB],
      c: Option[PartialTransformer3wResult.PortC],
  ) {

    /** Check, if the results can be consolidated
      *
      * @return
      *   true, if all results are at place
      */
    def ready: Boolean = Seq(a, b, c).forall(_.isDefined)

    /** Consolidate all partial results into one
      *
      * @return
      *   One consolidated [[Transformer3WResult]]
      */
    def consolidate: Try[Transformer3WResult] = (a, b, c) match {
      case (Some(aResult), Some(bResult), Some(cResult)) =>
        Success(
          new Transformer3WResult(
            aResult.time,
            aResult.input,
            Quantities.getQuantity(
              aResult.currentMagnitude.toAmperes,
              Units.AMPERE,
            ),
            Quantities.getQuantity(
              aResult.currentAngle.toDegrees,
              PowerSystemUnits.DEGREE_GEOM,
            ),
            Quantities.getQuantity(
              bResult.currentMagnitude.toAmperes,
              Units.AMPERE,
            ),
            Quantities.getQuantity(
              bResult.currentAngle.toDegrees,
              PowerSystemUnits.DEGREE_GEOM,
            ),
            Quantities.getQuantity(
              cResult.currentMagnitude.toAmperes,
              Units.AMPERE,
            ),
            Quantities.getQuantity(
              cResult.currentAngle.toDegrees,
              PowerSystemUnits.DEGREE_GEOM,
            ),
            aResult.tapPos,
          )
        )
      case _ =>
        Failure(
          new IllegalArgumentException(
            "Unable to consolidate three winding results, as not all results are at place."
          )
        )
    }

    def add(
        result: PartialTransformer3wResult
    ): Try[AggregatedTransformer3wResult] = {
      result match {
        case portA: PartialTransformer3wResult.PortA if a.isEmpty =>
          Success(copy(a = Some(portA)))
        case portB: PartialTransformer3wResult.PortB if b.isEmpty =>
          Success(copy(b = Some(portB)))
        case portC: PartialTransformer3wResult.PortC if c.isEmpty =>
          Success(copy(c = Some(portC)))
        case alreadySetPort =>
          Failure(
            new IllegalArgumentException(
              s"Already received a result for that port. Result, that caused the failure: $alreadySetPort, aggregated result: $this"
            )
          )
      }
    }
  }

  final object AggregatedTransformer3wResult {
    val EMPTY: AggregatedTransformer3wResult =
      AggregatedTransformer3wResult(None, None, None)
  }
}
