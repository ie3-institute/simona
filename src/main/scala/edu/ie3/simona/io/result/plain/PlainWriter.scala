/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result.plain

import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.io.result.plain.PlainResult.PlainNodeResult
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

/** Converts a [[ResultEntity]] into a [[PlainResult]] and vice versa.
  * @tparam F
  *   the type of [[ResultEntity]]
  * @tparam P
  *   the type of [[PlainResult]]
  */
sealed trait PlainWriter[F <: ResultEntity, P <: PlainResult] {

  /** Converts a regular [[ResultEntity]] of type [[F]] into a [[PlainResult]]
    * of type [[P]]
    * @param full
    *   the [[ResultEntity]] to convert
    * @return
    *   the resulting [[PlainResult]]
    */
  def writePlain(full: F): P

  /** Converts a [[PlainResult]] of type [[P]] into a regular [[ResultEntity]]
    * of type [[F]]
    * @param plain
    *   the [[PlainResult]] to convert
    * @return
    *   the resulting [[ResultEntity]]
    */
  def createFull(plain: P): F
}

object PlainWriter {
  private lazy val timeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  /** Converts [[NodeResult]]s into [[PlainNodeResult]]s and vice versa
    * @param simRunId
    *   the simulation run id to use for plain results
    */
  final case class NodeResultWriter(simRunId: UUID)
      extends PlainWriter[NodeResult, PlainNodeResult] {

    override def writePlain(full: NodeResult): PlainNodeResult = {
      PlainNodeResult(
        simRunId,
        createSimpleTimeStamp(full.getTime),
        full.getInputModel,
        full.getvMag.getValue.doubleValue(),
        full.getvAng.getValue.doubleValue(),
      )
    }

    override def createFull(plain: PlainNodeResult): NodeResult = {
      new NodeResult(
        ZonedDateTime.parse(plain.time, timeFormatter),
        plain.inputModel,
        Quantities.getQuantity(plain.vMag, PowerSystemUnits.PU),
        Quantities.getQuantity(plain.vAng, PowerSystemUnits.DEGREE_GEOM),
      )
    }
  }

  def createSimpleTimeStamp(dateTime: ZonedDateTime): String =
    dateTime.format(timeFormatter)
}
