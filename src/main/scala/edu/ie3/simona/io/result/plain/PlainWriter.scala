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

sealed trait PlainWriter[F <: ResultEntity, P <: PlainResult] {
  def writePlain(full: F): P

  def createFull(plain: P): F
}

object PlainWriter {
  private lazy val timeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  case class NodeResultWriter(simRunId: UUID)
      extends PlainWriter[NodeResult, PlainNodeResult] {

    override def writePlain(full: NodeResult): PlainNodeResult = {
      PlainNodeResult(
        simRunId,
        createSimpleTimeStamp(full.getTime),
        full.getUuid,
        full.getInputModel,
        full.getvMag.getValue.doubleValue(),
        full.getvAng.getValue.doubleValue()
      )
    }

    override def createFull(plain: PlainNodeResult): NodeResult = {
      new NodeResult(
        ZonedDateTime.parse(plain.time),
        plain.uuid,
        Quantities.getQuantity(plain.vMag, PowerSystemUnits.PU),
        Quantities.getQuantity(plain.vAng, PowerSystemUnits.DEGREE_GEOM)
      )
    }
  }

  def createSimpleTimeStamp(dateTime: ZonedDateTime): String =
    dateTime.format(timeFormatter)
}
