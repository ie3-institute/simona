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

  def createSimpleTimeStamp(dateTime: ZonedDateTime): String = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    dateTime.format(formatter)
  }
}

object PlainWriter{

  case class NodeResultWriter(runId: UUID)
    extends PlainWriter[NodeResult, PlainNodeResult] {

    override def writePlain(full: NodeResult): PlainNodeResult = {
      PlainNodeResult(
        runId,
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
}