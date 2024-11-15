/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result.plain

import java.util.UUID

/** Results that are sent out with Kafka and avro should use this trait and
  * corresponding implementing classes, since these give more control over
  * attribute types and naming, and they include sim run id. Plain result
  * objects can be created by [[PlainWriter]].
  */
sealed trait PlainResult

object PlainResult {

  /** Plain result class for [[edu.ie3.datamodel.models.result.NodeResult]].
    *
    * @param simRunId
    *   the simulation run id
    * @param time
    *   the current time, formatted by [[PlainWriter.createSimpleTimeStamp]]
    * @param inputModel
    *   the uuid of the model that created this event
    * @param vMag
    *   the voltage magnitude as a [[Double]] in
    *   [[edu.ie3.util.quantities.PowerSystemUnits#PU]]
    * @param vAng
    *   the voltage angle as a [[Double]] in
    *   [[edu.ie3.util.quantities.PowerSystemUnits#DEGREE_GEOM]]
    */
  final case class PlainNodeResult(
      simRunId: UUID,
      time: String,
      inputModel: UUID,
      vMag: Double,
      vAng: Double,
  ) extends PlainResult
}
