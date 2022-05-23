/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result.plain

import java.util.UUID

sealed trait PlainResult

object PlainResult {
  final case class PlainNodeResult(
      simRunId: UUID,
      time: String,
      uuid: UUID,
      inputModel: UUID,
      vMag: Double,
      vAng: Double
  ) extends PlainResult
}
