/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result.plain

import java.util.UUID

case class NodeResultPlain(
    runId: UUID,
    dateTime: String,
    uuid: UUID,
    vMagPU: Double,
    vAngDeg: Double
) extends ResultPlain
