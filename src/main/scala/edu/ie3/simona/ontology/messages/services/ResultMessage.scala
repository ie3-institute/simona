/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.ontology.messages.PowerMessage

sealed trait ResultMessage extends DataMessage

object ResultMessage {
  final case class ResultResponseMessage(
                                          result: ResultEntity,
                                          nextTick: Option[Long]
                                        )
      extends ResultMessage
  final case class ResultRequestMessage(
                                         currentTick: Long
                                       ) extends ResultMessage
}
