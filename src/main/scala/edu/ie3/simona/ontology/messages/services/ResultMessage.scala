package edu.ie3.simona.ontology.messages.services

import edu.ie3.datamodel.models.result.ResultEntity

sealed trait ResultMessage extends DataMessage

object ResultMessage {
  final case class ResultRequest(tick: Long)

  final case class ResultResponseMessage(result: ResultEntity) extends ResultMessage
}