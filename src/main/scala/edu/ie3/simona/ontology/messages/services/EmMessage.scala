/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  FlexResponse,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.DataResponseMessage
import org.apache.pekko.actor.typed.ActorRef
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID

sealed trait EmMessage

object EmMessage {

  trait EmResponseMessage extends EmMessage with DataResponseMessage

  final case class SimonaFlexOptionsResponse(
      receiver: ActorRef[FlexResponse],
      flexOptions: FlexOptionsResult,
  ) extends EmResponseMessage

  final case class IssueFlexControlResponse(
      receiver: ActorRef[FlexRequest],
      time: ZonedDateTime,
      model: UUID,
      setPoint: Option[Power],
  ) extends EmResponseMessage

}
