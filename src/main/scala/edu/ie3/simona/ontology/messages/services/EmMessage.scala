/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  FlexResponse,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.DataResponseMessage
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

sealed trait EmMessage

object EmMessage {

  trait EmResponseMessage extends EmMessage with DataResponseMessage

  final case class WrappedFlexResponse(
      flexResponse: FlexResponse,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  ) extends EmResponseMessage

  final case class WrappedFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  ) extends EmResponseMessage

}
