/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.simona.event.ResultEvent
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

final case class RequestResult(
    requestedResults: Seq[UUID],
    tick: Long,
    replyTo: ActorRef[ResultEvent.Response],
)
