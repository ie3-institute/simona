/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.datamodel.models.result.ResultEntity
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

object ResultMessage {

  /** Message to request results.
    *
    * @param requestedResults
    *   The uuids of the input models.
    * @param tick
    *   For which results are requested.
    * @param replyTo
    *   The actor that should receive the results.
    */
  final case class RequestResult(
      requestedResults: Seq[UUID],
      tick: Long,
      replyTo: ActorRef[Response],
  )

  /** Trait that is extended by all responses to a [[RequestResult]].
    */
  sealed trait Response

  /** Response message that contains the requested results.
    * @param results
    *   Map: uuid to results.
    */
  final case class ResultResponse(results: Map[UUID, Iterable[ResultEntity]])
      extends Response
}
