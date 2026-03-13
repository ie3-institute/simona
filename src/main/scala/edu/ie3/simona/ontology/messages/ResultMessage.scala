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

  /** Message send to the [[edu.ie3.simona.service.results.ResultServiceProxy]]
    * to request results.
    *
    * @param requestedResults
    *   The uuids of the input models.
    * @param tick
    *   For which results are requested.
    * @param replyTo
    *   The actor that should receive the results.
    * @param sendUnchangedResults
    *   If [[true]] results that have not changed are included in the
    *   [[ResultResponse]].
    */
  final case class RequestResult(
      requestedResults: Seq[UUID],
      tick: Long,
      replyTo: ActorRef[ResultResponse],
      sendUnchangedResults: Boolean = false,
  )

  /** Response message that is sent to a listener or
    * [[edu.ie3.simona.service.results.ExtResultProvider]] as an answer to a
    * [[RequestResult]].
    * @param results
    *   Map: uuid to results.
    */
  final case class ResultResponse(results: Map[UUID, Iterable[ResultEntity]])
}
