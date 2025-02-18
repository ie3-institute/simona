/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.datamodel.models.result.system.{
  FlexOptionsResult,
  SystemParticipantResult,
}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent,
}
import edu.ie3.simona.event.notifier.NotifierConfig
import org.apache.pekko.actor.typed.ActorRef

/** Handles all kind of results stemming from the participant by sending them to
  * the result listener, if applicable.
  *
  * @param listener
  *   The actor reference to the result listener.
  * @param config
  *   The result configuration.
  */
final case class ParticipantResultHandler(
    private val listener: Iterable[ActorRef[ResultEvent]],
    private val config: NotifierConfig,
) {

  /** Send the participant result to all listeners, if enabled.
    *
    * @param result
    *   The [[SystemParticipantResult]].
    */
  def maybeSend(result: SystemParticipantResult): Unit =
    if (config.simulationResultInfo) {
      listener.foreach(
        _ ! ParticipantResultEvent(result)
      )
    }

  /** Send the flex options result to all listeners, if enabled.
    *
    * @param result
    *   The [[FlexOptionsResult]].
    */
  def maybeSend(result: FlexOptionsResult): Unit =
    if (config.flexResult) {
      listener.foreach(
        _ ! FlexOptionsResultEvent(result)
      )
    }

}
