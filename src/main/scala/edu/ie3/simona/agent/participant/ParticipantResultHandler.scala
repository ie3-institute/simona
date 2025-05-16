/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system.{
  FlexOptionsResult,
  SystemParticipantResult,
}
import edu.ie3.datamodel.models.result.thermal.ThermalUnitResult
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent,
  ThermalResultEvent,
}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.CriticalFailureException
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
  def maybeSend(result: ResultEntity): Unit =
    if config.simulationResultInfo then {
      listener.foreach(actor =>
        result match {
          case thermalResult: ThermalUnitResult =>
            actor ! ThermalResultEvent(thermalResult)
          case participantResult: SystemParticipantResult =>
            actor ! ParticipantResultEvent(participantResult)
          case unsupported =>
            throw new CriticalFailureException(
              s"Results of class '${unsupported.getClass.getSimpleName}' are currently not supported."
            )
        }
      )
    }

  /** Send the flex options result to all listeners, if enabled.
    *
    * @param result
    *   The [[FlexOptionsResult]].
    */
  def maybeSend(result: FlexOptionsResult): Unit =
    if config.flexResult then {
      listener.foreach(
        _ ! FlexOptionsResultEvent(result)
      )
    }

}
