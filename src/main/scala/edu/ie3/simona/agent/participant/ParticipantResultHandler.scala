/*
 * © 2025-2026. TU Dortmund University,
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
import edu.ie3.simona.service.results.ResultServiceProxy.{
  ExpectResult,
  NoResult,
}
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

/** Handles all kind of results stemming from the participant by sending them to
  * the result proxy, if applicable.
  *
  * @param resultProxy
  *   The actor reference to the result resultProxy.
  * @param config
  *   The result configuration.
  */
final case class ParticipantResultHandler(
    private val resultProxy: ActorRef[ResultEvent | ExpectResult | NoResult],
    private val config: NotifierConfig,
) {

  /** Send the participant result to all listeners, if enabled.
    *
    * @param result
    *   The [[SystemParticipantResult]].
    */
  def maybeSend(result: ResultEntity): Unit =
    if config.simulationResultInfo then {
      result match {
        case thermalResult: ThermalUnitResult =>
          resultProxy ! ThermalResultEvent(thermalResult)
        case participantResult: SystemParticipantResult =>
          resultProxy ! ParticipantResultEvent(participantResult)
        case unsupported =>
          throw new CriticalFailureException(
            s"Results of class '${unsupported.getClass.getSimpleName}' are currently not supported."
          )
      }
    }

  /** Send the flex options result to all listeners, if enabled.
    *
    * @param result
    *   The [[FlexOptionsResult]].
    */
  def maybeSend(result: FlexOptionsResult): Unit =
    if config.flexResult then {
      resultProxy ! FlexOptionsResultEvent(result)
    }

  def sendNoResult(uuid: UUID, tick: Long): Unit =
    resultProxy ! NoResult(uuid, tick)

  def informProxy(
      uuid: UUID,
      tick: Long,
      waitForSetPoint: Boolean = false,
  ): Unit =
    resultProxy ! ExpectResult(uuid, tick, waitForSetPoint)

}
