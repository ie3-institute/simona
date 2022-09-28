/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event

import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  SwitchResult,
  Transformer2WResult
}
import edu.ie3.datamodel.models.result.system.{
  FlexOptionsResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult

sealed trait ResultEvent extends Event

/** Calculation result events
  */
object ResultEvent {

  /** Event that holds a calculation result of a
    * [[edu.ie3.simona.model.participant.SystemParticipant]]
    *
    * @param systemParticipantResult
    *   the calculation result
    */
  final case class ParticipantResultEvent(
      systemParticipantResult: SystemParticipantResult
  ) extends ResultEvent

  /** Event that holds all grid calculation results of a power flow calculation.
    * The usage of a type is necessary here, to avoid passing in other instances
    * of [[edu.ie3.datamodel.models.result.ResultEntity]] except of the wanted
    * ones
    *
    * @param nodeResults
    *   the power flow node results
    * @param switchResults
    *   the power flow switch results
    * @param lineResults
    *   the power flow line results
    * @param transformer2wResults
    *   the power flow results for two winding transformers
    * @param transformer3wResults
    *   the <b>partial</b> power flow results for three winding transformers
    */
  final case class PowerFlowResultEvent(
      nodeResults: Iterable[NodeResult],
      switchResults: Iterable[SwitchResult],
      lineResults: Iterable[LineResult],
      transformer2wResults: Iterable[Transformer2WResult],
      transformer3wResults: Iterable[PartialTransformer3wResult]
  ) extends ResultEvent

  /** Event that holds the flexibility options result of a
    * [[edu.ie3.simona.model.participant.SystemParticipant]]
    *
    * @param flexOptionsResult
    *   the flex options result
    */
  final case class FlexOptionsResultEvent(
      flexOptionsResult: FlexOptionsResult
  ) extends ResultEvent

}
