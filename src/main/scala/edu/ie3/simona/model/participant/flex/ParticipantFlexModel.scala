/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.flex

import edu.ie3.simona.model.participant.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
}
import edu.ie3.simona.ontology.messages.flex.FlexOptions
import edu.ie3.simona.service.DataTimeType
import squants.Power

/** Trait to be implemented by all flexibility models of [[ParticipantModel]].
  *
  * @tparam OP
  *   The type of operating point.
  * @tparam S
  *   The type of model state.
  */
trait ParticipantFlexModel[-OP <: OperatingPoint, -S <: ModelState] {

  /** Given the current state, this method determines the flexibility options
    * for the current tick. This usually means that the range of possible
    * operating points has be considered and subsequently distilled into a
    * [[FlexOptions]] message.
    *
    * @param state
    *   The current state.
    * @param dataTimeType
    *   The data time type to calculate flex options for.
    * @return
    *   The flexibility options.
    */
  def determineFlexOptions(state: S, dataTimeType: DataTimeType): FlexOptions

  /** Determines the [[OperationChangeIndicator]] for the current state and
    * operating point, i.e. the indication at which point in simulation time the
    * flexibility options change assuming the current operating point subsists.
    *
    * @param state
    *   The current state.
    * @param operatingPoint
    *   The current operating point.
    * @param setPower
    *   The power set point determined by EM.
    * @param dataTimeType
    *   The data time type.
    * @return
    *   The [[OperationChangeIndicator]].
    */
  def determineNextActivation(
      state: S,
      operatingPoint: OP,
      setPower: Power,
      dataTimeType: DataTimeType,
  ): OperationChangeIndicator

}
