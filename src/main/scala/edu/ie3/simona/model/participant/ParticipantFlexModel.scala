/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.model.participant.ParticipantModel.ModelState
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, FlexType}

trait ParticipantFlexModel[S <: ModelState] {

  /** Given the current state, this method determines the flexibility options
    * for the current tick. This usually means that the range of possible
    * operating points has be considered and subsequently distilled into a
    * [[FlexOptions]] message.
    *
    * @param state
    *   The current state.
    * @return
    *   The flexibility options.
    */
  def determineFlexOptions(state: S): FlexOptions

}
