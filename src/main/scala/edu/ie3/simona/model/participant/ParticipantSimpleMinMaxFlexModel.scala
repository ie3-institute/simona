/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelState,
}
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MinMaxFlexOptions}

/** Simple trait providing a flexibility model to [[ParticipantModel]]s with
  * [[ActivePowerOperatingPoint]]. It returns flex options that do not allow for
  * any flexibility around the current operating point.
  *
  * @tparam S
  *   The type of model state.
  */
class ParticipantSimpleMinMaxFlexModel[S <: ModelState](
    private val model: ParticipantModel[?, S]
) extends ParticipantFlexModel[S] {

  override def determineFlexOptions(
      state: S
  ): FlexOptions = {
    val (operatingPoint, _) = model.determineOperatingPoint(state)

    MinMaxFlexOptions.noFlexOption(operatingPoint.activePower)
  }

}
