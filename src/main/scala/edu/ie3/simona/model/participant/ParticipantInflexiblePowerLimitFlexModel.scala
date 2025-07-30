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
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  PowerLimitFlexOptions,
}

/** Simple flexibility model for [[ParticipantModel]]s with
  * [[ActivePowerOperatingPoint]] returning flex options that do not allow for
  * any flexibility around the current operating point.
  *
  * @tparam S
  *   The type of model state.
  */
class ParticipantInflexiblePowerLimitFlexModel[S <: ModelState](
    private val model: ParticipantModel[?, S]
) extends ParticipantFlexModel[S] {

  override def determineFlexOptions(
      state: S
  ): FlexOptions = {
    val (operatingPoint, _) = model.determineOperatingPoint(state)

    PowerLimitFlexOptions.noFlexOption(operatingPoint.activePower)
  }

}
