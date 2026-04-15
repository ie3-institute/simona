/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.flex

import edu.ie3.simona.model.participant.ParticipantModel.{
  FixedState,
  OperatingPoint,
}
import edu.ie3.simona.model.participant.ParticipantModel
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexOptions,
}
import edu.ie3.simona.service.DataTimeType

/** Flex model implementation for [[ParticipantModel]]s with fixed state
  * producing [[EnergyBoundariesFlexOptions]] that assume a constant power
  * within the forecast horizon.
  *
  * @param model
  *   The participant model to create energy boundary flex options for.
  */
class ParticipantConstantEnergyLimitFlexModel(
    model: ParticipantModel[?, FixedState]
) extends AbstractEnergyBoundariesFlexModel[FixedState] {

  override val hasEnergyFlexibility: Boolean = false

  override def determineFlexOptions(
      state: FixedState,
      dataTimeType: DataTimeType,
  ): FlexOptions = {

    val (op: OperatingPoint, _) = model.determineOperatingPoint(state)

    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(op.activePower, state.tick)
    )
  }

}
