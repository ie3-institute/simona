/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.flex

import edu.ie3.simona.model.participant.ParticipantModel
import edu.ie3.simona.model.participant.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
}
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  PowerLimitFlexOptions,
}
import edu.ie3.simona.service.DataTimeType
import squants.Power

/** Simple flexibility model for [[ParticipantModel]]s with
  * [[edu.ie3.simona.model.participant.ParticipantModel.ActivePowerOperatingPoint]]
  * returning flex options that do not allow for any flexibility around the
  * current operating point.
  *
  * @tparam S
  *   The type of model state.
  */
class ParticipantInflexiblePowerLimitFlexModel[S <: ModelState](
    private val model: ParticipantModel[?, S]
) extends ParticipantFlexModel[OperatingPoint, S] {

  override def determineFlexOptions(
      state: S,
      dataTimeType: DataTimeType,
  ): FlexOptions = {
    val (operatingPoint, _) = model.determineOperatingPoint(state)

    PowerLimitFlexOptions.noFlexOption(operatingPoint.activePower)
  }

  override def determineNextActivation(
      state: S,
      operatingPoint: OperatingPoint,
      setPower: Power,
      dataTimeType: DataTimeType,
  ): ParticipantModel.OperationChangeIndicator = OperationChangeIndicator.empty

}
