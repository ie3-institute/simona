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
}
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexOptions,
}
import edu.ie3.simona.service.DataTimeType

import scala.collection.immutable.SortedMap

/** Flex model implementation for [[ParticipantModel]]s producing
  * [[EnergyBoundariesFlexOptions]] based on a forecast of power values.
  *
  * @param model
  *   The participant model to create forecast series for.
  * @param determineStates
  *   A function creating the necessary states for the forecast, given the
  *   current state.
  * @tparam S
  *   The type of state of the participant model.
  */
class ParticipantInflexibleEnergyLimitFlexModel[S <: ModelState](
    model: ParticipantModel[?, S],
    determineStates: S => SortedMap[Long, S],
) extends AbstractEnergyBoundariesFlexModel[S] {

  override def determineFlexOptions(
      state: S,
      dateTimeType: DataTimeType,
  ): FlexOptions = {

    val powerMap = determineStates(state).map { case (tick, tickState) =>
      val (op: OperatingPoint, _) = model.determineOperatingPoint(tickState)
      tick -> op.activePower
    }

    EnergyBoundariesFlexOptions(AssetEnergyBoundaries(powerMap))
  }

}
