/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.flex

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
}
import edu.ie3.simona.service.DataTimeType
import squants.Power

/** Trait to be implemented by all flexibility models of
  * [[edu.ie3.simona.model.participant.ParticipantModel]] that produce
  * [[edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions]].
  *
  * @tparam S
  *   The type of model state.
  */
trait AbstractEnergyBoundariesFlexModel[S <: ModelState]
    extends ParticipantFlexModel[OperatingPoint, S] {

  override def determineNextActivation(
      state: S,
      operatingPoint: OperatingPoint,
      setPower: Power,
      dateTimeType: DataTimeType,
  ): OperationChangeIndicator = {

    val forecastResolution = dateTimeType match {
      case DataTimeType.CurrentAndForecast(_, resolution) =>
        resolution.toSeconds.toLong
      case other =>
        throw new CriticalFailureException(s"Unexpected date time type $other")
    }

    OperationChangeIndicator(
      changesAtTick = Some(state.tick + forecastResolution)
    )
  }

}
