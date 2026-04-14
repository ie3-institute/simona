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
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
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

  /** Whether the model offers flexibility.
    */
  val hasEnergyFlexibility: Boolean

  override def determineNextActivation(
      state: S,
      operatingPoint: OperatingPoint,
      setPower: Power,
      dataTimeType: DataTimeType,
  ): OperationChangeIndicator = {

    val forecastResolution = dataTimeType match {
      case DataTimeType.CurrentAndForecast(_, resolution) =>
        resolution.toSeconds.toLong
      case other =>
        throw new CriticalFailureException(s"Unexpected date time type $other")
    }

    // we assume that the first forecast window starts at tick 0
    val currentTick = state.tick
    val lastForecast = currentTick - currentTick % forecastResolution

    // if there is flexibility, and we're charging/discharging,
    // our state of energy is going to be different at every point in the future
    val changesAtNext =
      hasEnergyFlexibility && operatingPoint.activePower != zeroKW

    OperationChangeIndicator(
      changesAtNextActivation = changesAtNext,
      changesAtTick = Some(lastForecast + forecastResolution),
    )
  }

}
