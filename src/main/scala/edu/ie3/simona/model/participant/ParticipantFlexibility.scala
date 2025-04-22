/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
}
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MinMaxFlexOptions}
import squants.energy.Power

/** Trait for [[ParticipantModel]] to define methods related to flexibility.
  *
  * @tparam OP
  *   The type of operating point.
  * @tparam S
  *   The type of model state.
  */
trait ParticipantFlexibility[
    OP <: OperatingPoint,
    S <: ModelState,
] {

  this: ParticipantModel[OP, S] =>

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

  /** Given the current state, this method determines the operating point that
    * is currently valid until the next operating point is determined, given a
    * flex control power determined by EM. Also, optionally returns a tick at
    * which the state will change unless the operating point changes due to
    * external influences beforehand.
    *
    * This method should be able to handle calls at arbitrary points in
    * simulation time (i.e. ticks), which have to be situated after the tick of
    * the last state though.
    *
    * This method is only called if the participant '''is''' em-controlled. If
    * the participant is '''not''' em-controlled,
    * [[ParticipantModel.determineOperatingPoint]] determines the operating
    * point instead.
    *
    * @param state
    *   The current state.
    * @param setPower
    *   The power set point determined by EM.
    * @return
    *   The operating point and optionally a next activation tick.
    */
  def determineOperatingPoint(
      state: S,
      setPower: Power,
  ): (OP, OperationChangeIndicator)

}

object ParticipantFlexibility {

  /** Simple trait providing flexibility implementations to
    * [[ParticipantModel]]s with [[ActivePowerOperatingPoint]]. No flexibility
    * is provided.
    *
    * @tparam S
    *   The type of model state.
    */
  trait ParticipantSimpleFlexibility[
      S <: ModelState
  ] extends ParticipantFlexibility[ActivePowerOperatingPoint, S] {
    this: ParticipantModel[ActivePowerOperatingPoint, S] =>

    override def determineFlexOptions(
        state: S
    ): FlexOptions = {
      val (operatingPoint, _) = determineOperatingPoint(state)
      val power = operatingPoint.activePower

      MinMaxFlexOptions.noFlexOption(power)
    }

    override def determineOperatingPoint(
        state: S,
        setPower: Power,
    ): (ActivePowerOperatingPoint, OperationChangeIndicator) = {
      (ActivePowerOperatingPoint(setPower), OperationChangeIndicator())
    }
  }

}
