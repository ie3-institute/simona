/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  OperationChangeIndicator,
  ModelState,
  OperatingPoint,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.scala.quantities.DefaultQuantities
import squants.energy.Power

trait ParticipantFlexibility[
    OP <: OperatingPoint,
    S <: ModelState,
] {

  this: ParticipantModel[OP, S] =>

  def calcFlexOptions(state: S): ProvideFlexOptions

  def handlePowerControl(
      state: S,
      flexOptions: ProvideFlexOptions, // TODO is this needed?
      setPower: Power,
  ): (OP, OperationChangeIndicator)

}

object ParticipantFlexibility {

  trait ParticipantSimpleFlexibility[
      S <: ModelState
  ] extends ParticipantFlexibility[ActivePowerOperatingPoint, S] {
    this: ParticipantModel[ActivePowerOperatingPoint, S] =>

    override def calcFlexOptions(
        state: S
    ): ProvideFlexOptions = {
      val (operatingPoint, _) = determineOperatingPoint(state)
      val power = operatingPoint.activePower

      ProvideMinMaxFlexOptions(uuid, power, power, DefaultQuantities.zeroKW)
    }

    override def handlePowerControl(
        state: S,
        flexOptions: ProvideFlexOptions,
        setPower: Power,
    ): (ActivePowerOperatingPoint, OperationChangeIndicator) = {
      (ActivePowerOperatingPoint(setPower), OperationChangeIndicator())
    }
  }

}
