/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelChangeIndicator,
  ModelState,
  OperatingPoint,
  OperationRelevantData,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.scala.quantities.DefaultQuantities
import squants.energy.Power

trait ParticipantFlexibility[
    OP <: OperatingPoint[_],
    S <: ModelState,
    OR <: OperationRelevantData,
] {

  this: ParticipantModel[OP, S, OR] =>

  def calcFlexOptions(state: S, relevantData: OR): ProvideFlexOptions

  def handlePowerControl(
      state: S,
      flexOptions: ProvideFlexOptions,
      setPower: Power,
  ): (OP, ModelChangeIndicator)

}

object ParticipantFlexibility {

  trait ParticipantSimpleFlexibility[
      S <: ModelState,
      OR <: OperationRelevantData,
  ] extends ParticipantFlexibility[ActivePowerOperatingPoint, S, OR] {
    this: ParticipantModel[ActivePowerOperatingPoint, S, OR] =>

    override def calcFlexOptions(
        state: S,
        relevantData: OR,
    ): ProvideFlexOptions = {
      val (operatingPoint, _) = determineOperatingPoint(state, relevantData)
      val power = operatingPoint.activePower

      ProvideMinMaxFlexOptions(uuid, power, power, DefaultQuantities.zeroKW)
    }

    override def handlePowerControl(
        state: S,
        flexOptions: ProvideFlexOptions,
        setPower: Power,
    ): (ActivePowerOperatingPoint, ModelChangeIndicator) = {
      (ActivePowerOperatingPoint(setPower), ModelChangeIndicator())
    }
  }

}
