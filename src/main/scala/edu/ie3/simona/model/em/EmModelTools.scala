/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueFlexControl,
  IssueNoControl,
  IssuePowerControl,
  ProvideFlexOptions
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import squants.Power

// TODO scaladoc
object EmModelTools {
  def determineFlexPower(
      flexOptionsMsg: ProvideFlexOptions,
      flexCtrl: IssueFlexControl
  ): Power =
    flexOptionsMsg match {
      case flexOptions: ProvideMinMaxFlexOptions =>
        flexCtrl match {
          case IssuePowerControl(_, setPower) =>
            // sanity check: setPower is in range of latest flex options
            checkSetPower(flexOptions, setPower)

            setPower

          case IssueNoControl(_) =>
            // no override, take reference power
            flexOptions.referencePower
        }

      case unknownFlexOpt =>
        throw new CriticalFailureException(
          s"Unknown/unfitting flex messages $unknownFlexOpt"
        )
    }

  def checkSetPower(
      flexOptions: ProvideMinMaxFlexOptions,
      setPower: Power
  ): Unit = {
    if (setPower < flexOptions.minPower)
      throw new CriticalFailureException(
        s"The set power $setPower for ${flexOptions.modelUuid} must not be lower than the minimum power ${flexOptions.minPower}!"
      )
    else if (setPower > flexOptions.maxPower)
      throw new CriticalFailureException(
        s"The set power $setPower for ${flexOptions.modelUuid} must not be greater than the maximum power ${flexOptions.maxPower}!"
      )
  }
}
