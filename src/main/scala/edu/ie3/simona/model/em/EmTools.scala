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
  ProvideFlexOptions,
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import squants.Power

/** Tools used by agents that engage with energy management and flexibility
  */
object EmTools {

  /** Determines the set point given a flex options message and a flex control
    * message. Also validates the resulting power.
    *
    * @param flexOptionsMsg
    *   The flex options message
    * @param flexCtrl
    *   The flex control message
    * @return
    *   The resulting power set point
    */
  def determineFlexPower(
      flexOptionsMsg: ProvideFlexOptions,
      flexCtrl: IssueFlexControl,
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
            flexOptions.ref
        }

      case unknownFlexOpt =>
        throw new CriticalFailureException(
          s"Unknown/unfitting flex messages $unknownFlexOpt"
        )
    }

  /** Checks whether given setPower fits the provided flex options, i.e. whether
    * the set point is feasible given the flex options.
    *
    * @param flexOptions
    *   The flex options that the set point has to fit
    * @param setPower
    *   The set point
    */
  def checkSetPower(
      flexOptions: ProvideMinMaxFlexOptions,
      setPower: Power,
  ): Unit = {
    if (setPower < flexOptions.min)
      throw new CriticalFailureException(
        s"The set power $setPower for ${flexOptions.modelUuid} must not be lower than the minimum power ${flexOptions.min}!"
      )
    else if (setPower > flexOptions.max)
      throw new CriticalFailureException(
        s"The set power $setPower for ${flexOptions.modelUuid} must not be greater than the maximum power ${flexOptions.max}!"
      )
  }
}
