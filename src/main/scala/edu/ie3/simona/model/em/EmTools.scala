/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.simona.exceptions.FlexException
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MinMaxFlexOptions}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueFlexControl,
  IssueNoControl,
  IssuePowerControl,
}
import squants.Power

/** Tools used by agents that engage with energy management and flexibility
  */
object EmTools {

  /** Determines the set point given a flex options message and a flex control
    * message. Also validates the resulting power.
    *
    * @param flexOptions
    *   The flex options.
    * @param flexCtrl
    *   The flex control message.
    * @return
    *   The resulting power set point.
    */
  def determineFlexPower(
      flexOptions: FlexOptions,
      flexCtrl: IssueFlexControl,
  ): Power =
    flexOptions match {
      case flexOptions: MinMaxFlexOptions =>
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
        throw new FlexException(
          s"Unknown/unfitting flex messages $unknownFlexOpt"
        )
    }

  /** Checks whether given setPower fits the provided flex options, i.e. whether
    * the set point is feasible given the flex options.
    *
    * @param flexOptions
    *   The flex options that the set point has to fit.
    * @param setPower
    *   The set point.
    */
  def checkSetPower(
      flexOptions: MinMaxFlexOptions,
      setPower: Power,
  ): Unit = {
    if (setPower < flexOptions.min)
      throw new FlexException(
        s"The set power $setPower must not be lower than the minimum power ${flexOptions.min}!"
      )
    else if (setPower > flexOptions.max)
      throw new FlexException(
        s"The set power $setPower must not be greater than the maximum power ${flexOptions.max}!"
      )
  }
}
