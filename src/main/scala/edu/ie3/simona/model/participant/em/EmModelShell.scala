/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  IssueFlexControl,
  ProvideFlexOptions
}
import edu.ie3.simona.agent.participant.em.EmAgentTyped.Actor
import squants.energy.Power

/** Translating input data to a format that can be used by aggregation
  * strategies, em strats etc. Furthermore, sanity checks on calculated data is
  * performed.
  */
// TODO move package em out of participant
case class EmModelShell() {

  def aggregateFlexOptions(
      flexOptions: Iterable[
        (Actor, ProvideFlexOptions)
      ]
  ): (Power, Power, Power) = {
    // adaptFlexOptions

    // aggregateFlexOptions
  }

  def determineDeviceControl(
      flexOptions: Iterable[(Actor, ProvideFlexOptions)],
      target: Power
  ): Iterable[(Actor, Power)] = {
    // TODO sanity checks before strat calculation

    // sanity checks after strat calculation
    // checkSetPower(flexOptions, power)

  }

}
