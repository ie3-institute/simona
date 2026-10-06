/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.simona.model.participant.ParticipantModel.AdditionalFactoryData
import squants.energy.Energy
import squants.Power

import java.time.ZonedDateTime

object MarkovLoadModel {

  /** Holds additional data for the Markov load model factory.
    *
    * @param maxPower
    *   The maximal power of the Markov model.
    * @param energyScaling
    *   The energy scaling of the Markov model.
    * @param stepFunction
    *   A function, that takes a point in time, the previous state of the Markov
    *   chain as well as a seed and returns a load value and the next state of
    *   the chain.
    */
  final case class MarkovLoadFactoryData(
      maxPower: Option[Power],
      energyScaling: Option[Energy],
      stepFunction: (ZonedDateTime, Int, Long) => (Power, Int),
  ) extends AdditionalFactoryData

}
