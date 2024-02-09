/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.statedata

import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.event.notifier.NotifierConfig

/** Trait to denote all state data that are meant to carry additional
  * information needed to initialize a
  * [[edu.ie3.simona.agent.participant.ParticipantAgent]]
  */
trait InitializeStateData[+PD <: PrimaryData] extends ParticipantStateData[PD] {

  /** Config for the output behaviour of simulation results
    */
  val outputConfig: NotifierConfig
}

object InitializeStateData {
  final case class TrivialInitializeStateData[+PD <: PrimaryData](
      resultEventEmitter: String
  ) extends InitializeStateData[PD] {
    val outputConfig: NotifierConfig = NotifierConfig(
      simulationResultInfo = false,
      powerRequestReply = true,
      flexResult = false,
    )
  }
}
