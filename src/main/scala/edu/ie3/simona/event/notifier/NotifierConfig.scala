/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.notifier

/** A case class to configure the behaviour of the Participant w.r.t. its
  * implementation of the [[edu.ie3.simona.event.notifier.Notifier]] trait
  *
  * @param simulationResultInfo
  *   true, if the participant should inform about results
  */
final case class NotifierConfig(
    simulationResultInfo: Boolean,
    powerRequestReply: Boolean,
    flexResult: Boolean,
)
