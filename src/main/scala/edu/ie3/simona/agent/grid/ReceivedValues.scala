/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.ontology.messages.PowerMessage.PowerResponseMessage
import VoltageMessage.ProvideSlackVoltageMessage
import edu.ie3.simona.agent.grid.GridAgentMessage.GAMessage
import org.apache.pekko.actor.typed.ActorRef

/** Serves as a wrapper class that allows for matches against received values in
  * [[DBFSAlgorithm]]
  */
sealed trait ReceivedValues extends GAMessage

object ReceivedValues {

  private type ParticipantPowerRequestResponse =
    (
        ActorRef[_],
        PowerResponseMessage,
    ) // necessary, because participants are still classic actors
  private type GridPowerRequestResponse =
    (ActorRef[GridAgentMessage], PowerResponseMessage)
  private type ActorSlackVoltageRequestResponse =
    (ActorRef[GridAgentMessage], ProvideSlackVoltageMessage)

  sealed trait ReceivedPowerValues extends ReceivedValues {
    def values: Vector[(ActorRef[_], PowerResponseMessage)]
  }

  /** Wrapper for received asset power values (p, q)
    *
    * @param values
    *   the asset power values and their senders
    */
  final case class ReceivedAssetPowerValues(
      values: Vector[ParticipantPowerRequestResponse]
  ) extends ReceivedPowerValues

  /** Wrapper for received grid power values (p, q)
    *
    * @param values
    *   the grid power values and their senders
    */
  final case class ReceivedGridPowerValues(
      values: Vector[GridPowerRequestResponse]
  ) extends ReceivedPowerValues

  /** Wrapper for received slack voltage values (v)
    *
    * @param values
    *   the slack voltage values and their senders
    */
  final case class ReceivedSlackVoltageValues(
      values: Vector[ActorSlackVoltageRequestResponse]
  ) extends ReceivedValues

  /** Wrapper for received exception.
    * @param exception
    *   that was received
    */
  final case class ReceivedFailure(
      exception: Throwable
  ) extends ReceivedValues
}
