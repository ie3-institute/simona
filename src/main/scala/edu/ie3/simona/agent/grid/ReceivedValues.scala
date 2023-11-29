/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import org.apache.pekko.actor.ActorRef
import edu.ie3.simona.ontology.messages.PowerMessage.PowerResponseMessage
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage

/** Serves as a wrapper class that allows for matches against received values in
  * [[DBFSAlgorithm]]
  */
sealed trait ReceivedValues

object ReceivedValues {

  type ActorPowerRequestResponse = (ActorRef, PowerResponseMessage)
  type ActorSlackVoltageRequestResponse = (ActorRef, ProvideSlackVoltageMessage)

  sealed trait ReceivedPowerValues extends ReceivedValues {
    def values: Vector[ActorPowerRequestResponse]
  }

  /** Wrapper for received asset power values (p, q)
    *
    * @param values
    *   the asset power values and their senders
    */
  final case class ReceivedAssetPowerValues(
      values: Vector[ActorPowerRequestResponse]
  ) extends ReceivedPowerValues

  /** Wrapper for received grid power values (p, q)
    *
    * @param values
    *   the grid power values and their senders
    */
  final case class ReceivedGridPowerValues(
      values: Vector[ActorPowerRequestResponse]
  ) extends ReceivedPowerValues

  /** Wrapper for received slack voltage values (v)
    *
    * @param values
    *   the slack voltage values and their senders
    */
  final case class ReceivedSlackVoltageValues(
      values: Vector[ActorSlackVoltageRequestResponse]
  ) extends ReceivedValues

}
