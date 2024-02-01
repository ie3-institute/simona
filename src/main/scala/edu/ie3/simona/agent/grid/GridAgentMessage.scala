/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.event.listener.ResultEventListener.ResultMessage
import edu.ie3.simona.ontology.messages.{
  Activation,
  PowerMessage,
  VoltageMessage
}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import org.apache.pekko.actor.typed.ActorRef

/** Trait for [[GridAgent]] messages.
  */
sealed trait GridAgentMessage

/** Defines all messages that can be received by a [[GridAgent]] without the
  * need for an adapter.
  */
object GridAgentMessage {

  /** GridAgent initialization data can only be constructed once all GridAgent
    * actors are created. Thus, we need an extra initialization message.
    *
    * @param gridAgentInitData
    *   The initialization data
    */
  final case class CreateGridAgent(
      gridAgentInitData: GridAgentInitData,
      unlockKey: ScheduleKey
  ) extends GridAgentMessage

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * execute a power flow calculation
    *
    * @param tick
    *   current tick
    */
  final case class DoPowerFlowTrigger(tick: Long, currentSweepNo: Int)
      extends GridAgentMessage

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * activate the superior grid agent to check for deviation after two sweeps
    * and see if the power flow converges
    *
    * @param tick
    *   current tick
    */
  final case class CheckPowerDifferencesTrigger(tick: Long)
      extends GridAgentMessage

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * trigger the [[edu.ie3.simona.agent.grid.GridAgent]] s to prepare
    * themselves for a new sweep
    *
    * @param tick
    *   current tick
    */
  final case class PrepareNextSweepTrigger(tick: Long) extends GridAgentMessage

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * indicate that a result has been found and each
    * [[edu.ie3.simona.agent.grid.GridAgent]] should do it's cleanup work
    *
    * @param tick
    *   current tick
    */
  final case class FinishGridSimulationTrigger(tick: Long)
      extends GridAgentMessage

  /** Message that should be send by the
    */
  final object StopGridAgent extends GridAgentMessage

  /** Wrapper for string messages. NOTICE: Only for internal use.
    * @param str
    *   message
    * @param sender
    *   of the message
    */
  private[grid] final case class StringAdapter(
      str: String,
      sender: ActorRef[GridAgentMessage]
  ) extends GridAgentMessage

  /** Wrapper for activation values
    *
    * @param activation
    *   the tick
    */
  final case class ActivationAdapter(activation: Activation)
      extends GridAgentMessage

  final case class PMAdapter(msg: PowerMessage) extends GridAgentMessage

  final case class VMAdapter(msg: VoltageMessage) extends GridAgentMessage

  final case class ValuesAdapter(values: ReceivedValues)
      extends GridAgentMessage

  final case class ResultMessageAdapter(msg: ResultMessage)
      extends GridAgentMessage

}
