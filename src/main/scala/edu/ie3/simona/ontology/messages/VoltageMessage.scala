/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage.ExchangeVoltage
import squants.electro.ElectricPotential

import java.util.UUID

sealed trait VoltageMessage

/** Message that is send between [[edu.ie3.simona.agent.grid.GridAgent]] s to
  * provide voltage information for nodes
  */
object VoltageMessage {

  /** Request complex voltage at the nodes that the superior sub grid shares
    * with the sender's sub grid
    * @param currentSweepNo
    *   The current sweep
    * @param nodeUuids
    *   The UUIDs of the nodes that are bordering the sender's grid
    */
  final case class RequestSlackVoltageMessage(
      currentSweepNo: Int,
      nodeUuids: Seq[UUID]
  ) extends VoltageMessage

  /** Provide complex voltage at the nodes that the sender's sub grid shares
    * with the inferior sub grid, as a reply to a
    * [[RequestSlackVoltageMessage]].
    * @param nodalSlackVoltages
    *   The complex voltages of the shared nodes
    */
  final case class ProvideSlackVoltageMessage(
      currentSweepNo: Int,
      nodalSlackVoltages: Seq[ExchangeVoltage]
  ) extends VoltageMessage

  object ProvideSlackVoltageMessage {

    /** Defining the exchanged voltage at one interconnection point
      *
      * @param nodeUuid
      *   Unique identifier of the node for which complex voltage is shared
      * @param e
      *   Real part of the slack voltage
      * @param f
      *   Imaginary part of the slack voltage
      */
    final case class ExchangeVoltage(
        nodeUuid: UUID,
        e: ElectricPotential,
        f: ElectricPotential
    )
  }

}
