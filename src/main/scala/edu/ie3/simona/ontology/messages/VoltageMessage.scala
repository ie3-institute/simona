/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import java.util.UUID

import javax.measure.Quantity
import javax.measure.quantity.ElectricPotential

sealed trait VoltageMessage

/** Message that is send between [[edu.ie3.simona.agent.grid.GridAgent]]s to
  * provide voltage information for nodes
  */
object VoltageMessage {

  final case class RequestSlackVoltageMessage(
      currentSweepNo: Int,
      nodeUuid: UUID
  ) extends VoltageMessage

  final case class ProvideSlackVoltageMessage(
      currentSweepNo: Int,
      nodeUuid: UUID,
      e: Quantity[ElectricPotential],
      f: Quantity[ElectricPotential]
  ) extends VoltageMessage

}
