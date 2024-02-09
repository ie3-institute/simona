/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.graph.SubGridGate

import java.util.UUID

/** Trait containing helper methods for the [[GridAgent]] to reduce the amount
  * of code in the GridAgent
  */
private[grid] trait GridAgentDataHelper {

  protected val subgridGates: Vector[SubGridGate]
  protected val subgridId: Int

  // methods definition
  def superiorGridIds: Vector[String] =
    subgridGates.collect {
      case gate: SubGridGate if gate.getInferiorSubGrid == subgridId =>
        gate.getSuperiorSubGrid.toString
    }

  def inferiorGridIds: Vector[String] =
    subgridGates.collect {
      case gate: SubGridGate if gate.getSuperiorSubGrid == subgridId =>
        gate.getInferiorSubGrid.toString
    }

  def superiorGridNodeUuids: Vector[UUID] =
    subgridGates.collect {
      case gate: SubGridGate if gate.getInferiorSubGrid == subgridId =>
        gate.superiorNode.getUuid
    }

  def inferiorGridNodeUuids: Vector[UUID] =
    subgridGates.collect {
      case gate: SubGridGate if gate.getSuperiorSubGrid == subgridId =>
        gate.inferiorNode.getUuid
    }

  def superiorGridGates: Vector[SubGridGate] =
    subgridGates.collect {
      case gate: SubGridGate if gate.getInferiorSubGrid == subgridId =>
        gate
    }

  def inferiorGridGates: Vector[SubGridGate] =
    subgridGates.collect {
      case gate: SubGridGate if gate.getSuperiorSubGrid == subgridId =>
        gate
    }

  def isSuperior: Boolean =
    superiorGridNodeUuids.isEmpty

}
