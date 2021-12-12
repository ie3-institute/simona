/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import java.util.UUID

import edu.ie3.datamodel.graph.SubGridGate

/** Trait containing helper methods for the [[GridAgent]] to reduce the amount
  * of code in the GridAgent
  */
private[grid] trait GridAgentDataHelper {

  protected val subnetGates: Vector[SubGridGate]
  protected val subnetId: Int

  // methods definition
  def superiorGridIds: Vector[String] =
    subnetGates.collect {
      case gate: SubGridGate if gate.getInferiorSubGrid == subnetId =>
        gate.getSuperiorSubGrid.toString
    }

  def inferiorGridIds: Vector[String] =
    subnetGates.collect {
      case gate: SubGridGate if gate.getSuperiorSubGrid == subnetId =>
        gate.getInferiorSubGrid.toString
    }

  def superiorGridNodeUuids: Vector[UUID] =
    subnetGates.collect {
      case gate: SubGridGate if gate.getInferiorSubGrid == subnetId =>
        gate.getSuperiorNode.getUuid
    }

  def inferiorGridNodeUuids: Vector[UUID] =
    subnetGates.collect {
      case gate: SubGridGate if gate.getSuperiorSubGrid == subnetId =>
        gate.getInferiorNode.getUuid
    }

  def superiorGridGates: Vector[SubGridGate] =
    subnetGates.collect {
      case gate: SubGridGate if gate.getInferiorSubGrid == subnetId =>
        gate
    }

  def inferiorGridGates: Vector[SubGridGate] =
    subnetGates.collect {
      case gate: SubGridGate if gate.getSuperiorSubGrid == subnetId =>
        gate
    }

  def isSuperior: Boolean =
    superiorGridNodeUuids.isEmpty

}
