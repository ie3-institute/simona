/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import java.util.UUID

import edu.ie3.powerflow.model.NodeData.StateData
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.simona.exceptions.agent.DBFSAlgorithmException
import edu.ie3.simona.model.grid.NodeModel

/** Value store holding all that should be stored after each sweep of the
  * [[DBFSAlgorithm]] to be used by the next sweep.
  *
  * @param sweepData
  *   a map from node UUID to [[StateData]]
  */
final case class SweepValueStore private (
    sweepData: Map[UUID, StateData]
)

object SweepValueStore {

  /** Creates an empty [[SweepValueStore]] from on a valid power flow result
    *
    * @param validResult
    *   the valid power flow result
    * @param nodes
    *   the nodes of the grid under investigation
    * @param nodeUuidToIndexMap
    *   mapping of node uuids of the grid to their index in the admittance
    *   matrix
    * @return
    *   instance of [[SweepValueStore]] with all state data
    */
  def apply(
      validResult: ValidNewtonRaphsonPFResult,
      nodes: Set[NodeModel],
      nodeUuidToIndexMap: Map[UUID, Int]
  ): SweepValueStore = {
    val sweepDataValues = nodes.map { node =>
      val uuid = node.uuid
      val id = node.id
      val nodeIdxOpt = nodeUuidToIndexMap.get(uuid)
      val stateData = validResult.nodeData
        .find(stateData =>
          nodeIdxOpt
            .contains(stateData.index)
        )
        .getOrElse(
          throw new DBFSAlgorithmException(
            s"Cannot find power flow result data for node $id [$uuid]!"
          )
        )

      uuid -> stateData
    }.toMap

    new SweepValueStore(sweepDataValues)
  }
}
