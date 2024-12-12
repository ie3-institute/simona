/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import java.util.UUID
import edu.ie3.powerflow.model.NodeData.StateData
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.simona.agent.grid.SweepValueStore.SweepValueStoreData
import edu.ie3.simona.exceptions.agent.DBFSAlgorithmException
import edu.ie3.simona.model.grid.NodeModel

/** Value store holding all that should be stored after each sweep of the
  * [[DBFSAlgorithm]] to be used by the next sweep.
  *
  * @param sweepData
  *   a collection of [[SweepValueStoreData]]
  */
final case class SweepValueStore private (
    sweepData: Vector[SweepValueStoreData]
)

case object SweepValueStore {

  /** Data object that contains node specific data of one sweep of the
    * [[DBFSAlgorithm]]
    *
    * @param stateData
    *   power flow state data
    * @param nodeUuid
    *   node uuid of the sweep data
    */
  final case class SweepValueStoreData private (
      nodeUuid: UUID,
      stateData: StateData,
  )

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
    *   instance of [[SweepValueStore]] with all data to be saved as
    *   [[SweepValueStoreData]]
    */
  def apply(
      validResult: ValidNewtonRaphsonPFResult,
      nodes: Seq[NodeModel],
      nodeUuidToIndexMap: Map[UUID, Int],
  ): SweepValueStore = {
    val sweepDataValues = nodes.foldLeft(Vector.empty[SweepValueStoreData])(
      (valueStoreDataElements, node) => {
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

        valueStoreDataElements :+ SweepValueStoreData(uuid, stateData)
      }
    )

    new SweepValueStore(sweepDataValues)
  }
}
