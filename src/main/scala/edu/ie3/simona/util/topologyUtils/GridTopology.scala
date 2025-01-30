/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util.topologyUtils

import edu.ie3.simona.model.grid.GridModel
import edu.ie3.simona.util.ParsableEnumeration
import edu.ie3.simona.util.topologyUtils.GridTopology.NodeType.{
  EndNode,
  IntersectionNode,
  NodeToHigherVoltage,
  PathNode,
}

import java.util.UUID

sealed trait GridTopology {
  def canBeReconfigured: Boolean
}

object GridTopology {

  def apply(gridModel: GridModel): GridTopology = {
    val components = gridModel.gridComponents
    val lines = components.lines
    val switches = components.switches

    // group nodes
    val groupedNodes = GridModel.groupNodes(switches).map(_.toSet)
    val singleNodes =
      components.nodes.map(_.uuid).diff(groupedNodes.flatten).map(Set(_))

    val nodeGroups = (groupedNodes ++ singleNodes).zipWithIndex.map {
      case (nodes, idx) =>
        NodeGroup(idx, nodes)
    }.flatMap { group =>
      group.nodes.map(node => (node, group))
    }.toMap

    // get node that are connected to a transformer to higher voltage level
    val nodesToHigherVoltage = components.transformers.map(
      _.lvNodeUuid
    ) ++ components.transformers3w.flatMap(t => Set(t.mvNodeUuid, t.lvNodeUuid))

    // seq that contains all nodes that are connected to at least one line
    val (lineNodes, lineConnections) = lines.toSeq.foldLeft(Seq.empty, Map.empty) { case ((lineNodes, lineMap), line) =>
      val nodeA = line.nodeAUuid
      val nodeB = line.nodeBUuid

      (
        lineNodes ++ Seq(nodeA, nodeB),
        lineMap ++ Map(line.uuid -> (nodeGroups(nodeA), nodeGroups(nodeB)))
      )
    }


    val lineMap = lines
      .map(line => line.uuid -> (nodeGroups(line.nodeAUuid), nodeGroups(line.nodeBUuid)))
      .toMap


    // find node type
    val nodesWithType = nodeGroups.values.toSet
      .map { nodeGroup =>
        val nodes = nodeGroup.nodes

        val nodeType = if (nodes.exists(nodesToHigherVoltage.contains)) {
          NodeToHigherVoltage
        } else {
          nodes.count(lineNodes.contains) match {
            case 1 => EndNode
            case 2 => PathNode
            case _ => IntersectionNode
          }
        }

        nodeType -> nodeGroup
      }
      .groupMap(_._1)(_._2)

    buildTopology(
      nodesWithType.getOrElse(NodeToHigherVoltage, Set.empty),
      nodesWithType.getOrElse(IntersectionNode, Set.empty),
      nodesWithType.getOrElse(PathNode, Set.empty),
      nodesWithType.getOrElse(EndNode, Set.empty),
      lineMap,
    )
  }

  private def buildTopology(
      nodesToHigherVoltage: Set[NodeGroup],
      intersectionNodes: Set[NodeGroup],
      pathNodes: Set[NodeGroup],
      endNodes: Set[NodeGroup],
      lines: Map[UUID, (NodeGroup, NodeGroup)],
  ): GridTopology = ???

  final case class RadialGrid(
      override val canBeReconfigured: Boolean = false
  ) extends GridTopology

  final case class NodeGroup(
      index: Int,
      nodes: Set[UUID],
  )

  object NodeType extends ParsableEnumeration {
    val EndNode: Value = Value("EndNode")
    val PathNode: Value = Value("PathNode")
    val IntersectionNode: Value = Value("IntersectionNode")
    val NodeToHigherVoltage: Value = Value("NodeToHigherVoltage")
  }
}
