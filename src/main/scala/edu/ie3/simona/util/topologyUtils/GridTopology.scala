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
import scala.annotation.tailrec

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

    val nodeGroups = (groupedNodes ++ singleNodes).zipWithIndex
      .map { case (nodes, idx) =>
        NodeGroup(idx, nodes)
      }
      .flatMap { group =>
        group.nodes.map(node => (node, group))
      }
      .toMap

    // get node that are connected to a transformer to higher voltage level
    val nodesToHigherVoltage = components.transformers.map(
      _.lvNodeUuid
    ) ++ components.transformers3w.flatMap(t => Set(t.mvNodeUuid, t.lvNodeUuid))

    // seq that contains all nodes that are connected to at least one line
    val (lineNodes, lineConnections) =
      lines.toSeq.foldLeft(
        Seq.empty[UUID],
        Map.empty[UUID, (Int, Int)],
      ) { case ((lineNodes, lineMap), line) =>
        val nodeA = line.nodeAUuid
        val nodeB = line.nodeBUuid

        (
          lineNodes ++ Seq(nodeA, nodeB),
          lineMap ++ Map(
            line.uuid -> (nodeGroups(nodeA).index, nodeGroups(nodeB).index)
          ),
        )
      }

    // find node type
    val nodesWithType = nodeGroups.values.toSet
      .map { nodeGroup: NodeGroup =>
        val nodes = nodeGroup.nodes

        val nodeType = if (nodes.exists(nodesToHigherVoltage.contains)) {
          NodeToHigherVoltage
        } else {
          nodes.count(lineNodes.contains) match {
            case 0 =>
            case 1 => EndNode
            case 2 => PathNode
            case _ => IntersectionNode
          }
        }

        nodeType -> (nodeGroup.index, nodeGroup)
      }
      .groupMap(_._1)(_._2)
      .map { case (key, value) => key -> value.toMap }

    buildTopology(
      nodesWithType.getOrElse(NodeToHigherVoltage, Map.empty),
      nodesWithType.getOrElse(IntersectionNode, Map.empty),
      nodesWithType.getOrElse(PathNode, Map.empty),
      nodesWithType.getOrElse(EndNode, Map.empty),
      lineConnections,
      switches.filter(_.isOpen).map(_.uuid),
    )
  }

  final case class RadialGrid(
      override val canBeReconfigured: Boolean = false
  ) extends GridTopology

  final case class GridWithRings(
      connectionToHigherVoltage: Set[NodeGroup],
      openRings: Set[OpenRing],
      closedRings: Set[ClosedRing],
      override val canBeReconfigured: Boolean,
  ) extends GridTopology {}

  final case class MeshedGrid(
      connectionToHigherVoltage: Set[NodeGroup],
      intersections: Set[NodeGroup],
      paths: Set[SimpleGridPath],
      override val canBeReconfigured: Boolean,
  ) extends GridTopology

  sealed trait GridPath {
    def canBeReconfigured: Boolean

    def startNode: NodeGroup

    def endNode: NodeGroup

    def path: Seq[(UUID, NodeGroup)]
  }

  final case class SimpleGridPath(
      override val canBeReconfigured: Boolean,
      override val startNode: NodeGroup,
      override val endNode: NodeGroup,
      override val path: Seq[(UUID, NodeGroup)],
  ) extends GridPath

  final case class OpenRing(
      override val canBeReconfigured: Boolean,
      override val startNode: NodeGroup,
      override val endNode: NodeGroup,
      override val path: Seq[(UUID, NodeGroup)],
  ) extends GridPath

  final case class ClosedRing(
      override val canBeReconfigured: Boolean,
      override val startNode: NodeGroup,
      override val endNode: NodeGroup,
      override val path: Seq[(UUID, NodeGroup)],
  ) extends GridPath

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  private def buildTopology(
      nodesToHigherVoltage: Map[Int, NodeGroup],
      intersectionNodes: Map[Int, NodeGroup],
      pathNodes: Map[Int, NodeGroup],
      endNodes: Map[Int, NodeGroup],
      lines: Map[UUID, (Int, Int)],
      openSwitches: Set[UUID],
  ): GridTopology = {
    implicit lazy val lineConnections: Map[Int, Set[(UUID, Int)]] =
      buildLineConnections(lines)
    lazy val allIntersections: Set[Int] =
      intersectionNodes.keySet ++ nodesToHigherVoltage.keySet

    (allIntersections.size, endNodes.size) match {
      case (0, _) => RadialGrid()
      case (1, 0) =>
        // grid with rings
        val paths =
          traversePaths(allIntersections, endNodes.keySet, lines.values.toSet)

        ???
      case (1, _) =>
        val paths =
          traversePaths(allIntersections, endNodes.keySet, lines.values.toSet)

        paths.size

        RadialGrid()
      case (_, 0) =>
        // meshed grid

        ???
    }
  }

  private def traversePaths(
      intersections: Set[Int],
      ends: Set[Int],
      nodePairs: Set[(Int, Int)],
  )(implicit
      stops: Set[Int] = intersections ++ ends,
      lineConnections: Map[Int, Set[(UUID, Int)]],
  ): Set[Path] = {

    def traversPaths(
        start: Int,
        options: Set[(UUID, Int)],
        allVisited: Set[Int],
    ): (Set[Path], Set[Int]) = {
      options.foldLeft(Set.empty[Path], allVisited + start) {
        case ((paths, visited), (line, idx)) =>
          if (visited.contains(idx)) {
            (paths, visited)
          } else {

            val (path, newlyVisited) =
              traversePath(idx, visited, Set((line, idx)))

            val end = path.lastOption.map(_._2).getOrElse(-1)

            (
              paths ++ Set(Path(start, end, path)),
              visited ++ newlyVisited,
            )
          }
      }
    }

    val (paths, _) = intersections
      .foldLeft(Set.empty[Path], Set.empty[Int]) {
        case ((allPaths, visited), inter) =>
          val (paths, updatedVisited) =
            traversPaths(inter, lineConnections(inter), visited)

          (
            allPaths ++ paths,
            visited ++ updatedVisited,
          )
      }

    val additionalPaths = nodePairs
      .filter { case (a, b) =>
        intersections.contains(a) && intersections.contains(b)
      }
      .map { case (start, end) => Path(start, end) }

    paths ++ additionalPaths
  }

  @tailrec
  private def traversePath(
      current: Int,
      visited: Set[Int],
      elements: Set[(UUID, Int)],
  )(implicit
      stops: Set[Int],
      lineConnections: Map[Int, Set[(UUID, Int)]],
  ): (Set[(UUID, Int)], Set[Int]) =
    lineConnections(current).find { case (_, idx) =>
      !visited.contains(idx)
    } match {
      case Some((line, next)) =>
        if (stops.contains(current)) {
          (
            elements,
            visited ++ Set(current),
          )
        } else {
          traversePath(next, visited + current, elements ++ Set((line, next)))
        }
      case None =>
        (elements, visited)
    }

  private def buildLineConnections(
      lines: Map[UUID, (Int, Int)]
  ): Map[Int, Set[(UUID, Int)]] =
    lines.foldLeft(Map.empty[Int, Set[(UUID, Int)]]) {
      case (map, (line, (nodeA, nodeB))) =>
        map
          .updated(
            nodeA,
            map.getOrElse(nodeA, Set.empty) ++ Set((line, nodeB)),
          )
          .updated(
            nodeB,
            map.getOrElse(nodeB, Set.empty) ++ Set((line, nodeA)),
          )
    }

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

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

  private final case class Path(
      start: Int,
      end: Int,
      path: Set[(UUID, Int)] = Set.empty,
  )
}
