/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.linalg.DenseMatrix
import breeze.math.Complex
import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.datamodel.models.input.connector._
import edu.ie3.datamodel.models.input.container.SubGridContainer
import edu.ie3.simona.exceptions.GridInconsistencyException
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.{
  PowerFlowCaseA,
  PowerFlowCaseB,
  PowerFlowCaseC
}
import edu.ie3.simona.util.CollectionUtils
import org.jgrapht.Graph
import org.jgrapht.alg.connectivity.ConnectivityInspector
import org.jgrapht.graph.{DefaultEdge, SimpleGraph}

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.immutable.ListSet
import scala.jdk.CollectionConverters._

/** Representation of one physical electrical grid. It holds the references to
  * nodes, lines, switches and transformers and fundamental properties (like
  * nominal voltage etc.).
  */
final case class GridModel(
    subnetNo: Int,
    mainRefSystem: RefSystem,
    gridComponents: GridComponents
) {

  // init nodeUuidToIndexMap
  private var _nodeUuidToIndexMap: Map[UUID, Int] = _
  GridModel.updateUuidToIndexMap(this)

  val slackNodesIndices: Vector[Int] = gridComponents.nodes
    .filter(nodeModel => nodeModel.isInOperation & nodeModel.isSlack)
    .map(nodeModel =>
      _nodeUuidToIndexMap.getOrElse(
        nodeModel.uuid,
        throw new InvalidGridException(
          s"Requested slack node with uuid ${nodeModel.uuid} is not part of nodeToIndexMap!"
        )
      )
    )
    .toVector

  def nodeUuidToIndexMap: Map[UUID, Int] = _nodeUuidToIndexMap
}

case object GridModel {

  def apply(
      subGridContainer: SubGridContainer,
      refSystem: RefSystem,
      startDate: ZonedDateTime,
      endDate: ZonedDateTime
  ): GridModel = {
    buildAndValidate(subGridContainer, refSystem, startDate, endDate)
  }

  /** structure that represents all grid components that are needed by a grid
    * model
    */
  final case class GridComponents(
      nodes: Seq[NodeModel],
      lines: Set[LineModel],
      transformers: Set[TransformerModel],
      transformers3w: Set[Transformer3wModel],
      switches: Set[SwitchModel]
  )

  /** Checks the availability of node calculation models, that are connected by
    * the given [[ConnectorInput]]. If not both models can be found,
    * [[InvalidGridException]] s are thrown
    *
    * @param connector
    *   Connector, that connects the two queried nodes
    * @param nodes
    *   [[Array]] of [[NodeModel]] calculation models
    * @return
    *   A tuple of both connected nodes
    */
  private def getConnectedNodes(
      connector: ConnectorInput,
      nodes: Seq[NodeModel]
  ): (NodeModel, NodeModel) = {
    val nodeAOpt: Option[NodeModel] =
      nodes.find(_.uuid.equals(connector.getNodeA.getUuid))
    val nodeBOpt: Option[NodeModel] =
      nodes.find(_.uuid.equals(connector.getNodeB.getUuid))

    (nodeAOpt, nodeBOpt) match {
      case (Some(nodeA), Some(nodeB)) =>
        (nodeA, nodeB)
      case (None, Some(_)) =>
        throw new InvalidGridException(
          s"NodeA: ${connector.getNodeA.getUuid} for connector ${connector.getUuid} cannot be found."
        )
      case (Some(_), None) =>
        throw new InvalidGridException(
          s"NodeB: ${connector.getNodeB.getUuid} for connector ${connector.getUuid} cannot be found."
        )
      case _ =>
        throw new InvalidGridException(
          s"Nodes (nodeA: ${connector.getNodeA.getUuid}, nodeB: ${connector.getNodeB.getUuid})for connector ${connector.getUuid} cannot be found."
        )
    }
  }

  /** Checks the availability of node calculation models, that are connected by
    * the given [[Transformer3WInput]]. If not both models can be found,
    * [[InvalidGridException]] s are thrown
    *
    * @param transformerInput
    *   Three winding transformer, that connects the three queried nodes
    * @param nodes
    *   [[Array]] of [[NodeModel]] calculation models
    * @return
    *   A tuple of both connected nodes
    */
  private def getConnectedNodes(
      transformerInput: Transformer3WInput,
      nodes: Seq[NodeModel]
  ): (NodeModel, NodeModel, NodeModel) = {
    val (nodeA, nodeB) =
      getConnectedNodes(transformerInput.asInstanceOf[ConnectorInput], nodes)
    val nodeCOpt: Option[NodeModel] = nodes.find(
      _.uuid.equals(transformerInput.getNodeC.getUuid)
    )
    val nodeInternal: Option[NodeModel] =
      nodes.find(_.uuid.equals(transformerInput.getNodeInternal.getUuid))

    if (nodeInternal.isEmpty)
      throw new InvalidGridException(
        s"Internal node ${transformerInput.getNodeInternal.getId} of transformer3w ${transformerInput.getUuid} cannot be found in provided set of nodes!"
      )

    nodeCOpt match {
      case Some(nodeC) =>
        (nodeA, nodeB, nodeC)
      case None =>
        throw new InvalidGridException(
          s"NodeC: ${transformerInput.getNodeA.getUuid} for connector ${transformerInput.getUuid} cannot be found."
        )
    }
  }

  private val throwNodeNotFoundException: UUID => InvalidGridException = {
    nodeString: UUID =>
      throw new InvalidGridException(
        s"Node $nodeString is not in nodeUuidToIndexMap! Cannot build admittanceMatrix!"
      )
  }

  def composeAdmittanceMatrix(
      nodeUuidToIndexMap: Map[UUID, Int],
      gridComponents: GridComponents
  ): DenseMatrix[Complex] = {

    val _returnAdmittanceMatrixIfValid
        : DenseMatrix[Complex] => DenseMatrix[Complex] = {
      admittanceMatrix: DenseMatrix[Complex] =>
        if (
          !breeze.linalg.all(
            { entry: Complex =>
              !entry.imag.isNaN & !entry.real.isNaN & entry.imag.isFinite & entry.real.isFinite
            },
            admittanceMatrix
          )
        )
          throw new RuntimeException(s"Admittance matrix is illegal.")
        else
          admittanceMatrix
    }

    /*
    Nodes that are connected via a [closed] switch map to the same idx as we fuse them during the power flow.
    Therefore the admittance matrix has to be of the size of the distinct node idxs.
     */
    val linesAdmittanceMatrix = buildAssetAdmittanceMatrix(
      nodeUuidToIndexMap,
      gridComponents.lines,
      getLinesAdmittance
    )
    val trafoAdmittanceMatrix = buildAssetAdmittanceMatrix(
      nodeUuidToIndexMap,
      gridComponents.transformers,
      getTransformerAdmittance
    )
    val trafo3wAdmittanceMatrix = buildAssetAdmittanceMatrix(
      nodeUuidToIndexMap,
      gridComponents.transformers3w,
      getTransformer3wAdmittance
    )

    _returnAdmittanceMatrixIfValid(
      linesAdmittanceMatrix + trafoAdmittanceMatrix + trafo3wAdmittanceMatrix
    )
  }

  private def buildAssetAdmittanceMatrix[C <: SystemComponent](
      nodeUuidToIndexMap: Map[UUID, Int],
      assets: Set[C],
      getAssetAdmittance: (
          Map[UUID, Int],
          C
      ) => (Int, Int, Complex, Complex, Complex)
  ): DenseMatrix[Complex] = {
    val matrixDimension = nodeUuidToIndexMap.values.toSeq.distinct.size

    assets
      .filter(_.isInOperation)
      .foldLeft(DenseMatrix.zeros[Complex](matrixDimension, matrixDimension))(
        (admittanceMatrix, asset) => {
          val (i, j, yab, yaa, ybb) =
            getAssetAdmittance(nodeUuidToIndexMap, asset)

          admittanceMatrix(i, i) += (yab + yaa)
          admittanceMatrix(j, j) += (yab + ybb)
          admittanceMatrix(i, j) += (yab * -1)
          admittanceMatrix(j, i) += (yab * -1)
          admittanceMatrix
        }
      )
  }

  private def getLinesAdmittance(
      nodeUuidToIndexMap: Map[UUID, Int],
      line: LineModel
  ): (Int, Int, Complex, Complex, Complex) = {

    val (i: Int, j: Int) =
      (
        nodeUuidToIndexMap.getOrElse(
          line.nodeAUuid,
          throwNodeNotFoundException(line.nodeAUuid)
        ),
        nodeUuidToIndexMap
          .getOrElse(
            line.nodeBUuid,
            throwNodeNotFoundException(line.nodeBUuid)
          )
      )

    // yaa == ybb => we use yaa only
    val (yab, yaa) = (LineModel.yij(line), LineModel.y0(line))

    (i, j, yab, yaa, yaa)
  }

  private def getTransformerAdmittance(
      nodeUuidToIndexMap: Map[UUID, Int],
      trafo: TransformerModel
  ): (Int, Int, Complex, Complex, Complex) = {

    val (i: Int, j: Int) =
      (
        nodeUuidToIndexMap.getOrElse(
          trafo.hvNodeUuid,
          throwNodeNotFoundException(trafo.hvNodeUuid)
        ),
        nodeUuidToIndexMap.getOrElse(
          trafo.lvNodeUuid,
          throwNodeNotFoundException(trafo.lvNodeUuid)
        )
      )

    val (yab, yaa, ybb) = (
      TransformerModel.yij(trafo),
      TransformerModel.y0(trafo, ConnectorPort.A),
      TransformerModel.y0(trafo, ConnectorPort.B)
    )

    (i, j, yab, yaa, ybb)
  }

  private def getTransformer3wAdmittance(
      nodeUuidToIndexMap: Map[UUID, Int],
      trafo3w: Transformer3wModel
  ): (Int, Int, Complex, Complex, Complex) = {

    // start with power flow case specific parameters
    val (nodeAUuid: UUID, nodeBUuid: UUID, ybb: Complex) =
      trafo3w.powerFlowCase match {
        case PowerFlowCaseA =>
          (
            trafo3w.hvNodeUuid,
            trafo3w.nodeInternalUuid,
            Transformer3wModel
              .y0(trafo3w, Transformer3wModel.Transformer3wPort.INTERNAL)
          )

        case PowerFlowCaseB =>
          (trafo3w.nodeInternalUuid, trafo3w.mvNodeUuid, Complex.zero)

        case PowerFlowCaseC =>
          (trafo3w.nodeInternalUuid, trafo3w.lvNodeUuid, Complex.zero)
      }

    val (i: Int, j: Int) =
      (
        nodeUuidToIndexMap
          .getOrElse(nodeAUuid, throwNodeNotFoundException(nodeAUuid)),
        nodeUuidToIndexMap
          .getOrElse(nodeBUuid, throwNodeNotFoundException(nodeBUuid))
      )

    // these parameters are the same for all cases
    val yab: Complex = Transformer3wModel.yij(trafo3w)
    val yaa: Complex = Complex.zero

    (i, j, yab, yaa, ybb)
  }

  /** This checks whether the provided grid model graph is connected, that means
    * if every node can be reached from every other node trough a sequence of
    * edges. Also checks for referenced nodes that are missing. This check
    * considers the state (enabled/disabled) of the elements.
    *
    * @param gridModel
    *   the [[GridModel]] to check the connectivity for
    */
  private def validateConnectivity(gridModel: GridModel): Unit = {

    // build graph
    val graph: Graph[UUID, DefaultEdge] =
      new SimpleGraph(classOf[DefaultEdge])
    gridModel.gridComponents.nodes
      .filter(_.isInOperation) foreach (node => graph.addVertex(node.uuid))
    gridModel.gridComponents.lines
      .filter(_.isInOperation)
      .foreach(line => {
        graph.addEdge(line.nodeAUuid, line.nodeBUuid)
      })

    gridModel.gridComponents.transformers
      .filter(_.isInOperation)
      .foreach(trafo => {
        graph.addEdge(trafo.hvNodeUuid, trafo.lvNodeUuid)
      })

    gridModel.gridComponents.transformers3w
      .filter(_.isInOperation)
      .foreach(trafo3w =>
        /* Add an edge between the internal node and corresponding "real" node of the port */
        trafo3w.powerFlowCase match {
          case Transformer3wPowerFlowCase.PowerFlowCaseA =>
            graph.addEdge(trafo3w.hvNodeUuid, trafo3w.nodeInternalUuid)
          case Transformer3wPowerFlowCase.PowerFlowCaseB =>
            graph.addEdge(trafo3w.nodeInternalUuid, trafo3w.mvNodeUuid)
          case Transformer3wPowerFlowCase.PowerFlowCaseC =>
            graph.addEdge(trafo3w.nodeInternalUuid, trafo3w.lvNodeUuid)
        }
      )

    gridModel.gridComponents.switches
      .filter(_.isInOperation)
      .foreach(switch => {
        graph.addEdge(switch.nodeAUuid, switch.nodeBUuid)
      })

    val inspector: ConnectivityInspector[UUID, DefaultEdge] =
      new ConnectivityInspector(graph)

    if (!inspector.isConnected) {
      throw new GridInconsistencyException(
        s"The grid with subnetNo ${gridModel.subnetNo} is not connected! Please ensure that all elements are connected correctly and inOperation is set to true!"
      )
    }

  }

  private def validateConsistency(gridModel: GridModel): Unit = {
    // null or empty elements in grid elements?
    if (
      gridModel.gridComponents.nodes == null || gridModel.gridComponents.nodes.isEmpty
    )
      throw new InvalidGridException("The grid contains no nodes.")
    val noLines =
      gridModel.gridComponents.lines == null || gridModel.gridComponents.lines.isEmpty
    val noTransformers2w =
      gridModel.gridComponents.transformers == null || gridModel.gridComponents.transformers.isEmpty
    val noTransformers3w =
      gridModel.gridComponents.transformers3w == null || gridModel.gridComponents.transformers3w.isEmpty
    val noOfNodes = gridModel.gridComponents.nodes.size
    val noOfSlackNodes = gridModel.slackNodesIndices.size
    if (
      noLines && noTransformers2w && noTransformers3w && (noOfNodes > noOfSlackNodes)
    )
      throw new InvalidGridException(
        "The grid contains no basic branch elements (lines or transformers)."
      )

    // slack
    if (gridModel.slackNodesIndices.isEmpty)
      new InvalidGridException(
        s"The grid model for subnet ${gridModel.subnetNo} has no slack node!"
      )

    // electrical struct data
    if (gridModel.mainRefSystem.nominalPower.getValue.doubleValue < 0.0)
      throw new InvalidGridException(
        s"Nominal Power of a grid cannot be < 0. Please correct the value of the reference system for grid no ${gridModel.subnetNo}"
      )
    if (gridModel.mainRefSystem.nominalVoltage.getValue.doubleValue < 0.0)
      throw new InvalidGridException(
        s"Nominal Voltage of a grid cannot be < 0. Please correct the value of the reference system for grid no ${gridModel.subnetNo}"
      )

    // subnet no
    if (gridModel.subnetNo < 0)
      throw new InvalidGridException(
        s"The grid model for subnet ${gridModel.subnetNo} has a subnet number less then zero."
      )

    // duplicate names for nodes
    val nodeUuids: List[UUID] =
      gridModel.gridComponents.nodes.toList.iterator.map(_.uuid).toList
    if (CollectionUtils.listHasDuplicates(nodeUuids))
      throw new InvalidGridException(
        s"The grid model for subnet ${gridModel.subnetNo} has multiple nodes with the same name!"
      )

    // multiple switches @ one node -> not supported yet!
    val switchVector = gridModel.gridComponents.switches.foldLeft(
      Vector.empty[UUID]
    )((vector, switch) => (vector :+ switch.nodeAUuid) :+ switch.nodeBUuid)
    val uniqueSwitchNodeIds = switchVector.toSet.toList
    if (switchVector.diff(uniqueSwitchNodeIds).nonEmpty) {
      throw new InvalidGridException(
        s"The grid model for subnet ${gridModel.subnetNo} has nodes with multiple switches. This is not supported yet! Duplicates are located @ nodes: ${switchVector
            .diff(uniqueSwitchNodeIds)}"
      )
    }

  }

  private def buildAndValidate(
      subGridContainer: SubGridContainer,
      refSystem: RefSystem,
      startDate: ZonedDateTime,
      endDate: ZonedDateTime
  ): GridModel = {

    // build
    // / nodes
    // // the set of nodes is converted to a sequence here, since the
    // // order of nodes is important for data preparations related to
    // // power flow calculation
    val nodes = subGridContainer.getRawGrid.getNodes.asScala.toSeq.map {
      nodeInput => NodeModel(nodeInput, startDate, endDate)
    }

    // / lines
    val lines: Set[LineModel] =
      subGridContainer.getRawGrid.getLines.asScala.map { lineInput =>
        getConnectedNodes(lineInput, nodes)
        LineModel(lineInput, refSystem, startDate, endDate)
      }.toSet

    // / transformers
    val transformers: Set[TransformerModel] =
      subGridContainer.getRawGrid.getTransformer2Ws.asScala.map {
        transformer2wInput =>
          val (nodeA, _) = getConnectedNodes(transformer2wInput, nodes)
          if (nodeA.isSlack) {
            TransformerModel(
              transformer2wInput,
              refSystem,
              startDate,
              endDate
            )
          } else {
            throw new InvalidGridException(
              s"NodeA: ${transformer2wInput.getNodeA.getUuid} for transformer ${transformer2wInput.getUuid} is not set as slack. This has to be corrected first!"
            )
          }
      }.toSet

    // / transformers3w
    val transformer3ws: Set[Transformer3wModel] =
      subGridContainer.getRawGrid.getTransformer3Ws.asScala.map {
        transformer3wInput =>
          getConnectedNodes(transformer3wInput, nodes)
          Transformer3wModel(
            transformer3wInput,
            refSystem,
            subGridContainer.getSubnet,
            startDate,
            endDate
          )
      }.toSet

    /* Transformers are shipped as full models, therefore also containing two nodes, that do not belong in here.
     * Odd those nodes out. */
    val nodesToNeglect = transformer3ws.flatMap { transformer =>
      transformer.powerFlowCase match {
        case Transformer3wPowerFlowCase.PowerFlowCaseA =>
          Seq(transformer.mvNodeUuid, transformer.lvNodeUuid)
        case Transformer3wPowerFlowCase.PowerFlowCaseB =>
          Seq(transformer.hvNodeUuid, transformer.lvNodeUuid)
        case Transformer3wPowerFlowCase.PowerFlowCaseC =>
          Seq(transformer.hvNodeUuid, transformer.mvNodeUuid)
      }
    }
    val relevantNodes =
      nodes.filterNot(node => nodesToNeglect.contains(node.uuid))

    // / switches
    val switches: Set[SwitchModel] =
      subGridContainer.getRawGrid.getSwitches.asScala.map { switchInput =>
        getConnectedNodes(switchInput, nodes)
        SwitchModel(switchInput, startDate, endDate)
      }.toSet

    // build
    val gridComponents =
      GridComponents(
        relevantNodes,
        lines,
        transformers,
        transformer3ws,
        switches
      )

    val gridModel =
      GridModel(subGridContainer.getSubnet, refSystem, gridComponents)

    // validate
    validateConsistency(gridModel)
    validateConnectivity(gridModel)

    // return
    gridModel
  }

  /** Updates the internal state of the [[GridModel.nodeUuidToIndexMap]] to
    * account for changes on switches (open / close) It is highly recommended (=
    * mandatory) to call this method every time a node admittance matrix is
    * needed after a switch status has changed.
    *
    * @param gridModel
    *   the grid model we operate on
    */
  def updateUuidToIndexMap(gridModel: GridModel): Unit = {

    val switches = gridModel.gridComponents.switches
    val nodes = gridModel.gridComponents.nodes

    val nodesAndSwitches: ListSet[SystemComponent] = ListSet
      .empty[SystemComponent] ++ switches ++ nodes

    val updatedNodeToUuidMap = nodesAndSwitches
      .filter(_.isInOperation)
      .filter {
        case switch: SwitchModel => switch.isClosed
        case _: NodeModel        => true
      }
      .zipWithIndex
      .foldLeft(Map.empty[UUID, Int]) {
        case (map, (gridComponent, componentId)) =>
          gridComponent match {
            case switchModel: SwitchModel =>
              map ++ Map(
                switchModel.nodeAUuid -> componentId,
                switchModel.nodeBUuid -> componentId
              )

            case nodeModel: NodeModel =>
              if (!map.contains(nodeModel.uuid)) {
                val idx = map.values.toList.sorted.lastOption
                  .getOrElse(
                    -1
                  ) + 1 // if we didn't found anything in the list, we don't have switches and want to start @ 0
                map + (nodeModel.uuid -> idx)
              } else {
                map
              }
          }
      }

    gridModel._nodeUuidToIndexMap = updatedNodeToUuidMap
  }
}
