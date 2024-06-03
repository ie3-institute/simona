/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.grid

import breeze.linalg.DenseMatrix
import breeze.math.{Complex => C}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.grid.{LineModel, NodeModel}
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.OperationInterval
import squants.{Amperes, Each}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import javax.measure.Quantity
import javax.measure.quantity.{Dimensionless, ElectricPotential}

/** A simple grid consisting of 6 nodes and 5 lines. Besides the [[NodeModel]]s
  * and [[LineModel]]s it also contains the corresponding admittance matrix.
  *
  * {{{
  * (5)
  *  |
  * (3) ----- (4)
  *  |
  * (0) ----- (1)-----(2)
  * }}}
  *
  * Reference System: 400 kVA @ 10 kV --> Reference admittance: 4 mS
  *
  * Line type: r = 0.437 Ω/km, x = 0.356 Ω/km, g = 0 S/km, b = 25.8 nS/km
  *
  * Lines (params gij, bij, g0, b0): (0,1) -> 0.75 km -> g = 458.49661, b =
  * -373.51212, g/2 = 0, b/2 = 0.0048 (1,2) -> 1.00 km -> g = 343.87246, b =
  * -280.13409, g/2 = 0, b/2 = 0.0065 (0,3) -> 0.50 km -> g = 687.74492, b =
  * -560.26817, g/2 = 0, b/2 = 0.0032 (3,4) -> 0.25 km -> g = 1375.48984, b =
  * -1120.53635, g/2 = 0, b/2 = 0.0016 (3,5) -> 1.25 km -> g = 275.09797, b =
  * -224.10727, g/2 = 0, b/2 = 0.0081
  */
trait FiveLinesWithNodes {

  protected val linesRatedVoltage: ComparableQuantity[ElectricPotential] =
    Quantities.getQuantity(10, KILOVOLT)

  val _nodeCreator: (
      String,
      String,
      Boolean,
      ComparableQuantity[ElectricPotential],
  ) => NodeModel = { (nodeId, uuid, isSlack, vNominal) =>
    new NodeModel(
      UUID.fromString(uuid),
      nodeId,
      OperationInterval(0L, 7200L),
      isSlack,
      Each(1.0d),
      GermanVoltageLevelUtils.parse(vNominal),
    )
  }

  val _lineCreator: (
      String,
      String,
      NodeModel,
      NodeModel,
      Quantity[Dimensionless],
      Quantity[Dimensionless],
      Quantity[Dimensionless],
      Quantity[Dimensionless],
  ) => LineModel = { (lineId, uuid, nodeA, nodeB, r, x, g, b) =>
    new LineModel(
      UUID.fromString(uuid),
      lineId,
      OperationInterval(0L, 7200L),
      nodeA.uuid,
      nodeB.uuid,
      1,
      Amperes(300d),
      Each(r.getValue.doubleValue()),
      Each(x.getValue.doubleValue()),
      Each(g.getValue.doubleValue()),
      Each(b.getValue.doubleValue()),
    )
  }

  def node0: NodeModel =
    _nodeCreator(
      "node0",
      "51c03963-f28b-4892-9053-c6bb58d20a45",
      true,
      linesRatedVoltage,
    )
  def node1: NodeModel =
    _nodeCreator(
      "node1",
      "890fb76c-2c6c-4eea-a47d-cf0244750718",
      false,
      linesRatedVoltage,
    )
  def node2: NodeModel =
    _nodeCreator(
      "node2",
      "be77fa50-613e-4fc9-854a-cfb694443e2f",
      false,
      linesRatedVoltage,
    )
  def node3: NodeModel =
    _nodeCreator(
      "node3",
      "9a41fd03-fb9a-4966-925e-d847a28ca97d",
      false,
      linesRatedVoltage,
    )
  def node4: NodeModel =
    _nodeCreator(
      "node4",
      "7f058275-476a-4d84-b1fa-12381204ac4f",
      false,
      linesRatedVoltage,
    )
  def node5: NodeModel =
    _nodeCreator(
      "node5",
      "ea93feca-0947-4869-a961-9cf942143feb",
      false,
      linesRatedVoltage,
    )

  protected def nodes: Seq[NodeModel] =
    Seq(node0, node1, node2, node3, node4, node5)

  val line0To1: LineModel = _lineCreator(
    "line0To1",
    "95ce3bd5-8c56-403f-aaea-d605fb328542",
    node0,
    node1,
    Quantities.getQuantity(0.0013109999999999999, PU),
    Quantities.getQuantity(0.0010680000000000002, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.0000048375, PU),
  )

  val line1To2: LineModel = _lineCreator(
    "line1To2",
    "f6de6796-e880-45c3-80a6-b7141f3b686c",
    node1,
    node2,
    Quantities.getQuantity(0.001748, PU),
    Quantities.getQuantity(0.001424, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.00000645, PU),
  )

  val line0To3: LineModel = _lineCreator(
    "line0To3",
    "335ccb58-526f-4d80-ad4f-522b544913e2",
    node0,
    node3,
    Quantities.getQuantity(0.000874, PU),
    Quantities.getQuantity(0.000712, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.000003225, PU),
  )

  val line3To4: LineModel = _lineCreator(
    "line3To4",
    "b3b592f6-2112-4254-aca3-d093d220ff0f",
    node3,
    node4,
    Quantities.getQuantity(0.000437, PU),
    Quantities.getQuantity(0.000356, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.0000016125, PU),
  )

  val line3To5: LineModel = _lineCreator(
    "line3To5",
    "0ffa4c4a-c0fb-44b2-8073-c8c66cc105e8",
    node3,
    node5,
    Quantities.getQuantity(0.002185, PU),
    Quantities.getQuantity(0.00178, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.0000080625, PU),
  )

  protected val lines: Set[LineModel] =
    Set(line0To1, line1To2, line0To3, line3To4, line3To5)

  // nodeToIndexMap
  protected def nodeUuidToIndexMap: Map[UUID, Int] =
    Map(
      node0.uuid -> 0,
      node1.uuid -> 1,
      node2.uuid -> 2,
      node3.uuid -> 3,
      node4.uuid -> 4,
      node5.uuid -> 5,
    )

  // corresponding admittance matrix
  protected val lineAdmittanceMatrix: DenseMatrix[C] = DenseMatrix(
    (
      C(1146.2415343374096, -933.7802848111248),
      C(-458.4966137349637, 373.5121155369499),
      C.zero,
      C(-687.7449206024457, 560.2681733054249),
      C.zero,
      C.zero,
    ),
    (
      C(-458.4966137349637, 373.5121155369499),
      C(802.3690740361866, -653.6461965459123),
      C(-343.8724603012229, 280.1340866527124),
      C.zero,
      C.zero,
      C.zero,
    ),
    (
      C.zero,
      C(-343.8724603012229, 280.1340866527124),
      C(343.8724603012229, -280.1340834277124),
      C.zero,
      C.zero,
      C.zero,
    ),
    (
      C(-687.7449206024457, 560.2681733054249),
      C.zero,
      C.zero,
      C(2338.3327300483156, -1904.9117827884445),
      C(-1375.4898412048915, 1120.5363466108497),
      C(-275.0979682409783, 224.10726932216994),
    ),
    (
      C.zero,
      C.zero,
      C.zero,
      C(-1375.4898412048915, 1120.5363466108497),
      C(1375.4898412048915, -1120.5363458045997),
      C.zero,
    ),
    (
      C.zero,
      C.zero,
      C.zero,
      C(-275.0979682409783, 224.10726932216994),
      C.zero,
      C(275.0979682409783, -224.10726529091994),
    ),
  )

}
