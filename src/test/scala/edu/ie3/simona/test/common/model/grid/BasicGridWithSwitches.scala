/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.grid

import edu.ie3.simona.model.control.GridControls
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.{
  GridModel,
  LineModel,
  NodeModel,
  SwitchModel,
  Transformer3wModel,
}
import edu.ie3.util.quantities.PowerSystemUnits._
import tech.units.indriya.quantity.Quantities

import java.util.UUID

/** Note: the line parameters are NOT adapted. Instead, the line params from
  * [[FiveLinesWithNodes]] are used instead!
  *
  * {{{
  *                             (6)
  *                             /
  *                            /
  *                         trafo
  *                     (5)  /
  *                      |  /
  *                      | /
  * (0)--(15)S2(16)-----(3)-----(4)
  * |                    |
  * (17)                 |
  * S3                   |
  * (18)                 |
  * |                    |
  * (1)--(13)S1(14)-----(2)
  * }}}
  */
trait BasicGridWithSwitches extends BasicGrid {

  // switch nodes
  def node13: NodeModel =
    _nodeCreator(
      "node13",
      "69f08e1d-725d-4bae-80c3-5b5a472493c9",
      false,
      linesRatedVoltage,
    )
  def node14: NodeModel =
    _nodeCreator(
      "node14",
      "c09cb11f-4e2c-4871-84c6-a22dc6702679",
      false,
      linesRatedVoltage,
    )
  def node15: NodeModel =
    _nodeCreator(
      "node15",
      "2b45e1e2-591e-49c1-bcbe-0e4ceed79c9b",
      false,
      linesRatedVoltage,
    )
  def node16: NodeModel =
    _nodeCreator(
      "node16",
      "8aec5998-9c8a-453d-8556-e8630f4c053a",
      false,
      linesRatedVoltage,
    )
  def node17: NodeModel =
    _nodeCreator(
      "node17",
      "e1002827-0430-4ba0-950f-8107fefc09fa",
      false,
      linesRatedVoltage,
    )
  def node18: NodeModel =
    _nodeCreator(
      "node18",
      "4ab4904e-dde3-4591-8eb5-3e0ca4fd8e3d",
      false,
      linesRatedVoltage,
    )

  // add nodes to nodes list
  override protected def nodes: Seq[NodeModel] =
    super.nodes ++ Seq(node13, node14, node15, node16, node17, node18)

  // add nodes to nodeUuidToIndexMap
  override protected def nodeUuidToIndexMap: Map[UUID, Int] =
    super.nodeUuidToIndexMap ++ Map(
      node13.uuid -> 7,
      node14.uuid -> 8,
      node15.uuid -> 9,
      node16.uuid -> 10,
      node17.uuid -> 11,
      node18.uuid -> 12,
    )

  // rebuild lines
  def line0To17: LineModel = _lineCreator(
    "line0To17",
    "4fc9c100-7d76-42e2-b214-40b601c1e19f",
    node0,
    node17,
    Quantities.getQuantity(0.0013109999999999999, PU),
    Quantities.getQuantity(0.0010680000000000002, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.0000048375, PU),
  )
  def line18To1: LineModel = _lineCreator(
    "line18To1",
    "11ad6538-e40b-4210-9a7f-796fc8744901",
    node18,
    node1,
    Quantities.getQuantity(0.0013109999999999999, PU),
    Quantities.getQuantity(0.0010680000000000002, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.0000048375, PU),
  )
  def line1To13: LineModel = _lineCreator(
    "line1To13",
    "f1ad18e9-0f1c-4faa-9cee-18c99be120b7",
    node1,
    node13,
    Quantities.getQuantity(0.001748, PU),
    Quantities.getQuantity(0.001424, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.00000645, PU),
  )
  def line14To2: LineModel = _lineCreator(
    "line14To2",
    "f1ad18e9-0f1c-4faa-9cee-18c99be120b7", // TODO THIS SHOULD PRODUCE AN ERROR!
    node14,
    node2,
    Quantities.getQuantity(0.001748, PU),
    Quantities.getQuantity(0.001424, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.00000645, PU),
  )
  def line0To15: LineModel = _lineCreator(
    "line0To15",
    "c13c34ef-62bc-42cb-8861-749dc62e3672",
    node0,
    node15,
    Quantities.getQuantity(0.000874, PU),
    Quantities.getQuantity(0.000712, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.000003225, PU),
  )
  def line16To3: LineModel = _lineCreator(
    "line16To3",
    "ee26d14b-9062-4231-b046-1c4f2b6b0edd",
    node16,
    node3,
    Quantities.getQuantity(0.000874, PU),
    Quantities.getQuantity(0.000712, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.000003225, PU),
  )

  def line2To3: LineModel = _lineCreator(
    "line2To3",
    "88fdbe78-96b0-4d21-a8ab-7a8d5ece3131",
    node2,
    node3,
    Quantities.getQuantity(0.0013109999999999999, PU),
    Quantities.getQuantity(0.0010680000000000002, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.0000048375, PU),
  )

  override protected val lines: Set[LineModel] = Set(
    line0To17,
    line18To1,
    line1To13,
    line14To2,
    line0To15,
    line16To3,
    line3To4,
    line3To5,
    line2To3,
  )

  // switches
  val switch1 = new SwitchModel(
    UUID.fromString("009d808a-1497-4d17-aea6-fa718acf0294"),
    "TestSwitch1",
    defaultOperationInterval,
    node13.uuid,
    node14.uuid,
  )
  val switch2 = new SwitchModel(
    UUID.fromString("e9eb5598-1611-46ad-a44f-fde689c3f558"),
    "TestSwitch2",
    defaultOperationInterval,
    node15.uuid,
    node16.uuid,
  )
  val switch3 = new SwitchModel(
    UUID.fromString("01731a4a-7801-4656-96c9-26d002ff52da"),
    "TestSwitch3",
    defaultOperationInterval,
    node17.uuid,
    node18.uuid,
  )

  def switches: Set[SwitchModel] = Set(switch1, switch2, switch3)

  def createGridCopy(): GridModel = {
    // copy components because they are mutable and are altered by some tests
    // also enable components, otherwise they are not considered in building admittance matrices
    val gridNodes = nodes
    gridNodes.foreach(node => if (!node.isInOperation) node.enable())
    val gridLines = lines
    gridLines.foreach(_.enable())
    val gridSwitches = switches.map(_.copy())
    gridSwitches.foreach(switch => if (!switch.isInOperation) switch.enable())

    new GridModel(
      1,
      default400Kva10KvRefSystem,
      GridComponents(
        gridNodes,
        gridLines,
        Set(transformer2wModel),
        Set.empty[Transformer3wModel],
        gridSwitches,
      ),
      GridControls.empty,
    )
  }

  def openSwitches(gridModel: GridModel): Unit = {
    gridModel.gridComponents.switches.foreach(switch =>
      if (!switch.isOpen) switch.open()
    )
  }

  def closeSwitches(gridModel: GridModel): Unit = {
    gridModel.gridComponents.switches.foreach(switch =>
      if (!switch.isClosed) switch.close()
    )
  }

}
