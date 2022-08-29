/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.grid

import edu.ie3.simona.model.grid.{LineModel, NodeModel, SwitchModel}
import edu.ie3.util.quantities.PowerSystemUnits._
import tech.units.indriya.quantity.Quantities

import java.util.UUID

/** Note: the line parameters are NOT adapted. Instead the line params from
  * [[FiveLinesWithNodes]] are used instead!
  *
  * {{{
  * (6) / / trafo (5) /
  * | /
  * | / (0)--(15)S2(16)-----(3)-----(4)
  * | (17) S3 (18)
  * | (1)--(13)S1(14)-----(2)
  * }}}
  */
trait BasicGridWithSwitches extends BasicGrid {

  // switch nodes
  def node13: NodeModel =
    _nodeCreator(
      "node13",
      "69f08e1d-725d-4bae-80c3-5b5a472493c9",
      true,
      linesRatedVoltage
    )
  def node14: NodeModel =
    _nodeCreator(
      "node14",
      "c09cb11f-4e2c-4871-84c6-a22dc6702679",
      true,
      linesRatedVoltage
    )
  def node15: NodeModel =
    _nodeCreator(
      "node15",
      "2b45e1e2-591e-49c1-bcbe-0e4ceed79c9b",
      true,
      linesRatedVoltage
    )
  def node16: NodeModel =
    _nodeCreator(
      "node16",
      "8aec5998-9c8a-453d-8556-e8630f4c053a",
      true,
      linesRatedVoltage
    )
  def node17: NodeModel =
    _nodeCreator(
      "node17",
      "e1002827-0430-4ba0-950f-8107fefc09fa",
      true,
      linesRatedVoltage
    )
  def node18: NodeModel =
    _nodeCreator(
      "node18",
      "4ab4904e-dde3-4591-8eb5-3e0ca4fd8e3d",
      true,
      linesRatedVoltage
    )

  // add nodes to nodes list
  override protected def nodes: Seq[NodeModel] =
    super.nodes ++ Seq(node13, node14, node15, node16, node17, node18)

  // add nodes to nodeUuidToIndexMap
  override protected def nodeUuidToIndexMap: Map[UUID, Int] =
    super.nodeUuidToIndexMap ++ Map(
      UUID.fromString("69f08e1d-725d-4bae-80c3-5b5a472493c9") -> 7,
      UUID.fromString("c09cb11f-4e2c-4871-84c6-a22dc6702679") -> 8,
      UUID.fromString("2b45e1e2-591e-49c1-bcbe-0e4ceed79c9b") -> 9,
      UUID.fromString("8aec5998-9c8a-453d-8556-e8630f4c053a") -> 10,
      UUID.fromString("e1002827-0430-4ba0-950f-8107fefc09fa") -> 11,
      UUID.fromString("4ab4904e-dde3-4591-8eb5-3e0ca4fd8e3d") -> 12
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
    Quantities.getQuantity(0.0000048375, PU)
  )
  def line18To1: LineModel = _lineCreator(
    "line18_1",
    "11ad6538-e40b-4210-9a7f-796fc8744901",
    node18,
    node1,
    Quantities.getQuantity(0.0013109999999999999, PU),
    Quantities.getQuantity(0.0010680000000000002, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.0000048375, PU)
  )
  def line1To13: LineModel = _lineCreator(
    "line1_13",
    "f1ad18e9-0f1c-4faa-9cee-18c99be120b7",
    node1,
    node13,
    Quantities.getQuantity(0.001748, PU),
    Quantities.getQuantity(0.001424, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.00000645, PU)
  )
  def line14To2: LineModel = _lineCreator(
    "line14_2",
    "f1ad18e9-0f1c-4faa-9cee-18c99be120b7", // TODO THIS SHOULD PRODUCE AN ERROR!
    node14,
    node2,
    Quantities.getQuantity(0.001748, PU),
    Quantities.getQuantity(0.001424, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.00000645, PU)
  )
  def line0To15: LineModel = _lineCreator(
    "line0_15",
    "c13c34ef-62bc-42cb-8861-749dc62e3672",
    node0,
    node15,
    Quantities.getQuantity(0.000874, PU),
    Quantities.getQuantity(0.000712, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.000003225, PU)
  )
  def line16To3: LineModel = _lineCreator(
    "line16_3",
    "ee26d14b-9062-4231-b046-1c4f2b6b0edd",
    node16,
    node3,
    Quantities.getQuantity(0.000874, PU),
    Quantities.getQuantity(0.000712, PU),
    Quantities.getQuantity(0, PU),
    Quantities.getQuantity(0.000003225, PU)
  )

  override protected val lines: Set[LineModel] = Set(
    line0To17,
    line18To1,
    line1To13,
    line14To2,
    line0To15,
    line16To3,
    line34,
    line35
  )

  // switches
  val switch1 = new SwitchModel(
    UUID.fromString("009d808a-1497-4d17-aea6-fa718acf0294"),
    "TestSwitch1",
    defaultOperationInterval,
    UUID.fromString("69f08e1d-725d-4bae-80c3-5b5a472493c9"),
    UUID.fromString("c09cb11f-4e2c-4871-84c6-a22dc6702679")
  )
  val switch2 = new SwitchModel(
    UUID.fromString("e9eb5598-1611-46ad-a44f-fde689c3f558"),
    "TestSwitch2",
    defaultOperationInterval,
    UUID.fromString("2b45e1e2-591e-49c1-bcbe-0e4ceed79c9b"),
    UUID.fromString("8aec5998-9c8a-453d-8556-e8630f4c053a")
  )
  val switch3 = new SwitchModel(
    UUID.fromString("01731a4a-7801-4656-96c9-26d002ff52da"),
    "TestSwitch3",
    defaultOperationInterval,
    UUID.fromString("e1002827-0430-4ba0-950f-8107fefc09fa"),
    UUID.fromString("4ab4904e-dde3-4591-8eb5-3e0ca4fd8e3d")
  )

  def switches: Set[SwitchModel] = Set(switch1, switch2, switch3)

} //todo might be removable
