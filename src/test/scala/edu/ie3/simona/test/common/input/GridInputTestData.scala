/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.connector.{
  LineInput,
  SwitchInput,
  Transformer2WInput,
  Transformer3WInput,
}
import edu.ie3.datamodel.models.input.container.{
  RawGridElements,
  SubGridContainer,
}
import edu.ie3.datamodel.models.input.{MeasurementUnitInput, NodeInput}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils.{
  HV,
  MV_10KV,
}
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.simona.util.TestGridFactory
import testutils.TestObjectFactory

import java.util.UUID
import scala.jdk.CollectionConverters._

/** Contains a valid GridInputModel with the following structure
  *
  * {{{
  * (6) / / trafo (5) /
  * | /
  * | / (0)--(15)S2(16)-----(3)-----(4)
  * | (17) S3 (18)
  * | (1)--(13)S1(14)-----(2)
  * }}}
  *
  * Reference System: 400 kVA @ 10 kV
  */
trait GridInputTestData
    extends LineInputTestData
    with TransformerInputTestData
    with Transformer3wTestData
    with DefaultTestData {

  // create the nodes
  private val inputNode0: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)
  private val inputNode1: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)
  private val inputNode2: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)
  private val inputNode3: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)
  private val inputNode4: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)
  private val inputNode5: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)
  private val inputNode6: NodeInput =
    TestObjectFactory.buildNodeInput(true, HV, 1)

  private val inputNode13: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)
  private val inputNode14: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)
  private val inputNode15: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)
  private val inputNode16: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)
  private val inputNode17: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)
  private val inputNode18: NodeInput =
    TestObjectFactory.buildNodeInput(false, MV_10KV, 100)

  private val nodes: Set[NodeInput] =
    Set(
      inputNode0,
      inputNode1,
      inputNode2,
      inputNode3,
      inputNode4,
      inputNode5,
      inputNode6,
      inputNode13,
      inputNode14,
      inputNode15,
      inputNode16,
      inputNode17,
      inputNode18,
    )

  // create the lines
  private val line3_4: LineInput =
    TestObjectFactory.buildLineInput(inputNode3, inputNode4)
  private val line3_5: LineInput =
    TestObjectFactory.buildLineInput(inputNode3, inputNode5)
  private val line0_17: LineInput =
    TestObjectFactory.buildLineInput(inputNode0, inputNode17)
  private val line18_1: LineInput =
    TestObjectFactory.buildLineInput(inputNode18, inputNode1)
  private val line1_13: LineInput =
    TestObjectFactory.buildLineInput(inputNode1, inputNode13)
  private val line14_2: LineInput =
    TestObjectFactory.buildLineInput(inputNode14, inputNode2)
  private val line0_15: LineInput =
    TestObjectFactory.buildLineInput(inputNode0, inputNode15)
  private val line16_3: LineInput =
    TestObjectFactory.buildLineInput(inputNode16, inputNode3)

  val lines: Set[LineInput] =
    Set(
      line3_4,
      line3_5,
      line0_17,
      line18_1,
      line1_13,
      line14_2,
      line0_15,
      line16_3,
    )

  // create the switches
  private val switch1: SwitchInput =
    TestObjectFactory.buildSwitchInput(inputNode13, inputNode14)
  private val switch2: SwitchInput =
    TestObjectFactory.buildSwitchInput(inputNode17, inputNode18)
  private val switch3: SwitchInput =
    TestObjectFactory.buildSwitchInput(inputNode15, inputNode16)

  val switches: Set[SwitchInput] = Set(switch1, switch2, switch3)

  // create the transformer
  val adaptedTransformerInputModel: Transformer2WInput = new Transformer2WInput(
    UUID.fromString("4ea3f965-53b8-4bb6-a6a9-4eaf55131c5c"),
    transformerInput.getId,
    transformerInput.getOperator,
    transformerInput.getOperationTime,
    inputNode6,
    inputNode3,
    transformerInput.getParallelDevices,
    transformerInput.getType,
    transformerInput.getTapPos,
    transformerInput.isAutoTap,
  )

  val transformers: Set[Transformer2WInput] = Set(adaptedTransformerInputModel)
  // create the 3w transformers
  // todo CK

  // create the reference system
  protected def gridInputModelTestDataRefSystem: RefSystem =
    default400Kva10KvRefSystem

  // create the grid
  protected val validTestGridInputModel: SubGridContainer = {
    val rawGridElements = new RawGridElements(
      nodes.asJava,
      lines.asJava,
      transformers.asJava,
      Set.empty[Transformer3WInput].asJava,
      switches.asJava,
      Set.empty[MeasurementUnitInput].asJava,
    )
    TestGridFactory.createSubGrid(
      rawGridElements = rawGridElements
    )
  }

}
