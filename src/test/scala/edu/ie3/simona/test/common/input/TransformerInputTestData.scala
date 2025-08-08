/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.connector.`type`.Transformer2WTypeInput
import edu.ie3.datamodel.models.input.connector.{
  LineInput,
  SwitchInput,
  Transformer2WInput,
  Transformer3WInput,
}
import edu.ie3.datamodel.models.input.container.{
  JointGridContainer,
  RawGridElements,
}
import edu.ie3.datamodel.models.input.{
  MeasurementUnitInput,
  NodeInput,
  OperatorInput,
}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.simona.util.TestGridFactory
import edu.ie3.util.quantities.PowerSystemUnits._
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{OHM, PERCENT}

import java.util.UUID
import scala.jdk.CollectionConverters._

/** Test data for a [[Transformer2WInput]].
  *
  * {{{
  * Transformer type:
  *   d_v = 1.5 %
  *   tap_min = -13
  *   tap_max = 13
  *   tap_neut = 0
  *   is_autotap = true
  *   tapSide = high voltage side (ConnectorPort.B -> None in
  *     [[edu.ie3.simona.model.grid.TransformerTappingModel]])
  *   r = 30.25 Ω
  *   x = 4.5375 Ω
  *   g = 0
  *   b = 1.1 nS
  *   s_rated = 40000 kVA -> iNomHv = 209.9455524325912 A
  *     -> iNomLv = 2309.401076758503 A
  *   capex = 100.000 €
  *   opex = 0 €
  *
  * Transformer model:
  *   tap_side = hv (element port A)
  *   tap_pos = 0
  *   auto_tap = false
  *   amount = 1
  *   vHv = 110 kV
  *   vLv = 10 kV
  * }}}
  */
trait TransformerInputTestData extends DefaultTestData {
  private val nodeA = new NodeInput(
    UUID.fromString("c1c83216-f813-4f77-a63b-1f24dbd5afa0"),
    "nodeA",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1.0, PU),
    true,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.HV,
    1,
  )
  private val nodeB = new NodeInput(
    UUID.fromString("d46ac046-70c0-478f-8ab1-92d70f0ba172"),
    "nodeB",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1.0, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.MV_10KV,
    2,
  )

  protected val transformerType = new Transformer2WTypeInput(
    UUID.randomUUID(),
    "HS-MS_1",
    Quantities.getQuantity(30.25, OHM),
    Quantities.getQuantity(4.5375, OHM),
    Quantities.getQuantity(40d, MEGAVOLTAMPERE),
    Quantities.getQuantity(110d, KILOVOLT),
    Quantities.getQuantity(10d, KILOVOLT),
    Quantities.getQuantity(0d, NANOSIEMENS),
    Quantities.getQuantity(-1.1, NANOSIEMENS),
    Quantities.getQuantity(1.5, PERCENT),
    Quantities.getQuantity(0d, DEGREE_GEOM),
    false,
    0,
    -13,
    13,
  )

  val transformerInput = new Transformer2WInput(
    UUID.fromString("5641c062-3f8c-4d9d-a3a8-c871465e4503"),
    "testTransformer",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeA,
    nodeB,
    1,
    transformerType,
    10,
    false,
  )

  protected val gridContainer: JointGridContainer = {
    val rawGridElements = new RawGridElements(
      Set(nodeA, nodeB).asJava,
      Set.empty[LineInput].asJava,
      Set(transformerInput).asJava,
      Set.empty[Transformer3WInput].asJava,
      Set.empty[SwitchInput].asJava,
      Set.empty[MeasurementUnitInput].asJava,
    )
    TestGridFactory.createJointGrid(
      gridName = "twoWindingTestGrid",
      rawGridElements = rawGridElements,
    )
  }
}
