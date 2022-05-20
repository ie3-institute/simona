/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import java.util.UUID

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.connector.`type`.Transformer3WTypeInput
import edu.ie3.datamodel.models.input.connector.{
  LineInput,
  SwitchInput,
  Transformer2WInput,
  Transformer3WInput
}
import edu.ie3.datamodel.models.input.container.{
  GraphicElements,
  JointGridContainer,
  RawGridElements,
  SystemParticipants
}
import edu.ie3.datamodel.models.input.graphics.{
  LineGraphicInput,
  NodeGraphicInput
}
import edu.ie3.datamodel.models.input.system._
import edu.ie3.datamodel.models.input.{
  MeasurementUnitInput,
  NodeInput,
  OperatorInput
}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.util.quantities.PowerSystemUnits._
import javax.measure.MetricPrefix
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units._

import scala.jdk.CollectionConverters._

/** Simple grid structure with only one three winding transformer
  */
trait ThreeWindingTestData extends DefaultTestData {
  private val nodeA = new NodeInput(
    UUID.fromString("fc3a7c59-0402-4ec1-8a50-b26fb67238bc"),
    "nodeA",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1d, PU),
    true,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.EHV_380KV,
    1
  )
  private val nodeB = new NodeInput(
    UUID.fromString("3d4c66a3-dc11-4ec8-857a-53d77beb15ee"),
    "nodeB",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1d, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.HV,
    2
  )
  private val nodeC = new NodeInput(
    UUID.fromString("a865a429-615e-44be-9d00-d384298986f6"),
    "nodeC",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1d, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.MV_10KV,
    3
  )

  private val transformerType = new Transformer3WTypeInput(
    UUID.randomUUID(),
    "HöS-HS-MS_1",
    Quantities.getQuantity(120d, MEGAVOLTAMPERE),
    Quantities.getQuantity(60d, MEGAVOLTAMPERE),
    Quantities.getQuantity(40d, MEGAVOLTAMPERE),
    Quantities.getQuantity(380d, KILOVOLT),
    Quantities.getQuantity(110d, KILOVOLT),
    Quantities.getQuantity(10d, KILOVOLT),
    Quantities.getQuantity(0.3, OHM),
    Quantities.getQuantity(0.025, OHM),
    Quantities.getQuantity(0.0008, OHM),
    Quantities.getQuantity(1.0, OHM),
    Quantities.getQuantity(0.08, OHM),
    Quantities.getQuantity(0.003, OHM),
    Quantities.getQuantity(40d, MetricPrefix.NANO(SIEMENS)),
    Quantities.getQuantity(-1d, MetricPrefix.NANO(SIEMENS)),
    Quantities.getQuantity(1.5, PERCENT),
    Quantities.getQuantity(0d, DEGREE_GEOM),
    0,
    -10,
    10
  )

  private val transformer = new Transformer3WInput(
    UUID.fromString("6faeb364-030d-4b2c-bd3f-905ec9413fae"),
    "testTransformer",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    nodeA,
    nodeB,
    nodeC,
    1,
    transformerType,
    0,
    true
  )

  protected val threeWindingTestGrid: JointGridContainer = {
    val rawGridElements = new RawGridElements(
      Set(nodeA, nodeB, nodeC).asJava,
      Set.empty[LineInput].asJava,
      Set.empty[Transformer2WInput].asJava,
      Set(transformer).asJava,
      Set.empty[SwitchInput].asJava,
      Set.empty[MeasurementUnitInput].asJava
    )
    val systemParticipants = new SystemParticipants(
      Set.empty[BmInput].asJava,
      Set.empty[ChpInput].asJava,
      Set.empty[EvcsInput].asJava,
      Set.empty[EvInput].asJava,
      Set.empty[FixedFeedInInput].asJava,
      Set.empty[HpInput].asJava,
      Set.empty[LoadInput].asJava,
      Set.empty[PvInput].asJava,
      Set.empty[StorageInput].asJava,
      Set.empty[WecInput].asJava
    )
    val graphicElements = new GraphicElements(
      Set.empty[NodeGraphicInput].asJava,
      Set.empty[LineGraphicInput].asJava
    )
    new JointGridContainer(
      "threeWindingTestGrid",
      rawGridElements,
      systemParticipants,
      graphicElements
    )
  }
}
