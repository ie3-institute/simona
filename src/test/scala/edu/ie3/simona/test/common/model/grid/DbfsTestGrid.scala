/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.grid

import java.util.UUID

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.connector._
import edu.ie3.datamodel.models.input.connector.`type`.{
  LineTypeInput,
  Transformer2WTypeInput
}
import edu.ie3.datamodel.models.input.container.{
  GraphicElements,
  RawGridElements,
  SubGridContainer,
  SystemParticipants
}
import edu.ie3.datamodel.models.input.graphics.{
  LineGraphicInput,
  NodeGraphicInput
}
import edu.ie3.datamodel.models.input.system.characteristic.OlmCharacteristicInput
import edu.ie3.datamodel.models.input.system.{EvcsInput, _}
import edu.ie3.datamodel.models.input.{
  MeasurementUnitInput,
  NodeInput,
  OperatorInput
}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.datamodel.utils.GridAndGeoUtils
import edu.ie3.util.quantities.PowerSystemUnits._
import javax.measure.MetricPrefix
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units._

import scala.jdk.CollectionConverters._

/** Provides the high voltage level of SIMONA's test grid. Only consists of
  * lines, nodes and transformers.
  */
trait DbfsTestGrid extends SubGridGateMokka {
  // 4 HS nodes, 1 slack HöS node
  val node1 = new NodeInput(
    UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c"),
    "HS_NET1_Station_1",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1.0, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.HV,
    1
  )
  val node2 = new NodeInput(
    UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0"),
    "HS_NET1_Station_2",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1.0, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.HV,
    1
  )
  val node3 = new NodeInput(
    UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae"),
    "HS_NET1_Station_3",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1.0, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.HV,
    1
  )
  val node4 = new NodeInput(
    UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75"),
    "HS_NET1_Station_4",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1.0, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.HV,
    1
  )
  val slackNode = new NodeInput(
    UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e"),
    "HS_NET1_Station_1_380",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1.0, PU),
    true,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.EHV_380KV,
    1000
  )

  /* Mocking table of nodes of underlying grids
   *
   * MS4_01 @ 14 -> 1129b00d-3d89-4a4a-8ae1-2a56041b95aa @ 14
   * MS2_01 @ 12 -> 139c435d-e550-48d8-b590-ee897621f42a @ 12
   * MS1_01 @ 11 -> 1676e48c-5353-4f06-b671-c579cf6a7072 @ 11
   * MS3_01 @ 13 -> 9237e237-01e9-446f-899f-c3b5cf69d288 @ 13
   */

  // 5 lines between the nodes
  private val lineType1 = new LineTypeInput(
    UUID.randomUUID(),
    "Freileitung_110kV_1 ",
    Quantities.getQuantity(0.0, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0.0, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0.1094, OHM_PER_KILOMETRE),
    Quantities.getQuantity(0.4, OHM_PER_KILOMETRE),
    Quantities.getQuantity(680.0, AMPERE),
    Quantities.getQuantity(110.0, KILOVOLT)
  )

  private val lineType2 = new LineTypeInput(
    UUID.randomUUID(),
    "Kabel_110kV_1",
    Quantities.getQuantity(0.0, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0.0, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0.0283, OHM_PER_KILOMETRE),
    Quantities.getQuantity(0.11, OHM_PER_KILOMETRE),
    Quantities.getQuantity(800.0, AMPERE),
    Quantities.getQuantity(110.0, KILOVOLT)
  )

  private val lineType3 = new LineTypeInput(
    UUID.randomUUID(),
    "Freileitung_110kV_2",
    Quantities.getQuantity(0, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0.0547, OHM_PER_KILOMETRE),
    Quantities.getQuantity(0.4, OHM_PER_KILOMETRE),
    Quantities.getQuantity(1360.0, AMPERE),
    Quantities.getQuantity(110.0, KILOVOLT)
  )

  private val lineType4 = new LineTypeInput(
    UUID.randomUUID(),
    "HS_1",
    Quantities.getQuantity(0.000003, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0.109999999403954, OHM_PER_KILOMETRE),
    Quantities.getQuantity(0.379999995231628, OHM_PER_KILOMETRE),
    Quantities.getQuantity(550.0, AMPERE),
    Quantities.getQuantity(110.0, KILOVOLT)
  )

  val line1 = new LineInput(
    UUID.fromString("b6dff9c3-cebb-4aea-9f12-0556bdbf35dc"),
    "LTG_HS_NET1_Station_3-HS_NET1_Station_4",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    node3,
    node4,
    1,
    lineType1,
    Quantities.getQuantity(20, KILOMETRE),
    GridAndGeoUtils.buildSafeLineStringBetweenNodes(node3, node4),
    OlmCharacteristicInput.CONSTANT_CHARACTERISTIC
  )

  val line2 = new LineInput(
    UUID.fromString("c15ec4ad-3ff3-43f0-bc72-ee9a76f53afd"),
    "LTG_HS_NET1_Station_3-HS_NET1_Station_2",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    node3,
    node2,
    1,
    lineType2,
    Quantities.getQuantity(20.0, KILOMETRE),
    GridAndGeoUtils.buildSafeLineStringBetweenNodes(node3, node2),
    OlmCharacteristicInput.CONSTANT_CHARACTERISTIC
  )

  val line3 = new LineInput(
    UUID.fromString("8440825c-24c9-4b3d-9e94-a6bfb9643a6b"),
    "LTG_HS_NET1_Station_1-HS_NET1_Station_2",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    node1,
    node2,
    1,
    lineType3,
    Quantities.getQuantity(24.0, KILOMETRE),
    GridAndGeoUtils.buildSafeLineStringBetweenNodes(node1, node2),
    OlmCharacteristicInput.CONSTANT_CHARACTERISTIC
  )

  val line4 = new LineInput(
    UUID.fromString("e0ca3891-1757-4dea-ac9d-8f1194da453e"),
    "LTG_HS_NET1_Station_1-HS_NET1_Station_3",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    node1,
    node3,
    1,
    lineType4,
    Quantities.getQuantity(40, KILOMETRE),
    GridAndGeoUtils.buildSafeLineStringBetweenNodes(node1, node3),
    OlmCharacteristicInput.CONSTANT_CHARACTERISTIC
  )

  val line5 = new LineInput(
    UUID.fromString("147ae685-4fc7-406c-aca6-afb2bc6e19fc"),
    "LTG_HS_NET1_Station_4-HS_NET1_Station_1",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    node4,
    node1,
    1,
    lineType2,
    Quantities.getQuantity(30, KILOMETRE),
    GridAndGeoUtils.buildSafeLineStringBetweenNodes(node4, node1),
    OlmCharacteristicInput.CONSTANT_CHARACTERISTIC
  )

  // 1 transformer from HS to HöS
  private val trafoType = new Transformer2WTypeInput(
    UUID.randomUUID(),
    "HöS-HS_1",
    Quantities.getQuantity(5.415, OHM),
    Quantities.getQuantity(108.165, OHM),
    Quantities.getQuantity(200000.0, KILOVOLTAMPERE),
    Quantities.getQuantity(380.0, KILOVOLT),
    Quantities.getQuantity(110.0, KILOVOLT),
    Quantities.getQuantity(555.5, MetricPrefix.NANO(SIEMENS)),
    Quantities.getQuantity(-1.27, MetricPrefix.NANO(SIEMENS)),
    Quantities.getQuantity(1.5, PERCENT),
    Quantities.getQuantity(0, RADIAN),
    false,
    0,
    -5,
    5
  )

  val transformer1 = new Transformer2WInput(
    UUID.fromString("6e9d912b-b652-471b-84d2-6ed571e53a7b"),
    "HöS-Trafo_S1",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    slackNode,
    node1,
    1,
    trafoType,
    0,
    false
  )

  val (hvGridContainer, hvSubGridGates) = {
    val nodes = Set(node1, node2, node3, node4, slackNode)
    val lines = Set(line1, line2, line3, line4, line5)
    val transformers = Set(transformer1)
    val rawGridElements = new RawGridElements(
      nodes.asJava,
      lines.asJava,
      transformers.asJava,
      Set.empty[Transformer3WInput].asJava,
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

    /* Sub grid gates are the apparent gates to superior grids + artificial one to underlying grids */
    val subGridGates: Vector[SubGridGate] =
      (rawGridElements.getTransformer2Ws.asScala.map(
        SubGridGate.fromTransformer2W
      ) ++ rawGridElements.getTransformer3Ws.asScala.flatMap(transformer =>
        Vector(
          SubGridGate.fromTransformer3W(transformer, ConnectorPort.B),
          SubGridGate.fromTransformer3W(transformer, ConnectorPort.C)
        )
      ) ++ Vector(
        build2wSubGridGate(
          UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75"),
          1,
          UUID.fromString("1129b00d-3d89-4a4a-8ae1-2a56041b95aa"),
          14
        ),
        build2wSubGridGate(
          UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0"),
          1,
          UUID.fromString("139c435d-e550-48d8-b590-ee897621f42a"),
          12
        ),
        build2wSubGridGate(
          UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c"),
          1,
          UUID.fromString("1676e48c-5353-4f06-b671-c579cf6a7072"),
          11
        ),
        build2wSubGridGate(
          UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae"),
          1,
          UUID.fromString("9237e237-01e9-446f-899f-c3b5cf69d288"),
          13
        )
      )).toVector

    (
      new SubGridContainer(
        "centerGrid",
        1,
        rawGridElements,
        systemParticipants,
        graphicElements
      ),
      subGridGates
    )
  }

  val (ehvGridContainer, ehvSubGridGates) = {
    val nodes = Set(slackNode)
    val lines = Set.empty[LineInput]
    val transformers = Set.empty[Transformer2WInput]
    val rawGridElements = new RawGridElements(
      nodes.asJava,
      lines.asJava,
      transformers.asJava,
      Set.empty[Transformer3WInput].asJava,
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

    val subGridGates: Vector[SubGridGate] =
      (Set(transformer1).map(
        SubGridGate.fromTransformer2W
      ) ++ rawGridElements.getTransformer3Ws.asScala
        .flatMap(transformer =>
          Vector(
            SubGridGate.fromTransformer3W(transformer, ConnectorPort.B),
            SubGridGate.fromTransformer3W(transformer, ConnectorPort.C)
          )
        )).toVector

    (
      new SubGridContainer(
        "superiorGrid",
        1000,
        rawGridElements,
        systemParticipants,
        graphicElements
      ),
      subGridGates
    )
  }
}
