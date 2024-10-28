/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.grid

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.connector._
import edu.ie3.datamodel.models.input.connector.`type`.Transformer2WTypeInput
import edu.ie3.datamodel.models.input.container.{
  RawGridElements,
  SystemParticipants,
}
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.system._
import edu.ie3.datamodel.models.input.{
  MeasurementUnitInput,
  NodeInput,
  OperatorInput,
}
import edu.ie3.datamodel.models.profile.LoadProfile
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.util.TestGridFactory
import edu.ie3.util.quantities.PowerSystemUnits._
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units._

import java.util.UUID
import javax.measure.quantity.{Energy, Power}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** Provides the high voltage level of SIMONA's test grid. Only consists of
  * lines, nodes and transformers.
  */
trait DbfsTestGridWithParticipants extends SubGridGateMokka {
  // 2 hv nodes, 1 slack ehv node
  protected val node1 = new NodeInput(
    UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c"),
    "HV_NET1_Station_1",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1.0, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.HV,
    1,
  )

  protected val supNodeA = new NodeInput(
    UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e"),
    "HV_NET1_Station_1_380",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1.0, PU),
    true,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.EHV_380KV,
    1000,
  )

  // 1 transformer from hv to ehv
  private val trafoType = new Transformer2WTypeInput(
    UUID.randomUUID(),
    "EHV-HV_1",
    Quantities.getQuantity(5.415, OHM),
    Quantities.getQuantity(108.165, OHM),
    Quantities.getQuantity(200000.0, KILOVOLTAMPERE),
    Quantities.getQuantity(380.0, KILOVOLT),
    Quantities.getQuantity(110.0, KILOVOLT),
    Quantities.getQuantity(555.5, NANOSIEMENS),
    Quantities.getQuantity(-1.27, NANOSIEMENS),
    Quantities.getQuantity(1.5, PERCENT),
    Quantities.getQuantity(0, RADIAN),
    false,
    0,
    -5,
    5,
  )

  private val transformer1 = new Transformer2WInput(
    UUID.fromString("6e9d912b-b652-471b-84d2-6ed571e53a7b"),
    "EHV-Trafo_S2",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    supNodeA,
    node1,
    1,
    trafoType,
    0,
    false,
  )

  protected val load1 = new LoadInput(
    UUID.randomUUID(),
    "load_1",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    node1,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
    null,
    LoadProfile.DefaultLoadProfiles.NO_LOAD_PROFILE,
    false,
    Quantities.getQuantity(300000, KILOWATTHOUR): ComparableQuantity[Energy],
    Quantities.getQuantity(150, MEGAVOLTAMPERE): ComparableQuantity[Power],
    0.9,
  )

  protected val (hvGridContainer, hvSubGridGates) = {
    // LinkedHashSet in order to preserve the given order.
    // This is important as long as only one slack node between two sub grids can exist
    val nodes =
      mutable.LinkedHashSet(node1, supNodeA)
    val transformers = Set(transformer1)
    val rawGridElements = new RawGridElements(
      nodes.asJava,
      Set.empty[LineInput].asJava,
      transformers.asJava,
      Set.empty[Transformer3WInput].asJava,
      Set.empty[SwitchInput].asJava,
      Set.empty[MeasurementUnitInput].asJava,
    )

    val systemParticipants = new SystemParticipants(
      Set.empty[BmInput].asJava,
      Set.empty[ChpInput].asJava,
      Set.empty[EvcsInput].asJava,
      Set.empty[EvInput].asJava,
      Set.empty[FixedFeedInInput].asJava,
      Set.empty[HpInput].asJava,
      Set(load1).asJava,
      Set.empty[PvInput].asJava,
      Set.empty[StorageInput].asJava,
      Set.empty[WecInput].asJava,
    )

    /* Sub grid gates are the apparent gates to superior grids + artificial one to underlying grids */
    val subGridGates: Seq[SubGridGate] =
      rawGridElements.getTransformer2Ws.asScala.toSeq.map(
        SubGridGate.fromTransformer2W
      ) ++ rawGridElements.getTransformer3Ws.asScala.flatMap(transformer =>
        Seq(
          SubGridGate.fromTransformer3W(transformer, ConnectorPort.B),
          SubGridGate.fromTransformer3W(transformer, ConnectorPort.C),
        )
      )

    (
      TestGridFactory.createSubGrid(
        gridName = "gridAgentWithParticipants",
        subgrid = 1,
        rawGridElements = rawGridElements,
        systemParticipants = systemParticipants,
      ),
      subGridGates,
    )
  }

}
