/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.grid

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.connector.`type`.Transformer2WTypeInput
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
import edu.ie3.datamodel.models.input.{
  MeasurementUnitInput,
  NodeInput,
  OperatorInput,
}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.grid.{RefSystem, TransformerModel}
import edu.ie3.simona.util.TestGridFactory
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.OperationInterval
import squants.electro.Kilovolts
import squants.energy.Kilowatts
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units._

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters._

/** Represents a simple test grid, consisting of one transformer connecting two
  * nodes. It is a representation of a real SGB Smit DTTH 630 kVA transformer
  * (https://www.sgb-smit.com/fileadmin/user_upload/Downloads/Broschueren/Cast_Resin_Transformers/GT_Technik_UniQ_D.pdf)
  * It's tapping parameters are artificial.
  */
trait TransformerTestGrid {
  val defaultSimulationStart: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2019-01-01T00:00:00Z")
  val defaultSimulationEnd: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2019-12-31T23:00:00Z")

  private val operationTimeBuilder = OperationTime.builder()
  operationTimeBuilder.withStart(defaultSimulationStart)
  operationTimeBuilder.withEnd(defaultSimulationEnd)
  protected val defaultOperationTime: OperationTime =
    operationTimeBuilder.build()

  protected val defaultOperationInterval: OperationInterval =
    SystemComponent.determineOperationInterval(
      defaultSimulationStart,
      defaultSimulationEnd,
      defaultOperationTime,
    )

  def mainRefSystem: RefSystem = {
    val nominalPower = Kilowatts(400d)
    val nominalVoltage = Kilovolts(0.4d)
    RefSystem(nominalPower, nominalVoltage)
    /* Z_Ref = 0.4 Ω, Y_Ref = 2.5 Siemens */
  }

  val transformerTypeTapHv = new Transformer2WTypeInput(
    UUID.randomUUID(),
    "SGB Smit DTTH 630 kVA",
    Quantities.getQuantity(1.83925, OHM),
    Quantities.getQuantity(6.07698, OHM),
    Quantities.getQuantity(630d, KILOVOLTAMPERE),
    Quantities.getQuantity(10d, KILOVOLT),
    Quantities.getQuantity(0.4d, KILOVOLT),
    Quantities.getQuantity(0d, SIEMENS),
    Quantities.getQuantity(-15e-6, SIEMENS),
    Quantities.getQuantity(2.5d, PERCENT),
    Quantities.getQuantity(0d, DEGREE_GEOM),
    false,
    0,
    -10,
    10,
  )

  val transformerTypeTapLv = new Transformer2WTypeInput(
    UUID.randomUUID(),
    "SGB Smit DTTH 630 kVA",
    Quantities.getQuantity(1.83925, OHM),
    Quantities.getQuantity(6.07698, OHM),
    Quantities.getQuantity(630d, KILOVOLTAMPERE),
    Quantities.getQuantity(10d, KILOVOLT),
    Quantities.getQuantity(0.4d, KILOVOLT),
    Quantities.getQuantity(0d, SIEMENS),
    Quantities.getQuantity(-15e-6, SIEMENS),
    Quantities.getQuantity(2.5d, PERCENT),
    Quantities.getQuantity(0d, DEGREE_GEOM),
    true,
    0,
    -10,
    10,
  )

  val nodeA = new NodeInput(
    UUID.fromString("40ef4309-bfaa-4011-8cde-5bd0f2c96a4b"),
    "node_a",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1d, PU),
    true,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.MV_10KV,
    0,
  )

  val nodeB = new NodeInput(
    UUID.fromString("4ebb2ed8-e404-4f9c-a3bb-3f7c557f49bf"),
    "node_b",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1d, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.LV,
    1,
  )

  val transformerInputTapHv = new Transformer2WInput(
    UUID.fromString("9776d769-dfb9-40c4-a7d0-678214c59623"),
    "dut",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeA,
    nodeB,
    1,
    transformerTypeTapHv,
    0,
    false,
  )

  val transformerModelTapHv: TransformerModel = TransformerModel(
    transformerInputTapHv,
    mainRefSystem,
    defaultSimulationStart,
    defaultSimulationEnd,
  )

  val transformerInputTapLv = new Transformer2WInput(
    UUID.fromString("3e2a90f0-78ca-42e6-b056-0bc3f0a04c47"),
    "dut",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeA,
    nodeB,
    1,
    transformerTypeTapLv,
    0,
    false,
  )

  val transformerModelTapLv: TransformerModel = TransformerModel(
    transformerInputTapLv,
    mainRefSystem,
    defaultSimulationStart,
    defaultSimulationEnd,
  )

  val gridTapHv: SubGridContainer = {
    val rawGridElements = new RawGridElements(
      Set(nodeA, nodeB).asJava,
      Set.empty[LineInput].asJava,
      Set(transformerInputTapHv).asJava,
      Set.empty[Transformer3WInput].asJava,
      Set.empty[SwitchInput].asJava,
      Set.empty[MeasurementUnitInput].asJava,
    )
    TestGridFactory.createSubGrid(
      gridName = "transformer_test_grid",
      subgrid = 1,
      rawGridElements = rawGridElements,
    )
  }

  val gridTapLv: SubGridContainer = {
    val rawGridElements = new RawGridElements(
      Set(nodeA, nodeB).asJava,
      Set.empty[LineInput].asJava,
      Set(transformerInputTapLv).asJava,
      Set.empty[Transformer3WInput].asJava,
      Set.empty[SwitchInput].asJava,
      Set.empty[MeasurementUnitInput].asJava,
    )
    TestGridFactory.createSubGrid(
      gridName = "transformer_test_grid",
      subgrid = 1,
      rawGridElements = rawGridElements,
    )
  }
}
