/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.result

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.connector.Transformer3WInput
import edu.ie3.datamodel.models.input.connector.`type`.Transformer3WTypeInput
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  Transformer2WResult,
}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.model.grid._
import edu.ie3.simona.test.common.ConfigTestData
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits.PU
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import squants.electro.Kilovolts
import squants.energy.Kilowatts
import squants.{Amperes, Radians}
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID

trait CongestedComponentsTestData extends ConfigTestData with DbfsTestGrid {

  val startTime: ZonedDateTime = TimeUtil.withDefaults.toZonedDateTime(
    simonaConfig.simona.time.startDateTime
  )

  val endTime: ZonedDateTime = startTime.plusHours(2)

  protected val voltageLimits: VoltageLimits = VoltageLimits(0.9, 1.1)

  // additional inputs
  val nodeEHV = new NodeInput(
    UUID.randomUUID(),
    "EHV Node",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1.0, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.EHV_220KV,
    100,
  )

  val trafoType3W = new Transformer3WTypeInput(
    UUID.randomUUID(),
    "Hv-MV-LV",
    200000.asKiloVoltAmpere,
    140000.asKiloVoltAmpere,
    60000.asKiloVoltAmpere,
    380.asKiloVolt,
    220.asKiloVolt,
    110.asKiloVolt,
    3.asOhm,
    2.asOhm,
    1.asOhm,
    100.asOhm,
    80.asOhm,
    60.asOhm,
    555.5.asNanoSiemens,
    (-1.27).asNanoSiemens,
    1.5.asPercent,
    0.asDegreeGeom,
    0,
    -5,
    5,
  )

  val transformer3W = new Transformer3WInput(
    UUID.randomUUID(),
    "Transformer3W",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    supNodeB,
    nodeEHV,
    node2,
    1,
    trafoType3W,
    0,
    false,
  )

  protected val gridModel: GridModel = {
    val refSystem = RefSystem(Kilowatts(60), Kilovolts(110))

    val model = GridModel(
      hvGridContainer,
      refSystem,
      voltageLimits,
      startTime,
      endTime,
      simonaConfig,
    )

    val transformerModel1 = TransformerModel(
      transformer1,
      refSystem,
      startTime,
      endTime,
    )

    val transformerModel2 = TransformerModel(
      transformer2,
      refSystem,
      startTime,
      endTime,
    )

    val transformer3wModel = Transformer3wModel(
      transformer3W,
      refSystem,
      1,
      startTime,
      endTime,
    )

    val components = model.gridComponents
    val updatedTransformers =
      components.transformers ++ Seq(transformerModel1, transformerModel2)
    val transformers3w = components.transformers3w

    val updatedComponents = components.copy(
      transformers = updatedTransformers,
      transformers3w = transformers3w + transformer3wModel,
    )

    model.copy(gridComponents = updatedComponents)
  }

  // node results
  val nodeResultA = new NodeResult(
    startTime,
    supNodeA.getUuid,
    0.89.asPu,
    0.asDegreeGeom,
  )

  val nodeResultB = new NodeResult(
    startTime,
    supNodeB.getUuid,
    0.9.asPu,
    0.asDegreeGeom,
  )

  val nodeResult1 = new NodeResult(
    startTime,
    node1.getUuid,
    1.1.asPu,
    0.asDegreeGeom,
  )

  val nodeResult2 = new NodeResult(
    startTime,
    node2.getUuid,
    1.11.asPu,
    0.asDegreeGeom,
  )

  val nodeResult3 = new NodeResult(
    startTime,
    node3.getUuid,
    0.89.asPu,
    0.asDegreeGeom,
  )

  val nodeResult4 = new NodeResult(
    startTime,
    node4.getUuid,
    1.05.asPu,
    0.asDegreeGeom,
  )

  // line results
  val lineResult12 = new LineResult(
    startTime,
    line1To2.getUuid,
    1360d.asAmpere,
    0d.asDegreeGeom,
    1359d.asAmpere,
    0d.asDegreeGeom,
  )

  val lineResult23 = new LineResult(
    startTime,
    line2To3.getUuid,
    848d.asAmpere,
    0d.asDegreeGeom,
    849d.asAmpere,
    0d.asDegreeGeom,
  )

  // transformer results
  val transformerResult1 = new Transformer2WResult(
    startTime,
    transformer1.getUuid,
    344.asAmpere,
    0.asDegreeGeom,
    963.841.asAmpere,
    0.asDegreeGeom,
    0,
  )

  val transformerResult2 = new Transformer2WResult(
    startTime,
    transformer1.getUuid,
    321.asAmpere,
    0.asDegreeGeom,
    898.asAmpere,
    0.asDegreeGeom,
    0,
  )

  val transformerResult3W: PartialTransformer3wResult.PortC =
    PartialTransformer3wResult.PortC(
      startTime,
      transformer3W.getUuid,
      Amperes(307.353),
      Radians(0),
    )
}
