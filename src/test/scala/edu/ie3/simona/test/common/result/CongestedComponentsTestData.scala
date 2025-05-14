/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.result

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.connector.Transformer3WInput
import edu.ie3.datamodel.models.input.connector.`type`.Transformer3WTypeInput
import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  Transformer2WResult,
}
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.model.grid._
import edu.ie3.simona.test.common.ConfigTestData
import edu.ie3.simona.test.common.input.NodeInputTestData
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.*
import squants.electro.Kilovolts
import squants.energy.Kilowatts
import squants.{Amperes, Radians}

import java.time.ZonedDateTime
import java.util.UUID

trait CongestedComponentsTestData
    extends ConfigTestData
    with NodeInputTestData
    with DbfsTestGrid {

  protected val voltageLimits: VoltageLimits = VoltageLimits(0.9, 1.1)

  val trafoType3W = new Transformer3WTypeInput(
    UUID.randomUUID(),
    "EHV-EHV-HV",
    300.asMegaVoltAmpere,
    300.asMegaVoltAmpere,
    100.asMegaVoltAmpere,
    380.asKiloVolt,
    110.asKiloVolt,
    30.asKiloVolt,
    0.1444.asOhm,
    0.5776.asOhm,
    1.1552.asOhm,
    24.066121.asOhm,
    60.164118.asOhm,
    199.750106.asOhm,
    12.985.asNanoSiemens,
    -519.4864.asNanoSiemens,
    1.5.asPercent,
    0.asDegreeGeom,
    0,
    -10,
    10,
  )

  val transformer3W = new Transformer3WInput(
    UUID.randomUUID(),
    "Transformer3W",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    supNodeB,
    node2,
    nodeInputNoSlackMs30Kv,
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

  val lineResult34 = new LineResult(
    startTime,
    line3To4.getUuid,
    630d.asAmpere,
    0d.asDegreeGeom,
    630d.asAmpere,
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

  val transformerResult3W: PartialTransformer3wResult.PortB =
    PartialTransformer3wResult.PortB(
      startTime,
      transformer3W.getUuid,
      Amperes(1423.2795),
      Radians(0),
    )
}
