/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.grid

import edu.ie3.datamodel.models.input.connector.ConnectorPort
import edu.ie3.simona.model.grid.Transformer3wPowerFlowCase._
import edu.ie3.simona.model.grid._
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.OperationInterval
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import squants.energy.Watts
import squants.{Amperes, Each}
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.Dimensionless

/** Hold my cup of coffee and let me mock you some models.
  */
trait GridComponentsMokka extends MockitoSugar {

  protected def nodeModel(
      uuid: UUID = UUID.randomUUID(),
      subnetNo: Int = 1,
  ): NodeModel = {
    val node = mock[NodeModel]
    when(node.uuid).thenReturn(uuid)
    when(node.subnet).thenReturn(subnetNo)
    node
  }

  protected def lineModel(
      nodeA: UUID,
      nodeB: UUID,
      iNom: Double = 10.0,
      uuid: UUID = UUID.randomUUID(),
  ): LineModel = {
    val line = mock[LineModel]
    when(line.uuid).thenReturn(uuid)
    when(line.nodeAUuid).thenReturn(nodeA)
    when(line.nodeBUuid).thenReturn(nodeB)
    when(line.iNom).thenReturn(Amperes(iNom))

    line
  }

  protected def dummyTappingModel(
      deltaV: ComparableQuantity[Dimensionless] = 1.5.asPercent,
      currentTapPos: Int = 1,
      tapMax: Int = 5,
      tapMin: Int = -5,
      tapNeutr: Int = 0,
      autoTap: Boolean = true,
      tapSide: ConnectorPort = ConnectorPort.A,
  ): TransformerTappingModel =
    TransformerTappingModel(
      deltaV,
      currentTapPos,
      tapMax,
      tapMin,
      tapNeutr,
      autoTap,
      tapSide,
    )

  protected def dummyTransformerModel(
      tappingModel: TransformerTappingModel
  ): TransformerModel =
    TransformerModel(
      UUID.randomUUID(),
      id = "dummy",
      operationInterval = OperationInterval(0L, 1L),
      hvNodeUuid = UUID.randomUUID(),
      lvNodeUuid = UUID.randomUUID(),
      tappingModel,
      amount = 1,
      voltRatioNominal = BigDecimal(110),
      iNomHv = Amperes(1),
      iNomLv = Amperes(10),
      sRated = Watts(1),
      r = Each(1),
      x = Each(1),
      g = Each(1),
      b = Each(1),
    )

  protected def dummyTransformer3wModel(
      tappingModel: TransformerTappingModel
  ): Transformer3wModel =
    Transformer3wModel(
      UUID.randomUUID(),
      id = "dummy",
      operationInterval = OperationInterval(0L, 1L),
      hvNodeUuid = UUID.randomUUID(),
      mvNodeUuid = UUID.randomUUID(),
      lvNodeUuid = UUID.randomUUID(),
      nodeInternalUuid = UUID.randomUUID(),
      voltRatioNominal = BigDecimal(110),
      tappingModel,
      amount = 1,
      powerFlowCase = PowerFlowCaseA,
      sRated = Watts(1),
      r = Each(1),
      x = Each(1),
      g = Each(1),
      b = Each(1),
    )

  protected def mockTransformerTappingModel(
      uuid: UUID = UUID.randomUUID(),
      autoTap: Boolean,
      tapMax: Int,
      tapMin: Int,
      currentTapPos: Int,
      deltaV: ComparableQuantity[Dimensionless],
  ): TransformerModel = {
    val transformer = mock[TransformerModel]
    when(transformer.uuid).thenReturn(uuid)

    when(transformer.hasAutoTap).thenReturn(autoTap)
    when(transformer.tapMax).thenReturn(tapMax)
    when(transformer.tapMin).thenReturn(tapMin)
    when(transformer.currentTapPos).thenReturn(currentTapPos)
    when(transformer.deltaV).thenReturn(deltaV)

    transformer
  }

  protected def mockTransformerModel(
      uuid: UUID = UUID.randomUUID(),
      hasAutoTap: Boolean = false,
  ): TransformerModel = {
    val transformer = mock[TransformerModel]
    when(transformer.uuid).thenReturn(uuid)
    when(transformer.hasAutoTap).thenReturn(hasAutoTap)

    transformer
  }

  protected def mockTransformer3wModel(
      uuid: UUID = UUID.randomUUID(),
      hasAutoTap: Boolean = false,
  ): (Transformer3wModel, Transformer3wModel, Transformer3wModel) = {
    val transformerA = mock[Transformer3wModel]
    val transformerB = mock[Transformer3wModel]
    val transformerC = mock[Transformer3wModel]
    when(transformerA.uuid).thenReturn(uuid)
    when(transformerB.uuid).thenReturn(uuid)
    when(transformerC.uuid).thenReturn(uuid)

    when(transformerA.hasAutoTap).thenReturn(hasAutoTap)
    when(transformerB.hasAutoTap).thenReturn(hasAutoTap)
    when(transformerC.hasAutoTap).thenReturn(hasAutoTap)

    when(transformerA.powerFlowCase).thenReturn(PowerFlowCaseA)
    when(transformerB.powerFlowCase).thenReturn(
      Transformer3wPowerFlowCase.PowerFlowCaseB
    )
    when(transformerC.powerFlowCase).thenReturn(PowerFlowCaseC)

    (transformerA, transformerB, transformerC)
  }

}
