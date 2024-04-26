/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.grid

import edu.ie3.simona.model.grid.{
  LineModel,
  NodeModel,
  Transformer3wModel,
  TransformerModel,
  TransformerTappingModel,
}
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import squants.Amperes
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.Dimensionless

/** Hold my cup of coffee and let me mock you some models.
  */
trait GridComponentsMokka extends MockitoSugar {

  protected def nodeModel(
      uuid: UUID = UUID.randomUUID()
  ): NodeModel = {
    val node = mock[NodeModel]
    when(node.uuid).thenReturn(uuid)
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

  protected def mockTransformerModel(
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

  protected def mockTransformer3wModel(
      uuid: UUID
  ): Transformer3wModel = {
    val transformer = mock[Transformer3wModel]
    when(transformer.uuid).thenReturn(uuid)

    transformer
  }

}
