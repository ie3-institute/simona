/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.grid

import edu.ie3.simona.model.grid.{
  NodeModel,
  TransformerModel,
  TransformerTappingModel,
}
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits._
import squants.{Amperes, Each}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units._

import java.util.UUID
import javax.measure.quantity.ElectricPotential

/** Same as [[FiveLinesWithNodes]] but extended with an additional
  * [[TransformerModel]] and two corresponding additional nodes. For details on
  * used lines and nodes see [[FiveLinesWithNodes]].
  *
  * {{{
  *                  (6)
  *                  /
  *                 /
  *             trafo
  *         (5)  /
  *          |  /
  *          | /
  * (0)-----(3)-----(4)
  * |
  * |
  * (1)-----(2)
  * }}}
  *
  * Reference System: 400 kVA @ 10 kV --> Reference admittance: 4 mS
  *
  * Transformer type: d_v = 1.5 % tap_min = -13 tap_max = 13 tap_neut = 0
  * is_autotap = true tapSide = high voltage side (ConnectorPort.B -> None in
  * [[TransformerTappingModel]]) r = 30.25 Ω -> 0.001 p.u. x = 4.5375 Ω ->
  * 0.00015 p.u. g = 0 -> 0.0 p.u. b = 1.1 nS -> -0.000033275 p.u.K s_rated =
  * 40000 kVA
  * -> iNomHv = 209.9455524325912 A
  * -> iNomLv = 2309.401076758503 A
  *
  * Transformer model: tap_side = hv (element port A) tap_pos = 0 amount = 1 vHv
  * \= 110 kV (node 6) vLv = 10 kV (node 3)
  *
  * Transformer admittance matrix data: gij = bij = g0 hv = 0 b0 hv =
  * -0.0000166375 // todo CK check g0 lv = b0 lv =
  */
trait BasicGrid extends FiveLinesWithNodes with DefaultTestData {

  protected val transformerHvVoltLvl: ComparableQuantity[ElectricPotential] =
    Quantities.getQuantity(110, KILOVOLT)

  // nodes
  // / adapt node0 and remove slack capabilities
  override def node0: NodeModel =
    _nodeCreator(
      "node0",
      "5f2b9b3e-faa6-493b-a6ee-22a4a516ad0e",
      false,
      linesRatedVoltage,
    )
  // / create transformer HV node @ 110kV
  def node6: NodeModel =
    _nodeCreator(
      "node6",
      "3d2d3626-5043-4ec7-892d-cead983c046e",
      true,
      transformerHvVoltLvl,
    )

  override protected def nodes: Seq[NodeModel] =
    super.nodes :+ node6

  // update nodeToIndexMap
  // nodeToIndexMap
  override protected def nodeUuidToIndexMap: Map[UUID, Int] =
    super.nodeUuidToIndexMap + (node6.uuid -> 6)

  // transformer
  // / transformer tapping model
  protected val transformerTappingModel: TransformerTappingModel =
    TransformerTappingModel(
      Quantities.getQuantity(1.5, PERCENT),
      0,
      13,
      -13,
      0,
      autoTap = true,
    )

  // / electric params in pu
  protected val transformerRInPu: squants.Dimensionless =
    Each(0.001d)
  protected val transformerXInPu: squants.Dimensionless =
    Each(0.00015d)
  protected val transformerGInPu: squants.Dimensionless =
    Each(0d)
  protected val transformerBInPu: squants.Dimensionless =
    Each(-0.000033275d)

  // / iNomHv, iNomLv
  protected val iNomHv: squants.electro.ElectricCurrent =
    Amperes(209.9455524325912d)
  protected val iNomLv: squants.electro.ElectricCurrent =
    Amperes(2309.401076758503d)

  // / transformer
  protected val transformer2wModel = new TransformerModel(
    UUID.fromString("a28eb631-2c26-4831-9d05-aa1b3f90b96a"),
    "transformer63",
    defaultOperationInterval,
    node6.uuid,
    node3.uuid,
    transformerTappingModel,
    1,
    BigDecimal("11"),
    iNomHv,
    iNomLv,
    transformerRInPu,
    transformerXInPu,
    transformerGInPu,
    transformerBInPu,
  )

  // init transformer tapping
  transformer2wModel.initTapping()

  // enable the transformer model
  transformer2wModel.enable()
//  // transformer admittance matrix
//  protected val trafoAdmittanceMatrix: DenseMatrix[C] = ??? // todo CK when transformer tapping is revised

}
