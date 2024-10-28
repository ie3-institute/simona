/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.math.Complex
import breeze.numerics.pow
import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.datamodel.models.input.connector.{
  ConnectorPort,
  Transformer2WInput,
}
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.OperationInterval
import squants.Each
import squants.electro.{Kilovolts, Ohms, Siemens}
import squants.energy.Watts
import tech.units.indriya.unit.Units._

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.Quantity
import javax.measure.quantity.ElectricCurrent
import scala.math.BigDecimal.RoundingMode

/** This model represents a two winding transformer with tapping capabilities
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param hvNodeUuid
  *   high voltage side node uuid of the model
  * @param lvNodeUuid
  *   low voltage side node uuid of the model
  * @param transformerTappingModel
  *   the [[TransformerTappingModel]]
  * @param amount
  *   number of parallel transformers
  * @param voltRatioNominal
  *   nominal voltage ratio, with the tap changer in neutral position
  * @param iNomHv
  *   nominal current on the high voltage side of the transformer
  * @param iNomLv
  *   nominal current on the low voltage side of the transformer
  * @param r
  *   resistance r, real part of the transformer impedance z (referenced to the
  *   nominal impedance of the grid) in p.u.
  * @param x
  *   reactance x, imaginary part of the transformer impedance z(referenced to
  *   the nominal impedance of the grid) in p.u.
  * @param g
  *   conductance g, real part of the transformer admittance y (referenced to
  *   the nominal impedance of the grid) in p.u.
  * @param b
  *   susceptance b, imaginary part of the transformer admittance y (referenced
  *   to the nominal impedance of the grid)in p.u.
  */
final case class TransformerModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    hvNodeUuid: UUID,
    lvNodeUuid: UUID,
    protected val transformerTappingModel: TransformerTappingModel,
    amount: Int,
    voltRatioNominal: BigDecimal,
    iNomHv: squants.electro.ElectricCurrent,
    iNomLv: squants.electro.ElectricCurrent,
    protected val r: squants.Dimensionless,
    protected val x: squants.Dimensionless,
    protected val g: squants.Dimensionless,
    protected val b: squants.Dimensionless,
) extends SystemComponent(
      uuid,
      id,
      operationInterval,
    )
    with PiEquivalentCircuit
    with TransformerTapping {

  private val tapSide = transformerTappingModel.tapSide
}

case object TransformerModel {

  def apply(
      transformerInput: Transformer2WInput,
      refSystem: RefSystem,
      startDate: ZonedDateTime,
      endDate: ZonedDateTime,
  ): TransformerModel = {

    // validate the input model first
    validateInputModel(transformerInput, refSystem)

    // build the transformer model
    buildTransformerModel(transformerInput, refSystem, startDate, endDate)

  }

  /** Internal method to construct a new [[TransformerModel]] based on a
    * provided [[Transformer2WInput]]
    *
    * @param transformerInput
    *   instance of [[Transformer2WInput]] this [[TransformerModel]] should be
    *   built from
    * @param gridRefSystem
    *   the [[RefSystem]] of the subnet the transformer is located in
    * @param simulationStartDate
    *   Starting date of the simulation
    * @param simulationEndDate
    *   End date of the simulation
    * @return
    *   a ready-to-use [[TransformerModel]] with referenced electric parameters
    */
  private def buildTransformerModel(
      transformerInput: Transformer2WInput,
      gridRefSystem: RefSystem,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): TransformerModel = {

    // get referenced electric values
    val trafoType = transformerInput.getType
    val voltRatioNominal = BigDecimal
      .apply(trafoType.getvRatedA().to(KILOVOLT).getValue.toString)
      .setScale(5, RoundingMode.HALF_UP) / BigDecimal
      .apply(trafoType.getvRatedB().to(KILOVOLT).getValue.toString)
      .setScale(5, RoundingMode.HALF_UP)
    val squaredNominalVoltRatio = voltRatioNominal * voltRatioNominal

    /* Determine the physical pi equivalent circuit diagram parameters from the perspective
     * of the transformer's low voltage side */
    val (rTrafo, xTrafo, gTrafo, bTrafo) = (
      Ohms(
        trafoType.getrSc
          .to(OHM)
          .divide(squaredNominalVoltRatio)
          .getValue
          .doubleValue()
      ),
      Ohms(
        trafoType.getxSc
          .to(OHM)
          .divide(squaredNominalVoltRatio)
          .getValue
          .doubleValue()
      ),
      Siemens(
        trafoType.getgM
          .to(SIEMENS)
          .multiply(squaredNominalVoltRatio)
          .getValue
          .doubleValue()
      ),
      Siemens(
        trafoType.getbM
          .to(SIEMENS)
          .multiply(squaredNominalVoltRatio)
          .getValue
          .doubleValue()
      ),
    )

    /* Transfer the dimensionless parameters into the grid reference system */
    val (r, x, g, b) = (
      gridRefSystem.rInPu(rTrafo),
      gridRefSystem.xInPu(xTrafo),
      gridRefSystem.gInPu(gTrafo),
      gridRefSystem.bInPu(bTrafo),
    )

    // iNomHv, iNomLv
    val calcINom
        : squants.electro.ElectricPotential => squants.electro.ElectricCurrent = {
      portVoltage: squants.electro.ElectricPotential =>
        Watts(
          trafoType.getsRated
            .to(VOLTAMPERE)
            .getValue
            .doubleValue()
        ) / Math.sqrt(3) / portVoltage

    }
    val (iNomHv, iNomLv) =
      (
        calcINom(
          Kilovolts(
            trafoType.getvRatedA.to(KILOVOLT).getValue.doubleValue()
          )
        ),
        calcINom(
          Kilovolts(
            trafoType.getvRatedB.to(KILOVOLT).getValue.doubleValue()
          )
        ),
      )

    // get the element port, where the transformer tap is located
    // if trafoType.isTapSide == true, tapper is on the low voltage side (== ConnectorPort.B)
    val tapSide = if (trafoType.isTapSide) ConnectorPort.B else ConnectorPort.A

    // / transformer tapping
    val transformerTappingModel = TransformerTappingModel(
      trafoType.getdV(),
      transformerInput.getTapPos,
      trafoType.getTapMax,
      trafoType.getTapMin,
      trafoType.getTapNeutr,
      transformerInput.isAutoTap,
      tapSide,
    )

    val operationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        transformerInput.getOperationTime,
      )

    val transformerModel = new TransformerModel(
      transformerInput.getUuid,
      transformerInput.getId,
      operationInterval,
      transformerInput.getNodeA.getUuid,
      transformerInput.getNodeB.getUuid,
      transformerTappingModel,
      transformerInput.getParallelDevices,
      voltRatioNominal,
      iNomHv,
      iNomLv,
      r,
      x,
      g,
      b,
    )

    // if the transformer input model is in operation, enable the model
    if (operationInterval.includes(SimonaConstants.FIRST_TICK_IN_SIMULATION))
      transformerModel.enable()

    // initialize tapping
    transformerModel.initTapping()

    // sanity check after creation for piEquivalent parameters r,x,g,b
    transformerModel.piEquivalentSanityCheck("transformer model")

    transformerModel
  }

  /** Central place to check and validate a provided [[Transformer2WInput]] to
    * see if all parameters are reasonable from a [[TransformerModel]]
    * perspective, which means for construction of a [[TransformerModel]]. All
    * sanity checks and validations should be putted below!
    *
    * @param transformerInput
    *   instance of [[Transformer2WInput]] that should be validated
    * @param refSystem
    *   the main [[RefSystem]] of the grid
    */
  def validateInputModel(
      transformerInput: Transformer2WInput,
      refSystem: RefSystem,
  ): Unit = {
    val trafoType = transformerInput.getType

    // check if transformer params are given for the low voltage side
    val vRef = refSystem.nominalVoltage.toKilovolts
    if (
      Math.abs(
        vRef - trafoType.getvRatedA.to(KILOVOLT).getValue.doubleValue()
      )
        < Math.abs(
          vRef - trafoType.getvRatedB.to(KILOVOLT).getValue.doubleValue()
        )
    )
      throw new InvalidGridException(
        s"The rated voltage of the high voltage side (${transformerInput.getType.getvRatedA()}) of transformer " +
          s"${transformerInput.getUuid} is closer to the reference voltage ($vRef), as the rated voltage of the " +
          s"low voltage side (${transformerInput.getType.getvRatedB()}). Is the transformer connected the right way " +
          s"round?"
      )

    // valid r,x,g,b values?
    val (r, x, g, b) =
      (trafoType.getrSc, trafoType.getxSc, trafoType.getgM, trafoType.getbM)
    if (
      r.getValue.doubleValue.isNaN ||
      x.getValue.doubleValue.isNaN ||
      g.getValue.doubleValue.isNaN ||
      b.getValue.doubleValue.isNaN
    )
      throw new InvalidGridException(
        s"Attempted to create a transformer with invalid values.\ntrafo: ${transformerInput.getUuid}, type: ${trafoType.getUuid}, r: $r, x: $x, g: $g, b: $b"
      )
  }

  /** Calculate the phase-to-ground admittance of a given transformer model
    *
    * @param transformerModel
    *   instance of [[TransformerModel]] the phase-to-ground admittance should
    *   be calculated for
    * @param port
    *   Port of the transformer, the result is requested for
    * @return
    *   phase-to-ground admittance Y_0 of the transformer model in p.u.
    */
  def y0(transformerModel: TransformerModel, port: ConnectorPort): Complex = {
    val amount = transformerModel.amount
    val tapSide = transformerModel.tapSide
    val tapRatio = transformerModel.tapRatio
    val g0 = transformerModel.g0().value.doubleValue()
    val b0 = transformerModel.b0().value.doubleValue()
    val gij = transformerModel.gij().value.doubleValue()
    val bij = transformerModel.bij().value.doubleValue()

    /* the following code duplicates are by intention to get a fast overview about the formulas used. Even
     * if code aggregation is possible it is intended not to do so, to improve readability. */
    val y0Single = (port, tapSide) match {
      case (ConnectorPort.A, ConnectorPort.A) =>
        /* Admittance for HV side, tap changer is on HV side */
        val gii = 1 / pow(tapRatio, 2) * ((1 - tapRatio) * gij + g0 / 2)
        val bii = 1 / pow(tapRatio, 2) * ((1 - tapRatio) * bij + b0 / 2)
        Complex(gii, bii)
      case (ConnectorPort.A, ConnectorPort.B) =>
        /* Admittance for HV side, tap changer is on LV side */
        val gii = (1 - 1 / tapRatio) * gij + g0 / 2
        val bii = (1 - 1 / tapRatio) * bij + b0 / 2
        Complex(gii, bii)
      case (ConnectorPort.B, ConnectorPort.A) =>
        /* Admittance for LV side, tap changer is on HV side */
        val gjj = (1 - 1 / tapRatio) * gij + g0 / 2
        val bjj = (1 - 1 / tapRatio) * bij + b0 / 2
        Complex(gjj, bjj)
      case (ConnectorPort.B, ConnectorPort.B) =>
        /* Admittance for LV side, tap changer is on LV side */
        val gjj = 1 / pow(tapRatio, 2) * ((1 - tapRatio) * gij + g0 / 2)
        val bjj = 1 / pow(tapRatio, 2) * ((1 - tapRatio) * bij + b0 / 2)
        Complex(gjj, bjj)
      case _ =>
        throw new InvalidGridException(
          s"A two winding transformer can not calculate the phase-to-ground admittance for port $port"
        )
    }
    y0Single * amount
  }

  /** Calculate the branch admittance of a given transformer model. NOTE: Tap
    * ratio and amount is always considered!
    *
    * @param transformerModel
    *   instance of [[TransformerModel]] the branch admittance should be
    *   calculated for
    * @return
    *   branch admittance Y_ij between node a and b of the transformer model in
    *   p.u.
    */
  def yij(transformerModel: TransformerModel): Complex = {
    val amount = transformerModel.amount
    val tapRatio = transformerModel.tapRatio

    new Complex(
      transformerModel.gij().value.doubleValue(),
      transformerModel.bij().value.doubleValue(),
    ) * amount / tapRatio
  }

  /** Calculates the utilisation of a given transformer model
    *
    * @param transformerModel
    *   instance of [[Transformer3wModel]] the utilisation should be calculated
    *   from
    * @param iNodeHv
    *   current @ high voltage node
    * @param iNodeLv
    *   current @ low voltage node
    * @return
    *   the current utilisation of the transformer in percentage
    */
  def utilisation(
      transformerModel: TransformerModel,
      iNodeHv: Quantity[ElectricCurrent],
      iNodeLv: Quantity[ElectricCurrent],
  ): squants.Dimensionless = {
    Each(
      Math.max(
        iNodeHv.getValue.doubleValue() / transformerModel.iNomHv.value
          .doubleValue(),
        iNodeLv.getValue.doubleValue() / transformerModel.iNomLv.value
          .doubleValue(),
      ) * 100
    )
  }

}
