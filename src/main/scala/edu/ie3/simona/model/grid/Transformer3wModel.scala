/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import java.time.ZonedDateTime
import java.util.UUID
import breeze.math.Complex
import breeze.numerics.pow
import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.datamodel.models.input.connector.Transformer3WInput
import edu.ie3.datamodel.models.input.connector.`type`.Transformer3WTypeInput
import edu.ie3.simona.exceptions.{
  InvalidActionRequestException,
  InvalidParameterException
}
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.{
  PowerFlowCaseA,
  PowerFlowCaseB,
  PowerFlowCaseC
}
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.OperationInterval

import javax.measure.Quantity
import javax.measure.quantity.{Dimensionless, ElectricPotential}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import scala.math.BigDecimal.RoundingMode

/** This model represents a three winding transformer incorporating a virtual
  * node between the three voltage levels. From node A --> node Temp the
  * transformer is modeled as an ideal transformer with voltage ration 1:1. But
  * it captures the tap changer, because it's commonly installed on the highest
  * voltage side. From node Temp --> node B and node Temp --> node C the voltage
  * ratios are as defined by the rated voltages.
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param hvNodeUuid
  *   high voltage side node uuid of the model
  * @param mvNodeUuid
  *   medium voltage side node uuid of the model
  * @param lvNodeUuid
  *   low voltage side node uuid of the model
  * @param nodeInternalUuid
  *   uuid of internalNode
  * @param transformerTappingModel
  *   the [[TransformerTappingModel]]
  * @param amount
  *   number of parallel transformers
  * @param powerFlowCase
  *   the [[Transformer3wPowerFlowCase]]
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
final case class Transformer3wModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    hvNodeUuid: UUID,
    mvNodeUuid: UUID,
    lvNodeUuid: UUID,
    nodeInternalUuid: UUID,
    private val voltRatioNominal: BigDecimal,
    override protected val transformerTappingModel: TransformerTappingModel,
    amount: Int,
    powerFlowCase: Transformer3wPowerFlowCase,
    protected val r: ComparableQuantity[Dimensionless],
    protected val x: ComparableQuantity[Dimensionless],
    protected val g: ComparableQuantity[Dimensionless],
    protected val b: ComparableQuantity[Dimensionless]
) extends SystemComponent(
      uuid,
      id,
      operationInterval
    )
    with PiEquivalentCircuit
    with TransformerTapping {

  /** Update the transformer tap position. This method shall only be used in
    * [[PowerFlowCaseB]] and [[PowerFlowCaseC]] to handle the tapping decision
    * taken by the highest voltage [[edu.ie3.simona.agent.grid.GridAgent]]. For
    * the highest voltage [[edu.ie3.simona.agent.grid.GridAgent]] tap change use
    * [[incrTapPos()]] or [[decrTapPos()]].
    *
    * @param newTapPos
    *   the wanted tap position
    */
  override def updateTapPos(newTapPos: Int): Unit = {
    powerFlowCase match {
      case PowerFlowCaseA =>
        if (newTapPos != currentTapPos)
          super.updateTapPos(newTapPos)
      case _ =>
        throw new InvalidActionRequestException(
          s"Updating tap position for transformer3w $uuid is not allowed in power flow case B and C."
        )
    }
  }

  /** Increase transformer tap position by the provided delta value. The
    * operation is only allowed in power flow case A. In all other cases an
    * [[InvalidActionRequestException]] it thrown.
    *
    * @param deltaTap
    *   The amount of tap positions to increase
    * @throws InvalidActionRequestException
    *   in power flow cases B and C
    */
  override def incrTapPos(deltaTap: Int = 1): Unit = {
    powerFlowCase match {
      case PowerFlowCaseA =>
        super.incrTapPos(deltaTap)
      case _ =>
        throw new InvalidActionRequestException(
          s"Increasing tap position for transformer3w $uuid is not allowed in power flow case B and C."
        )
    }
  }

  /** Decrease transformer tap position by the provided delta value. The
    * operation is only allowed in power flow case A. In all other cases an
    * [[InvalidActionRequestException]] it thrown.
    *
    * @param deltaTap
    *   The amount of tap positions to increase
    * @throws InvalidActionRequestException
    *   in power flow cases B and C
    */
  override def decrTapPos(deltaTap: Int): Unit = {
    powerFlowCase match {
      case PowerFlowCaseA => super.decrTapPos(deltaTap)
      case _ =>
        throw new InvalidActionRequestException(
          s"Decreasing tap position for transformer3w $uuid is not allowed in power flow case B and C."
        )
    }
  }
}

case object Transformer3wModel {

  def apply(
      transformer3wInput: Transformer3WInput,
      refSystem: RefSystem,
      subnetNo: Int,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): Transformer3wModel = {
    // validate the input model first
    validateInputModel(transformer3wInput)

    // build
    buildTransformer3wModel(
      transformer3wInput,
      refSystem,
      subnetNo,
      simulationStartDate,
      simulationEndDate
    )
  }

  /** Denoting the internally used ports of a disjoint three winding transformer
    * model in SIMONA
    */
  object Transformer3wPort extends Enumeration {
    val A, B, C, INTERNAL = Value
  }

  /** Internal method to construct a new [[Transformer3wModel]] based on a
    * provided [[Transformer3WInput]]
    *
    * @param transformer3wInput
    *   instance of [[Transformer3WInput]] this [[Transformer3wModel]] should be
    *   built from
    * @param gridRefSystem
    *   the [[RefSystem]] of the subnet the transformer is located in
    * @param subnetNo
    *   the subnet the transformer is located in (used to determine the
    *   [[Transformer3wPowerFlowCase]]
    * @return
    *   a ready-to-use [[Transformer3wModel]] with referenced electric
    *   parameters
    */
  private def buildTransformer3wModel(
      transformer3wInput: Transformer3WInput,
      gridRefSystem: RefSystem,
      subnetNo: Int,
      startDate: ZonedDateTime,
      endDate: ZonedDateTime
  ): Transformer3wModel = {
    // build the model
    val trafo3wType = transformer3wInput.getType

    // / determine the power flow case based on the subnetNo
    val powerFlowCase: Transformer3wPowerFlowCase =
      Transformer3wPowerFlowCase(transformer3wInput, subnetNo)

    // / determine r,x,g,b
    val (r, x, g, b) = rxgbInPu(trafo3wType, gridRefSystem, powerFlowCase)

    // / get the internal node uuid
    val internalNodeUuid: UUID = transformer3wInput.getNodeInternal.getUuid

    // / transformer tapping
    val transformerTappingModel = TransformerTappingModel(
      trafo3wType.getdV(),
      powerFlowCase match {
        case PowerFlowCaseA => transformer3wInput.getTapPos
        case _              => trafo3wType.getTapNeutr
      },
      trafo3wType.getTapMax,
      trafo3wType.getTapMin,
      trafo3wType.getTapNeutr,
      transformer3wInput.isAutoTap
    )

    val voltRatioNominal = powerFlowCase match {
      case PowerFlowCaseA =>
        BigDecimal.apply("1").setScale(5, RoundingMode.HALF_UP)
      case PowerFlowCaseB =>
        BigDecimal
          .apply(trafo3wType.getvRatedA.to(KILOVOLT).getValue.doubleValue)
          .setScale(5, RoundingMode.HALF_UP) / BigDecimal
          .apply(trafo3wType.getvRatedB.to(KILOVOLT).getValue.doubleValue)
          .setScale(5, RoundingMode.HALF_UP)
      case PowerFlowCaseC =>
        BigDecimal
          .apply(trafo3wType.getvRatedA.to(KILOVOLT).getValue.doubleValue)
          .setScale(5, RoundingMode.HALF_UP) / BigDecimal
          .apply(trafo3wType.getvRatedC.to(KILOVOLT).getValue.doubleValue)
          .setScale(5, RoundingMode.HALF_UP)
    }

    val operationInterval =
      SystemComponent.determineOperationInterval(
        startDate,
        endDate,
        transformer3wInput.getOperationTime
      )

    val transformer3wModel = new Transformer3wModel(
      transformer3wInput.getUuid,
      transformer3wInput.getId,
      operationInterval,
      transformer3wInput.getNodeA.getUuid,
      transformer3wInput.getNodeB.getUuid,
      transformer3wInput.getNodeC.getUuid,
      internalNodeUuid,
      voltRatioNominal.setScale(5, RoundingMode.HALF_UP),
      transformerTappingModel,
      transformer3wInput.getParallelDevices,
      powerFlowCase,
      Quantities.getQuantity(r.getValue.doubleValue(), PU),
      Quantities.getQuantity(x.getValue.doubleValue(), PU),
      Quantities.getQuantity(g.getValue.doubleValue(), PU),
      Quantities.getQuantity(b.getValue.doubleValue(), PU)
    )

    // if the transformer3w input model is in operation, enable the model
    if (operationInterval.includes(SimonaConstants.FIRST_TICK_IN_SIMULATION))
      transformer3wModel.enable()

    // init the transformer tapping
    transformer3wModel.initTapping()

    // sanity check after creation for piEquivalent parameters r,x,g,b
    transformer3wModel.piEquivalentSanityCheck(
      "three winding transformer model"
    )

    transformer3wModel
  }

  /** Calculates the r,x,g,b parameters of the [[Transformer3wModel]] based on
    * it's [[Transformer3wPowerFlowCase]]
    *
    * @param transformerType
    *   the electrical parameter of the [[Transformer3WInput]]
    * @param refSystem
    *   the [[RefSystem]] of the subnet the transformer is located in
    * @param powerFlowCase
    *   the [[Transformer3wPowerFlowCase]] for this specific
    *   [[Transformer3wModel]]
    * @return
    *   r,x,g,b values in p.u.
    */
  private def rxgbInPu(
      transformerType: Transformer3WTypeInput,
      refSystem: RefSystem,
      powerFlowCase: Transformer3wPowerFlowCase
  ): (
      ComparableQuantity[Dimensionless],
      ComparableQuantity[Dimensionless],
      ComparableQuantity[Dimensionless],
      ComparableQuantity[Dimensionless]
  ) = {
    val transformerRefSystem =
      RefSystem(transformerType.getsRatedA, transformerType.getvRatedA)

    /* Extract the equivalent circuit diagram parameters from type, with the perspective of the
     * transformer */
    val (rTrafo, xTrafo, gTrafo, bTrafo) = powerFlowCase match {
      case PowerFlowCaseA =>
        (
          transformerRefSystem.rInPu(transformerType.getrScA),
          transformerRefSystem.xInPu(transformerType.getxScA),
          transformerRefSystem.gInPu(transformerType.getgM),
          transformerRefSystem.gInPu(transformerType.getbM)
        )
      case PowerFlowCaseB =>
        (
          transformerRefSystem.rInPu(transformerType.getrScB),
          transformerRefSystem.xInPu(transformerType.getxScB),
          Quantities.getQuantity(0d, PU),
          Quantities.getQuantity(0d, PU)
        )
      case PowerFlowCaseC =>
        (
          transformerRefSystem.rInPu(transformerType.getrScC),
          transformerRefSystem.xInPu(transformerType.getxScC),
          Quantities.getQuantity(0d, PU),
          Quantities.getQuantity(0d, PU)
        )
    }

    /* Translate the single parameters to the grid's reference system */
    (
      /* r */
      Quantities.getQuantity(
        RefSystem
          .transferImpedance(rTrafo, transformerRefSystem, refSystem)
          .getValue
          .doubleValue(),
        PU
      ),
      /* x */
      Quantities.getQuantity(
        RefSystem
          .transferImpedance(xTrafo, transformerRefSystem, refSystem)
          .getValue
          .doubleValue(),
        PU
      ),
      /* g */
      Quantities.getQuantity(
        RefSystem
          .transferAdmittance(gTrafo, transformerRefSystem, refSystem)
          .getValue
          .doubleValue(),
        PU
      ),
      /* b */
      Quantities.getQuantity(
        RefSystem
          .transferAdmittance(bTrafo, transformerRefSystem, refSystem)
          .getValue
          .doubleValue(),
        PU
      )
    )
  }

  /** Central place to check and validate a provided [[Transformer3WInput]] to
    * see if all parameters are reasonable from a [[Transformer3wModel]]
    * perspective, which means for construction of a [[Transformer3wModel]]. All
    * sanity checks and validations should be putted below!
    *
    * @param transformer3wInput
    *   instance of [[Transformer3WInput]] that should be validated
    */
  def validateInputModel(transformer3wInput: Transformer3WInput): Unit = {

    // check for nominal voltage deviation between node{A,B,C} and transformer type v{A,B,C}
    val trafo3wType = transformer3wInput.getType

    val (vNodeAVal, vTypeAVal) =
      (
        transformer3wInput.getNodeA.getVoltLvl.getNominalVoltage.getValue.doubleValue,
        trafo3wType.getvRatedA.getValue.doubleValue
      )
    val (vNodeBVal, vTypeBVal) =
      (
        transformer3wInput.getNodeB.getVoltLvl.getNominalVoltage.getValue.doubleValue,
        trafo3wType.getvRatedB.getValue.doubleValue
      )
    val (vNodeCVal, vTypeCVal) =
      (
        transformer3wInput.getNodeC.getVoltLvl.getNominalVoltage.getValue.doubleValue,
        trafo3wType.getvRatedC.getValue.doubleValue
      )

    val nomVoltDevA = vNodeAVal - vTypeAVal
    val nomVoltDevB = vNodeBVal - vTypeBVal
    val nomVoltDevC = vNodeCVal - vTypeCVal

    val maxAllowedDeviation = 0.05

    if (nomVoltDevA > maxAllowedDeviation)
      throw new InvalidGridException(
        s"The rated voltage of node A is ${transformer3wInput.getNodeA.getVoltLvl.getNominalVoltage}, but the winding A is only rated for ${trafo3wType.getvRatedA}."
      )
    if (nomVoltDevB > maxAllowedDeviation)
      throw new InvalidGridException(
        s"The rated voltage of node B is ${transformer3wInput.getNodeB.getVoltLvl.getNominalVoltage}, but the winding B is only rated for ${trafo3wType.getvRatedB}."
      )
    if (nomVoltDevC > maxAllowedDeviation)
      throw new InvalidGridException(
        s"The rated voltage of node C is ${transformer3wInput.getNodeC.getVoltLvl.getNominalVoltage}, but the winding C is only rated for ${trafo3wType.getvRatedC}."
      )

    // check for wrong positioning by comparing node{A,B,C} voltage and transformer type v{A,B,C}
    val transformerWrongPositionExceptionString: (
        Quantity[ElectricPotential],
        Quantity[ElectricPotential]
    ) => String = { (vRatedNodeX, typeVNodeX) =>
      s"The rated voltage of node A is $vRatedNodeX, " +
        s"but the winding A is only rated for $typeVNodeX. Is the transformer connected correctly?"
    }
    if (vNodeAVal < vTypeAVal)
      throw new InvalidGridException(
        transformerWrongPositionExceptionString(
          transformer3wInput.getNodeA.getVoltLvl.getNominalVoltage,
          trafo3wType.getvRatedA
        )
      )
    if (vNodeBVal < vTypeBVal)
      throw new InvalidGridException(
        transformerWrongPositionExceptionString(
          transformer3wInput.getNodeB.getVoltLvl.getNominalVoltage,
          trafo3wType.getvRatedB
        )
      )
    if (vNodeCVal < vTypeCVal)
      throw new InvalidGridException(
        transformerWrongPositionExceptionString(
          transformer3wInput.getNodeC.getVoltLvl.getNominalVoltage,
          trafo3wType.getvRatedC
        )
      )

    // check if nominal power of winding A is able to supply winding B and C
    val nomSwindingA = trafo3wType.getsRatedA.getValue.doubleValue
    val nomSwindingB = trafo3wType.getsRatedB.getValue.doubleValue
    val nomSwindingC = trafo3wType.getsRatedC.getValue.doubleValue

    if (nomSwindingA < (nomSwindingB + nomSwindingC))
      throw new InvalidParameterException(
        s"The winding A of transformer type has a lower rating as both windings B and C together! " +
          s"($nomSwindingA < $nomSwindingB + $nomSwindingC)"
      )
  }

  /** Calculates the current, tap dependent voltage ratio between the high
    * voltage side and the side that is defined by the power flow case of the
    * considered model. [[PowerFlowCaseA]] --> tap dependent voltage ratio
    * between node A and internal node [[PowerFlowCaseB]] --> tap INdependent
    * nominal voltage ratio between node A and node B [[PowerFlowCaseC]] --> tap
    * INdependent nominal voltage ratio between node A and node C The scale is
    * at 5 decimal places.
    *
    * @param transformerModel
    *   Transformer model to consider
    * @return
    *   Voltage ratio between high and low voltage side
    */
  def voltRatio(transformerModel: Transformer3wModel): BigDecimal = {
    transformerModel.powerFlowCase match {
      case Transformer3wPowerFlowCase.PowerFlowCaseA =>
        BigDecimal
          .apply(transformerModel.tapRatio)
          .setScale(5, RoundingMode.HALF_UP)
      case Transformer3wPowerFlowCase.PowerFlowCaseB |
          Transformer3wPowerFlowCase.PowerFlowCaseC =>
        transformerModel.voltRatioNominal
    }
  }

  /** Calculate the phase-to-ground admittance of a given transformer model. As
    * only the partial model for [[PowerFlowCaseA]] does contain a
    * phase-to-ground admittance at the internal port, in all other cases the
    * complex zero is returned
    *
    * @param transformer3wModel
    *   instance of [[Transformer3wModel]] the phase-to-ground admittance should
    *   be calculated for
    * @param port
    *   Port of the transformer, the result is requested for
    * @return
    *   phase-to-ground admittance Y_0 of the transformer model in p.u.
    */
  def y0(
      transformer3wModel: Transformer3wModel,
      port: Transformer3wPort.Value
  ): Complex = {
    val amount = transformer3wModel.amount
    transformer3wModel.powerFlowCase match {
      case PowerFlowCaseA if port.equals(Transformer3wPort.INTERNAL) =>
        val gij = transformer3wModel.gij().getValue.doubleValue()
        val bij = transformer3wModel.bij().getValue.doubleValue()
        val gii = transformer3wModel.g0().getValue.doubleValue()
        val bii = transformer3wModel.b0().getValue.doubleValue()
        amount * ((1 - transformer3wModel.tapRatio) * Complex(
          gij,
          bij
        ) + Complex(
          gii,
          bii
        ))
      case _ => Complex.zero
    }
  }

  /** Calculate the branch admittance of a given transformer model. The amount
    * of parallel transformers is accounted for in any case. The tap position
    * only in [[PowerFlowCaseA]], as the tap changer is always installed at the
    * higher voltage side.
    *
    * @param transformer3wModel
    *   instance of [[Transformer3wModel]] the branch admittance should be
    *   calculated for
    * @return
    *   branch admittance Y_ij between internal node and a power flow dependant
    *   node{A,B,C} of the transformer model in p.u.
    */
  def yij(transformer3wModel: Transformer3wModel): Complex = {
    val amount = transformer3wModel.amount
    val gij = transformer3wModel.gij().getValue.doubleValue()
    val bij = transformer3wModel.bij().getValue.doubleValue()
    transformer3wModel.powerFlowCase match {
      case PowerFlowCaseA =>
        amount * pow(transformer3wModel.tapRatio, 2) * Complex(gij, bij)
      case _ => amount * Complex(gij, bij)
    }
  }

}
