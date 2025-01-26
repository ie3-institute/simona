/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.math.Complex
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.datamodel.models.input.connector.LineInput
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.OperationInterval
import squants.Each
import squants.electro.{Amperes, Ohms, Siemens}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units
import tech.units.indriya.unit.Units._

import java.time.ZonedDateTime
import java.util.UUID

/** This model represents an electric wire or overhead line
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human-readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param nodeAUuid
  *   uuid of node a
  * @param nodeBUuid
  *   uuid of node b
  * @param amount
  *   number of parallel lines
  * @param iNom
  *   nominal current of the line (normally assumed as iMax)
  * @param r
  *   resistance r, real part of the line impedance z (referenced to the nominal
  *   impedance of the grid) in p.u.
  * @param x
  *   reactance x, imaginary part of the line impedance z(referenced to the
  *   nominal impedance of the grid) in p.u.
  * @param g
  *   conductance g, real part of the line admittance y (referenced to the
  *   nominal impedance of the grid) in p.u.
  * @param b
  *   susceptance b, imaginary part of the line admittance y (referenced to the
  *   nominal impedance of the grid)in p.u.
  */
final case class LineModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    nodeAUuid: UUID,
    nodeBUuid: UUID,
    amount: Int,
    iNom: squants.electro.ElectricCurrent,
    protected val r: squants.Dimensionless,
    protected val x: squants.Dimensionless,
    protected val g: squants.Dimensionless,
    protected val b: squants.Dimensionless,
) extends SystemComponent(
      uuid,
      id,
      operationInterval,
    )
    with PiEquivalentCircuit {

  /** see [[PiEquivalentCircuit.gij()]]
    *
    * @return
    *   branch conductance g_ij between node A and B of the element in p.u.
    */
  override def gij(): squants.Dimensionless =
    super.gij() * amount

  /** see [[PiEquivalentCircuit.g0()]]
    *
    * @return
    *   phase-to-ground conductance g_0 in p.u.
    */
  override def g0(): squants.Dimensionless =
    super.g0() * amount / 2

  /** see [[PiEquivalentCircuit.bij()]]
    *
    * @return
    *   phase-to-ground conductance g_0 in p.u.
    */
  override def bij(): squants.Dimensionless =
    super.bij() * amount

  /** see [[PiEquivalentCircuit.b0()]]
    *
    * @return
    *   phase-to-ground susceptance b_0 in p.u.
    */
  override def b0(): squants.Dimensionless =
    super.b0() * amount / 2

}

case object LineModel extends LazyLogging {

  def apply(
      lineInput: LineInput,
      refSystem: RefSystem,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): LineModel = {
    // validate the input model first
    validateInputModel(lineInput)

    // build the model
    buildLineModel(
      lineInput,
      refSystem,
      simulationStartDate,
      simulationEndDate,
    )
  }

  /** Internal method to construct a new [[LineModel]] based on a provided
    * [[LineInput]]
    *
    * @param lineInput
    *   instance of [[LineInput]] this line model should be built from
    * @param refSystem
    *   the [[RefSystem]] of the subnet the transformer is located in
    * @param startDate
    *   Start date of the simulation
    * @param endDate
    *   End date of simulation
    * @return
    *   a ready-to-use [[LineModel]] with referenced electric parameters
    */
  private def buildLineModel(
      lineInput: LineInput,
      refSystem: RefSystem,
      startDate: ZonedDateTime,
      endDate: ZonedDateTime,
  ): LineModel = {

    val lineType = lineInput.getType
    val (r, x, g, b) = (
      refSystem.rInPu(
        Ohms(
          lineType.getR
            .to(OHM_PER_KILOMETRE)
            .multiply(lineInput.getLength.to(KILOMETRE))
            .getValue
            .doubleValue()
        )
      ),
      refSystem.xInPu(
        Ohms(
          lineType.getX
            .to(OHM_PER_KILOMETRE)
            .multiply(lineInput.getLength.to(KILOMETRE))
            .getValue
            .doubleValue()
        )
      ),
      refSystem.gInPu(
        Siemens(
          lineType.getG
            .to(SIEMENS_PER_KILOMETRE)
            .multiply(lineInput.getLength.to(KILOMETRE))
            .getValue
            .doubleValue()
        )
      ),
      refSystem.bInPu(
        Siemens(
          lineType.getB
            .to(SIEMENS_PER_KILOMETRE)
            .multiply(lineInput.getLength.to(KILOMETRE))
            .getValue
            .doubleValue()
        )
      ),
    )

    val operationInterval =
      SystemComponent.determineOperationInterval(
        startDate,
        endDate,
        lineInput.getOperationTime,
      )

    val lineModel = new LineModel(
      lineInput.getUuid,
      lineInput.getId,
      operationInterval,
      lineInput.getNodeA.getUuid,
      lineInput.getNodeB.getUuid,
      lineInput.getParallelDevices,
      Amperes(
        lineType.getiMax().to(AMPERE).getValue.doubleValue()
      ),
      r,
      x,
      g,
      b,
    )

    // if the line input model is in operation, enable the model
    if (operationInterval.includes(SimonaConstants.FIRST_TICK_IN_SIMULATION))
      lineModel.enable()

    // sanity check after creation for piEquivalent parameters r,x,g,b
    lineModel.piEquivalentSanityCheck("line model")

    lineModel
  }

  /** Validates a provided [[LineInput]] in the way that checks are performed
    * from a [[LineModel]] perspective. This means that only parameters relevant
    * for [[LineModel]] s are checked to ensure that the provided [[LineInput]]
    * can be used for the construction of a [[LineModel]]. All sanity checks and
    * validations should be putted below!
    *
    * @param lineInput
    *   instance of [[LineInput]] that should be validated
    */
  def validateInputModel(lineInput: LineInput): Unit = {

    if (lineInput.getNodeA == null || lineInput.getNodeB == null)
      throw new InvalidGridException(
        s"Nodes of line ${lineInput.getUuid} are missing." +
          s"NodeA: ${lineInput.getNodeA}, NodeB: ${lineInput.getNodeB}."
      )

    val ratedVoltageTolerance = Quantities.getQuantity(100d, Units.VOLT)
    val lineType = lineInput.getType
    val vRatedNodeA = lineInput.getNodeA.getVoltLvl.getNominalVoltage
    val vRatedNodeB = lineInput.getNodeB.getVoltLvl.getNominalVoltage

    if (
      lineType
        .getvRated()
        .isLessThan(vRatedNodeA.subtract(ratedVoltageTolerance))
    )
      throw new InvalidGridException(
        s"Line ${lineInput.getUuid} (${lineInput.getId}) has a rated voltage of ${lineType
            .getvRated()} but is connected to node A (${lineInput.getNodeA.getUuid} / ${lineInput.getNodeA.getId}), which has a rated voltage of $vRatedNodeA."
      )
    else if (
      lineType
        .getvRated()
        .isLessThan(vRatedNodeB.subtract(ratedVoltageTolerance))
    )
      throw new InvalidGridException(
        s"Line ${lineInput.getUuid} (${lineInput.getId}) has a rated voltage of ${lineType
            .getvRated()} but is connected to node B (${lineInput.getNodeB.getUuid} / ${lineInput.getNodeB.getId}), which has a rated voltage of $vRatedNodeB."
      )
    else if (
      lineType
        .getvRated()
        .isGreaterThan(vRatedNodeA.add(ratedVoltageTolerance))
    )
      logger.warn(
        s"Line ${lineInput.getUuid} (${lineInput.getId}) has a rated voltage of ${lineType
            .getvRated()} but is connected to node A (${lineInput.getNodeA.getUuid} / ${lineInput.getNodeA.getId}), which has a lower rated voltage of $vRatedNodeA."
      )
    else if (
      lineType
        .getvRated()
        .isGreaterThan(vRatedNodeB.add(ratedVoltageTolerance))
    )
      logger.warn(
        s"Line ${lineInput.getUuid} (${lineInput.getId}) has a rated voltage of ${lineType
            .getvRated()} but is connected to node B (${lineInput.getNodeB.getUuid} / ${lineInput.getNodeB.getId}), which has a lower rated voltage of $vRatedNodeB."
      )

    // length
    if (lineInput.getLength.getValue.doubleValue() <= 0)
      throw new InvalidGridException(
        s"Line ${lineInput.getUuid} has an invalid length of ${lineInput.getLength}"
      )

    // electric params
    if (lineType.getR.getValue.doubleValue() <= 0)
      throw new InvalidGridException(
        s"Resistance r of lineType ${lineType.getUuid} used in line ${lineInput.getUuid} is 0 or smaller, " +
          s"which is not allowed! Please correct parameters!"
      )

    if (lineType.getX.getValue.doubleValue() <= 0)
      throw new InvalidGridException(
        s"Reactance x of lineType ${lineType.getUuid} used in line ${lineInput.getUuid} is 0 or smaller," +
          s" which is not allowed! Please correct parameters!"
      )

    if (lineType.getB.getValue.doubleValue() < 0)
      throw new InvalidGridException(
        s"Susceptance b of lineType ${lineType.getUuid} used in line ${lineInput.getUuid} is smaller " +
          s"than 0, which is not allowed! Please correct parameters!"
      )

    if (lineType.getG.getValue.doubleValue() < 0)
      throw new InvalidGridException(
        s"Conductance g of lineType ${lineType.getUuid} used in line ${lineInput.getUuid} is smaller " +
          s"than 0, which is not allowed! Please correct parameters!"
      )
  }

  /** Calculate the phase-to-ground admittance of a given line model
    *
    * @param lineModel
    *   instance of [[LineModel]] the phase-to-ground admittance should be
    *   calculated from
    * @return
    *   phase-to-ground admittance Y_0 of the line model in p.u.
    */
  def y0(lineModel: LineModel): Complex = {
    new Complex(
      lineModel.g0().value.doubleValue(),
      lineModel.b0().value.doubleValue(),
    )
  }

  /** Calculate the branch admittance of a given line model
    *
    * @param lineModel
    *   instance of [[LineModel]] the branch admittance should be calculated
    *   from
    * @return
    *   branch admittance Y_ij between node a and b of the line model in p.u.
    */
  def yij(lineModel: LineModel): Complex = new Complex(
    lineModel.gij().value.doubleValue(),
    lineModel.bij().value.doubleValue(),
  )

  /** Calculates the utilisation of a given line model
    *
    * @param lineModel
    *   instance of [[LineModel]] the utilisation should be calculated from
    * @param iNodeA
    *   current @ node a
    * @param iNodeB
    *   current @ node b
    * @return
    *   the current utilisation of the line in percentage
    */
  def utilisation(
      lineModel: LineModel,
      iNodeA: squants.electro.ElectricCurrent,
      iNodeB: squants.electro.ElectricCurrent,
  ): squants.Dimensionless = {
    Each(
      Math.max(
        iNodeA.toAmperes,
        iNodeB.toAmperes,
      ) / lineModel.iNom.toAmperes * 100 / lineModel.amount
    )
  }

}
