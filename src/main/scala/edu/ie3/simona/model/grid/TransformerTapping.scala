/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.datamodel.models.input.connector.ConnectorPort
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.QuantityUtils.asPu
import tech.units.indriya.ComparableQuantity

import javax.measure.Quantity
import javax.measure.quantity.Dimensionless
import tech.units.indriya.quantity.Quantities

/** should be mixed into every transformer model that is capable of transformer
  * tapping. Currently mixed into [[TransformerModel]] and
  * [[Transformer3wModel]]. Depending on the implementation it might be
  * necessary to override updateTapPos (e.g. in [[Transformer3wModel]]). The
  * provided [[TransformerTappingModel]] *should* be protected and not be
  * accessible from outside to prevent direct access to internal functions!
  * Instead, all the functions provided here should be used for tap position
  * manipulation.
  */
trait TransformerTapping {

  protected val transformerTappingModel: TransformerTappingModel

  protected var tapRatio: Double = _

  def getTapRation: Double = tapRatio

  /** Returns [[TransformerTappingModel.autoTap]].
    */
  def hasAutoTap: Boolean = transformerTappingModel.autoTap

  /** Returns the maximal tap position.
    */
  def tapMax: Int = transformerTappingModel.tapMax

  /** Returns the minimal tap position.
    */
  def tapMin: Int = transformerTappingModel.tapMin

  /** Returns the voltage change per tap position in pu.
    */
  def deltaV: ComparableQuantity[Dimensionless] =
    transformerTappingModel.deltaV.getValue.doubleValue().asPu

  /** Returns the current tap position.
    */
  def currentTapPos: Int = transformerTappingModel.currentTapPos

  /** Initialize the tapping model. Should be called after creating the
    * implementing model
    */
  def initTapping(): Unit =
    tapRatio = transformerTappingModel.updateTapPos(currentTapPos)

  /** Update the transformer tap position
    *
    * @param newTapPos
    *   the wanted tap position
    */
  def updateTapPos(newTapPos: Int): Unit =
    tapRatio = transformerTappingModel.updateTapPos(newTapPos)

  /** Increase transformer tap position by the provided delta value
    *
    * @param deltaTap
    *   number of tap positions to increase
    */
  def incrTapPos(deltaTap: Int = 1): Unit =
    tapRatio = transformerTappingModel.incrTapPos(deltaTap)

  /** Decrease transformer tap position by the provided delta value
    *
    * @param deltaTap
    *   number of tap positions to decrease
    */
  def decrTapPos(deltaTap: Int = 1): Unit =
    tapRatio = transformerTappingModel.decrTapPos(deltaTap)

  /** Determine the amount of tap positions to increase oder decrease in order
    * to meet the desired change in voltage magnitude at the given transformer
    * side. For details on the implementation see
    * [[TransformerTappingModel.computeDeltaTap()]]. This method considers the
    * side at which the change is requested.
    *
    * @param vChangeRequest
    *   desired change in voltage magnitude (> 0 --> increase voltage, < 0 -->
    *   decrease voltage)
    * @param tapSide
    *   the side of the transformer at which the given voltage change is desired
    * @param deadBand
    *   as a portion of the transformer voltage ratio per tap, it defaults to 75
    *   % of the deltaV of a tap
    * @return
    *   the needed in- or decrease of the transformer tap position to reach the
    *   desired change in voltage magnitude or zero if not possible
    */
  def computeDeltaTap(
      vChangeRequest: Quantity[Dimensionless],
      tapSide: ConnectorPort = ConnectorPort.A,
      deadBand: Quantity[Dimensionless] = Quantities.getQuantity(0.75, PU),
  ): Int = {
    if (isSameSide(tapSide)) {
      transformerTappingModel.computeDeltaTap(vChangeRequest, deadBand)
    } else {
      transformerTappingModel.computeDeltaTap(
        vChangeRequest.multiply(-1),
        deadBand,
      )
    }
  }

  /** Determines all possible voltage deltas that can be achieved by tapping.
    * This method considers the side at which the change is requested.
    *
    * @param maxIncrease
    *   maximum allowed voltage increase
    * @param maxDecrease
    *   maximal allowed voltage decrease
    * @param tapSide
    *   side of the tapping
    * @return
    *   a list of possible voltage deltas
    */
  def getPossibleVoltageChanges(
      maxIncrease: ComparableQuantity[Dimensionless],
      maxDecrease: ComparableQuantity[Dimensionless],
      tapSide: ConnectorPort = ConnectorPort.A,
  ): List[ComparableQuantity[Dimensionless]] = {
    val plus = tapMax - currentTapPos
    val minus = tapMin - currentTapPos

    val range =
      Range.inclusive(minus, plus).map(deltaV.multiply(_).divide(100)).toList

    val values = if (isSameSide(tapSide)) {
      range
    } else {
      range.map(_.multiply(-1)).sortBy(_.getValue.doubleValue())
    }

    (
      maxIncrease.isLessThan(maxDecrease),
      maxIncrease.isLessThan(0.asPu),
    ) match {
      case (true, true) =>
        // maximal increase is less then maximal allowed decrease -> only max decrease as possible change
        values.filter(_.isEquivalentTo(maxDecrease))
      case (true, _) =>
        // maximal decrease is greater then maximal allowed increase -> only max increase as possible change
        values.filter(_.isEquivalentTo(maxIncrease))
      case _ =>
        // find all values between the maximal allowed increase and decrease
        values.filter(value =>
          value.isLessThanOrEqualTo(maxIncrease) && value
            .isGreaterThanOrEqualTo(
              maxDecrease
            )
        )
    }
  }

  /** Determine the amount of tap positions to increase oder decrease in order
    * to meet the desired change in voltage magnitude at the given transformer
    * side. For details on the implementation see
    * [[TransformerTappingModel.computeDeltaTap()]] and the resulting voltage
    * delta. This method considers the side at which the change is requested.
    *
    * @param vChangeRequest
    *   desired change in voltage magnitude (> 0 --> increase voltage, < 0 -->
    *   decrease voltage)
    * @param tapSide
    *   the side of the transformer at which the given voltage change is desired
    * @param deadBand
    *   as a portion of the transformer voltage ratio per tap, it defaults to 75
    *   % of the deltaV of a tap
    * @return
    *   the needed in- or decrease of the transformer tap position to reach the
    *   desired change in voltage magnitude or zero if not possible and the
    *   resulting voltage delta
    */
  def computeDeltas(
      vChangeRequest: Quantity[Dimensionless],
      tapSide: ConnectorPort = ConnectorPort.A,
      deadBand: Quantity[Dimensionless] = Quantities.getQuantity(0.75, PU),
  ): (Int, ComparableQuantity[Dimensionless]) = {
    val taps = computeDeltaTap(vChangeRequest, tapSide, deadBand)
    val deltaV =
      transformerTappingModel.deltaV.to(PU).getValue.doubleValue() * taps

    if (isSameSide(tapSide)) {
      (taps, deltaV.asPu)
    } else {
      (taps, deltaV.asPu.multiply(-1))
    }
  }

  /** Method to check if a given port matches the port of this model.
    * @param tapSide
    *   to check.
    * @return
    *   true if both ports are either on the higher or lower side
    */
  private def isSameSide(tapSide: ConnectorPort): Boolean =
    (transformerTappingModel.tapSide, tapSide) match {
      case (ConnectorPort.A, ConnectorPort.A) => true // both on higher side
      case (ConnectorPort.A, _) => false // both on different sides
      case (ConnectorPort.B, ConnectorPort.A) |
          (ConnectorPort.C, ConnectorPort.A) =>
        false // both on different sides
      case (ConnectorPort.B, _) | (ConnectorPort.C, _) =>
        true // both on lower side
    }
}
