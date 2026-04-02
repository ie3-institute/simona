/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.datamodel.models.input.connector.ConnectorPort
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroPU
import squants.{Dimensionless, Each}

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

  protected var tapRatio: Double = scala.compiletime.uninitialized

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
  def deltaV: Dimensionless =
    transformerTappingModel.deltaV

  /** Returns the current tap position.
    */
  def currentTapPos: Int = transformerTappingModel.currentTapPos

  /** Initialize the tapping model. Should be called after creating the
    * implementing model.
    */
  def initTapping(): Unit =
    tapRatio = transformerTappingModel.updateTapPos(currentTapPos)

  /** Update the transformer tap position.
    *
    * @param newTapPos
    *   The wanted tap position.
    */
  def updateTapPos(newTapPos: Int): Unit =
    tapRatio = transformerTappingModel.updateTapPos(newTapPos)

  /** Increase transformer tap position by the provided delta value.
    *
    * @param deltaTap
    *   Number of tap positions to increase.
    */
  def incrTapPos(deltaTap: Int = 1): Unit =
    tapRatio = transformerTappingModel.incrTapPos(deltaTap)

  /** Decrease transformer tap position by the provided delta value.
    *
    * @param deltaTap
    *   Number of tap positions to decrease.
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
    *   Desired change in voltage magnitude (> 0 --> increase voltage, < 0 -->
    *   decrease voltage).
    * @param tapSide
    *   The side of the transformer at which the given voltage change is
    *   desired.
    * @param deadBand
    *   As a portion of the transformer voltage ratio per tap, it defaults to 75
    *   % of the deltaV of a tap.
    * @return
    *   The needed in- or decrease of the transformer tap position to reach the
    *   desired change in voltage magnitude or zero if not possible.
    */
  def computeDeltaTap(
      vChangeRequest: Dimensionless,
      tapSide: ConnectorPort = ConnectorPort.A,
      deadBand: Dimensionless = Each(0.75),
  ): Int = {
    if isSameSide(tapSide) then {
      transformerTappingModel.computeDeltaTap(vChangeRequest, deadBand)
    } else {
      transformerTappingModel.computeDeltaTap(
        vChangeRequest * -1,
        deadBand,
      )
    }
  }

  /** Determines all possible voltage deltas that can be achieved by tapping.
    * This method considers the side at which the change is requested.
    *
    * @param maxIncrease
    *   Maximum allowed voltage increase.
    * @param maxDecrease
    *   Maximal allowed voltage decrease.
    * @param tapSide
    *   Side of the tapping.
    * @return
    *   A list of possible voltage deltas.
    */
  def getPossibleVoltageChanges(
      maxIncrease: Dimensionless,
      maxDecrease: Dimensionless,
      tapSide: ConnectorPort = ConnectorPort.A,
  ): List[Dimensionless] = {
    val plus = tapMax - currentTapPos
    val minus = tapMin - currentTapPos

    val range = Range.inclusive(minus, plus).map(deltaV * _).toList

    val values = if isSameSide(tapSide) then {
      range
    } else {
      range.map(_ * -1).sortBy(_.toEach)
    }

    // pu tolerance
    given Dimensionless = Each(1e-3)

    (
      maxIncrease < maxDecrease,
      maxIncrease < zeroPU,
    ) match {
      case (true, true) =>
        // maximal increase is less than maximal allowed decrease -> only max decrease as possible change
        values.filter(_ ~= maxDecrease)
      case (true, _) =>
        // maximal decrease is greater than maximal allowed increase -> only max increase as possible change
        values.filter(_ ~= maxIncrease)
      case _ =>
        // find all values between the maximal allowed increase and decrease
        values.filter(value => value <= maxIncrease && value >= maxDecrease)
    }
  }

  /** Determine the amount of tap positions to increase oder decrease in order
    * to meet the desired change in voltage magnitude at the given transformer
    * side. For details on the implementation see
    * [[TransformerTappingModel.computeDeltaTap()]] and the resulting voltage
    * delta. This method considers the side at which the change is requested.
    *
    * @param vChangeRequest
    *   Desired change in voltage magnitude (> 0 --> increase voltage, < 0 -->
    *   decrease voltage).
    * @param tapSide
    *   The side of the transformer at which the given voltage change is
    *   desired.
    * @param deadBand
    *   As a portion of the transformer voltage ratio per tap, it defaults to 75
    *   % of the deltaV of a tap.
    * @return
    *   The needed in- or decrease of the transformer tap position to reach the
    *   desired change in voltage magnitude or zero if not possible and the
    *   resulting voltage delta.
    */
  def computeDeltas(
      vChangeRequest: Dimensionless,
      tapSide: ConnectorPort = ConnectorPort.A,
      deadBand: Dimensionless = Each(0.75),
  ): (Int, Dimensionless) = {
    val taps = computeDeltaTap(vChangeRequest, tapSide, deadBand)
    val deltaV = transformerTappingModel.deltaV * taps

    if isSameSide(tapSide) then {
      (taps, deltaV)
    } else {
      (taps, deltaV * -1)
    }
  }

  /** Method to check if a given port matches the port of this model.
    * @param tapSide
    *   To check.
    * @return
    *   True if both ports are either on the higher or lower side.
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
