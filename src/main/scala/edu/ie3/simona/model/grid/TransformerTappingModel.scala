/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.numerics.{abs, floor, signum}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.connector.ConnectorPort
import edu.ie3.simona.exceptions.InvalidActionRequestException
import edu.ie3.util.quantities.PowerSystemUnits._

import javax.measure.Quantity
import javax.measure.quantity.Dimensionless
import tech.units.indriya.quantity.Quantities

/** Holds all functions a transformer with tapping control is capable of. If
  * used by a transformer model, it is recommended to be used via
  * [[TransformerTapping]] instead of allowing direct access to the model. See
  * [[TransformerModel]] for a sample implementation.
  *
  * @param deltaV
  *   the voltage change by one tap increase or decrease as delta value in
  *   percentage
  * @param tapMax
  *   the maximum tap position
  * @param tapMin
  *   the minimum tap position
  * @param tapNeutr
  *   the neutral tap position
  * @param autoTap
  *   enable/disable automatic tapping
  * @param tapSide
  *   the position of the tap changer, only has to be provided if position is
  *   NOT on the HV side
  */
final case class TransformerTappingModel(
    deltaV: Quantity[Dimensionless],
    private var _currentTapPos: Int,
    tapMax: Int,
    tapMin: Int,
    tapNeutr: Int,
    autoTap: Boolean,
    tapSide: ConnectorPort = ConnectorPort.A,
) extends LazyLogging {

  private val deltaVval = deltaV.to(PU).getValue.doubleValue()

  def currentTapPos: Int = _currentTapPos

  /** Increase tap position by the provided number of delta taps
    *
    * @param deltaTap
    *   the delta value to increase the current tap position
    * @return
    *   the new tap ratio of the transformer
    */
  def incrTapPos(deltaTap: Int = 1): Double = {
    val newTapPos = Math.min(_currentTapPos + Math.abs(deltaTap), tapMax)
    if (_currentTapPos == newTapPos)
      logger.warn(
        "Maximal tap position reached. Cannot increase tap position anymore!"
      )
    updateTapPos(newTapPos)
  }

  /** Decrease tap position by the provided number of delta taps
    *
    * @param deltaTap
    *   the delta value to decrease the current tap position
    * @return
    *   the new tap ratio of the transformer
    */
  def decrTapPos(deltaTap: Int = 1): Double = {
    val newTapPos = Math.max(_currentTapPos - Math.abs(deltaTap), tapMin)
    if (_currentTapPos == newTapPos)
      logger.warn(
        "Minimal tap position reached. Cannot decrease tap position anymore!"
      )
    updateTapPos(newTapPos)
  }

  /** Update the current tap position by the directly providing the wanted tap
    * position. Takes [[tapMin]] and [[tapMax]] into account.
    *
    * @param newTapPos
    *   newly wanted tap position
    * @return
    *   the new tap ratio of the transformer
    */
  def updateTapPos(newTapPos: Int): Double = {
    if (newTapPos > tapMax | newTapPos < tapMin)
      throw new InvalidActionRequestException(
        s"Provided tap pos $newTapPos is not between allowed tapping range of tapMin: $tapMin and tapMax: $tapMax!"
      )
    _currentTapPos = newTapPos
    1 + (_currentTapPos - tapNeutr) * deltaVval
  }

  /** Determine the amount of tap positions to increase oder decrease in order
    * to meet the desired change in voltage magnitude. In order to avoid
    * rattling in the iterative process, another tap position is only added, if
    * at least the dead band of the voltage increase per tap is needed.
    *
    * [[Transformer3wModel]] always have their tap changer on the high voltage
    * side, therefore a desired voltage increase leads always to an increase in
    * tap position.
    *
    * [[TransformerModel]] may have their tap changer on the low or high voltage
    * side. This is considered by the tap side multiplier accordingly. When the
    * tap changer is on low voltage side (ElementPort.B) the tap direction is
    * inverted.
    *
    * Furthermore, this method also considers the transformer not being able to
    * change its tap position anymore. Hence, 0 is returned, if no change is
    * possible anymore.
    *
    * @param vChangeRequest
    *   desired change in voltage magnitude (> 0 --> increase voltage, < 0 -->
    *   decrease voltage)
    * @param deadBandPerTap
    *   as a portion of the transformer voltage ratio per tap, prevents
    *   fluctuating. It defaults to 75 % of the deltaV of the step
    * @return
    *   the needed in- or decrease of the transformer tap position to reach the
    *   desired change in voltage magnitude or the minimum permissible change if
    *   not possible
    */
  def computeDeltaTap(
      vChangeRequest: Quantity[Dimensionless],
      deadBandPerTap: Quantity[Dimensionless] = Quantities.getQuantity(0.75, PU),
  ): Int = {
    /* Determine the tap change, that has to be done in any case, as well as the remainder to fully
     * fulfill the voltage change request */
    val vChangeVal = vChangeRequest.getValue.doubleValue()
    val deltaAnyways =
      (signum(vChangeVal) * floor(abs(vChangeVal / deltaVval))).intValue
    val remainder = vChangeVal % deltaVval

    /* If the remainder is at least as big as the dead band per tap, add another step into the
     * correct direction */
    val deadBandVal = deadBandPerTap
      .to(PU)
      .getValue
      .doubleValue * deltaVval
    val deltaTap = remainder match {
      case _ if abs(remainder) > deadBandVal =>
        deltaAnyways + signum(remainder).intValue
      case _ => deltaAnyways
    }

    /* If the required change would violate the limits --> limit the change */
    deltaTap match {
      case _ if (_currentTapPos + deltaTap) > tapMax => tapMax - _currentTapPos
      case _ if (_currentTapPos + deltaTap) < tapMin => tapMin - _currentTapPos
      case _                                         => deltaTap
    }
  }
}

case object TransformerTappingModel {

  def apply(
      deltaV: Quantity[Dimensionless],
      currentTapPos: Int,
      tapMax: Int,
      tapMin: Int,
      tapNeutr: Int,
      autoTap: Boolean,
      elementPort: ConnectorPort = ConnectorPort.A,
  ): TransformerTappingModel = {
    val tapModel =
      new TransformerTappingModel(
        deltaV,
        currentTapPos,
        tapMax,
        tapMin,
        tapNeutr,
        autoTap,
        elementPort,
      )

    // update internal state variables
    tapModel._currentTapPos = currentTapPos

    tapModel
  }

}
