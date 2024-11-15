/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.util.quantities.PowerSystemUnits._
import javax.measure.Quantity
import javax.measure.quantity.Dimensionless
import tech.units.indriya.quantity.Quantities

/** should be mixed into every transformer model that is capable of transformer
  * tapping. Currently mixed into [[TransformerModel]] and
  * [[Transformer3wModel]]. Depending on the implementation it might be
  * necessary to override updateTapPos (e.g. in [[Transformer3wModel]]). The
  * provided [[TransformerTappingModel]] *should* be protected and not be
  * accessible from outside to prevent direct access to internal functions!
  * Instead all the functions provided here should be used for tap position
  * manipulation.
  */
trait TransformerTapping {

  protected val transformerTappingModel: TransformerTappingModel

  protected var tapRatio: Double = _

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
    * to meet the desired change in voltage magnitude. For details on the
    * implementation see [[TransformerTappingModel.computeDeltaTap()]]
    *
    * @param vChangeRequest
    *   desired change in voltage magnitude (> 0 --> increase voltage, < 0 -->
    *   decrease voltage)
    * @param deadBand
    *   as a portion of the transformer voltage ratio per tap, it defaults to 75
    *   % of the deltaV of a tap
    * @return
    *   the needed in- or decrease of the transformer tap position to reach the
    *   desired change in voltage magnitude or zero if not possible
    */
  def computeDeltaTap(
      vChangeRequest: Quantity[Dimensionless],
      deadBand: Quantity[Dimensionless] = Quantities.getQuantity(0.75, PU),
  ): Int =
    transformerTappingModel.computeDeltaTap(vChangeRequest, deadBand)

}
