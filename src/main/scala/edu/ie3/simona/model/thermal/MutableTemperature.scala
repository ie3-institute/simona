/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import javax.measure.quantity.Temperature
import tech.units.indriya.ComparableQuantity

/** This trait enables implementations of a [[ThermalHouse]], which need a
  * mutable inner temperature. The trait can only be used by subclasses of
  * [[ThermalHouse]] (look [[self]]). <p> <strong>Important:</strong> The field
  * innerTemperature is a variable.
  */
trait MutableTemperature {
  self: ThermalHouse =>

  /** Inner temperature level
    */
  protected var _innerTemperature: ComparableQuantity[Temperature]

  final def getInnerTemperature: ComparableQuantity[Temperature] =
    _innerTemperature

  /** Set innerTemperature to a new value.
    *
    * @param newValue
    *   new inner temperature
    * @return
    *   old inner temperature
    */
  def setInnerTemperature(
      newValue: ComparableQuantity[Temperature]
  ): Unit =
    _innerTemperature = newValue

}
