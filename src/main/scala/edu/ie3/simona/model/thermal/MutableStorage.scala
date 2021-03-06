/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import javax.measure.quantity.Energy
import tech.units.indriya.ComparableQuantity

/** This trait enables implementations of a [[ThermalStorage]], which need a
  * mutable storage. The trait can only be used by subclasses of
  * [[ThermalStorage]] (look [[self]]). <p> <strong>Important:</strong> The
  * field storedEnergy is a variable and set to 0kWh by default.
  */
trait MutableStorage {
  self: ThermalStorage =>

  /** Current storage level
    */
  protected var _storedEnergy: ComparableQuantity[Energy]

  def isDemandCoveredByStorage(demand: ComparableQuantity[Energy]): Boolean =
    usableThermalEnergy.isGreaterThanOrEqualTo(demand)

  /** Overridden in such manner, that this method returns the usable thermal
    * energy, meaning that amount of energy which is allowed to be taken from
    * storage.
    *
    * @return
    *   usable energy
    */
  def usableThermalEnergy: ComparableQuantity[Energy]

  /** Add energy to storage and check if stored energy exceeds maximum. Return
    * the potential surplus energy.
    *
    * @param addedEnergy
    *   energy added to storage
    * @return
    *   surplus
    */
  def tryToStoreAndReturnRemainder(
      addedEnergy: ComparableQuantity[Energy]
  ): Option[ComparableQuantity[Energy]]

  /** Take energy from storage and check if stored energy falls to minimum.
    * Return the potential lack of energy.
    *
    * @param takenEnergy
    *   energy taken from storage
    * @return
    *   lack
    */
  def tryToTakeAndReturnLack(
      takenEnergy: ComparableQuantity[Energy]
  ): Option[ComparableQuantity[Energy]]
}
