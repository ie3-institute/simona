/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

/** This trait enables implementations of a [[ThermalStorage]], which need a
  * mutable storage. The trait can only be used by subclasses of
  * [[ThermalStorage]] (look [[self]]). <p> <strong>Important:</strong> The
  * field storedEnergy is a variable and set to 0kWh by default.
  */
@deprecated("Use thermal storage state instead")
trait MutableStorage {
  self: ThermalStorage =>

  /** Current storage level
    */
  protected var _storedEnergy: squants.Energy

  def isDemandCoveredByStorage(demand: squants.Energy): Boolean =
    usableThermalEnergy >= demand

  /** Overridden in such manner, that this method returns the usable thermal
    * energy, meaning that amount of energy which is allowed to be taken from
    * storage.
    *
    * @return
    *   usable energy
    */
  def usableThermalEnergy: squants.Energy

  /** Add energy to storage and check if stored energy exceeds maximum. Return
    * the potential surplus energy.
    *
    * @param addedEnergy
    *   energy added to storage
    * @return
    *   surplus
    */
  def tryToStoreAndReturnRemainder(
      addedEnergy: squants.Energy
  ): Option[squants.Energy]

  /** Take energy from storage and check if stored energy falls to minimum.
    * Return the potential lack of energy.
    *
    * @param takenEnergy
    *   energy taken from storage
    * @return
    *   lack
    */
  def tryToTakeAndReturnLack(
      takenEnergy: squants.Energy
  ): Option[squants.Energy]
}
