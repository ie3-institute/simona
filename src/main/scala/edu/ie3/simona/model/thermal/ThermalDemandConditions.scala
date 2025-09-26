/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.simona.model.participant.hp.HpModel.HpState
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW

private case class ThermalDemandConditions(
    shouldContinueHouseHeating: Boolean,
    houseDemand: Boolean,
    heatStorageDemand: Boolean,
    housePossible: Boolean,
    heatStoragePossible: Boolean,
    houseHeatedLastState: Boolean,
)

private object ThermalDemandConditions {

  /** Handles the case, when a grid has feed in. Depending on which entity has
    * some heat demand the house or the storage will be heated up / filled up.
    * First the actions from last operating point will be considered and checked
    * if the behaviour should be continued. This might be the case, if we got
    * activated by updated weather data. If this is not the case, all other
    * cases will be handled.
    */
  def from(state: HpState): ThermalDemandConditions = ThermalDemandConditions(
    /* Consider the action in the last state
     * We can continue using the qDots from last operating point to keep continuity.
     *If the house was heated in lastState and has still some demand.
     */
    shouldContinueHouseHeating =
      state.lastHpOperatingPoint.thermalOps.qDotHouse > zeroKW &&
        state.thermalDemands.houseDemand.hasPossibleDemand,
    houseDemand = state.thermalDemands.houseDemand.hasRequiredDemand,
    heatStorageDemand =
      state.thermalDemands.heatStorageDemand.hasRequiredDemand ||
        state.thermalDemands.heatStorageDemand.hasPossibleDemand,
    housePossible = state.thermalDemands.houseDemand.hasPossibleDemand,
    heatStoragePossible =
      state.thermalDemands.heatStorageDemand.hasPossibleDemand,
    houseHeatedLastState =
      state.lastHpOperatingPoint.thermalOps.qDotHouse > zeroKW &&
        state.lastHpOperatingPoint.thermalOps.qDotHp > zeroKW,
  )
}
