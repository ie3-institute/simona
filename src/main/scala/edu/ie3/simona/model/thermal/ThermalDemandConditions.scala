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
    waterStorageDemand: Boolean,
    heatStorageDemand: Boolean,
    housePossible: Boolean,
    waterStoragePossible: Boolean,
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
  def from(state: HpState): ThermalDemandConditions = {
    val lastOperatingPoint = state.lastHpOperatingPoint.thermalOps
    val houseDemand = state.thermalDemands.houseDemand
    val heatStorageDemand = state.thermalDemands.heatStorageDemand

    val isHouseHeatedLastState =
      lastOperatingPoint.qDotHouse > zeroKW && lastOperatingPoint.qDotHp > zeroKW

    ThermalDemandConditions(
      /* Consider the action in the last state. We can continue using the qDots for the house
       * from last operating point to keep continuity if:
       * - the house was heated in lastState, and
       * - has still some demand and the domestic, and
       * - hot water storage has no demand. */
      shouldContinueHouseHeating =
        lastOperatingPoint.qDotHouse > zeroKW && houseDemand.hasPossibleDemand &&
          !state.thermalDemands.domesticHotWaterStorageDemand.hasRequiredDemand,
      houseDemand = houseDemand.hasRequiredDemand,
      waterStorageDemand =
        state.thermalDemands.domesticHotWaterStorageDemand.hasRequiredDemand,
      heatStorageDemand =
        heatStorageDemand.hasRequiredDemand || heatStorageDemand.hasPossibleDemand,
      housePossible = houseDemand.hasPossibleDemand,
      waterStoragePossible =
        state.thermalDemands.domesticHotWaterStorageDemand.hasPossibleDemand,
      heatStoragePossible = heatStorageDemand.hasPossibleDemand,
      houseHeatedLastState = isHouseHeatedLastState,
    )
  }
}
