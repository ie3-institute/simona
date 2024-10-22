/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.model.participant2.evcs.EvcsModel.ChargingStrategy
import squants.Power

import java.util.UUID

object MaximumPowerStrategy extends ChargingStrategy {

  /** Determine scheduling for charging the EVs currently parked at the charging
    * station until their departure. In this case, each EV is charged with
    * maximum power from current time until it reaches either 100% SoC or its
    * departure time.
    *
    * @param currentTick
    *   current tick
    * @param evs
    *   currently parked evs at the charging station
    * @return
    *   scheduling for charging the EVs
    */
  def determineChargingPowers(
      evs: Iterable[EvModelWrapper],
      currentTick: Long,
      chargingProps: EvcsChargingProperties,
  ): Map[UUID, Power] = evs
    .filter(ev => ev.storedEnergy < ev.eStorage)
    .map { ev =>
      ev.uuid -> chargingProps.getMaxAvailableChargingPower(ev)
    }
    .toMap
}
