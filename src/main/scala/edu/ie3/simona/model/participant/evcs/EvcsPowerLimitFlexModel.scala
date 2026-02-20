/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.model.participant.ParticipantFlexModel
import edu.ie3.simona.model.participant.evcs.EvcsModel.EvcsState
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  PowerLimitFlexOptions,
}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW

class EvcsPowerLimitFlexModel(private val model: EvcsModel)
    extends ParticipantFlexModel[EvcsState] {

  override def determineFlexOptions(
      state: EvcsState,
      dateTimeType: DataTimeType,
  ): FlexOptions = {

    val preferredPowers =
      model.strategy.determineChargingPowers(
        state.evs.filter(!model.isFull(_)),
        state.tick,
        model,
      )

    val (maxCharging, preferredPower, forcedCharging, minCharging) =
      state.evs.foldLeft(
        (zeroKW, zeroKW, zeroKW, zeroKW)
      ) {
        case (
              (chargingSum, preferredSum, forcedSum, dischargingSum),
              ev,
            ) =>
          val maxPower = model.getMaxAvailableChargingPower(ev)

          val preferredPower = preferredPowers.get(ev.uuid)

          val maxCharging =
            if !model.isFull(ev) then maxPower
            else zeroKW

          val forced =
            if model.requiresMaxCharging(ev, state.tick) then maxPower
            else zeroKW

          val maxDischarging =
            if !model.isEmpty(ev) && model.vehicle2grid then maxPower * -1
            else zeroKW

          (
            chargingSum + maxCharging,
            preferredSum + preferredPower.getOrElse(zeroKW),
            forcedSum + forced,
            dischargingSum + maxDischarging,
          )
      }

    // if we need to charge at least one EV, we cannot discharge any other
    val (adaptedPreferred, adaptedMinCharging) =
      if forcedCharging > zeroKW then
        (preferredPower.max(forcedCharging), forcedCharging)
      else (preferredPower, minCharging)

    PowerLimitFlexOptions(
      adaptedPreferred,
      adaptedMinCharging,
      maxCharging,
    )
  }

}
