/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.model.participant.{ChargingHelper, ParticipantModel}
import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  OperationChangeIndicator,
}
import edu.ie3.simona.model.participant.flex.ParticipantFlexModel
import edu.ie3.simona.model.participant.storage.StorageModel.StorageState
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  PowerLimitFlexOptions,
}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.Power

class StoragePowerLimitFlexModel(private val model: StorageModel)
    extends ParticipantFlexModel[ActivePowerOperatingPoint, StorageState] {

  override def determineFlexOptions(
      state: StorageState,
      dataTimeType: DataTimeType,
  ): FlexOptions = {

    val chargingPossible = !model.isFull(state.storedEnergy)
    val dischargingPossible = !model.isEmpty(state.storedEnergy)

    val refPower = model.refTargetSoc
      .map { targetParams =>
        if state.storedEnergy <= targetParams.targetWithPosMargin then {
          if state.storedEnergy >= targetParams.targetWithNegMargin then {
            // is within target +/- margin, no charging needed
            zeroKW
          } else {
            // below target - margin, charge up to target
            model.pMax
          }
        } else {
          // above target + margin, discharge to target
          model.pMax * -1d
        }
      }
      .getOrElse {
        // no target set
        zeroKW
      }

    PowerLimitFlexOptions(
      refPower,
      if dischargingPossible then model.pMax * -1 else zeroKW,
      if chargingPossible then model.pMax else zeroKW,
    )
  }

  override def determineNextActivation(
      state: StorageState,
      operatingPoint: ActivePowerOperatingPoint,
      setPower: Power,
      dataTimeType: DataTimeType,
  ): OperationChangeIndicator = {

    val adaptedSetPower = operatingPoint.activePower

    // if the storage is at minimum or maximum charged energy AND we are charging
    // or discharging, flex options will be different at the next activation
    val isEmptyOrFull =
      model.isEmpty(state.storedEnergy) || model.isFull(state.storedEnergy)
    // if target soc is enabled, we can also be at that exact point
    val isAtTarget = model.refTargetSoc.exists { targetParams =>
      state.storedEnergy <= targetParams.targetWithPosMargin &&
      state.storedEnergy >= targetParams.targetWithNegMargin
    }
    val isChargingOrDischarging = adaptedSetPower != zeroKW
    // if we've been triggered just before we hit the minimum or maximum energy,
    // and we're still discharging or charging respectively (happens in edge cases),
    // we already set the power to zero and also want to refresh flex options
    // at the next activation.
    // Similarly, if the ref target margin area is hit before hitting target SOC, we want
    // to refresh flex options.
    val hasObsoleteFlexOptions =
      (model.isFull(state.storedEnergy) && setPower > zeroKW) ||
        (model.isEmpty(state.storedEnergy) && setPower < zeroKW) ||
        (isAtTarget && setPower != zeroKW)

    val activateAtNextTick =
      ((isEmptyOrFull || isAtTarget) && isChargingOrDischarging) || hasObsoleteFlexOptions

    // when charging, calculate time until we're full or at target energy
    val chargingEnergyTarget = () =>
      model.refTargetSoc
        .filter(_.targetWithNegMargin >= state.storedEnergy)
        .map(_.targetSoc)
        .getOrElse(model.eStorage)

    // when discharging, calculate time until we're at lowest energy allowed or at target energy
    val dischargingEnergyTarget = () =>
      model.refTargetSoc
        .filter(_.targetWithPosMargin <= state.storedEnergy)
        .map(_.targetSoc)
        .getOrElse(model.minEnergy)

    // calculate the tick from time span
    val maybeNextTick = ChargingHelper.calcNextEventTick(
      state.storedEnergy,
      adaptedSetPower,
      state.tick,
      chargingEnergyTarget,
      dischargingEnergyTarget,
      model.eta,
    )(using model.powerTolerance)

    OperationChangeIndicator(
      activateAtNextTick,
      maybeNextTick,
    )
  }

}
