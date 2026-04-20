/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.model.participant.ParticipantModel.OperationChangeIndicator
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsOperatingPoint,
  EvcsState,
}
import edu.ie3.simona.model.participant.flex.ParticipantFlexModel
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  PowerLimitFlexOptions,
}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.Power

class EvcsPowerLimitFlexModel(private val model: EvcsModel)
    extends ParticipantFlexModel[EvcsOperatingPoint, EvcsState] {

  override def determineFlexOptions(
      state: EvcsState,
      dataTimeType: DataTimeType,
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

  override def determineNextActivation(
      state: EvcsState,
      operatingPoint: EvcsOperatingPoint,
      setPower: Power,
      dataTimeType: DataTimeType,
  ): OperationChangeIndicator =
    state.evs
      .flatMap { ev =>
        operatingPoint.evOperatingPoints.get(ev.uuid).map(ev -> _)
      }
      .map { case (ev, chargingPower) =>
        val chargingLimitTick =
          model.determineChargingLimitEvent(ev, chargingPower, state.tick)
        val requiredChargingTick =
          determineRequiredChargingEvent(ev, chargingPower, state.tick)

        val nextTick = Seq(
          chargingLimitTick,
          requiredChargingTick,
          Some(ev.departureTick),
        ).flatten.minOption

        OperationChangeIndicator(
          changesAtNextActivation =
            (model.isFull(ev) || model.isEmpty(ev)) && chargingPower != zeroKW,
          changesAtTick = nextTick,
        )
      }
      .reduceOption(_ | _)
      .getOrElse(OperationChangeIndicator.empty)

  private def determineRequiredChargingEvent(
      ev: EvModelWrapper,
      power: Power,
      currentTick: Long,
  ): Option[Long] = {
    implicit val tolerance: Power = model.calcPowerTolerance

    val maxPower = model.getMaxAvailableChargingPower(ev)

    // we determine the last possible tick until which the EV
    // can charge with the given power and still reach its
    // departure SOC target when charging with full power afterward
    Option
      .unless(power ~= maxPower) {
        val targetEnergy = ev.eStorage * model.departureTargetSoc

        val timeToEvent =
          (ev.storedEnergy - targetEnergy +
            ev.timeToDeparture(currentTick) * maxPower) /
            (maxPower - power)

        currentTick + timeToEvent.toSeconds.toLong
      }
      .filter(candidateTick =>
        // should not be situated in the past,
        // which happens if target cannot be reached any more before departure
        candidateTick > currentTick &&
          // should not be situated after departure,
          // which happens if target is already met at current charging power
          candidateTick < ev.departureTick
      )
  }

}
