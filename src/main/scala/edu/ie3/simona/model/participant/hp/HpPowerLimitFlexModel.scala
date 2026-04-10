/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.hp

import edu.ie3.simona.model.participant.ParticipantModel
import edu.ie3.simona.model.participant.ParticipantModel.OperationChangeIndicator
import edu.ie3.simona.model.participant.flex.ParticipantFlexModel
import edu.ie3.simona.model.participant.hp.HpModel.{HpOperatingPoint, HpState}
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  PowerLimitFlexOptions,
}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import squants.Power

class HpPowerLimitFlexModel(private val model: HpModel)
    extends ParticipantFlexModel[HpOperatingPoint, HpState] {

  override def determineFlexOptions(
      state: HpState,
      dateTimeType: DataTimeType,
  ): FlexOptions = {
    val wasRunningLastOp = state.lastHpOperatingPoint.activePower > zeroKW
    // Determining the operation point and limitations at this tick
    val (turnOn, canOperate, canBeOutOfOperation) =
      model.determineHpOperatingOptions(
        state.thermalGridState,
        state.thermalDemands,
        wasRunningLastOp,
      )
    val refOn = model.sRated.toActivePower(model.cosPhiRated)

    val shouldRunHeatPump = {
      state.lastHpOperatingPoint.activePower > zeroKW &&
      state.thermalDemands.houseDemand.hasPossibleDemand &&
      state.thermalGridState.heatStorageState
        .map(_.storedEnergy)
        .getOrElse(zeroKWh) == zeroKWh
    }

    val (refPower, minPower) = (turnOn, canBeOutOfOperation) match {
      case (true, true) =>
        if shouldRunHeatPump then {
          // if Hp was running last state AND there is demand from the house AND the storage is empty,
          // we would like to keep that behaviour even in strict interpretation of flexibility we could
          // be out of operation for flex reasons. Thus, we force Hp to run.
          (refOn, refOn)
        } else {
          (refOn, zeroKW)
        }
      case (true, false) =>
        (refOn, refOn)
      case (false, true) =>
        (zeroKW, zeroKW)
      case _ =>
        throw new IllegalStateException(
          "An unsupported FlexOption for a heat pump has been determined."
        )
      // should not be possible to reach
    }

    val maxPower = if canOperate then refOn else zeroKW

    PowerLimitFlexOptions(refPower, minPower, maxPower)
  }

  override def determineNextActivation(
      state: HpState,
      operatingPoint: HpOperatingPoint,
      setPower: Power,
      dateTimeType: DataTimeType,
  ): ParticipantModel.OperationChangeIndicator =
    OperationChangeIndicator(
      changesAtNextActivation = true,
      changesAtTick = model.getNextActivation(state, operatingPoint),
    )

}
