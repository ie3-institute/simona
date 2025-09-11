/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.hp

import edu.ie3.simona.model.participant.ParticipantFlexModel
import edu.ie3.simona.model.participant.hp.HpModel.HpState
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  PowerLimitFlexOptions,
}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW

class HpPowerLimitFlexModel(private val model: HpModel)
    extends ParticipantFlexModel[HpState] {

  override def determineFlexOptions(
      state: HpState
  ): FlexOptions = {
    val wasRunningLastOp = state.lastHpOperatingPoint.activePower > zeroKW
    // Determining the operation point and limitations at this tick
    val (turnOn, canOperate, canBeOutOfOperation) =
      model.determineHpOperatingOptions(
        state.thermalGridState,
        state.thermalDemands,
        wasRunningLastOp,
      )

    PowerLimitFlexOptions(
      if turnOn then model.sRated.toActivePower(model.cosPhiRated) else zeroKW,
      if canBeOutOfOperation then zeroKW else model.pRated,
      if canOperate then model.pRated else zeroKW,
    )
  }

}
