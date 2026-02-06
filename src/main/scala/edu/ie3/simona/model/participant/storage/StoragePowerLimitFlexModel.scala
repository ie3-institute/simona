/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.model.participant.ParticipantFlexModel
import edu.ie3.simona.model.participant.storage.StorageModel.StorageState
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  FlexType,
  PowerLimitFlexOptions,
}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW

class StoragePowerLimitFlexModel(private val model: StorageModel)
    extends ParticipantFlexModel[StorageState] {

  override val flexType: FlexType = FlexType.PowerLimit

  override def determineFlexOptions(
      state: StorageState
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
}
