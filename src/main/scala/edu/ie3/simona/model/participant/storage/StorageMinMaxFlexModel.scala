/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.model.participant.ParticipantFlexModel
import edu.ie3.simona.model.participant.storage.StorageModel.StorageState
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MinMaxFlexOptions}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW

class StorageMinMaxFlexModel(private val model: StorageModel)
    extends ParticipantFlexModel[StorageState] {

  override def determineFlexOptions(
      state: StorageState
  ): FlexOptions = {

    val chargingPossible = !model.isFull(state.storedEnergy)
    val dischargingPossible = !model.isEmpty(state.storedEnergy)

    val refPower = model.refTargetSoc
      .map { targetParams =>
        if (state.storedEnergy <= targetParams.targetWithPosMargin) {
          if (state.storedEnergy >= targetParams.targetWithNegMargin) {
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

    MinMaxFlexOptions(
      refPower,
      if (dischargingPossible) model.pMax * -1 else zeroKW,
      if (chargingPossible) model.pMax else zeroKW,
    )
  }
}
