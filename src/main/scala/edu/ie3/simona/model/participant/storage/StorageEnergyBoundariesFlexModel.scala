/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.model.participant.flex.AbstractEnergyBoundariesFlexModel
import edu.ie3.simona.model.participant.storage.StorageModel.StorageState
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexOptions,
}
import edu.ie3.simona.service.DataTimeType

class StorageEnergyBoundariesFlexModel(private val model: StorageModel)
    extends AbstractEnergyBoundariesFlexModel[StorageState] {

  override val hasEnergyFlexibility: Boolean = true

  override def determineFlexOptions(
      state: StorageState,
      dateTimeType: DataTimeType,
  ): FlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        eStorage = model.eStorage,
        currentEnergy = state.storedEnergy,
        pMax = model.pMax,
        etaCharge = model.eta,
        etaDischarge = model.eta,
        currentTick = state.tick,
      )
    )

}
