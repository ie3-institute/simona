/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.model.participant.ParticipantFlexModel
import edu.ie3.simona.model.participant.storage.StorageModel.StorageState
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.ParticipantEnergyBoundaries
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexOptions,
}
import edu.ie3.util.interval.ClosedInterval

import scala.collection.immutable.SortedMap

class StorageEnergyBoundariesFlexModel(private val model: StorageModel)
    extends ParticipantFlexModel[
      StorageState
    ] {

  override def determineFlexOptions(state: StorageState): FlexOptions =
    EnergyBoundariesFlexOptions(
      ParticipantEnergyBoundaries(
        energyLimits = SortedMap(
          state.tick -> ClosedInterval(
            -state.storedEnergy,
            model.eStorage - state.storedEnergy,
          )
        ),
        powerLimits = ClosedInterval(-model.pMax, model.pMax),
        etaCharge = model.eta,
        etaDischarge = model.eta,
      )
    )

}
