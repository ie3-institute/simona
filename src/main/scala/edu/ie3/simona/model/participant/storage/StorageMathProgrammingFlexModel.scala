/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.model.participant.ParticipantFlexModel
import edu.ie3.simona.model.participant.storage.StorageMathProgrammingFlexModel.MPFlexOptions
import edu.ie3.simona.model.participant.storage.StorageModel.StorageState
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  MathProgrammingFlexOptions,
}
import optimus.algebra.{Double2Const, Int2Const}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPBinaryVar, MPFloatVar, MPVar}
import squants.Time
import squants.energy.{Energy, Power}

class StorageMathProgrammingFlexModel(private val model: StorageModel)
    extends ParticipantFlexModel[
      StorageState
    ] {

  override def determineFlexOptions(state: StorageState): FlexOptions =
    MPFlexOptions(
      state.storedEnergy,
      model.eStorage,
      model.pMax,
    )
}

object StorageMathProgrammingFlexModel {

  final case class StorageStateVars(storedEnergy: MPVar)

  final case class StorageOperationVars(p: MPVar)

  final case class MPFlexOptions(
      currentEnergy: Energy,
      eStorage: Energy,
      pMax: Power,
  ) extends MathProgrammingFlexOptions[StorageStateVars, StorageOperationVars] {

    override def addInitialState(using model: MPModel): StorageStateVars = {
      val currentKWh = currentEnergy.toKilowattHours
      val storedEnergy = MPFloatVar(currentKWh, currentKWh)

      StorageStateVars(storedEnergy)
    }

    override def addOperationConstraints(state: StorageStateVars)(using
        model: MPModel
    ): StorageOperationVars = {
      val zCharge = MPBinaryVar()
      val zDischarge = MPBinaryVar()
      val p = MPFloatVar(-pMax.toKilowatts, pMax.toKilowatts)

      model.add(
        state.storedEnergy <:= eStorage.toKilowattHours - 0.0001 + (1e10 * (1 - zCharge))
      )
      model.add(p <:= pMax.toKilowatts * zCharge)
      model.add(state.storedEnergy >:= 0.0001 - (1e10 * (1 - zDischarge)))
      model.add(p >:= -pMax.toKilowatts * zDischarge)

      StorageOperationVars(p)
    }

    override def addNewStateConstraints(
        oldState: StorageStateVars,
        op: StorageOperationVars,
        timeSpan: Time,
    )(using model: MPModel): StorageStateVars = {
      val storedEnergy = MPFloatVar(0, eStorage.toKilowattHours)

      model.add(storedEnergy := oldState.storedEnergy + op.p * timeSpan.toHours)

      StorageStateVars(storedEnergy)
    }
  }

}
