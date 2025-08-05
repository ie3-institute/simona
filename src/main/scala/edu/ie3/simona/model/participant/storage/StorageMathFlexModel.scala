/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.model.participant.ParticipantFlexModel
import edu.ie3.simona.model.participant.storage.StorageMathFlexModel.StorageMathFlexOptions
import edu.ie3.simona.model.participant.storage.StorageModel.StorageState
import edu.ie3.simona.ontology.messages.flex.MathFlexOptions.OperationVars
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MathFlexOptions}
import optimus.algebra.{Double2Const, Expression}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPFloatVar, MPVar}
import squants.energy.{Energy, Kilowatts, Power}
import squants.{Dimensionless, Time}

class StorageMathFlexModel(private val model: StorageModel)
    extends ParticipantFlexModel[
      StorageState
    ] {

  override def determineFlexOptions(state: StorageState): FlexOptions =
    StorageMathFlexOptions(
      state.storedEnergy,
      model.eStorage,
      model.pMax,
      model.eta,
    )
}

object StorageMathFlexModel {

  final case class StorageStateVars(storedEnergy: MPVar)

  final case class StorageOperationVars(pCharge: MPVar, pDischarge: MPVar)
      extends OperationVars {

    override def getPowerExpression: Expression =
      pCharge - pDischarge

    override def getPowerSolution: Option[Power] =
      pCharge.value.zip(pDischarge.value).map { case (pChValue, pDischValue) =>
        Kilowatts(pChValue - pDischValue)
      }

    override def getSoftConstraints: Option[Expression] = {
      val penalty = 1e-6
      Some(penalty * pCharge * pDischarge)
    }
  }

  final case class StorageMathFlexOptions(
      currentEnergy: Energy,
      eStorage: Energy,
      pMax: Power,
      eta: Dimensionless,
  ) extends MathFlexOptions[StorageStateVars, StorageOperationVars] {

    override def addInitialState(using model: MPModel): StorageStateVars = {
      val currentKWh = currentEnergy.toKilowattHours
      val storedEnergy = MPFloatVar(currentKWh, currentKWh)

      StorageStateVars(storedEnergy)
    }

    override def addOperationConstraints(state: StorageStateVars)(using
        model: MPModel
    ): StorageOperationVars = {
      val pCharge = MPFloatVar(0, pMax.toKilowatts)
      val pDischarge = MPFloatVar(0, pMax.toKilowatts)

      // soft constraint on simultaneous charging and discharging
      model.add(pCharge + pDischarge <:= pMax.toKilowatts)

      StorageOperationVars(pCharge, pDischarge)
    }

    override def addNewStateConstraints(
        oldState: StorageStateVars,
        op: StorageOperationVars,
        timeSpan: Time,
    )(using model: MPModel): StorageStateVars = {
      val storedEnergy = MPFloatVar(0, eStorage.toKilowattHours)

      model.add(
        storedEnergy := oldState.storedEnergy + (op.pCharge * eta.toEach - op.pDischarge * (1 / eta.toEach)) * timeSpan.toHours
      )

      StorageStateVars(storedEnergy)
    }
  }

}
