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
import optimus.algebra.{Long2Const, Double2Const, Expression}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPFloatVar, MPVar}
import squants.Dimensionless
import squants.energy.{Energy, Kilowatts, Power}

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

  final case class StorageStateVars(storedEnergy: MPVar, tick: Long)

  final case class StorageOperationVars(pCharge: MPVar, pDischarge: MPVar)
      extends OperationVars {

    override def getPowerExpression: Expression =
      pCharge - pDischarge

    override def getPowerSolution: Option[Power] =
      pCharge.value.zip(pDischarge.value).map { case (pChValue, pDischValue) =>
        Kilowatts(pChValue - pDischValue)
      }

    override def getSoftConstraints: Option[Expression] = {
      // putting a penalty on the sum of charging and positive discharging power,
      // so that the optimizer is incentivized to set at least one of both to 0
      val penalty = 1e-9
      Some(penalty * (pCharge + pDischarge))
    }
  }

  final case class StorageMathFlexOptions(
      currentEnergy: Energy,
      eStorage: Energy,
      pMax: Power,
      eta: Dimensionless,
  ) extends MathFlexOptions[StorageStateVars, StorageOperationVars] {

    override def addInitialState(
        tick: Long
    )(using model: MPModel): StorageStateVars = {
      val currentKWh = currentEnergy.toKilowattHours
      // todo formulate as constant?
      val storedEnergy = MPFloatVar("storedEnergy", currentKWh, currentKWh)

      StorageStateVars(storedEnergy, tick)
    }

    override def addOperationConstraints(state: StorageStateVars)(using
        model: MPModel
    ): StorageOperationVars = {
      val pCharge = MPFloatVar("pCharge", 0, pMax.toKilowatts)
      val pDischarge = MPFloatVar("pDischarge", 0, pMax.toKilowatts)

      // soft constraint on simultaneous charging and discharging
      model.add(pCharge + pDischarge <:= pMax.toKilowatts)

      StorageOperationVars(pCharge, pDischarge)
    }

    override def addNewStateConstraints(
        formerState: StorageStateVars,
        op: StorageOperationVars,
        tick: Long,
    )(using model: MPModel): StorageStateVars = {

      val storedEnergy = MPFloatVar("storedEnergy", 0, eStorage.toKilowattHours)
      val timeInHours = (tick - formerState.tick) / 3600

      model.add(
        storedEnergy := formerState.storedEnergy + (op.pCharge * eta.toEach - op.pDischarge * (1 / eta.toEach)) * timeInHours
      )

      StorageStateVars(storedEnergy, tick)
    }
  }

}
