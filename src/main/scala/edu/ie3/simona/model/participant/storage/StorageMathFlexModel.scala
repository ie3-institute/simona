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
import optimus.algebra.{Double2Const, Expression, Long2Const}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPFloatVar, MPVar}
import squants.{Dimensionless, Each}
import squants.energy.{Energy, Kilowatts, Power}

class StorageMathFlexModel(private val model: StorageModel)
    extends ParticipantFlexModel[
      StorageState
    ] {

  override def determineFlexOptions(state: StorageState): FlexOptions =
    StorageMathFlexOptions.createAdaptedFlexOptions(
      state.storedEnergy,
      model.eStorage,
      model.pMax,
      model.eta,
      model.eta,
    )
}

object StorageMathFlexModel {

  final case class StorageStateVars(storedEnergy: MPVar, tick: Long)

  final case class StorageOperationVars(p: MPVar, pAbs: MPVar)
      extends OperationVars {

    override def getPowerExpression: Expression =
      p

    override def getPowerSolution: Option[Power] =
      p.value.map(Kilowatts.apply)

    override def getSoftConstraints: Option[Expression] = {
      // putting a penalty on pAbs, so that it comes as close as possible to the absolute power
      val penalty = 1e-2
      Some(penalty * pAbs)
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
      val p = MPFloatVar("p", -pMax.toKilowatts, pMax.toKilowatts)
      val pAbs = MPFloatVar("pAbs", 0, pMax.toKilowatts)

      model.add(pAbs >:= p)
      model.add(pAbs >:= -p)

      StorageOperationVars(p, pAbs)
    }

    override def addNewStateConstraints(
        formerState: StorageStateVars,
        op: StorageOperationVars,
        tick: Long,
    )(using model: MPModel): StorageStateVars = {

      val storedEnergy = MPFloatVar("storedEnergy", 0, eStorage.toKilowattHours)
      val timeInHours = (tick - formerState.tick) / 3600

      model.add(
        storedEnergy := formerState.storedEnergy + (op.p - op.pAbs * (1 - eta.toEach)) * timeInHours
      )

      StorageStateVars(storedEnergy, tick)
    }
  }

  object StorageMathFlexOptions {

    def createAdaptedFlexOptions(
        currentEnergy: Energy,
        eStorage: Energy,
        pMax: Power,
        etaCharging: Dimensionless,
        etaDischarging: Dimensionless,
    ): StorageMathFlexOptions = {

      val etaAvg =
        (2 * etaCharging.toEach * etaDischarging.toEach) / (1 + etaCharging.toEach * etaDischarging.toEach)

      val adaptedCurrentEnergy = (currentEnergy / etaCharging.toEach) * etaAvg
      val adaptedEStorage = (eStorage / etaCharging.toEach) * etaAvg

      new StorageMathFlexOptions(
        adaptedCurrentEnergy,
        adaptedEStorage,
        pMax,
        Each(etaAvg),
      )
    }
  }

}
