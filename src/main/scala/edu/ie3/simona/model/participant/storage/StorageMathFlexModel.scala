/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantFlexModel
import edu.ie3.simona.model.participant.storage.StorageMathFlexModel.StorageMathFlexOptions
import edu.ie3.simona.model.participant.storage.StorageModel.StorageState
import edu.ie3.simona.ontology.messages.flex.MathFlexOptions.{
  OperationVars,
  SoftConstraint,
}
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MathFlexOptions}
import optimus.algebra.{Const, Double2Const, Expression, Long2Const}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPFloatVar, MPVar}
import squants.{Dimensionless, Each, Time}
import squants.energy.{Energy, Kilowatts, Power}

/** Flex model implementation for [[StorageModel]] producing
  * [[MathFlexOptions]].
  *
  * @param model
  *   The [[StorageModel]] to use parameters from.
  */
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

  /** Flex options for a storage that uses adapted energy and efficiency values
    * in order to stay in linear programming. This model can be derived from any
    * storage model using separate charging and discharging efficiencies (refer
    * to [[StorageMathFlexOptions.createAdaptedFlexOptions]] for more
    * information). Here, a common loss is subtracted from stored energy with
    * every charging and discharging operation.
    *
    * For this model, the absolute value of the charging/discharging power is
    * required. To achieve this in linear programming, an epigraph constraint on
    * the power variable is used, which requires a soft constraint as part of
    * the objective.
    *
    * @param currentEnergy
    *   The stored energy currently stored within [[StorageModel]], adapted for
    *   linearity.
    * @param eStorage
    *   The storage capacity of the [[StorageModel]], adapted for linearity.
    * @param pMax
    *   The maximum charging and discharging power.
    * @param eta
    *   The efficiency of charging and discharging operations, adapted for
    *   linearity.
    */
  class StorageMathFlexOptions(
      val currentEnergy: Energy,
      val eStorage: Energy,
      val pMax: Power,
      val eta: Dimensionless,
  ) extends MathFlexOptions[StorageStateVars, StorageOperationVars] {

    override def addInitialState(
        tick: Long
    )(using model: MPModel): StorageStateVars = {
      val storedEnergy = Const(currentEnergy.toKilowattHours)
      StorageStateVars(storedEnergy, tick)
    }

    override def addOperationConstraints(state: StorageStateVars)(using
        model: MPModel
    ): StorageOperationVars = {
      val p = MPFloatVar("p", -pMax.toKilowatts, pMax.toKilowatts)
      val pAbs = MPFloatVar("pAbs", 0, pMax.toKilowatts)

      model.add(pAbs >:= p)
      model.add(pAbs >:= -p)

      StorageOperationVars(p, pAbs, eta)
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

    /** Creates an equivalent linear battery model using parameters of a regular
      * model. A common efficiency needs to be calculated for charging and
      * discharging operations. Furthermore, energy amounts need to be adapted.
      *
      * @param currentEnergy
      *   The currently stored energy.
      * @param eStorage
      *   The storage capacity.
      * @param pMax
      *   The maximum charging and discharging power.
      * @param etaCharging
      *   The charging efficiency.
      * @param etaDischarging
      *   The discharging efficiency.
      * @return
      *   An adapted model.
      */
    def createAdaptedFlexOptions(
        currentEnergy: Energy,
        eStorage: Energy,
        pMax: Power,
        etaCharging: Dimensionless,
        etaDischarging: Dimensionless,
    ): StorageMathFlexOptions = {

      val etaCh = etaCharging.toEach
      val etaDis = etaDischarging.toEach

      val etaAvg = (2 * etaCh * etaDis) / (1 + etaCh * etaDis)

      val adaptedCurrentEnergy = (currentEnergy / etaCh) * etaAvg
      val adaptedEStorage = (eStorage / etaCh) * etaAvg

      new StorageMathFlexOptions(
        adaptedCurrentEnergy,
        adaptedEStorage,
        pMax,
        Each(etaAvg),
      )
    }

  }

  /** Relevant data related to a storage state.
    *
    * @param storedEnergy
    *   The amount of stored energy either as a constant ([[Const]]) or as a
    *   variable ([[MPVar]]).
    * @param tick
    *   The tick of this state.
    */
  final case class StorageStateVars(storedEnergy: Expression, tick: Long)

  /** Relevant data related to a storage operating point.
    *
    * @param p
    *   The charging/discharging power in kW. Positive means charging.
    * @param pAbs
    *   The absolute power in kW. This should approximate [[p]] closely.
    * @param eta
    *   The common charging/discharging efficiency.
    */
  final case class StorageOperationVars(
      p: MPVar,
      pAbs: MPVar,
      eta: Dimensionless,
  ) extends OperationVars {

    override def getPowerExpression: Expression =
      p

    override def getPowerSolution: Option[Power] =
      p.value.map(Kilowatts.apply)

    override def getSoftConstraint(duration: Time): Option[SoftConstraint] = {
      // putting a penalty on pAbs, so that it comes
      // as close as possible to the absolute power
      Some(new StorageSoftConstraint(duration))
    }

    /** Soft constraint for storage operation variables. Required for [[pAbs]]
      * to closely approximate the absolute value of [[p]].
      *
      * @param duration
      *   The sample time, thus charging/discharging duration.
      */
    private class StorageSoftConstraint(duration: Time) extends SoftConstraint {

      override def getExpression: Expression = {
        // Total penalty is slightly larger than the losses
        // calculated by StorageMathFlexOptions. Thus, the
        // value of pAbs should be pushed down to the absolute
        // of p.
        val epsilon = 1e-6
        pAbs * (1 - eta.toEach + epsilon) * duration.toHours
      }

      override def getError: Double = {
        val (pValue, pAbsValue) = getVals
        math.abs(math.abs(pValue) - pAbsValue)
      }

      override def getWarningMessage: String = {
        val (pValue, pAbsValue) = getVals
        s"Soft constraint for storage: Approximated absolute power value $pAbsValue and absolute power value |$pValue| are $getError apart."
      }

      private def getVals: (Double, Double) = p.value
        .zip(pAbs.value)
        .getOrElse(
          throw new CriticalFailureException(
            "Solution are expected to be determined at this point!"
          )
        )

    }
  }

}
