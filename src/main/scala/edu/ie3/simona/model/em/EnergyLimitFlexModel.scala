/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.storage.StorageMathFlexModel.StorageMathFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyLimitFlexOptions.ParticipantEnergyBoundaries
import edu.ie3.simona.ontology.messages.flex.MathFlexOptions.SoftConstraint
import optimus.algebra.{Const, Double2Const, Expression}
import optimus.optimization.MPModel
import optimus.optimization.model.MPFloatVar
import squants.energy.{Energy, Power}
import squants.{Dimensionless, Each, Time}

object EnergyLimitFlexModel {

  def addStep(
      flexOptions: ParticipantEnergyBoundaries,
      tick: Long,
      duration: Time,
      formerState: Option[Expression],
  )(using model: MPModel): StepResults = {

    val energyLimits = flexOptions.energyLimits
      .maxBefore(tick + 1)
      .map { case (_, limits) =>
        limits
      }
      .getOrElse(throw new CriticalFailureException("No energy limits found!"))

    // todo
    val eta = Each(1)

    if energyLimits.getUpper == energyLimits.getLower then {
      // there is no flexibility at all, thus we don't need any state to keep track of

      val fixedPower = energyLimits.getUpper / duration
      StepResults(None, Const(fixedPower.toKilowatts), None)
    } else {
      // we do have some flexibility at this point in time, model it

      // determining a previous state
      val oldState = formerState.getOrElse {

        val formerEnergyLimits = flexOptions.energyLimits.maxBefore(tick).map {
          case (_, limits) => limits
        }

        // we have been given no former state as a parameter. Either...
        formerEnergyLimits
          .map { limits =>
            if limits.getLower == limits.getUpper then
              // ... there was no flexibility in the last step, thus we use the last energy value
              Const(limits.getUpper.toKilowattHours)
            else
              throw new CriticalFailureException(
                "No former state was given, although there was flexibility in the last step"
              )
          }
          // ... or this is the initial step, thus we start at 0
          .getOrElse(Const(0d))
      }

      // modeling the new state
      val newState = MPFloatVar(
        "state",
        energyLimits.getLower.toKilowattHours,
        energyLimits.getUpper.toKilowattHours,
      )

      // modeling the operating point (power)
      val p = MPFloatVar(
        "p",
        flexOptions.powerLimits.getLower.toKilowatts,
        flexOptions.powerLimits.getUpper.toKilowatts,
      )

      val softConstraint =
        if eta == Each(1) then {
          model.add(newState := oldState + p * duration.toHours)
          None
        } else {
          val pAbs =
            MPFloatVar("pAbs", 0, flexOptions.powerLimits.getUpper.toKilowatts)

          model.add(pAbs >:= p)
          model.add(pAbs >:= -p)

          model.add(
            newState := oldState + (p - pAbs * (1 - eta.toEach)) * duration.toHours
          )

          Some(new SoftConstraint {

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

          })
        }

      StepResults(Some(newState), p, softConstraint)
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

      // todo adapt
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

  final case class StepResults(
      state: Option[Expression],
      operation: Expression,
      softConstraint: Option[SoftConstraint],
  )

}
