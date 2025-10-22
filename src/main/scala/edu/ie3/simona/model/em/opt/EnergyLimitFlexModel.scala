/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.EnergyLimitFlexOptions.ParticipantEnergyBoundaries
import edu.ie3.simona.ontology.messages.flex.MathFlexOptions.SoftConstraint
import edu.ie3.util.interval.ClosedInterval
import optimus.algebra.{Const, Double2Const, Expression}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPFloatVar, MPVar}
import squants.energy.Kilowatts
import squants.{Each, Power, Time}

object EnergyLimitFlexModel {

  def addStep(
      energyBoundaries: ParticipantEnergyBoundaries,
      tick: Long,
      duration: Time,
      lastState: Option[Expression],
  )(using model: MPModel): StepResults = {

    val energyLimits = energyBoundaries.energyLimits
      .maxBefore(tick + 1)
      .map { case (_, limits) =>
        limits
      }
      .getOrElse(throw new CriticalFailureException("No energy limits found!"))

    if energyLimits.getUpper == energyLimits.getLower then {
      // there is no flexibility at all, thus we don't need any state to keep track of

      val fixedPower = energyLimits.getUpper / duration
      StepResults(None, Const(fixedPower.toKilowatts), None)
    } else {
      // we do have some flexibility at this point in time, model it

      // we use charging efficiency for both charging and discharging,
      // since we use the adapted storage model here
      val eta = energyBoundaries.etaCharge

      // determining a previous state
      val previousState = lastState.getOrElse {

        val formerEnergyLimits =
          energyBoundaries.energyLimits.maxBefore(tick).map {
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

      // modeling the operating point (power),
      // valid between that previous and new state
      val p = MPFloatVar(
        "p",
        energyBoundaries.powerLimits.getLower.toKilowatts,
        energyBoundaries.powerLimits.getUpper.toKilowatts,
      )

      // modeling the new state (stored energy)
      val newState = MPFloatVar(
        "state",
        energyLimits.getLower.toKilowattHours,
        energyLimits.getUpper.toKilowattHours,
      )

      val softConstraint =
        if eta == Each(1) then {
          // there are no charging/discharging losses, we can keep it simple

          model.add(newState := previousState + p * duration.toHours)
          None
        } else {
          // there are charging/discharging losses, thus use the full model

          val pAbs =
            MPFloatVar(
              "pAbs",
              0,
              energyBoundaries.powerLimits.getUpper.toKilowatts,
            )

          model.add(pAbs >:= p)
          model.add(pAbs >:= -p)

          model.add(
            newState := previousState + (p - pAbs * (1 - eta.toEach)) * duration.toHours
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

  def adaptEnergyBoundaries(
      boundaries: ParticipantEnergyBoundaries
  ) = {

    val etaCh = boundaries.etaCharge.toEach
    val etaDis = boundaries.etaDischarge.toEach

    val etaAvg = (2 * etaCh * etaDis) / (1 + etaCh * etaDis)

    val newEnergyLimits = boundaries.energyLimits.map { case (tick, limits) =>
      val newLower = (limits.getLower / etaCh) * etaAvg
      val newUpper = (limits.getUpper / etaCh) * etaAvg

      tick -> ClosedInterval(newLower, newUpper)
    }

    val etaAvgEach = Each(etaAvg)

    boundaries.copy(
      energyLimits = newEnergyLimits,
      etaCharge = etaAvgEach,
      etaDischarge = etaAvgEach,
    )

  }

  final case class StepResults(
      state: Option[Expression],
      operation: Const | MPVar,
      softConstraint: Option[SoftConstraint],
  ) {

    def getOperationResult: Power = Kilowatts(operation match {
      case const: Const => const.value
      case variable: MPVar =>
        variable.value.getOrElse(
          throw new CriticalFailureException(
            s"No result present for variable $variable"
          )
        )
    })
  }

}
