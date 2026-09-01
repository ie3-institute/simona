/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.model.em.opt.OptimizingFlexStrat.*
import edu.ie3.simona.model.em.opt.PowerVariableObjectiveFactory.{
  FixedPowerVarAssetStepSymbols,
  MinAbsPowerObjective,
  PowerVarAssetStepSymbols,
}
import edu.ie3.simona.model.em.opt.SplitPowerVarsObjectiveFactory.*
import edu.ie3.simona.model.em.opt.SplitPowerVarsObjectiveFactory.SplitPowerVarsAdditionalConstraints.*
import optimus.algebra.{Const, Expression}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPBinaryVar, MPFloatVar, MPVar}
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power}

/** Implementation for storage optimization that is quite common in literature.
  * Charging and discharging each are modeled with their own variable. Multiple
  * additional constraints are possible: The relaxed model allows for
  * simultaneous charging and discharging; the complementarity constraint
  * involving a binary variable delivers exact results, but makes the problem
  * MILP.
  */
abstract class SplitPowerVarsObjectiveFactory
    extends PowerVariableObjectiveFactory {

  val additionalConstraints: SplitPowerVarsAdditionalConstraints

  override def createAssetSymbols(
      assetParams: AssetStepParameters
  )(using model: MPModel): PowerVarAssetStepSymbols =
    assetParams match {
      case fixedPower: FixedPowerStepParameters =>
        FixedSplitPowerAssetStepSymbols(fixedPower)

      case varPower: VariablePowerStepParameters =>
        val etaCh = varPower.etaCharge.toEach
        val etaDis = varPower.etaCharge.toEach
        val pChMax = varPower.pMax.toKilowatts
        val pDisMax = -varPower.pMin.toKilowatts
        val eMin = varPower.eMin.toKilowattHours
        val eMax = varPower.eMax.toKilowattHours
        val sampleHours = varPower.sampleTime.toHours

        // modeling the new state (stored energy)
        val newState: MPVar | Const =
          if eMin == eMax then Const(eMax)
          else
            MPFloatVar(
              symbol = s"e_${varPower.stepEndTick}",
              lowerBound = eMin,
              upperBound = eMax,
            )

        if varPower.isInefficient then {
          // there are charging/discharging losses, thus use the full model

          // modeling the operating point (power),
          // valid between previous and new state
          val pCharge = MPFloatVar(
            symbol = s"p_ch${varPower.stepStartTick}",
            lowerBound = 0,
            upperBound = pChMax,
          )
          val pDischarge = MPFloatVar(
            symbol = s"p_dis${varPower.stepStartTick}",
            lowerBound = 0,
            upperBound = pDisMax,
          )

          model.add(
            newState := varPower.previousStateEnergy
              + (pCharge * Const(etaCh)
                - pDischarge * Const(1d / etaDis))
              * Const(sampleHours)
          )

          additionalConstraints match {
            case RelaxedConstraints =>
              // additional constraints for tighter boundaries
              model.add(
                pCharge * Const(1d / pChMax)
                  + pDischarge * Const(1d / pDisMax)
                  <:= Const(1d)
              )
              model.add(
                varPower.previousStateEnergy <:= Const(eMax)
                  - pCharge * Const(sampleHours * etaCh)
              )
              model.add(
                varPower.previousStateEnergy >:= Const(eMin)
                  + pDischarge * Const(sampleHours / etaDis)
              )

            case BinaryConstraint =>
              // binary constraints for exact solutions
              val zCharging = MPBinaryVar(
                symbol = s"z_${varPower.stepStartTick}"
              )
              model.add(pCharge <:= zCharging * Const(pChMax))
              model.add(pDischarge <:= (Const(1) - zCharging) * Const(pDisMax))

            case NoAdditions =>
            // no additional constraints
          }

          InefficientSplitPowerAssetStepSymbols(
            varPower,
            pCharge,
            pDischarge,
            newState,
          )

        } else {
          // there are no charging/discharging losses, we can keep it simple

          val power = MPFloatVar(
            symbol = s"p_${varPower.stepStartTick}",
            lowerBound = varPower.pMin.toKilowatts,
            upperBound = varPower.pMax.toKilowatts,
          )

          model.add(
            newState := varPower.previousStateEnergy +
              power * Const(varPower.sampleTime.toHours)
          )

          EfficientSplitPowerAssetStepSymbols(
            varPower,
            power,
            newState,
          )
        }

    }

}

object SplitPowerVarsObjectiveFactory {

  /** Enumeration that allows specification of additional constraints, which
    * will be added by [[SplitPowerVarsObjectiveFactory.createAssetSymbols]].
    */
  enum SplitPowerVarsAdditionalConstraints:
    case
      /** Tighter boundaries that exclude some, but not all configurations with
        * simultaneous charging and discharging.
        */
      RelaxedConstraints,

      /** Constraints that use a binary variable to produce exact results.
        */
      BinaryConstraint,

      /** No additional constraints.
        */
      NoAdditions

  /** Creates an objective that simply minimizes the absolute value of the sum
    * of power by using an epigraph constraint.
    */
  final case class MinAbsPowerObjectiveFactory(
      override val additionalConstraints: SplitPowerVarsAdditionalConstraints
  ) extends SplitPowerVarsObjectiveFactory
      with MinAbsPowerObjective

  /** Trait for container that provides symbols for a specific asset and
    * optimization time step, to be used by [[SplitPowerVarsObjectiveFactory]].
    * Soft constraints (objective addition) are not used.
    */
  private trait SplitPowerAssetStepSymbols extends PowerVarAssetStepSymbols {

    override lazy val objectiveAddition: Option[Expression] = None

  }

  /** Container that provides symbols for a specific asset and for an
    * optimization time step in which power is fixed, to be used by
    * [[SplitPowerVarsObjectiveFactory]].
    *
    * @param assetParams
    *   Parameters for the asset at the specific time step.
    */
  private final case class FixedSplitPowerAssetStepSymbols(
      override val assetParams: FixedPowerStepParameters
  ) extends SplitPowerAssetStepSymbols
      with FixedPowerVarAssetStepSymbols

  /** Container that provides symbols for a specific asset and for an
    * optimization time step in which power is variable and efficiency is 1, to
    * be used by [[SplitPowerVarsObjectiveFactory]].
    *
    * @param assetParams
    *   Parameters for the asset at the specific time step.
    * @param power
    *   The operation variable, describing the power in kW to get from the
    *   energy state at the start to the state at the end of the interval.
    * @param stepEndState
    *   The state variable, describing the state of energy in kWh at the end of
    *   the time step interval.
    */
  private final case class EfficientSplitPowerAssetStepSymbols(
      assetParams: VariablePowerStepParameters,
      power: MPVar,
      stepEndState: MPVar | Const,
  ) extends SplitPowerAssetStepSymbols {

    override def getOperationPowerSymbol: Expression = power

    override def getStateSymbol: Expression = stepEndState

    override def getOperatingPowerResult: Power = Kilowatts(power.getValue)

    override def getStateOfEnergyResult: Energy =
      KilowattHours(stepEndState.getValue)

    override def getAccuracyCheck: Option[ResultAccuracyCheck] = None

  }

  /** Container that provides symbols for a specific asset and for an
    * optimization time step in which power is variable and efficiency is below
    * 1, to be used by [[SplitPowerVarsObjectiveFactory]].
    *
    * @param assetParams
    *   Parameters for the asset at the specific time step.
    * @param powerCharge
    *   The charging power variable, describing the power in kW to get from the
    *   energy state at the start to the state at the end of the interval when
    *   charging.
    * @param powerDischarge
    *   The discharging power variable, describing the power in kW to get from
    *   the energy state at the start to the state at the end of the interval
    *   when discharging.
    * @param stepEndState
    *   The state variable, describing the state of energy in kWh at the end of
    *   the time step interval.
    */
  private final case class InefficientSplitPowerAssetStepSymbols(
      assetParams: VariablePowerStepParameters,
      powerCharge: MPVar,
      powerDischarge: MPVar,
      stepEndState: MPVar | Const,
  ) extends SplitPowerAssetStepSymbols {

    override def getOperationPowerSymbol: Expression =
      powerCharge - powerDischarge

    override def getStateSymbol: Expression = stepEndState

    override def getOperatingPowerResult: Power = Kilowatts(
      powerCharge.getValue - powerDischarge.getValue
    )

    override def getStateOfEnergyResult: Energy =
      KilowattHours(stepEndState.getValue)

    override def getAccuracyCheck: Option[ResultAccuracyCheck] =
      Some(SplitPowerVariablesAccuracyCheck(powerCharge, powerDischarge))

  }

  /** Accuracy check that detects simultaneous charging and discharging.
    *
    * @param powerCharge
    *   The charging power (positive).
    * @param powerDischarge
    *   The discharging power (positive).
    */
  private final case class SplitPowerVariablesAccuracyCheck(
      powerCharge: MPVar,
      powerDischarge: MPVar,
  ) extends ResultAccuracyCheck {

    override def getError: Double =
      math.abs(math.min(powerCharge.getValue, powerDischarge.getValue))

    override def getWarningMessage: String =
      "Asset is charging and discharging at the same time: " +
        s"${powerCharge.getValue} kW charging, ${powerDischarge.getValue} kW discharging."

  }

}
