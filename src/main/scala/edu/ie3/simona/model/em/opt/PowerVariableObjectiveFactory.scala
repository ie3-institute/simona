/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.model.em.opt.OptimizingFlexStrat.{
  AssetStepSymbols,
  AssetSymbolContainer,
  FixedPowerStepParameters,
  MPSymbol,
  ObjectiveFactory,
}
import edu.ie3.simona.model.em.opt.PowerVariableObjectiveFactory.PowerVarAssetStepSymbols
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.service.{Data, ServiceType}
import optimus.algebra.{Const, Expression, Zero}
import optimus.optimization.MPModel
import squants.{Energy, Power}

import java.util.UUID

/** Trait for objective factories that rely on power variables to describe the
  * energy change between time steps.
  */
trait PowerVariableObjectiveFactory
    extends ObjectiveFactory[PowerVarAssetStepSymbols] {

  /** Creates an absolute variable of the difference between power sum and
    * target power.
    *
    * @param assetSymbols
    *   The asset symbols to optimize for.
    * @param target
    *   The target power for each time step.
    * @param stepStartTick
    *   The tick at the start of the interval.
    * @param model
    *   The optimization model to add variables and constraints to.
    * @return
    *   The absolute difference variable.
    */
  protected def createAbsDifference(
      assetSymbols: Iterable[PowerVarAssetStepSymbols],
      target: Power,
      stepStartTick: Long,
  )(using model: MPModel): Expression = {
    val difference =
      createPowerSum(assetSymbols) - Const(target.toKilowatts)

    createAbsoluteVariable(difference, s"differenceAbs_$stepStartTick")
  }

  protected def createPowerSum(
      assetSymbols: Iterable[PowerVarAssetStepSymbols]
  ): Expression =
    assetSymbols
      .map(_.getOperationPowerSymbol)
      .reduceOption[Expression](_ + _)
      .getOrElse(Zero)

}

object PowerVariableObjectiveFactory {

  /** Creates an objective that simply minimizes the absolute value of the sum
    * of power by using an epigraph constraint.
    */
  trait MinAbsPowerObjective extends PowerVariableObjectiveFactory {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable.empty

    override def build(
        flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
        assetSymbols: Iterable[
          AssetSymbolContainer[PowerVarAssetStepSymbols]
        ],
        target: Power,
        receivedData: Iterable[Data.SecondaryData],
    )(using model: MPModel): Expression = {
      sortSymbolsByTick(assetSymbols)
        // create objective expression for every time step
        .map { case (stepStartTick, tickAssetSymbols) =>
          val absDiff =
            createAbsDifference(tickAssetSymbols, target, stepStartTick)
          val softConstraint = tickAssetSymbols
            .flatMap(_.objectiveAddition)
            .reduceOption[Expression](_ + _)
            .getOrElse(Zero)

          absDiff + softConstraint
        }
        // combine expressions of all time steps
        .reduceOption[Expression](_ + _)
        .getOrElse(Zero)
    }

  }

  /** Trait that needs to be extended by all [[AssetStepSymbols]] that a
    * [[PowerVariableObjectiveFactory]] should handle. The operation power needs
    * to be returned as an expression and optionally, an addition to the
    * objective can be given.
    */
  trait PowerVarAssetStepSymbols extends AssetStepSymbols {

    /** An additional expression that can be (but is not required to be) added
      * to the objective for the given asset and time step.
      */
    lazy val objectiveAddition: Option[Expression]

    /** Returns the operation power expression in kW for the given asset and
      * time step.
      */
    def getOperationPowerSymbol: Expression

  }

  /** Trait that provides basic functionality for time steps with fixed power.
    */
  trait FixedPowerVarAssetStepSymbols extends PowerVarAssetStepSymbols {

    override val parameters: FixedPowerStepParameters

    override lazy val objectiveAddition: Option[Expression] = None

    private lazy val power = parameters.energyChange / parameters.sampleTime

    override def getOperationPowerSymbol: Expression = Const(power.toKilowatts)

    override def getStepEndStateSymbol: MPSymbol =
      Const(parameters.stepEndEnergy.toKilowattHours)

    override def getOperatingPowerResult: Power = power

    override def getStepEndEnergyResult: Energy = parameters.stepEndEnergy

  }

}
