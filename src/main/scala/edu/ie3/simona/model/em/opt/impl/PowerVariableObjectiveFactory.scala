/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt.impl

import edu.ie3.simona.model.em.opt.FlexibilityOptimization.{
  FixedPowerStepParameters,
  MPSymbol,
}
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory.{
  AssetStepSymbols,
  AssetSymbolContainer,
}
import edu.ie3.simona.model.em.opt.impl.PowerVariableObjectiveFactory.PowerVarAssetStepSymbols
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.service.Data.SecondaryData.ProsumerPrice
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroEurPerKWh
import optimus.algebra.{Const, Expression, Zero}
import optimus.optimization.MPModel
import squants.{Energy, Power}

import java.util.UUID
import scala.collection.immutable.SortedMap

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

  /** Creates an objective that simply minimizes the maximum absolute power of
    * all time steps by using an epigraph constraint.
    */
  trait PeakShavingObjective
      extends PowerVariableObjectiveFactory
      with ObjectiveFactory.PeakShavingObjective[PowerVarAssetStepSymbols] {

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
      val sortedByTick = sortSymbolsByTick(assetSymbols)

      val allVariants = sortedByTick.toSeq
        // create objective expression for every time step
        .flatMap { case (_, tickAssetSymbols) =>
          val powerSum = createPowerSum(tickAssetSymbols)

          Seq(powerSum, -powerSum)
        }

      val epigraphVar = createEpigraphVar(allVariants, "max_abs_epigraph")

      val softConstraints = sortedByTick
        .flatMap { case (_, tickAssetSymbols) =>
          tickAssetSymbols.flatMap(_.objectiveAddition)
        }
        .reduceOption[Expression](_ + _)
        .getOrElse(Zero)

      epigraphVar + softConstraints
    }

  }

  /** Creates an objective that simply minimizes the absolute value of the sum
    * of power by using an epigraph constraint.
    */
  trait MinAbsPowerObjective
      extends PowerVariableObjectiveFactory
      with ObjectiveFactory.MinAbsPowerObjective[PowerVarAssetStepSymbols] {

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

  /** Creates an objective based on the current and projected price of energy
    * for the prediction horizon.
    *
    * Since we assume that the buying price is always higher than the selling
    * price, we can use an epigraph to derive a linear objective.
    */
  trait PriceObjective
      extends PowerVariableObjectiveFactory
      with ObjectiveFactory.PriceObjective[PowerVarAssetStepSymbols] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable(ServiceType.PriceService)

    override def build(
        flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
        assetSymbols: Iterable[
          AssetSymbolContainer[PowerVarAssetStepSymbols]
        ],
        target: Power,
        receivedData: Iterable[Data.SecondaryData],
    )(using model: MPModel): Expression = {

      val priceSeries = extractPriceSeries(receivedData)

      // Whether at least one selling price is negative. Neg.
      // selling price is a requirement for a neg. buying
      // price as well (selling price < buying price).
      val negPriceExists = priceSeries.exists { case (_, priceData) =>
        priceData.priceSell < zeroEurPerKWh
      }

      val adaptedPriceSeries = transformPrices(priceSeries)

      sortSymbolsByTick(assetSymbols)
        // create objective expression for every time step
        .map { case (stepStartTick, tickAssetSymbols) =>
          val totalPower = createPowerSum(tickAssetSymbols)

          val priceData =
            getAndCheckPriceData(adaptedPriceSeries, stepStartTick)

          // extract prices in EUR / kWh
          val priceSell = priceData.priceSell.toEuroPerKilowattHour
          val priceBuy = priceData.priceBuy.toEuroPerKilowattHour

          // convex, since priceSell < priceBuy
          val epigraphVar = createEpigraphVar(
            Seq(Const(priceSell) * totalPower, Const(priceBuy) * totalPower),
            s"cost_$stepStartTick",
          )
          if negPriceExists then {
            // Add soft constraint only when prices are negative.
            // For positive prices, the optimum is always to not
            // exaggerate losses.
            epigraphVar + tickAssetSymbols
              .flatMap(_.objectiveAddition)
              .reduceOption[Expression](_ + _)
              .getOrElse(Zero)
          } else epigraphVar
        }
        // combine expressions of all time steps
        .reduceOption[Expression](_ + _)
        .getOrElse(Zero)
    }

    def transformPrices(
        priceSeries: SortedMap[Long, ProsumerPrice]
    ): SortedMap[Long, ProsumerPrice] = priceSeries

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
