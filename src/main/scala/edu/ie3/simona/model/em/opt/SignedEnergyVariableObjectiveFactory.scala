/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.opt.OptimizingFlexStrat.*
import edu.ie3.simona.model.em.opt.SignedEnergyVariableObjectiveFactory.SignedEnergyStepSymbols
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.service.Data.SecondaryData
import edu.ie3.simona.service.Data.SecondaryData.ProsumerPrice
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.scala.quantities.DefaultQuantities.{
  onePU,
  zeroEurPerKWh,
  zeroKW,
}
import optimus.algebra.{Const, Expression, Zero}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPFloatVar, MPVar}
import org.slf4j.{Logger, LoggerFactory}
import squants.energy.KilowattHours
import squants.energy.PowerConversions.PowerNumeric
import squants.{Energy, Power}

import java.util.UUID
import scala.collection.SortedSet

/** Optimization model based on the fundamental idea of Hashmi et al., "Optimal
  * Storage Arbitrage under Net Metering using Linear Programming", formulating
  * the objective in terms of energy change between states instead of power.
  * This makes the problem convex, but only for positive prices.
  */
object SignedEnergyVariableObjectiveFactory
    extends ObjectiveFactory[SignedEnergyStepSymbols] {

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    Iterable(ServiceType.PriceService)

  private val log: Logger =
    LoggerFactory.getLogger(SignedEnergyVariableObjectiveFactory.getClass)

  override def createAssetSymbols(
      assetParams: AssetStepParameters
  )(using model: MPModel): SignedEnergyStepSymbols =
    assetParams match {
      case fixedPower: FixedPowerStepParameters =>
        FixedSignedEnergyStepSymbols(fixedPower)
      case varPower: VariablePowerStepParameters =>
        val lowerBound =
          varPower.pMin * varPower.sampleTime / varPower.etaDischarge.toEach
        val upperBound =
          varPower.pMax * varPower.sampleTime * varPower.etaCharge.toEach

        // modeling the energy change between previous and new state
        // instead of power, which most models would do
        val energyChange = MPFloatVar(
          symbol = s"E_delta_${varPower.stepStartTick}",
          lowerBound = lowerBound.toKilowattHours,
          upperBound = upperBound.toKilowattHours,
        )

        // modeling the new state (stored energy)
        val newEnergy: MPSymbol =
          if varPower.eMin == varPower.eMax then
            Const(varPower.eMax.toKilowattHours)
          else
            MPFloatVar(
              symbol = s"E_${varPower.stepEndTick}",
              lowerBound = varPower.eMin.toKilowattHours,
              upperBound = varPower.eMax.toKilowattHours,
            )

        model.add(newEnergy := varPower.previousStateEnergy + energyChange)

        VariableSignedEnergyStepSymbols(
          varPower,
          energyChange,
          newEnergy,
        )
    }

  override def build(
      flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      assetSymbols: Iterable[AssetSymbolContainer[SignedEnergyStepSymbols]],
      target: Power,
      receivedData: Iterable[SecondaryData],
  )(using model: MPModel): Expression = {

    val priceSeries = extractPriceSeries(receivedData)

    val negPrices = priceSeries
      .filter { case (_, ProsumerPrice(priceSell, _)) =>
        // only check selling price, since it must be smaller than buying price
        priceSell < zeroEurPerKWh
      }
      .map { case (tick, _) =>
        tick
      }
      .to(SortedSet)

    if negPrices.nonEmpty then
      log.warn(
        s"Negative prices were provided for tick(s): ${negPrices.mkString(", ")}. " +
          "Results might deviate from optimal solution."
      )

    sortSymbolsByTick(assetSymbols)
      // create objective expression for every time step
      .map { (stepStartTick, tickAssetSymbols) =>
        val segments = tickAssetSymbols.foldLeft(Seq[Expression](Zero)) {
          case (previousSegments, assetSymbolsAdd) =>
            assetSymbolsAdd.getObjectiveVariations.flatMap { term =>
              previousSegments.map(_ + term)
            }
        }

        val priceData = priceSeries
          .maxBefore(stepStartTick + 1)
          .map { case (_, priceData) => priceData }
          .getOrElse(
            throw new CriticalFailureException("No price data was given!")
          )

        // extract prices in EUR / kWh
        val priceSell = priceData.priceSell.toEuroPerKilowattHour
        val priceBuy = priceData.priceBuy.toEuroPerKilowattHour

        if priceSell > priceBuy then
          throw new CriticalFailureException(
            s"Selling price $priceSell is higher than buying price $priceBuy. " +
              "Objective factory does not know how to handle this."
          )

        // convex, since priceSell < priceBuy
        val costSegments = Seq(priceSell, priceBuy).flatMap { price =>
          segments.map(Const(price) * _)
        }

        createEpigraphVar(
          costSegments,
          s"epigraph_$stepStartTick",
        )
      }
      // combine expressions of all time steps
      .reduceOption[Expression](_ + _)
      .getOrElse(Zero)
  }

  // todo duplicate
  override def getComparableObjectiveValue(
      flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      assetSymbols: Iterable[
        AssetSymbolContainer[SignedEnergyStepSymbols]
      ],
      target: Power,
      receivedData: Iterable[SecondaryData],
  ): Double = {

    val priceSeries = extractPriceSeries(receivedData)

    sortSymbolsByTick(assetSymbols).map { (stepStartTick, tickAssetSymbols) =>
      val priceData = priceSeries
        .maxBefore(stepStartTick + 1)
        .map { case (_, priceData) => priceData }
        .getOrElse(
          throw new CriticalFailureException(
            s"No price data was given for tick $stepStartTick!"
          )
        )

      val priceSell = priceData.priceSell.toEuroPerKilowattHour
      val priceBuy = priceData.priceBuy.toEuroPerKilowattHour

      val powerSum =
        tickAssetSymbols.map(_.getOperatingPowerResult).sum.toKilowatts

      math.max(powerSum * priceSell, powerSum * priceBuy)
    }.sum

  }

  /** Trait for container that provides symbols for a specific asset and
    * optimization time step, to be used by
    * [[SignedEnergyVariableObjectiveFactory]].
    */
  trait SignedEnergyStepSymbols extends AssetStepSymbols {

    /** Get all variations of the terms (consisting of signed energy variable
      * and coefficient) to be used within cross products in the objective.
      * @return
      */
    def getObjectiveVariations: Seq[Expression]

  }

  /** Container that provides symbols for a specific asset and for an
    * optimization time step in which energy change is fixed, to be used by
    * [[SignedEnergyVariableObjectiveFactory]].
    *
    * @param parameters
    *   Parameters for the asset at the specific time step.
    */
  private final case class FixedSignedEnergyStepSymbols(
      override val parameters: FixedPowerStepParameters
  ) extends SignedEnergyStepSymbols {

    override def getObjectiveVariations: Seq[Expression] =
      Seq(Const(parameters.energyChange.toKilowattHours))

    override def getStepEndStateSymbol: MPSymbol = Const(
      parameters.stepEndEnergy.toKilowattHours
    )

    override def getOperatingPowerResult: Power =
      parameters.energyChange / parameters.sampleTime

    override def getStepEndEnergyResult: Energy = parameters.stepEndEnergy

  }

  /** Container that provides symbols for a specific asset and for an
    * optimization time step in which energy change is variable, to be used by
    * [[SignedEnergyVariableObjectiveFactory]].
    *
    * @param parameters
    *   Parameters for the asset at the specific time step.
    * @param energyChange
    *   The operation variable, describing the energy change in kWh to get from
    *   the energy state at the start to the state at the end of the time step
    *   interval.
    * @param stepEndState
    *   The state variable, describing the amount of energy in kWh at the end of
    *   the time step interval.
    */
  private final case class VariableSignedEnergyStepSymbols(
      parameters: VariablePowerStepParameters,
      energyChange: MPVar,
      stepEndState: MPSymbol,
  ) extends SignedEnergyStepSymbols {

    override def getStepEndStateSymbol: MPSymbol = stepEndState

    override def getObjectiveVariations: Seq[Expression] =
      if isInefficient then
        // Convex, because etaDischarge < 1/etaCharge.
        Seq(
          Const(1d / parameters.etaCharge.toEach) * energyChange,
          Const(parameters.etaDischarge.toEach) * energyChange,
        )
      else Seq(energyChange)

    private def isInefficient: Boolean =
      parameters.etaCharge < onePU || parameters.etaDischarge < onePU

    override def getOperatingPowerResult: Power = {

      // power on storage side
      val storagePower =
        KilowattHours(energyChange.getValue) / parameters.sampleTime

      val factor =
        if storagePower < zeroKW then parameters.etaDischarge.toEach
        else 1d / parameters.etaCharge.toEach

      // outside power
      storagePower * factor
    }

    override def getStepEndEnergyResult: Energy =
      KilowattHours(stepEndState.getValue)

  }

}
