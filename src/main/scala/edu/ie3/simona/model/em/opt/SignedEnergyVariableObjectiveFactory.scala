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
        val newEnergy: MPVar | Const =
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
      receivedData: Seq[SecondaryData],
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

  trait SignedEnergyStepSymbols extends AssetStepSymbols {

    def getObjectiveVariations: Seq[Expression]

  }

  private final case class FixedSignedEnergyStepSymbols(
      assetParams: FixedPowerStepParameters
  ) extends SignedEnergyStepSymbols {

    override def getObjectiveVariations: Seq[Expression] =
      Seq(Const(assetParams.energyChange.toKilowattHours))

    override def getStateSymbol: Expression = Const(
      assetParams.stepEndEnergy.toKilowattHours
    )

    override def getOperatingPowerResult: Power =
      assetParams.energyChange / assetParams.sampleTime

    override def getStateOfEnergyResult: Energy = assetParams.stepEndEnergy

  }

  /** Container holding the relevant variables for a specific asset and
    * optimization time step. Soft constraints are not used.
    *
    * @param energyChange
    *   The operation variable, describing the energy change in kWh to get from
    *   the energy state at the start to the state at the end of the time step
    *   interval.
    * @param state
    *   The state variable, describing the amount of energy in kWh at the end of
    *   the time step interval.
    * @param stepStartTick
    *   The tick at the start of the time step interval, i.e. the tick at which
    *   the operation of the step starts.
    * @param stepEndTick
    *   The tick at the end of the time step interval, i.e. the tick at which
    *   the operation of the step ends (and a next step might start).
    * @param etaCharge
    *   The charging efficiency of the asset.
    * @param etaDischarge
    *   The discharging efficiency of the asset.
    */
  private final case class VariableSignedEnergyStepSymbols(
      assetParams: VariablePowerStepParameters,
      energyChange: MPVar,
      stepEndState: MPVar | Const,
  ) extends SignedEnergyStepSymbols {

    override def getStateSymbol: Expression = stepEndState

    override def getObjectiveVariations: Seq[Expression] =
      if isInefficient then
        // Using energy change variables to preserve
        // linearity of the problem. Convex, because
        // etaDischarge < 1/etaCharge.
        Seq(
          Const(1d / assetParams.etaCharge.toEach) * energyChange,
          Const(assetParams.etaDischarge.toEach) * energyChange,
        )
      else Seq(energyChange)

    private def isInefficient: Boolean =
      assetParams.etaCharge < onePU || assetParams.etaDischarge < onePU

    override def getOperatingPowerResult: Power = {

      val storagePower =
        KilowattHours(energyChange.getValue) / assetParams.sampleTime

      val factor =
        if storagePower < zeroKW then assetParams.etaDischarge.toEach
        else 1d / assetParams.etaCharge.toEach

      // outside power
      storagePower * factor
    }

    override def getStateOfEnergyResult: Energy =
      KilowattHours(stepEndState.getValue)

  }

}
