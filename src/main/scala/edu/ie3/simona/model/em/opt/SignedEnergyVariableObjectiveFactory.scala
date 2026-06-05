/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.opt.OptimizedFlexStrat.*
import edu.ie3.simona.model.em.opt.SignedEnergyVariableObjectiveFactory.SignedEnergyStepVars
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
import squants.{Dimensionless, Energy, Power, Seconds, Time}

import java.util.UUID
import scala.collection.SortedSet

/** Optimization model based on the fundamental idea of Hashmi et al., "Optimal
  * Storage Arbitrage under Net Metering using Linear Programming", formulating
  * the objective in terms of energy change between states instead of power.
  * This makes the problem convex, but only for positive prices.
  */
object SignedEnergyVariableObjectiveFactory
    extends ObjectiveFactory[SignedEnergyStepVars] {

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    Iterable(ServiceType.PriceService)

  private val log: Logger =
    LoggerFactory.getLogger(SignedEnergyVariableObjectiveFactory.getClass)

  override def createAssetVars(
      assetParams: AssetStepParameters,
      stepStartTick: Long,
      stepEndTick: Long,
      sampleTime: Time,
  )(using model: MPModel): SignedEnergyStepVars =
    assetParams match {
      case fixedPower: FixedPowerStepParameters =>
        val energyChange = Const(fixedPower.energyChange.toKilowattHours)
        SignedEnergyStepVars(
          energyChange,
          None,
          stepStartTick,
          stepEndTick,
        )
      case varPower: VariablePowerStepParameters =>
        val lowerBound =
          varPower.pMin * sampleTime / varPower.etaDischarge.toEach
        val upperBound =
          varPower.pMax * sampleTime * varPower.etaCharge.toEach

        // modeling the energy change between previous and new state
        // instead of power, which most models would do
        val energyChange = MPFloatVar(
          symbol = s"E_delta_$stepStartTick",
          lowerBound = lowerBound.toKilowattHours,
          upperBound = upperBound.toKilowattHours,
        )

        // modeling the new state (stored energy)
        val newEnergy: MPVar | Const =
          if varPower.eMin == varPower.eMax then
            Const(varPower.eMax.toKilowattHours)
          else
            MPFloatVar(
              symbol = s"E_$stepEndTick",
              lowerBound = varPower.eMin.toKilowattHours,
              upperBound = varPower.eMax.toKilowattHours,
            )

        model.add(newEnergy := varPower.previousStateEnergy + energyChange)

        SignedEnergyStepVars(
          energyChange,
          Some(newEnergy),
          stepStartTick,
          stepEndTick,
          etaCharge = varPower.etaCharge,
          etaDischarge = varPower.etaDischarge,
        )
    }

  override def build(
      flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      assetVars: Iterable[AssetVarContainer[SignedEnergyStepVars]],
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

    sortVarsByTick(assetVars)
      // create objective expression for every time step
      .map { (stepStartTick, tickAssetVars) =>
        val segments = tickAssetVars.foldLeft(Seq[Expression](Zero)) {
          case (previousSegments, assetVars) =>
            val terms =
              if assetVars.isInefficient then
                // Using energy change variables to preserve
                // linearity of the problem. Convex, because
                // etaDischarge < 1/etaCharge.
                Seq(
                  Const(1d / assetVars.etaCharge.toEach) *
                    assetVars.energyChange,
                  Const(assetVars.etaDischarge.toEach) *
                    assetVars.energyChange,
                )
              else Seq(assetVars.energyChange)

            terms.flatMap { term =>
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
  final case class SignedEnergyStepVars(
      energyChange: MPVar | Const,
      state: Option[MPVar | Const],
      override val stepStartTick: Long,
      override val stepEndTick: Long,
      etaCharge: Dimensionless = onePU,
      etaDischarge: Dimensionless = onePU,
  ) extends AssetStepVars {

    def isInefficient: Boolean =
      etaCharge < onePU || etaDischarge < onePU

    override def getOperatingPowerResult: Power = {
      val duration = Seconds(stepEndTick - stepStartTick)

      val storagePower = KilowattHours(energyChange.getValue) / duration

      val factor =
        if storagePower < zeroKW then etaDischarge.toEach
        else 1d / etaCharge.toEach

      // outside power
      storagePower * factor
    }

    override def getStateOfEnergyResult: Option[Energy] =
      state.map(_.getValue).map(KilowattHours(_))

  }

}
