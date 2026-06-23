/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt.impl

import edu.ie3.simona.model.em.opt.FlexibilityOptimization.*
import edu.ie3.simona.model.em.opt.impl.CommonLossObjectiveFactory.*
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory.{
  AssetSymbolContainer,
  RelativeStateErrorHelper,
  VariableAssetStepSymbols,
}
import edu.ie3.simona.model.em.opt.impl.PowerVariableObjectiveFactory.{
  FixedPowerVarAssetStepSymbols,
  MinAbsPowerObjective,
  PowerVarAssetStepSymbols,
  PriceObjective,
}
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.service.Data.SecondaryData.ProsumerPrice
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import optimus.algebra.{Const, Expression, Zero}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPFloatVar, MPVar}
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Dimensionless, Each, Energy, Power}

import java.util.UUID
import scala.collection.immutable.SortedMap

/** Produces asset symbols and an optimization objective that uses a single
  * power variable and a common loss term for both charging and discharging.
  * This means that with this model, it is no longer necessary to distinguish
  * between charging and discharging when formulating the state constraint. For
  * this to work, a common efficiency needs to be determined for charging and
  * discharging operations. This in turn means that all energy values need to be
  * adapted as well (see [[calculateCommonEta]] and
  * [[calculateConversionFactor]]).
  *
  * The loss is calculated using an approximation of the absolute power value,
  * which is kept close to the real absolute value by using a soft constraint.
  */
abstract class CommonLossObjectiveFactory
    extends PowerVariableObjectiveFactory {

  override def createAssetSymbols(
      assetParams: AssetStepParameters
  )(using model: MPModel): PowerVarAssetStepSymbols =
    assetParams match {
      case fixedPower: FixedPowerStepParameters =>
        FixedCommonLossAssetStepSymbols(fixedPower)

      case varPower: VariablePowerStepParameters =>
        val etaCh = varPower.etaCharge
        val etaDis = varPower.etaCharge

        // modeling the operating point (power),
        // valid between previous and new state
        val p = MPFloatVar(
          symbol = s"p_${varPower.stepStartTick}",
          lowerBound = varPower.pMin.toKilowatts,
          upperBound = varPower.pMax.toKilowatts,
        )

        // we use an adapted charging efficiency
        val etaCommon = calculateCommonEta(etaCh, etaDis)
        val conversionFactor = calculateConversionFactor(etaCh, etaCommon)

        // modeling the new state (stored energy)
        val newState: MPSymbol =
          if varPower.eMin == varPower.eMax then
            Const(varPower.eMax.toKilowattHours * conversionFactor)
          else
            MPFloatVar(
              symbol = s"e_${varPower.stepEndTick}",
              lowerBound = varPower.eMin.toKilowattHours * conversionFactor,
              upperBound = varPower.eMax.toKilowattHours * conversionFactor,
            )

        if varPower.isInefficient then {
          // there are charging/discharging losses, thus use the full model

          // approximation of the absolute value of p,
          // kept as close as possible to the actual absolute value
          // by using a soft constraint
          val pAbsMax = varPower.pMax.max(-varPower.pMin)

          val pAbs = MPFloatVar(
            symbol = s"pAbs_${varPower.stepStartTick}",
            lowerBound = 0,
            upperBound = pAbsMax.toKilowatts,
          )

          model.add(pAbs >:= p)
          model.add(pAbs >:= -p)

          val adaptedPreviousEnergy: MPSymbol =
            varPower.previousStateEnergy match {
              // constants are taken from energy boundary values
              // and need to be converted to the adapted model
              case constState: Const =>
                Const(constState.value * conversionFactor)
              case other => other
            }

          model.add(
            newState := adaptedPreviousEnergy +
              (p - pAbs * Const(1 - etaCommon.toEach)) *
              Const(varPower.sampleTime.toHours)
          )
          InefficientCommonLossAssetStepSymbols(
            varPower,
            p,
            pAbs,
            adaptedPreviousEnergy,
            newState,
            etaCommon,
            conversionFactor,
          )
        } else {
          // there are no charging/discharging losses, we can keep it simple

          model.add(
            newState := varPower.previousStateEnergy + p * Const(
              varPower.sampleTime.toHours
            )
          )

          EfficientCommonLossAssetStepSymbols(
            varPower,
            p,
            newState,
          )
        }

    }

}

object CommonLossObjectiveFactory {

  /** Creates an objective that simply minimizes the absolute value of the sum
    * of power by using an epigraph constraint.
    */
  object MinAbsPowerObjectiveFactory
      extends CommonLossObjectiveFactory
      with MinAbsPowerObjective

  /** Creates an objective that uses a piecewise-linear (over-)approximation of
    * the quadratic function on the sum of power. Effectively, higher power
    * values are punished more than lower ones.
    *
    * The quadratic function is approximated by using secant lines (called
    * segments here) and establishing the epigraph of these segments.
    *
    * @param segmentCount
    *   The number of segments (secant lines) to create. Increasing the number
    *   of segments improves the accuracy of the approximation, but might impact
    *   efficiency.
    */
  class LinearizedQuadraticPowerObjectiveFactory(
      segmentCount: Int
  ) extends CommonLossObjectiveFactory
      with PowerVariableObjectiveFactory {

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

      val lowerLimit = flexOptions
        .flatMap { case (_, boundaries) =>
          boundaries.energyBoundaries.map(_.etaCharge)
        }
        .minOption
        .map { lowestEta =>
          // we have to be at least a bit above the penalties of all soft constraints
          1d - lowestEta.toEach + penaltyEpsilon * 2
        }
        .getOrElse(0d)

      val (minTotalPower, maxTotalPower) = flexOptions
        .flatMap { case (_, fo) =>
          fo.energyBoundaries
        }
        .map { boundaries =>
          (boundaries.powerLimits.getLower, boundaries.powerLimits.getUpper)
        }
        .reduceOption { case ((lower1, upper1), (lower2, upper2)) =>
          (lower1 + lower2, upper1 + upper2)
        }
        .getOrElse((zeroKW, zeroKW))

      val absTotalPowerKW = {
        val absPower = maxTotalPower.max(-minTotalPower).toKilowatts
        if absPower == 0.0 then {
          // if there is zero maximum absolute power, the only solution is zero power
          // for all assets. We thus just assume a placeholder value here so that
          // numerics do not break
          1.0
        } else absPower
      }
      val segmentSize = absTotalPowerKW / segmentCount
      val adaptFactor = (1d - lowerLimit) / absTotalPowerKW

      sortSymbolsByTick(assetSymbols)
        // create objective expression for every time step
        .map { case (stepStartTick, tickAssetSymbols) =>
          val differenceAbs =
            createAbsDifference(tickAssetSymbols, target, stepStartTick)

          val epigraph = MPFloatVar.positive(s"epigraph_$stepStartTick")

          Range
            .inclusive(0, segmentCount)
            .map(_ * segmentSize)
            .sliding(2)
            .foreach { case Seq(uCurrent, uNext) =>
              val m = adaptFactor * (uCurrent + uNext)
              val b =
                -adaptFactor * uCurrent * uNext + lowerLimit * absTotalPowerKW

              model.add(epigraph >:= Const(m) * differenceAbs + Const(b))
            }

          val softConstraint = tickAssetSymbols
            .flatMap(_.objectiveAddition)
            .reduceOption[Expression](_ + _)
            .getOrElse(Zero)

          epigraph + softConstraint
        }
        // combine expressions of all time steps
        .reduceOption[Expression](_ + _)
        .getOrElse(Zero)
    }

    // todo
    override def getComparableObjectiveValue(
        flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
        assetSymbols: Iterable[
          AssetSymbolContainer[PowerVarAssetStepSymbols]
        ],
        target: Power,
        receivedData: Iterable[Data.SecondaryData],
    ): Double = ???

  }

  object PriceObjectiveFactory
      extends CommonLossObjectiveFactory
      with PriceObjective {

    override def transformPrices(
        priceSeries: SortedMap[Long, ProsumerPrice]
    ): SortedMap[Long, ProsumerPrice] =
      priceSeries
        .maxByOption { case (_, priceData) =>
          priceData.priceBuy
        }
        .map { case (_, priceData) =>
          priceData.priceBuy.toEuroPerKilowattHour
        }
        .map { maxPrice =>
          priceSeries.map { case (tick, priceData) =>
            tick -> ProsumerPrice(
              priceData.priceSell / maxPrice,
              priceData.priceBuy / maxPrice,
            )
          }
        }
        .getOrElse(priceSeries)

  }

  /** Small number to add to the constraint penalty, in order for the penalty to
    * be slightly larger than the absolute value.
    */
  private val penaltyEpsilon: Double = 1e-6

  /** Trait for container that provides symbols for a specific asset and
    * optimization time step, to be used by [[CommonLossObjectiveFactory]].
    */
  private trait CommonLossAssetStepSymbols extends PowerVarAssetStepSymbols

  /** Container that provides symbols for a specific asset and for an
    * optimization time step in which power is fixed, to be used by
    * [[CommonLossObjectiveFactory]]. Soft constraints (objective addition) are
    * not used.
    *
    * @param parameters
    *   Parameters for the asset at the specific time step.
    */
  private final case class FixedCommonLossAssetStepSymbols(
      override val parameters: FixedPowerStepParameters
  ) extends CommonLossAssetStepSymbols
      with FixedPowerVarAssetStepSymbols

  /** Container that provides symbols for a specific asset and for an
    * optimization time step in which power is variable, to be used by
    * [[CommonLossObjectiveFactory]]. A soft constraint via objective addition
    * can potentially be used.
    *
    * @param parameters
    *   Parameters for the asset at the specific time step.
    * @param power
    *   The operation variable, describing the power in kW to get from the
    *   energy state at the start to the state at the end of the interval.
    * @param stepEndState
    *   The state variable, describing the state of energy in kWh at the end of
    *   the time step interval.
    */
  private final case class EfficientCommonLossAssetStepSymbols(
      override val parameters: VariablePowerStepParameters,
      power: MPVar,
      stepEndState: MPSymbol,
  ) extends CommonLossAssetStepSymbols
      with VariableAssetStepSymbols {

    override lazy val objectiveAddition: Option[Expression] = None

    override def getOperationPowerSymbol: Expression = power

    override def getStepEndStateSymbol: MPSymbol = stepEndState

    override def getOperatingPowerResult: Power = Kilowatts(power.getValue)

    override def getStepEndEnergyResult: Energy =
      KilowattHours(stepEndState.getValue)

  }

  /** Container that provides symbols for a specific asset and for an
    * optimization time step in which power is variable, to be used by
    * [[CommonLossObjectiveFactory]]. A soft constraint via objective addition
    * can potentially be used.
    *
    * @param parameters
    *   Parameters for the asset at the specific time step.
    * @param power
    *   The operation variable, describing the power in kW to get from the
    *   energy state at the start to the state at the end of the interval.
    * @param powerAbs
    *   The approximated absolute value of the [[power]] variable, in kW.
    * @param stepStartState
    * @param stepEndState
    *   The state variable, describing the state of energy in kWh at the end of
    *   the time step interval.
    * @param etaCommon
    *   The common efficiency eta for charging and discharging.
    * @param energyConversionFactor
    *   Since the model adapts energy values, this is the conversion factor that
    *   allows deriving the actual energy values.
    */
  private final case class InefficientCommonLossAssetStepSymbols(
      override val parameters: VariablePowerStepParameters,
      power: MPVar,
      powerAbs: MPVar,
      stepStartState: MPSymbol,
      stepEndState: MPSymbol,
      etaCommon: Dimensionless,
      energyConversionFactor: Double = 1d,
  ) extends CommonLossAssetStepSymbols
      with VariableAssetStepSymbols
      with RelativeStateErrorHelper {

    override lazy val objectiveAddition: Option[Expression] = Some(
      powerAbs * Const(1 - etaCommon.toEach + penaltyEpsilon)
    )

    override def getOperationPowerSymbol: Expression = power

    override def getStepEndStateSymbol: MPSymbol = stepEndState

    override def getOperatingPowerResult: Power = Kilowatts(power.getValue)

    override def getStepStartEnergyResult: Energy =
      KilowattHours(stepStartState.getValue / energyConversionFactor)

    override def getStepEndEnergyResult: Energy =
      KilowattHours(stepEndState.getValue / energyConversionFactor)

  }

  /** Calculates the common efficiency that is used for the loss that occurs
    * both with charging and discharging.
    *
    * @param etaCharge
    *   The charging efficiency.
    * @param etaDischarge
    *   The discharging efficiency.
    * @return
    *   The common efficiency used for charging and discharging.
    */
  def calculateCommonEta(
      etaCharge: Dimensionless,
      etaDischarge: Dimensionless,
  ): Dimensionless = {
    val etaCh = etaCharge.toEach
    val etaDis = etaDischarge.toEach

    Each((2 * etaCh * etaDis) / (1 + etaCh * etaDis))
  }

  /** Calculates the conversion factor to derive adapted energy values (working
    * with the common loss model) from the physical energy values. The adapted
    * energy values can be calculated by multiplying physical energy values and
    * the factor.
    *
    * @param etaCharge
    *   The charging efficiency.
    * @param etaCommon
    *   The common charging efficiency (e.g. calculated by
    *   [[calculateCommonEta]].
    * @return
    *   The conversion factor.
    */
  def calculateConversionFactor(
      etaCharge: Dimensionless,
      etaCommon: Dimensionless,
  ): Double =
    etaCommon.toEach / etaCharge.toEach

}
