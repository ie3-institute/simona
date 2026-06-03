/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.opt.CommonLossObjectiveFactory.{
  SplitLossAssetStepVars,
  calculateCommonEta,
  calculateConversionFactor,
}
import edu.ie3.simona.model.em.opt.OptimizedFlexStrat.*
import edu.ie3.simona.model.em.opt.SoftConstraint.AbsValueSoftConstraint
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import edu.ie3.util.scala.quantities.EnergyPrice
import optimus.algebra.{Const, Expression, Zero}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPFloatVar, MPVar}
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Dimensionless, Each, Energy, Power, Time}

import java.util.UUID

/** Produces asset variables and an optimization objective that uses a single
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
    extends ObjectiveFactory[SplitLossAssetStepVars] {

  override def createAssetVars(
      assetParams: AssetStepParameters,
      stepStartTick: Long,
      stepEndTick: Long,
      sampleTime: Time,
  )(using model: MPModel): SplitLossAssetStepVars =
    assetParams match {
      case fixedPower: FixedPowerStepParameters =>
        val fixedPowerVar = Const(
          (fixedPower.energyChange / sampleTime).toKilowatts
        )
        SplitLossAssetStepVars(
          fixedPowerVar,
          None,
          stepStartTick,
          stepEndTick,
        )
      case varPower: VariablePowerStepParameters =>
        val etaCh = varPower.etaCharge
        val etaDis = varPower.etaCharge

        // modeling the operating point (power),
        // valid between previous and new state
        val p = MPFloatVar(
          symbol = s"p_$stepStartTick",
          lowerBound = varPower.pMin.toKilowatts,
          upperBound = varPower.pMax.toKilowatts,
        )

        // we use an adapted charging efficiency
        val etaCommon = calculateCommonEta(etaCh, etaDis)
        val conversionFactor = calculateConversionFactor(etaCh, etaCommon)

        // modeling the new state (stored energy)
        val newState: MPVar | Const =
          if varPower.eMin == varPower.eMax then
            Const(varPower.eMax.toKilowattHours * conversionFactor)
          else
            MPFloatVar(
              symbol = s"e_$stepEndTick",
              lowerBound = varPower.eMin.toKilowattHours * conversionFactor,
              upperBound = varPower.eMax.toKilowattHours * conversionFactor,
            )

        val softConstraint =
          if varPower.isInefficient then {
            // there are charging/discharging losses, thus use the full model

            // approximation of the absolute value of p,
            // kept as close as possible to the actual absolute value
            // by using a soft constraint
            val pAbsMax = varPower.pMax.max(-varPower.pMin)

            val pAbs = MPFloatVar(
              symbol = s"pAbs_$stepStartTick",
              lowerBound = 0,
              upperBound = pAbsMax.toKilowatts,
            )

            model.add(pAbs >:= p)
            model.add(pAbs >:= -p)

            model.add(
              newState := varPower.previousStateEnergy +
                (p - pAbs * Const(1 - etaCommon.toEach)) *
                Const(sampleTime.toHours)
            )

            Some(AbsValueSoftConstraint(p, pAbs, etaCommon))
          } else {
            // there are no charging/discharging losses, we can keep it simple

            model.add(
              newState := varPower.previousStateEnergy + p * Const(
                sampleTime.toHours
              )
            )
            None
          }

        SplitLossAssetStepVars(
          p,
          Some(newState),
          stepStartTick,
          stepEndTick,
          softConstraint,
          conversionFactor,
        )
    }

  /** Creates an absolute variable of the difference between power sum and
    * target power.
    *
    * @param assetVars
    *   The asset vars to optimize for.
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
      assetVars: Iterable[SplitLossAssetStepVars],
      target: Power,
      stepStartTick: Long,
  )(using model: MPModel): Expression = {
    val difference =
      createPowerSum(assetVars) - Const(target.toKilowatts)

    createAbsoluteVariable(difference, s"differenceAbs_$stepStartTick")
  }

  protected def createPowerSum(
      assetVars: Iterable[SplitLossAssetStepVars]
  ): Expression =
    assetVars
      .map(_.operationVar)
      .reduceOption[Expression](_ + _)
      .getOrElse(Zero)

}

object CommonLossObjectiveFactory {

  /** Creates an objective that simply minimizes the absolute value of the sum
    * of power by using an epigraph constraint.
    */
  object MinAbsPowerObjectiveFactory extends CommonLossObjectiveFactory {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable.empty

    override def build(
        flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
        assetVars: Iterable[AssetVarContainer[SplitLossAssetStepVars]],
        target: Power,
        receivedData: Seq[Data.SecondaryData],
    )(using model: MPModel): Expression = {
      sortVarsByTick(assetVars)
        // create objective expression for every time step
        .map { case (stepStartTick, tickAssetVars) =>
          createAbsDifference(tickAssetVars, target, stepStartTick)
        }
        // combine expressions of all time steps
        .reduceOption[Expression](_ + _)
        .getOrElse(Zero)
    }

  }

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
  ) extends CommonLossObjectiveFactory {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable.empty

    override def build(
        flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
        assetVars: Iterable[AssetVarContainer[SplitLossAssetStepVars]],
        target: Power,
        receivedData: Seq[Data.SecondaryData],
    )(using model: MPModel): Expression = {

      val lowerLimit = flexOptions
        .flatMap { case (_, boundaries) =>
          boundaries.energyBoundaries.map(_.etaCharge)
        }
        .minOption
        .map { lowestEta =>
          // we have to be at least a bit above the penalties of all soft constraints
          1d - lowestEta.toEach + SoftConstraint.epsilon * 2
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

      sortVarsByTick(assetVars)
        // create objective expression for every time step
        .map { case (stepStartTick, tickAssetVars) =>
          val differenceAbs =
            createAbsDifference(tickAssetVars, target, stepStartTick)

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

          epigraph
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
  object PriceObjectiveFactory extends CommonLossObjectiveFactory {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable(ServiceType.PriceService)

    override def build(
        flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
        assetVars: Iterable[AssetVarContainer[SplitLossAssetStepVars]],
        target: Power,
        receivedData: Seq[Data.SecondaryData],
    )(using model: MPModel): Expression = {

      val priceSeries = extractPriceSeries(receivedData)

      val maxPrice = priceSeries
        .maxByOption { case (_, priceData) =>
          priceData.priceBuy
        }
        .map { case (_, priceData) =>
          priceData.priceBuy.toEuroPerKilowattHour
        }
        .getOrElse(
          throw new CriticalFailureException(
            s"No prices were given with secondary data $receivedData"
          )
        )

      val transformFunc = (price: EnergyPrice) =>
        price.toEuroPerKilowattHour / maxPrice

      sortVarsByTick(assetVars)
        // create objective expression for every time step
        .map { case (stepStartTick, tickAssetVars) =>
          val totalPower = createPowerSum(tickAssetVars)

          val priceData = priceSeries
            .maxBefore(stepStartTick + 1)
            .map { case (_, priceData) => priceData }
            .getOrElse(
              throw new CriticalFailureException(
                s"No price data was given for tick $stepStartTick!"
              )
            )

          // extract prices in EUR / kWh
          val priceSell = transformFunc(priceData.priceSell)
          val priceBuy = transformFunc(priceData.priceBuy)

          if priceSell > priceBuy then
            throw new CriticalFailureException(
              s"Selling price $priceSell is higher than buying price $priceBuy. " +
                "Objective factory does not know how to handle this."
            )

          // convex, since priceSell < priceBuy
          createEpigraphVar(
            Seq(Const(priceSell) * totalPower, Const(priceBuy) * totalPower),
            s"cost_$stepStartTick",
          )
        }
        // combine expressions of all time steps
        .reduceOption[Expression](_ + _)
        .getOrElse(Zero)
    }
  }

  /** Container holding the relevant variables and potentially a soft constraint
    * for a specific asset and optimization time step.
    *
    * @param operationVar
    *   The operation variable, describing the power in kW to get from the
    *   energy state at the start to the state at the end of the interval.
    * @param stateVar
    *   The state variable, describing the amount of energy in kWh at the end of
    *   the interval. Generally, the state energy signifies the upwards and
    *   downwards change of energy, compared to the energy potential (zero kWh)
    *   at the start of the prediction horizon (the current simulation tick).
    * @param stepStartTick
    *   The tick at the start of the interval, i.e. the tick at which the
    *   operation of the step starts.
    * @param stepEndTick
    *   The tick at the end of the interval, i.e. the tick at which the
    *   operation of the step ends (and a next step might start).
    * @param softConstraint
    *   Optionally a soft constraint to be added to the objective.
    * @param energyConversionFactor
    *   Since the model adapts energy values, this is the conversion factor that
    *   allows deriving proper energy values for use within the simulation.
    */
  final case class SplitLossAssetStepVars(
      override val operationVar: MPVar | Const,
      override val stateVar: Option[MPVar | Const],
      override val stepStartTick: Long,
      override val stepEndTick: Long,
      override val softConstraint: Option[SoftConstraint] = None,
      energyConversionFactor: Double = 1d,
  ) extends AssetStepVars {

    override def getOperationResult: Power = Kilowatts(operationVar.getValue)

    override def getStateResult: Energy =
      stateVar
        .map(_.getValue / energyConversionFactor)
        .map(KilowattHours(_))
        .getOrElse(zeroKWh)
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
