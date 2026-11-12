/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt.impl

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.opt.FlexibilityOptimization.*
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory.AssetStepSymbols
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.service.Data.SecondaryData
import edu.ie3.simona.service.Data.SecondaryData.{
  ProsumerPrice,
  SecondarySeriesData,
}
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import optimus.algebra.{Const, Expression}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPFloatVar, MPVar}
import squants.energy.KilowattHours
import squants.energy.PowerConversions.PowerNumeric
import squants.time.Hours
import squants.{Energy, Power}

import java.util.UUID
import scala.collection.immutable.SortedMap

/** Trait to be implemented by factories of power objectives.
  *
  * @tparam AS
  *   The type of [[AssetStepSymbols]] that the objective factory produces and
  *   uses.
  */
trait ObjectiveFactory[AS <: AssetStepSymbols] {

  /** @return
    *   All secondary services required by the optimization model.
    */
  def getRequiredSecondaryServices: Iterable[ServiceType]

  /** Creates asset symbols of type [[AS]] from given [[AssetStepParameters]]
    * according to the requirements of the objective factory.
    *
    * @param assetParams
    *   The asset parameters to use for creating asset symbols.
    * @param previousState
    *   The state of energy symbol of the previous step.
    * @param model
    *   The optimization model to add symbols and constraints to.
    * @return
    *   The asset symbols for the given asset and time step.
    */
  def createAssetSymbols(
      assetParams: AssetStepParameters,
      previousState: MPSymbol,
  )(using
      model: MPModel
  ): AS

  /** Builds an objective to minimize given the asset symbols and an objective
    * factory.
    *
    * @param flexOptions
    *   The flex options that connected assets provided.
    * @param assetSymbols
    *   The asset symbols to optimize for.
    * @param target
    *   The target power for each time step.
    * @param receivedData
    *   The received secondary data.
    * @param model
    *   The optimization model to add variables and constraints to.
    * @return
    *   The full objective, excluding eventual soft constraints.
    */
  def build(
      flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      assetSymbols: SortedMap[Long, Iterable[AS]],
      target: Power,
      receivedData: Iterable[SecondaryData],
  )(using model: MPModel): Expression

  // todo scaladoc
  def getComparableObjectiveValue(
      assetSymbols: SortedMap[Long, Iterable[AS]],
      target: Power,
      receivedData: Iterable[SecondaryData],
  ): Double

  /** Extracts a price series map (if available) from the given received
    * secondary data. If no price series is available, an exception is thrown.
    *
    * @param receivedData
    *   The received data to extract prices from.
    * @return
    *   A map from tick to price data.
    */
  protected def extractPriceSeries(
      receivedData: Iterable[SecondaryData]
  ): SortedMap[Long, ProsumerPrice] =
    receivedData
      .collectFirst { case SecondarySeriesData(series) =>
        series.map {
          case (tick, priceData: ProsumerPrice) =>
            tick -> priceData
          case (_, unexpectedData) =>
            throw new CriticalFailureException(
              s"Unexpected secondary data $unexpectedData"
            )
        }
      }
      .getOrElse(
        throw new CriticalFailureException(
          s"No price series data was provided."
        )
      )

  protected def getAndCheckPriceData(
      priceSeries: SortedMap[Long, ProsumerPrice],
      tick: Long,
  ): ProsumerPrice = {
    val priceData = priceSeries
      .maxBefore(tick + 1)
      .map { case (_, priceData) => priceData }
      .getOrElse(
        throw new CriticalFailureException(
          s"No price data was given for tick $tick!"
        )
      )

    if priceData.priceSell > priceData.priceBuy then
      throw new CriticalFailureException(
        s"Selling price ${priceData.priceSell} is higher than buying price ${priceData.priceBuy}. " +
          "Objective factory does not know how to handle this."
      )

    priceData
  }

  /** Creates an unbounded continuous variable that is constrained to be greater
    * than all given expressions.
    *
    * @param segments
    *   The segments to create the epigraph variable for.
    * @param name
    *   The name of the variable.
    * @param model
    *   The optimization model to add the variable and constraints to.
    * @return
    *   The new epigraph variable.
    */
  protected def createEpigraphVar(segments: Seq[Expression], name: String)(using
      model: MPModel
  ): MPFloatVar = {
    val epigraph = MPFloatVar(name)
    segments.foreach(segment => model.add(epigraph >:= segment))

    epigraph
  }

  /** Creates a positive continuous variable that is constrained to be greater
    * than both the positive and negative version of given value.
    *
    * @param value
    *   The expression to create the absolute variable for.
    * @param name
    *   The name of the variable.
    * @param model
    *   The optimization model to add the variable and constraints to.
    * @return
    *   The new absolute variable.
    */
  protected def createAbsoluteVariable(value: Expression, name: String)(using
      model: MPModel
  ): MPFloatVar = {
    val valueAbs = MPFloatVar.positive(name)
    model.add(valueAbs >:= value)
    model.add(valueAbs >:= -value)
    valueAbs
  }

  protected def createEndStateForFixedPower(
      parameters: FixedPowerStepParameters,
      previousState: MPSymbol,
  )(using model: MPModel): MPSymbol = {
    val energyChange =
      (parameters.power * parameters.sampleTime).toKilowattHours

    previousState match {
      case const: Const =>
        Const(const.value + energyChange)
      case variable: MPVar =>
        val newState = MPFloatVar(symbol = "E")
        model.add(newState := variable + Const(energyChange))
        newState
    }
  }

}

object ObjectiveFactory {

  trait PeakShavingObjective[AS <: AssetStepSymbols]
      extends ObjectiveFactory[AS] {

    override def getComparableObjectiveValue(
        assetSymbols: SortedMap[Long, Iterable[AS]],
        target: Power,
        receivedData: Iterable[SecondaryData],
    ): Double =
      assetSymbols
        .map { case (_, tickAssetSymbols) =>
          tickAssetSymbols.map(_.getOperatingPowerResult).sum
        }
        .max
        .abs
        .toKilowatts

  }

  trait MinAbsPowerObjective[AS <: AssetStepSymbols]
      extends ObjectiveFactory[AS] {

    override def getComparableObjectiveValue(
        assetSymbols: SortedMap[Long, Iterable[AS]],
        target: Power,
        receivedData: Iterable[SecondaryData],
    ): Double =
      assetSymbols.map { case (_, tickAssetSymbols) =>
        val powerSum =
          tickAssetSymbols.map(_.getOperatingPowerResult).sum.toKilowatts

        math.abs(powerSum)
      }.sum

  }

  trait QuadraticPowerObjective[AS <: AssetStepSymbols]
      extends ObjectiveFactory[AS] {

    override def getComparableObjectiveValue(
        assetSymbols: SortedMap[Long, Iterable[AS]],
        target: Power,
        receivedData: Iterable[SecondaryData],
    ): Double =
      assetSymbols.map { case (_, tickAssetSymbols) =>
        val powerSum =
          tickAssetSymbols.map(_.getOperatingPowerResult).sum.toKilowatts

        powerSum * powerSum
      }.sum

  }

  trait PriceObjective[AS <: AssetStepSymbols] extends ObjectiveFactory[AS] {

    override def getComparableObjectiveValue(
        assetSymbols: SortedMap[Long, Iterable[AS]],
        target: Power,
        receivedData: Iterable[SecondaryData],
    ): Double = {

      val priceSeries = extractPriceSeries(receivedData)

      assetSymbols.map { (stepStartTick, tickAssetSymbols) =>
        val priceData = getAndCheckPriceData(priceSeries, stepStartTick)

        val sampleTime = tickAssetSymbols.headOption
          .map(_.parameters.sampleTime)
          .getOrElse(Hours(0))
        val energySum =
          tickAssetSymbols.map(_.getOperatingPowerResult).sum * sampleTime

        val price =
          if energySum > zeroKWh then priceData.priceBuy
          else priceData.priceSell

        price.toEuroPerKilowattHour * energySum.toKilowattHours
      }.sum

    }

  }

  /** Trait for containers providing symbols (variables or constants) used in
    * optimization for an asset at a single time step.
    */
  trait AssetStepSymbols {

    val parameters: AssetStepParameters

    /** The symbol for the asset state of energy at the end of the time step in
      * kWh.
      */
    val stepStartState: MPSymbol

    /** The symbol for the asset state of energy at the end of the time step in
      * kWh.
      */
    val stepEndState: MPSymbol

    /** Returns the resulting operating power.
      *
      * This method should only be called after optimization has successfully
      * completed. Otherwise, results might not be available and an exception
      * might be thrown.
      */
    def getOperatingPowerResult: Power

    /** Returns the resulting state of energy at the start of the step.
      *
      * This method should only be called after optimization has successfully
      * completed. Otherwise, results might not be available and an exception
      * might be thrown.
      */
    def getStepStartEnergyResult: Energy =
      KilowattHours(stepStartState.getValue)

    /** Returns the resulting state of energy at the end of the step.
      *
      * This method should only be called after optimization has successfully
      * completed. Otherwise, results might not be available and an exception
      * might be thrown.
      */
    def getStepEndEnergyResult: Energy =
      KilowattHours(stepEndState.getValue)

    /** todo */
    def getActualLoss: Energy = zeroKWh

    /** todo
      */
    def getExcessLoss: Energy = zeroKWh

  }

  trait FixedAssetStepSymbols extends AssetStepSymbols {

    override val parameters: FixedPowerStepParameters

    override def getOperatingPowerResult: Power =
      parameters.power

  }

  trait VariableAssetStepSymbols extends AssetStepSymbols {

    override val parameters: VariablePowerStepParameters

    override def getActualLoss: Energy = {
      val sampleTime = parameters.sampleTime

      val power = getOperatingPowerResult

      val lossFactor =
        if power > zeroKW then 1d - parameters.etaCharge.toEach
        else 1d / parameters.etaDischarge.toEach - 1d

      (power * sampleTime * lossFactor).abs
    }

  }

  /** todo
    */
  trait RelativeStateErrorHelper {
    this: VariableAssetStepSymbols =>

    override def getExcessLoss: Energy = {
      val sampleTime = parameters.sampleTime

      val power = getOperatingPowerResult
      val startEnergy = getStepStartEnergyResult
      val endEnergy = getStepEndEnergyResult

      val correctEndEnergy = startEnergy + power * sampleTime - getActualLoss
      (correctEndEnergy - endEnergy).abs
    }
  }

  /** Container holding all optimization variables and soft constraints (as
    * [[AssetStepSymbols]]) for one or more assets referenced by a single UUID.
    *
    * @param assetUuid
    *   The UUID of the assets.
    * @param results
    *   All [[AssetStepSymbols]] of the assets, including state and operation
    *   variables. The step variables are referenced by stepStartTick within the
    *   sorted map. The sequence can contain several variable maps, similar to
    *   how [[EnergyBoundariesFlexOptions]] can contain several asset
    *   boundaries. It can also be empty though.
    */
  final case class AssetSymbolContainer[AS <: AssetStepSymbols](
      assetUuid: UUID,
      results: Seq[SortedMap[Long, AS]],
  ) {

    def getStateCalcErrors: Seq[Energy] =
      results.flatten.map { case (_, assetSymbols) =>
        assetSymbols.getExcessLoss
      }

    /** Returns the [[AssetStepSymbols]] grouped by their by tick.
      *
      * @param ticks
      *   The whole set of ticks.
      * @return
      *   The [[AssetStepSymbols]] ordered by tick. Every value iterable is
      *   non-empty.
      */
    def resultsByTick(ticks: Seq[Long]): SortedMap[Long, Iterable[AS]] = {
      // sort all asset symbols by tick
      ticks
        .flatMap { stepStartTick =>
          val tickResults = results.flatMap(_.get(stepStartTick))
          Option.when(tickResults.nonEmpty)(stepStartTick -> tickResults)
        }
        .to(SortedMap)
    }

  }

  object AssetSymbolContainer {

    extension [AS <: AssetStepSymbols](
        assetSymbols: Iterable[AssetSymbolContainer[AS]]
    ) {

      /** Re-orders the [[AssetStepSymbols]] inside given
        * [[AssetSymbolContainer]]s to be grouped by their by tick.
        *
        * @param ticks
        *   The full set of ticks.
        * @return
        *   The [[AssetStepSymbols]] ordered by tick. Every value iterable is
        *   non-empty.
        */
      def sortByTick(ticks: Seq[Long]): SortedMap[Long, Iterable[AS]] = {
        // sort all asset symbols by tick
        ticks
          .flatMap { stepStartTick =>
            val tickResults = assetSymbols.flatMap {
              _.results.flatMap(_.get(stepStartTick))
            }
            Option.when(tickResults.nonEmpty)(stepStartTick -> tickResults)
          }
          .to(SortedMap)
      }

    }

  }

}
