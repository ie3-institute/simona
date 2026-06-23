/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.EmModelStrat
import edu.ie3.simona.model.em.opt.FlexibilityOptimization.{
  OptimizationParams,
  TimeParams,
  extractSetPoints,
  optimize,
}
import edu.ie3.simona.model.em.opt.OptimizingFlexStrat.accuracyWarningThreshold
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory.AssetStepSymbols
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.service.Data.SecondaryData
import edu.ie3.simona.service.{DataTimeType, ServiceRegistrationData}
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import org.slf4j.{Logger, LoggerFactory}
import squants.energy.EnergyConversions.EnergyNumeric
import squants.energy.WattHours
import squants.{Energy, Power, Time}

import java.util.UUID

/** Energy management strategy that optimizes flexibility usage over a fixed
  * amount of time steps into the future. Takes [[EnergyBoundariesFlexOptions]]
  * as inputs, which provide the required energy and power limits for
  * constraining asset operation.
  *
  * @param sampleTime
  *   The amount of time between the steps.
  * @param predictionHorizon
  *   The amount of time that is predicted into the future, i.e. the last step
  *   is this amount of time away from the current point in simulation time.
  *   Should be a multiple of [[sampleTime]].
  * @param objectiveFactory
  *   The factory creating asset symbols and the optimization objective to use.
  */
final case class OptimizingFlexStrat(
    sampleTime: Time,
    predictionHorizon: Time,
    objectiveFactory: ObjectiveFactory[? <: AssetStepSymbols],
) extends EmModelStrat[EnergyBoundariesFlexOptions] {

  private val logger: Logger = LoggerFactory.getLogger(
    s"${classOf[OptimizingFlexStrat].getSimpleName}(${objectiveFactory.getClass.getSimpleName})"
  )

  override def getServiceRegistrationData: ServiceRegistrationData = {
    ServiceRegistrationData(
      objectiveFactory.getRequiredSecondaryServices,
      DataTimeType.CurrentAndForecast(
        forecastLength = predictionHorizon,
        forecastResolution = sampleTime,
      ),
    )
  }

  /** The power target might not be considered by all types of objectives.
    */
  override def determineFlexControl(
      flexOptions: Iterable[(? <: AssetInput, EnergyBoundariesFlexOptions)],
      target: Power,
      currentTick: Long,
      receivedData: Seq[SecondaryData],
  ): Iterable[(UUID, Power)] = {

    val flexOptionsById =
      flexOptions.map { case (asset: AssetInput, fo) => asset.getUuid -> fo }

    val optimizationParams = OptimizationParams(
      flexOptionsById = flexOptionsById,
      receivedData = receivedData,
      target = target,
      timeParams = TimeParams(
        sampleTime = sampleTime,
        predictionHorizon = predictionHorizon,
        currentTick = currentTick,
      ),
      objectiveFactory = objectiveFactory,
      solverLib = SolverLib.oJSolver,
      tightenBoundaries = true,
    )

    val result = optimize(optimizationParams)

    if result.solutionStatus != SolutionStatus.OPTIMAL then
      throw new CriticalFailureException(
        s"Optimization ended with unexpected status ${result.solutionStatus}, ${SolutionStatus.OPTIMAL} was expected."
      )

    val errors = result.assetSymbols
      .flatMap(_.getStateCalcErrors)
      .filter(_ > accuracyWarningThreshold)

    if errors.nonEmpty then {
      val meanError = errors.sum / errors.size
      logger.warn(
        s"${errors.size} of all state of energy calculation steps were inaccurate, with a mean error of $meanError"
      )
    }

    extractSetPoints(flexOptionsById, result)
  }

  override def adaptFlexOptions(
      assetInput: AssetInput,
      flexOptions: EnergyBoundariesFlexOptions,
  ): EnergyBoundariesFlexOptions = flexOptions

}

object OptimizingFlexStrat {

  /** Threshold for the error of result accuracy checks after optimization.
    * Every error should stay below this threshold. todo
    */
  private val accuracyWarningThreshold: Energy = WattHours(10)

}
