/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.event.ResultEvent.EmOptMetricsResult
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.EmModelStrat
import edu.ie3.simona.model.em.opt.FlexibilityOptimization.{
  OptimizationParams,
  TimeParams,
}
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory.AssetStepSymbols
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.service.Data.SecondaryData
import edu.ie3.simona.service.{DataTimeType, ServiceRegistrationData}
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import org.slf4j.{Logger, LoggerFactory}
import squants.{Power, Time}

import java.util.UUID
import scala.collection.immutable.SortedMap
import scala.util.Random

final case class ComparativeOptimizingFlexStrat(
    sampleTime: Time,
    predictionHorizon: Time,
    objectiveFactories: Seq[(String, ObjectiveFactory[? <: AssetStepSymbols])],
    referenceModelId: String,
) extends EmModelStrat[EnergyBoundariesFlexOptions] {

  private val logger: Logger = LoggerFactory.getLogger(
    s"${classOf[ComparativeOptimizingFlexStrat].getSimpleName}"
  )

  override def getServiceRegistrationData: ServiceRegistrationData = {
    ServiceRegistrationData(
      objectiveFactories.flatMap { case (_, of) =>
        of.getRequiredSecondaryServices
      }.distinct,
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

    val results = Random
      .shuffle(objectiveFactories)
      .map { case (id, objectiveFactory) =>
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

        val result = FlexibilityOptimization.optimize(optimizationParams)

        resultListener ! EmOptMetricsResult(id, result)

        id -> result
      }
      .toMap

    val (_, referenceResult) = results
      .find { case (id, _) =>
        referenceModelId == id
      }
      .getOrElse(
        throw new CriticalFailureException("Reference model not found.")
      )

    if !referenceResult.solutionFound then
      throw new CriticalFailureException(
        s"Optimization ended with unexpected status ${referenceResult.solutionStatus}, (optimal) solution was not found."
      )

    results.to(SortedMap).foreach { case (id, result) =>
      if result.solutionStatus != SolutionStatus.OPTIMAL then
        logger.warn(s"$id: Optimization ended with ${result.solutionStatus}")
    }

    FlexibilityOptimization.extractSetPoints(flexOptionsById, referenceResult)
  }

  private def aape(value: Double, reference: Double): Double =
    math.atan(math.abs((value - reference) / reference))

  override def adaptFlexOptions(
      assetInput: AssetInput,
      flexOptions: EnergyBoundariesFlexOptions,
  ): EnergyBoundariesFlexOptions = flexOptions

}

object ComparativeOptimizingFlexStrat {

  def apply(
      sampleTime: Time,
      predictionHorizon: Time,
  ): PartialFunction[String, EmModelStrat[EnergyBoundariesFlexOptions]] = {
    case "COMP_PS" =>
      ComparativeOptimizingFlexStrat(
        sampleTime = sampleTime,
        predictionHorizon = predictionHorizon,
        objectiveFactories = Seq(
          "OPT_CLM_NC_PS",
          "OPT_CLM_HC_PS",
          "OPT_CLM_SC_PS",
          "OPT_SPM_NC_PS",
          "OPT_SPM_HC_PS",
          "OPT_SPM_BC_PS",
        ).map(stratId => stratId -> EmModelStrat.getSingleModel(stratId)),
        referenceModelId = "OPT_SPM_BC_PS",
      )
    case "COMP_QUAD" =>
      ComparativeOptimizingFlexStrat(
        sampleTime = sampleTime,
        predictionHorizon = predictionHorizon,
        objectiveFactories = Seq(
          "OPT_CLM_SC_QUAD",
          "OPT_CLM_SC_LINQUAD",
        ).map(stratId => stratId -> EmModelStrat.getSingleModel(stratId)),
        referenceModelId = "OPT_CLM_SC_QUAD",
      )
    case "COMP_MINABS" =>
      ComparativeOptimizingFlexStrat(
        sampleTime = sampleTime,
        predictionHorizon = predictionHorizon,
        objectiveFactories = Seq(
          "OPT_CLM_NC_MINABS",
          "OPT_CLM_SC_MINABS",
          "OPT_SPM_HC_MINABS",
          "OPT_SPM_BC_MINABS",
        ).map(stratId => stratId -> EmModelStrat.getSingleModel(stratId)),
        referenceModelId = "OPT_SPM_BC_MINABS",
      )
    case "COMP_PRICE" =>
      ComparativeOptimizingFlexStrat(
        sampleTime = sampleTime,
        predictionHorizon = predictionHorizon,
        objectiveFactories = Seq(
          "OPT_CLM_NC_PR",
          "OPT_CLM_SC_PR",
          "OPT_SEM_PR",
          "OPT_SPM_HC_PR",
          "OPT_SPM_BC_PR",
        ).map(stratId => stratId -> EmModelStrat.getSingleModel(stratId)),
        referenceModelId = "OPT_SPM_BC_PR",
      )
  }

}
