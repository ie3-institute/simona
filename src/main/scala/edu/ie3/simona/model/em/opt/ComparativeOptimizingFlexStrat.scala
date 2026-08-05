/*
 * © 2026. TU Dortmund University,
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
}
import edu.ie3.simona.model.em.opt.impl.CommonLossObjectiveFactory.CommonLossVariant
import edu.ie3.simona.model.em.opt.impl.{
  CommonLossObjectiveFactory,
  ObjectiveFactory,
  SignedEnergyVariableObjectiveFactory,
  SplitPowerVarsObjectiveFactory,
}
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory.AssetStepSymbols
import edu.ie3.simona.model.em.opt.impl.SplitPowerVarsObjectiveFactory.SplitPowerVarsAdditionalConstraints.*
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.service.Data.SecondaryData
import edu.ie3.simona.service.{DataTimeType, ServiceRegistrationData}
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import org.slf4j.{Logger, LoggerFactory}
import squants.energy.EnergyConversions.EnergyNumeric
import squants.{Power, Time}

import scala.util.Random
import java.util.UUID
import scala.collection.immutable.SortedMap

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

    val referenceObjValue = referenceResult.objectiveValue.getOrElse(
      throw new CriticalFailureException(
        s"$referenceModelId: Reference optimization failed: $referenceResult"
      )
    )

    results.to(SortedMap).foreach { case (id, result) =>
      if result.solutionStatus != SolutionStatus.OPTIMAL then
        logger.warn(s"$id: Optimization ended with ${result.solutionStatus}")
      else {
        // there should always be an objective value here, since solution status is optimal
        val objValue = result.objectiveValue.getOrElse(
          throw new CriticalFailureException(
            s"$id: Objective value not provided."
          )
        )
        val objValueRelError = math.abs(objValue - referenceObjValue)

        val stateCalcError = result.assetSymbols
          .flatMap(_.getStateCalcErrors)
          .sum

        val totalTime =
          (result.timeMeasurements.variables + result.timeMeasurements.objectiveFunction + result.timeMeasurements.constraints + result.timeMeasurements.solution).toDouble / 1000000d

        if id.equals("OPT_CLM_SC_PS") && objValueRelError > 0.5 then
          logger.warn("!")

        // if objValueRelError > 1e-9 || stateCalcError > WattHours(1e-6) then
        logger.warn(
          s"$id: Objective error $objValueRelError compared to $referenceModelId, state calc error $stateCalcError, total time: $totalTime ms"
        )
      }
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

  def createPeakShavingComp(
      sampleTime: Time,
      predictionHorizon: Time,
  ): ComparativeOptimizingFlexStrat = {
    ComparativeOptimizingFlexStrat(
      sampleTime = sampleTime,
      predictionHorizon = predictionHorizon,
      objectiveFactories = Seq(
        "OPT_CLM_SC_PS" -> CommonLossObjectiveFactory
          .PeakShavingObjectiveFactory(variant =
            CommonLossVariant.SoftConstraints
          ),
        "OPT_CLM_NSC_PS" -> CommonLossObjectiveFactory
          .PeakShavingObjectiveFactory(variant =
            CommonLossVariant.NoSoftConstraints
          ),
        "OPT_SPM_REL_PS" -> SplitPowerVarsObjectiveFactory
          .PeakShavingObjectiveFactory(RelaxedConstraints),
        "OPT_SPM_BIN_PS" -> SplitPowerVarsObjectiveFactory
          .PeakShavingObjectiveFactory(BinaryConstraint),
      ),
      referenceModelId = "OPT_SPM_BIN_MINABS"
    )
  }

  def createQuadraticComp(
                             sampleTime: Time,
                             predictionHorizon: Time,
                           ): ComparativeOptimizingFlexStrat = {
    ComparativeOptimizingFlexStrat(
      sampleTime = sampleTime,
      predictionHorizon = predictionHorizon,
      objectiveFactories = Seq(
        "OPT_CLM_SC_QUAD" -> CommonLossObjectiveFactory
          .QuadraticPowerObjectiveFactory(variant =
            CommonLossVariant.SoftConstraints
          ),
        "OPT_CLM_SC_LINQUAD" -> CommonLossObjectiveFactory
          .LinearizedQuadraticPowerObjectiveFactory(variant =
            CommonLossVariant.SoftConstraints,
            segmentCount = 10
          ),
      ),
      referenceModelId = "OPT_CLM_SC_QUAD"
    )
  }

  def createMinAbsComp(
      sampleTime: Time,
      predictionHorizon: Time,
  ): ComparativeOptimizingFlexStrat = {
    ComparativeOptimizingFlexStrat(
      sampleTime = sampleTime,
      predictionHorizon = predictionHorizon,
      objectiveFactories = Seq(
        "OPT_CLM_SC_MINABS" -> CommonLossObjectiveFactory
          .MinAbsPowerObjectiveFactory(variant =
            CommonLossVariant.SoftConstraints
          ),
        "OPT_CLM_NSC_MINABS" -> CommonLossObjectiveFactory
          .MinAbsPowerObjectiveFactory(variant =
            CommonLossVariant.NoSoftConstraints
          ),
        "OPT_SPM_REL_MINABS" -> SplitPowerVarsObjectiveFactory
          .MinAbsPowerObjectiveFactory(RelaxedConstraints),
        "OPT_SPM_BIN_MINABS" -> SplitPowerVarsObjectiveFactory
          .MinAbsPowerObjectiveFactory(BinaryConstraint),
      ),
      referenceModelId = "OPT_SPM_BIN_PS"
    )
  }

  def createPriceObjComp(
      sampleTime: Time,
      predictionHorizon: Time,
  ): ComparativeOptimizingFlexStrat = {
    ComparativeOptimizingFlexStrat(
      sampleTime = sampleTime,
      predictionHorizon = predictionHorizon,
      objectiveFactories = Seq(
        "OPT_CLM_SC_PRICE" -> CommonLossObjectiveFactory
          .PriceObjectiveFactory(variant = CommonLossVariant.SoftConstraints),
        "OPT_CLM_NSC_PRICE" -> CommonLossObjectiveFactory
          .PriceObjectiveFactory(variant = CommonLossVariant.NoSoftConstraints),
        "OPT_SEC_PRICE" -> SignedEnergyVariableObjectiveFactory.PriceObjectiveFactory,
        "OPT_SPM_REL_PRICE" -> SplitPowerVarsObjectiveFactory
          .PriceObjectiveFactory(RelaxedConstraints),
        "OPT_SPM_BIN_PRICE" -> SplitPowerVarsObjectiveFactory
          .PriceObjectiveFactory(BinaryConstraint),
      ),
      referenceModelId = "OPT_SPM_BIN_PRICE"
    )
  }

}
