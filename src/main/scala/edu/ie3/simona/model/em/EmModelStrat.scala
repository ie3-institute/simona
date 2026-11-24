/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.config.RuntimeConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.model.em.opt.{
  ComparativeOptimizingFlexStrat,
  OptimizingFlexStrat,
}
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory.AssetStepSymbols
import edu.ie3.simona.model.em.opt.impl.{
  CommonLossObjectiveFactory,
  ObjectiveFactory,
  SignedEnergyVariableObjectiveFactory,
  SplitPowerVarsObjectiveFactory,
}
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexOptions,
  PowerLimitFlexOptions,
}
import edu.ie3.simona.service.Data.SecondaryData
import edu.ie3.simona.service.ServiceRegistrationData
import edu.ie3.simona.util.OptionExtension.orFail
import org.apache.pekko.actor.typed.ActorRef
import squants.{Power, Time}
import squants.energy.Kilowatts

import java.util.UUID

/** Trait that can be enhanced by multiple strategies to disaggregate
  * flexibility control, i.e. given a target power, determining flex control for
  * connected agents
  */
trait EmModelStrat[FO <: FlexOptions] {

  // FIXME very hacky
  var resultListener: ActorRef[ResultEvent] = null

  /** Returns the service registration data specifying the data provision
    * required for running for this model strategy. To be overridden by
    * implementing class. Per default returns data that specifies no services.
    *
    * @return
    *   The data specifying a service registration.
    */
  def getServiceRegistrationData: ServiceRegistrationData =
    ServiceRegistrationData.noServices

  /** Determine the target power (set points) of connected agents that provided
    * flex options before. Connected agents that have no result assigned in
    * return data are
    *
    * @param flexOptions
    *   The flex options per connected agent.
    * @param target
    *   The target power to aim for when utilizing flexibility.
    * @param currentTick
    *   The current tick.
    * @param receivedData
    *   The secondary data received by the EM agent.
    * @return
    *   Power set points for connected agents, if applicable.
    */
  def determineFlexControl(
      flexOptions: Iterable[
        (? <: AssetInput, FO)
      ],
      target: Power,
      currentTick: Long,
      receivedData: Seq[SecondaryData],
  ): Iterable[(UUID, Power)]

  /** Depending on the model strategy used, not all flex options provided by
    * connected agents might be usable by the parent
    * [[edu.ie3.simona.agent.em.EmAgent]]. This method adapts the given flex
    * options based on the given [[AssetInput]].
    *
    * @param assetInput
    *   The [[AssetInput]] of the connected agent providing the flex options
    * @param flexOptions
    *   The flex options
    * @return
    *   adapted flex options
    */
  def adaptFlexOptions(
      assetInput: AssetInput,
      flexOptions: FO,
  ): FO
}

object EmModelStrat {
  val tolerance: Power = Kilowatts(1e-6d)

  def parsePowerLimitModel(
      modelConfig: EmRuntimeConfig
  ): PartialFunction[String, EmModelStrat[PowerLimitFlexOptions]] = {
    case "PROPORTIONAL" => ProportionalFlexStrat
    case "PRIORITIZED" =>
      PrioritizedFlexStrat(modelConfig.curtailRegenerative)
  }

  def parseOptimizingModel(
      sampleTime: Time,
      predictionHorizon: Time,
  ): PartialFunction[String, EmModelStrat[EnergyBoundariesFlexOptions]] =
    getSingleModelSafe.unlift
      .andThen(objFactory =>
        new OptimizingFlexStrat(
          sampleTime = sampleTime,
          predictionHorizon = predictionHorizon,
          objFactory,
        )
      )
      .orElse(
        ComparativeOptimizingFlexStrat.apply(
          sampleTime = sampleTime,
          predictionHorizon = predictionHorizon,
        )
      )

  def getSingleModel(stratId: String): ObjectiveFactory[? <: AssetStepSymbols] =
    getSingleModelSafe(stratId).orFail(s"Model strategy $stratId not available")

  private def getSingleModelSafe(
      stratId: String
  ): Option[ObjectiveFactory[? <: AssetStepSymbols]] = {
    val splitId = stratId.split("_")

    if splitId.size < 3 || splitId(0) != "OPT" then None
    else {
      splitId(1) match {
        case "CLM" =>
          Option
            .when(splitId.size == 4) {
              CommonLossObjectiveFactory(splitId(2), splitId(3))
            }
            .flatten

        case "SPM" =>
          Option
            .when(splitId.size == 4) {
              SplitPowerVarsObjectiveFactory(splitId(2), splitId(3))
            }
            .flatten

        case "SEM" =>
          SignedEnergyVariableObjectiveFactory(splitId(2))
      }
    }
  }

}
