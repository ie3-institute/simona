/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import squants.Power

import java.util.UUID

/** Translating input data to a format that can be used by aggregation
  * strategies, em strats etc. Furthermore, sanity checks on calculated data is
  * performed.
  */
final case class EmModelShell(
    uuid: UUID,
    id: String,
    modelStrategy: EmModelStrat,
    aggregateFlex: EmAggregateFlex,
    modelToParticipantInput: Map[UUID, AssetInput] = Map.empty,
) {

  def addParticipant(modelUuid: UUID, inputModel: AssetInput): EmModelShell =
    copy(
      modelToParticipantInput =
        modelToParticipantInput.updated(modelUuid, inputModel)
    )

  def aggregateFlexOptions(
      allFlexOptions: Iterable[
        (UUID, ProvideFlexOptions)
      ]
  ): (Power, Power, Power) = {
    val updatedAllFlexOptions = allFlexOptions.map {
      case (modelUuid, flexOptions) =>
        val assetInput = modelToParticipantInput.getOrElse(
          modelUuid,
          throw new CriticalFailureException(
            s"Asset input for model with UUID $modelUuid was not found."
          ),
        )

        val minMaxFlexOptions = flexOptions match {
          case flex: ProvideMinMaxFlexOptions => flex
          case unsupported =>
            throw new CriticalFailureException(
              s"Received unsupported flex options $unsupported."
            )
        }

        val updatedFlexOptions =
          modelStrategy.adaptFlexOptions(assetInput, minMaxFlexOptions)

        assetInput -> updatedFlexOptions
    }

    aggregateFlex.aggregateFlexOptions(updatedAllFlexOptions)
  }

  def determineFlexControl(
      allFlexOptions: Iterable[(UUID, ProvideFlexOptions)],
      target: Power,
  ): Iterable[(UUID, Power)] = {

    val minMaxFlexOptions = allFlexOptions.toMap.view.mapValues {
      case flex: ProvideMinMaxFlexOptions => flex
      case unsupported =>
        throw new CriticalFailureException(
          s"Received unsupported flex options $unsupported."
        )
    }.toMap

    val uuidToFlexOptions = minMaxFlexOptions.map {
      case (modelUuid, flexOptions) =>
        val assetInput = modelToParticipantInput.getOrElse(
          modelUuid,
          throw new CriticalFailureException(
            s"Asset input for model with UUID $modelUuid was not found."
          ),
        )
        assetInput -> flexOptions
    }

    val setPoints =
      modelStrategy.determineFlexControl(uuidToFlexOptions, target)

    setPoints.map { case (model, power) =>
      val flexOptions =
        minMaxFlexOptions.getOrElse(model, throw new RuntimeException())
      if (!flexOptions.fits(power))
        throw new CriticalFailureException(
          s"Calculated set point $power does not fit flex option"
        )

      model -> power
    }

    // TODO sanity checks after strat calculation
    // checkSetPower(flexOptions, power)

  }

}

object EmModelShell {
  def apply(
      uuid: UUID,
      id: String,
      modelStrat: String,
      modelConfig: EmRuntimeConfig,
  ): EmModelShell = {

    val modelStrategy = modelStrat match {
      case "PROPORTIONAL" => ProportionalFlexStrat
      case "PRIORITIZED"  => PrioritizedFlexStrat(modelConfig.pvFlex)
      case unknown =>
        throw new CriticalFailureException(s"Unknown model strategy $unknown")
    }

    val aggregateFlex = modelConfig.aggregateFlex match {
      case "SELF_OPT_EXCL_PV" => EmAggregateSelfOpt(false)
      case "SELF_OPT"         => EmAggregateSelfOpt(true)
      case "SIMPLE_SUM"       => EmAggregateSimpleSum
      case unknown =>
        throw new CriticalFailureException(
          s"Unknown aggregate flex strategy $unknown"
        )
    }

    EmModelShell(uuid, id, modelStrategy, aggregateFlex)
  }
}
