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
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.Power
import squants.energy.Kilowatts

import java.util.UUID

/** Translating input data to a format that can be used by aggregation
  * strategies, em strategies etc.. Furthermore, sanity checks on calculated
  * data is performed.
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
        minMaxFlexOptions.getOrElse(
          model,
          throw new CriticalFailureException(
            s"Set point for model $model has been calculated by ${modelStrategy.getClass.getSimpleName}, which is not connected to this EM."
          ),
        )

      // sanity checks after strat calculation
      EmTools.checkSetPower(flexOptions, power)

      model -> power
    }
  }

}

object EmModelShell {
  def apply(
      uuid: UUID,
      id: String,
      modelStrategyName: String,
      modelConfig: EmRuntimeConfig,
  ): EmModelShell = {

    val modelStrategy = modelStrategyName match {
      case "PROPORTIONAL" => ProportionalFlexStrat
      case "PRIORITIZED" =>
        PrioritizedFlexStrat(modelConfig.curtailRegenerative)
      case unknown =>
        throw new CriticalFailureException(s"Unknown model strategy $unknown")
    }

    val aggregateFlex = modelConfig.aggregateFlex match {
      case "SELF_OPT_EXCL_REG" => EmAggregatePowerOpt(zeroKW, false)
      case "SELF_OPT"          => EmAggregatePowerOpt(zeroKW, true)
      case "SIMPLE_SUM"        => EmAggregateSimpleSum

      case powerTargetAbsString
          if powerTargetAbsString.startsWith("SELF_POWER_") =>
        val pattern = """SELF_POWER_([\d.]+)(_EXCL_REG)?""".r
        powerTargetAbsString match {
          case pattern(value, exclReg) =>
            try {
              val powerTargetAbs = BigDecimal(value)
              val curtailRegenerative = exclReg == null
              EmAggregatePowerOpt(
                Kilowatts(powerTargetAbs),
                curtailRegenerative,
              )
            } catch {
              case _: NumberFormatException =>
                throw new CriticalFailureException(
                  s"Invalid numeric value in aggregate flex strategy: $powerTargetAbsString"
                )
            }
          case _ =>
            throw new CriticalFailureException(
              s"Invalid format for aggregate flex strategy: $powerTargetAbsString"
            )
        }
      case unknown =>
        throw new CriticalFailureException(
          s"Unknown aggregate flex strategy $unknown"
        )
    }

    EmModelShell(uuid, id, modelStrategy, aggregateFlex)
  }
}
