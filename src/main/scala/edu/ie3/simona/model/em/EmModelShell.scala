/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.simona.config.RuntimeConfig.EmRuntimeConfig
import edu.ie3.simona.exceptions.{CriticalFailureException, FlexException}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.IssueFlexControl
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  FlexOptionsMeta,
  FlexType,
  MinMaxFlexOptions,
}
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID

/** Translating input data to a format that can be used by aggregation
  * strategies, em strategies etc.. Furthermore, sanity checks on calculated
  * data is performed.
  */
final case class EmModelShell[FO <: FlexOptions](
    uuid: UUID,
    id: String,
    modelStrategy: EmModelStrat[FO],
    aggregateFlex: EmAggregateFlex[FO],
    private val modelToParticipantInput: Map[UUID, AssetInput] = Map.empty,
    private val flexOptions: Option[FO] = None,
    private val flexOptionsMeta: FlexOptionsMeta[FO],
) {

  /** Returns a unique identifier for the model held by this model shell,
    * including UUID and id of the model, for the purpose of log or exception
    * messaging.
    *
    * @return
    *   A unique identifier for the model
    */
  def identifier: String =
    s"EmModel[$id/$uuid]"

  def getFlexType: FlexType =
    flexOptionsMeta.flexType

  /** Returns the current flex options, if present, or throws a
    * [[CriticalFailureException]]. Only call this if you are certain the flex
    * options have been set.
    *
    * @return
    *   The flex options.
    */
  def getFlexOptions: FO =
    flexOptions.getOrElse(
      throw new CriticalFailureException(
        s"$identifier: Flex options have not been calculated!"
      )
    )

  def addParticipant(
      modelUuid: UUID,
      inputModel: AssetInput,
  ): EmModelShell[FO] =
    copy(
      modelToParticipantInput =
        modelToParticipantInput.updated(modelUuid, inputModel)
    )

  def updateFlexOptions(
      allFlexOptions: Iterable[
        (UUID, FlexOptions)
      ]
  ): EmModelShell[FO] = {
    val updatedAllFlexOptions = allFlexOptions.map {
      case (modelUuid, flexOptions) =>
        val assetInput = modelToParticipantInput.getOrElse(
          modelUuid,
          throw new CriticalFailureException(
            s"Asset input for model with UUID $modelUuid was not found."
          ),
        )

        val typedFlexOptions = flexOptionsMeta.castFlexOptions(flexOptions)

        val updatedFlexOptions =
          modelStrategy.adaptFlexOptions(assetInput, typedFlexOptions)

        assetInput -> updatedFlexOptions
    }

    val aggregatedFlex =
      aggregateFlex.aggregateFlexOptions(updatedAllFlexOptions)

    copy(flexOptions = Some(aggregatedFlex))
  }

  def determineFlexPower(flexCtrl: IssueFlexControl): Power =
    flexOptionsMeta.determineFlexPower(getFlexOptions, flexCtrl)

  def determineFlexControl(
      allFlexOptions: Iterable[(UUID, FlexOptions)],
      target: Power,
  ): Iterable[(UUID, Power)] = {

    val typedFlexOptions =
      allFlexOptions.toMap.view
        .mapValues(flexOptionsMeta.castFlexOptions)
        .toMap

    val uuidToFlexOptions = typedFlexOptions.map { case (modelUuid, fo) =>
      val assetInput = modelToParticipantInput.getOrElse(
        modelUuid,
        throw new CriticalFailureException(
          s"Asset input for model with UUID $modelUuid was not found."
        ),
      )
      assetInput -> fo
    }

    val setPoints =
      modelStrategy.determineFlexControl(uuidToFlexOptions, target)

    setPoints.map { case (model, power) =>
      val fo = typedFlexOptions.getOrElse(
        model,
        throw new CriticalFailureException(
          s"Set point for model $model has been calculated by ${modelStrategy.getClass.getSimpleName}, which is not connected to this EM."
        ),
      )

      // sanity checks after strat calculation
      try {
        flexOptionsMeta.checkSetPower(fo, power)
      } catch {
        case fe: FlexException =>
          throw new CriticalFailureException(
            s"Determining flex power failed for asset $model",
            fe,
          )
      }

      model -> power
    }
  }

  /** Returns a result for the current flex options.
    *
    * @param dateTime
    *   The current date and time.
    * @return
    *   A flex options result.
    */
  def determineResults(dateTime: ZonedDateTime): FlexOptionsResult =
    flexOptionsMeta.createResult(
      getFlexOptions,
      uuid,
      dateTime,
    )

}

object EmModelShell {

  def apply(
      uuid: UUID,
      id: String,
      modelStrategyName: String,
      modelConfig: EmRuntimeConfig,
  ): EmModelShell[?] = {

    case class StratFactoryWrapper[FO <: FlexOptions](
        modelStrat: PartialFunction[String, EmModelStrat[FO]],
        aggregateFlex: PartialFunction[String, EmAggregateFlex[FO]],
        flexOptionsMeta: FlexOptionsMeta[FO],
    )

    val allFactories = Seq(
      StratFactoryWrapper(
        EmModelStrat.parseMinMax(modelConfig),
        EmAggregateFlex.parseMinMax,
        MinMaxFlexOptions,
      )
    )

    val aggregateFlexName = modelConfig.aggregateFlex

    allFactories
      .find {
        case StratFactoryWrapper(modelStrat, aggregateFlex, flexOptionsMeta) =>
          val modelFound = modelStrat.isDefinedAt(modelStrategyName)
          val aggregateFlexFound =
            aggregateFlex.isDefinedAt(aggregateFlexName)

          if (modelFound && !aggregateFlexFound)
            throw new CriticalFailureException(
              s"Unknown aggregate flex strategy $aggregateFlexName for flex type ${flexOptionsMeta.classTag.runtimeClass.getSimpleName}."
            )
          else if (!modelFound && aggregateFlexFound)
            throw new CriticalFailureException(
              s"Unknown model flex strategy $modelStrategyName for flex type ${flexOptionsMeta.classTag.runtimeClass.getSimpleName}."
            )

          modelFound && aggregateFlexFound
      }
      .map {
        case StratFactoryWrapper(modelStrat, aggregateFlex, flexOptionsMeta) =>
          EmModelShell(
            uuid,
            id,
            modelStrat(modelStrategyName),
            aggregateFlex(aggregateFlexName),
            flexOptionsMeta = flexOptionsMeta,
          )
      }
      .getOrElse {
        throw new CriticalFailureException(
          s"Model strategy $modelStrategyName and aggregate flex $aggregateFlexName not found."
        )
      }

  }

}
