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
import edu.ie3.simona.ontology.messages.flex.*
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexInit,
  IssueFlexControl,
}
import edu.ie3.simona.service.Data.SecondaryData
import edu.ie3.simona.service.DataTimeType
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
    private val modelToAssetInput: Map[UUID, AssetInput] = Map.empty,
    private val flexOptions: Option[FO] = None,
    private val flexOptionsExtra: FlexOptionsExtra[FO],
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

  /** Returns the data time type of the model strategy.
    *
    * @return
    *   The data time type.
    */
  def getDataTimeType: DataTimeType =
    modelStrategy.getServiceRegistrationData.dataTimeType

  /** Returns the type of flex options that are expected by the energy
    * management model as input and by the aggregation model as input and
    * output.
    *
    * @return
    *   The flex type of the model.
    */
  def getFlexType: FlexType =
    flexOptionsExtra.flexType

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

  /** Adds an asset controlled by this EM to the model shell.
    *
    * @param modelUuid
    *   The asset model UUID.
    * @param assetInput
    *   The asset input model.
    * @return
    *   An updated model shell.
    */
  def addControlledAsset(
      modelUuid: UUID,
      assetInput: AssetInput,
  ): EmModelShell[FO] =
    copy(
      modelToAssetInput = modelToAssetInput.updated(modelUuid, assetInput)
    )

  /** Updates the aggregated flex options of this EM.
    *
    * @param allFlexOptions
    *   The current flex options of controlled assets.
    * @return
    *   An updated model shell with current flex options.
    */
  def updateAggregatedFlexOptions(
      allFlexOptions: Iterable[
        (UUID, FlexOptions)
      ]
  ): EmModelShell[FO] = {
    val updatedAllFlexOptions = allFlexOptions.map {
      case (modelUuid, flexOptions) =>
        val assetInput = modelToAssetInput.getOrElse(
          modelUuid,
          throw new CriticalFailureException(
            s"$identifier: Asset input for model with UUID $modelUuid was not found."
          ),
        )

        val typedFlexOptions = flexOptionsExtra.castFlexOptions(flexOptions)

        val updatedFlexOptions =
          modelStrategy.adaptFlexOptions(assetInput, typedFlexOptions)

        assetInput -> updatedFlexOptions
    }

    val aggregatedFlex =
      aggregateFlex.aggregateFlexOptions(updatedAllFlexOptions)

    copy(flexOptions = Some(aggregatedFlex))
  }

  /** Determines and returns the power set point for this EM given an
    * [[IssueFlexControl]] message received by the superior EM.
    *
    * @param flexCtrl
    *   The flexibility control message.
    * @return
    *   The power set point.
    */
  def determineFlexPower(flexCtrl: IssueFlexControl): Power =
    flexOptionsExtra.determineFlexPower(getFlexOptions, flexCtrl)

  /** Determines and returns the flexibility control messages for the controlled
    * assets given their flex options and a target power.
    *
    * @param allFlexOptions
    *   The current flex options of controlled assets.
    * @param target
    *   The target power value.
    * @param currentTick
    *   The current tick.
    * @param receivedData
    *   The secondary data received by the EM agent.
    * @return
    *   The flexibility control for controlled assets as a map from asset uuid
    *   to its target power.
    */
  def determineFlexControl(
      allFlexOptions: Iterable[(UUID, FlexOptions)],
      target: Power,
      currentTick: Long,
      receivedData: Seq[SecondaryData],
  ): Iterable[(UUID, Power)] = {

    val typedFlexOptions =
      allFlexOptions.toMap.view
        .mapValues(flexOptionsExtra.castFlexOptions)
        .toMap

    val uuidToFlexOptions = typedFlexOptions.map { case (modelUuid, fo) =>
      val assetInput = modelToAssetInput.getOrElse(
        modelUuid,
        throw new CriticalFailureException(
          s"$identifier: Asset input for model with UUID $modelUuid was not found."
        ),
      )
      assetInput -> fo
    }

    val setPoints =
      modelStrategy.determineFlexControl(
        uuidToFlexOptions,
        target,
        currentTick,
        receivedData,
      )

    setPoints.map { case (model, power) =>
      val fo = typedFlexOptions.getOrElse(
        model,
        throw new CriticalFailureException(
          s"$identifier: Set point for model $model has been calculated by ${modelStrategy.getClass.getSimpleName}, which is not connected to this EM."
        ),
      )

      // sanity checks after strat calculation
      try {
        flexOptionsExtra.checkSetPower(fo, power)
      } catch {
        case fe: FlexException =>
          throw new CriticalFailureException(
            s"$identifier: Determining flex power failed for asset $model",
            fe,
          )
      }

      model -> power
    }
  }

  /** Determines and returns a result for the current aggregated flex options
    * for this EM unit.
    *
    * @param dateTime
    *   The current date and time.
    * @return
    *   A flex options result.
    */
  def determineResults(dateTime: ZonedDateTime): FlexOptionsResult =
    flexOptionsExtra.createResult(
      getFlexOptions,
      uuid,
      dateTime,
    )

  /** Validates the initialization message regarding flex type and data time
    * type. For now, both types have to be identical, respectively, but more
    * sophisticated validation can be implemented at a later time.
    *
    * @param msg
    *   The received [[FlexInit]] message.
    */
  def validateInit(msg: FlexInit): Unit = {
    if msg.flexType != getFlexType then
      throw new CriticalFailureException(
        s"$identifier: Flex type of parent ${msg.flexType} and our flex type $getFlexType do not match!"
      )

    if msg.dataTimeType != getDataTimeType then
      throw new CriticalFailureException(
        s"$identifier: Data time type of parent ${msg.dataTimeType} and our data time type $getDataTimeType do not match!"
      )
  }
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
        flexOptionsExtra: FlexOptionsExtra[FO],
    )

    val allFactories = Seq(
      StratFactoryWrapper(
        EmModelStrat.parsePowerLimitModel(modelConfig),
        EmAggregateFlex.parsePowerLimitModel,
        PowerLimitFlexOptions,
      ),
      StratFactoryWrapper(
        EmModelStrat.parseOptimizingModel,
        EmAggregateFlex.parseOptimizingModel,
        EnergyBoundariesFlexOptions,
      ),
    )

    val aggregateFlexName = modelConfig.aggregateFlex

    allFactories
      .find {
        case StratFactoryWrapper(modelStrat, aggregateFlex, flexOptionsExtra) =>
          val modelStratFound = modelStrat.isDefinedAt(modelStrategyName)
          val aggregateFlexFound =
            aggregateFlex.isDefinedAt(aggregateFlexName)

          if modelStratFound && !aggregateFlexFound then
            throw new CriticalFailureException(
              s"Unknown aggregate flex strategy $aggregateFlexName for flex type ${flexOptionsExtra.classTag.runtimeClass.getSimpleName}."
            )
          else if !modelStratFound && aggregateFlexFound then
            throw new CriticalFailureException(
              s"Unknown model flex strategy $modelStrategyName for flex type ${flexOptionsExtra.classTag.runtimeClass.getSimpleName}."
            )

          modelStratFound && aggregateFlexFound
      }
      .map {
        case StratFactoryWrapper(modelStrat, aggregateFlex, flexOptionsExtra) =>
          EmModelShell(
            uuid,
            id,
            modelStrat(modelStrategyName),
            aggregateFlex(aggregateFlexName),
            flexOptionsExtra = flexOptionsExtra,
          )
      }
      .getOrElse {
        throw new CriticalFailureException(
          s"Model strategy $modelStrategyName and aggregate flex $aggregateFlexName not found."
        )
      }

  }

}
