/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueFlexControl,
  IssueNoControl,
  IssuePowerControl,
  ProvideFlexOptions
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions

import squants.Power

import java.util.UUID

/** Translating input data to a format that can be used by aggregation
  * strategies, em strats etc. Furthermore, sanity checks on calculated data is
  * performed.
  */
// TODO move package em out of participant
final case class EmModelShell(
    uuid: UUID,
    id: String,
    modelStrategy: EmModelStrat,
    aggregateFlex: EmAggregateFlex,
    modelToParticipantInput: Map[UUID, AssetInput] = Map.empty
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
      case (actor, flexOptions) =>
        val spi = modelToParticipantInput.getOrElse(
          actor,
          throw new RuntimeException()
        ) // TODO

        val minMaxFlexOptions = flexOptions match {
          case flex: ProvideMinMaxFlexOptions => flex
          case _                              => throw new RuntimeException()
        }

        val updatedFlexOptions =
          modelStrategy.adaptFlexOptions(spi, minMaxFlexOptions)

        spi -> updatedFlexOptions
    }

    aggregateFlex.aggregateFlexOptions(updatedAllFlexOptions)
  }

  def determineDeviceControl(
      allFlexOptions: Iterable[(UUID, ProvideFlexOptions)],
      target: Power
  ): Iterable[(UUID, Power)] = {
    // TODO sanity checks before strat calculation

    val minMaxFlexOptions = allFlexOptions.toMap.view.mapValues {
      case flex: ProvideMinMaxFlexOptions => flex
      case _                              => throw new RuntimeException()
    }.toMap

    val uuidToFlexOptions = minMaxFlexOptions.map { case (actor, flexOptions) =>
      val spi = modelToParticipantInput.getOrElse(
        actor,
        throw new RuntimeException()
      ) // TODO
      spi -> flexOptions
    }

    val setPoints =
      modelStrategy.determineDeviceControl(uuidToFlexOptions, target)

    setPoints.map { case (model, power) =>
      val flexOptions =
        minMaxFlexOptions.getOrElse(model, throw new RuntimeException())
      if (!flexOptions.fits(power))
        throw new RuntimeException() // TODO

      model -> power
    }

    // sanity checks after strat calculation
    // checkSetPower(flexOptions, power)

  }

}

object EmModelShell {
  def apply(
      uuid: UUID,
      id: String,
      modelStrat: String,
      modelConfig: EmRuntimeConfig
  ): EmModelShell = {

    val modelStrategy = modelStrat match {
      case "PROPORTIONAL" => ProportionalFlexStrat
      case "PRIORITIZED"  => PrioritizedFlexStrat(modelConfig.pvFlex)
    }

    val aggregateFlex = modelConfig.aggregateFlex match {
      case "SELF_OPT_EXCL_PV" => EmAggregateSelfOptExclPv
      case "SELF_OPT"         => EmAggregateSelfOpt
      case "SIMPLE_SUM"       => EmAggregateSimpleSum
    }

    EmModelShell(uuid, id, modelStrategy, aggregateFlex)
  }

  def determineResultingFlexPower(
      flexOptionsMsg: ProvideFlexOptions,
      flexCtrl: IssueFlexControl
  ): Either[String, Power] =
    flexOptionsMsg match {
      case flexOptions: ProvideMinMaxFlexOptions =>
        flexCtrl match {
          case IssuePowerControl(_, setPower) =>
            // sanity check: setPower is in range of latest flex options
            checkSetPower(flexOptions, setPower).map { _ =>
              // override, take setPower
              setPower
            }

          case IssueNoControl(_) =>
            // no override, take reference power
            Right(flexOptions.referencePower)
        }

      case unknownFlexOpt =>
        Left(
          s"Unknown/unfitting flex messages $unknownFlexOpt"
        )
    }

  def checkSetPower(
      flexOptions: ProvideMinMaxFlexOptions,
      setPower: squants.Power
  ): Either[String, Unit] = {
    if (setPower < flexOptions.minPower)
      Left(
        s"The set power $setPower for ${flexOptions.modelUuid} must not be lower than the minimum power ${flexOptions.minPower}!"
      )
    else if (setPower > flexOptions.maxPower)
      Left(
        s"The set power $setPower for ${flexOptions.modelUuid} must not be greater than the maximum power ${flexOptions.maxPower}!"
      )
    else
      Right(())
  }
}
