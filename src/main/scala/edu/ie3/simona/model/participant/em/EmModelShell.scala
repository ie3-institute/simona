/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.participant.em.EmAgent.Actor
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  ProvideFlexOptions,
  ProvideMinMaxFlexOptions
}
import squants.energy.Power

import java.util.UUID

/** Translating input data to a format that can be used by aggregation
  * strategies, em strats etc. Furthermore, sanity checks on calculated data is
  * performed.
  */
// TODO move package em out of participant
final case class EmModelShell(
    id: String,
    modelStrategy: EmModelStrat,
    aggregateFlex: EmAggregateFlex,
    actorToParticipant: Map[Actor, SystemParticipantInput] = Map.empty,
    uuidToActor: Map[UUID, Actor] = Map.empty
) {

  def addParticipant(actor: Actor, spi: SystemParticipantInput): EmModelShell =
    copy(
      actorToParticipant = actorToParticipant.updated(actor, spi),
      uuidToActor = uuidToActor.updated(spi.getUuid, actor)
    )

  def aggregateFlexOptions(
      allFlexOptions: Iterable[
        (Actor, ProvideFlexOptions)
      ]
  ): (Power, Power, Power) = {
    val updatedAllFlexOptions = allFlexOptions.map {
      case (actor, flexOptions) =>
        val spi = actorToParticipant.getOrElse(
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
      allFlexOptions: Iterable[(Actor, ProvideFlexOptions)],
      target: Power
  ): Iterable[(Actor, Power)] = {
    // TODO sanity checks before strat calculation

    val minMaxFlexOptions = allFlexOptions.toMap.view.mapValues {
      case flex: ProvideMinMaxFlexOptions => flex
      case _                              => throw new RuntimeException()
    }.toMap

    val uuidToFlexOptions = minMaxFlexOptions.map { case (actor, flexOptions) =>
      val spi = actorToParticipant.getOrElse(
        actor,
        throw new RuntimeException()
      ) // TODO
      spi -> flexOptions
    }

    val setPoints =
      modelStrategy.determineDeviceControl(uuidToFlexOptions, target)

    setPoints.map { case (uuid, power) =>
      val actor =
        uuidToActor.getOrElse(uuid, throw new RuntimeException()) // TODO

      val flexOptions =
        minMaxFlexOptions.getOrElse(actor, throw new RuntimeException())
      if (!flexOptions.fits(power))
        throw new RuntimeException() // TODO

      actor -> power
    }

    // sanity checks after strat calculation
    // checkSetPower(flexOptions, power)

  }

}

object EmModelShell {
  def apply(
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

    EmModelShell(id, modelStrategy, aggregateFlex)
  }
}
