/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.evcs

import akka.actor.Props
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER
}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorEvMovementsService
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.agent.participant.{
  ParticipantAgent,
  ParticipantAgentFundamentals
}
import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.model.participant.EvcsModel
import edu.ie3.simona.model.participant.EvcsModel.EvcsRelevantData
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Power

object EvcsAgent {
  def props(
      scheduler: SimonaActorRef,
      listener: Iterable[SimonaActorRef]
  ): Props =
    Props(
      new EvcsAgent(
        scheduler,
        listener
      )
    )

  val neededServices: Vector[Class[_ <: SecondaryDataService[_]]] = Vector(
    classOf[ActorEvMovementsService]
  )
}

class EvcsAgent(
    scheduler: SimonaActorRef,
    override val listener: Iterable[SimonaActorRef]
) extends ParticipantAgent[
      ApparentPower,
      EvcsRelevantData,
      ParticipantStateData[ApparentPower],
      EvcsInput,
      EvcsRuntimeConfig,
      EvcsModel
    ](scheduler)
    with EvcsAgentFundamentals {
  override val alternativeResult: ApparentPower = ZERO_POWER

  /** Determine the average result within the given tick window
    *
    * @param tickToResults
    *   Mapping from data tick to actual data
    * @param windowStart
    *   First, included tick of the time window
    * @param windowEnd
    *   Last, included tick of the time window
    * @param activeToReactivePowerFuncOpt
    *   An Option on a function, that transfers the active into reactive power
    * @return
    *   The averaged result
    */
  override def averageResults(
      tickToResults: Map[Long, ApparentPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        ComparableQuantity[Power] => ComparableQuantity[Power]
      ]
  ): ApparentPower =
    ParticipantAgentFundamentals.averageApparentPower(
      tickToResults,
      windowStart,
      windowEnd,
      activeToReactivePowerFuncOpt,
      log
    )
}
