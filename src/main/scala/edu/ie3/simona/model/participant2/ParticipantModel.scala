/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationRelevantData,
  ResultsContainer,
}
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.service.ServiceType
import org.apache.pekko.actor.typed.javadsl.ActorContext
import squants.energy.Power

import java.time.ZonedDateTime
import java.util.UUID

abstract class ParticipantModel[
    OP <: OperatingPoint,
    S <: ModelState,
    OR <: OperationRelevantData,
](val uuid: UUID)
    extends ParticipantFlexibility[OP, S, OR] {

  def determineOperatingPoint(state: S, relevantData: OR): (OP, Option[Long])

  def determineState(lastState: S, operatingPoint: OP, currentTick: Long): S

  def createResults(
      lastState: S,
      operatingPoint: OP,
      complexPower: ApparentPower,
      dateTime: ZonedDateTime,
  ): ResultsContainer

  // todo split off the following to ParticipantModelMeta?
  def getRequiredServices: Iterable[ServiceType]

  /** @param receivedData
    * @throws CriticalFailureException
    *   if unexpected type of data was provided
    * @return
    */
  def createRelevantData(receivedData: Seq[SecondaryData], tick: Long): OR

  /** Handling requests that are not part of the standard participant protocol
    *
    * @param state
    *   The current state
    * @param ctx
    *   The actor context that can be used to send replies
    * @param msg
    *   The received request
    * @return
    *   An updated state, or the same state provided as parameter
    */
  def handleRequest(
      state: S,
      ctx: ActorContext[ParticipantAgent.Request],
      msg: ParticipantRequest,
  ): S =
    throw new NotImplementedError(s"Method not implemented by $getClass")

}

object ParticipantModel {

  trait OperationRelevantData

  trait OperatingPoint {
    val activePower: Power
  }

  case class ActivePowerOperatingPoint(override val activePower: Power)
      extends OperatingPoint

  trait ModelState {
    val tick: Long
  }

  case object ConstantState extends ModelState {
    override val tick = -1 // is there a better way?
  }

  final case class ResultsContainer(
      power: Power,
      modelResults: Seq[SystemParticipantResult],
  )

}
