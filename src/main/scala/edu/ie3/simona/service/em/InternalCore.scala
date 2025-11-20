/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.connection.ExtEmDataConnection.EmMode
import edu.ie3.simona.api.data.model.em.{EmSetPoint, FlexOptions}
import edu.ie3.simona.api.ontology.em.{
  EmCompletion,
  EmDataMessageFromExt,
  EmDataResponseMessageToExt,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.EmServiceRegistration
import edu.ie3.simona.ontology.messages.flex.FlexType.PowerLimit
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.simona.util.SimonaConstants.PRE_INIT_TICK
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger

import java.util.UUID

case class InternalCore(
    override val mode: EmMode,
    override val lastFinishedTick: Long = PRE_INIT_TICK,
    override val uuidToAgent: Map[UUID, ActorRef[EmAgent.Message]] = Map.empty,
    override val agentToUuid: Map[
      ActorRef[FlexRequest] | ActorRef[FlexResponse],
      UUID,
    ] = Map.empty,
    override val uncontrolled: Set[UUID] = Set.empty,
    override val uuidToInferior: Map[UUID, Set[UUID]] = Map.empty,
    override val uuidToParent: Map[UUID, UUID] = Map.empty,
    override val completions: ReceiveDataMap[UUID, FlexCompletion] =
      ReceiveDataMap.empty,
    override val nextActivation: Map[UUID, Long] = Map.empty,
    override val allFlexOptions: Map[UUID, FlexOptions] = Map.empty,
    disaggregated: Map[UUID, Boolean] = Map.empty,
    flexOptions: ReceiveDataMap[UUID, FlexOptions] = ReceiveDataMap.empty,
    sendOptionsToExt: Boolean = false,
    canHandleSetPoints: Boolean = false,
    setPointOption: Option[Map[UUID, EmSetPoint]] = None,
) extends EmServiceCore {

  /** Method to handle a registration message.
    *
    * @param emServiceRegistration
    *   The registration to handle.
    * @return
    *   An updated service core.
    */
  def handleRegistration(
      emServiceRegistration: EmServiceRegistration
  ): EmServiceCore = {
    val uuid = emServiceRegistration.inputUuid
    val ref = emServiceRegistration.requestingActor

    val (updatedUncontrolled, updatedInferior, updatedUuidToParent) =
      emServiceRegistration.parentUuid match {
        case Some(parent) =>
          val inferior = uuidToInferior.get(parent) match {
            case Some(inferiorUuids) =>
              inferiorUuids ++ Seq(uuid)
            case None =>
              Set(uuid)
          }

          (
            uncontrolled,
            uuidToInferior.updated(parent, inferior),
            uuidToParent.updated(uuid, parent),
          )
        case None =>
          (uncontrolled + uuid, uuidToInferior, uuidToParent)
      }

    copy(
      uuidToAgent = uuidToAgent.updated(uuid, ref),
      agentToUuid = agentToUuid.updated(ref, uuid),
      uncontrolled = updatedUncontrolled,
      uuidToInferior = updatedInferior,
      uuidToParent = updatedUuidToParent,
      nextActivation = nextActivation.updated(uuid, 0),
    )
  }

  override def handleExtMessage(tick: Long, extMsg: EmDataMessageFromExt)(using
      log: Logger
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    log.warn("Handling of external message not possible!")

    (this, None)
  }

  def sendActivations(tick: Long): InternalCore = {
    uncontrolled.filter(nextActivation(_) == tick).foreach {
      uuidToAgent(_) ! FlexActivation(tick, PowerLimit)
    }

    this
  }

  override def handleFlexResponse(
      tick: Long,
      flexResponse: FlexResponse,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    flexResponse match {
      case FlexibilityMessage.ProvideFlexOptions(modelUuid, flexOptions) =>
        receiver match {
          case Left(uuid) => uuidToAgent(uuid) ! IssueNoControl(tick)
          case Right(ref) => ref ! flexResponse
        }

        (this, None)

      case FlexCompletion(modelUuid, requestAtNextActivation, requestAtTick) =>
        (receiver, requestAtTick) match {
          case (Left(_), Some(nextTick)) =>
            (
              copy(
                lastFinishedTick = tick,
                nextActivation = nextActivation.updated(modelUuid, nextTick),
              ),
              Some(new EmCompletion(getMaybeNextExtTick)),
            )

          case (Left(_), None) =>
            (
              copy(lastFinishedTick = tick),
              Some(new EmCompletion(getMaybeNextExtTick)),
            )

          case (Right(ref), Some(nextTick)) =>
            ref ! flexResponse

            (
              copy(nextActivation =
                nextActivation.updated(modelUuid, nextTick)
              ),
              None,
            )

          case (Right(ref), None) =>
            ref ! flexResponse

            (this, None)
        }

      case other =>
        log.warn(s"Cannot handle: $other")
        (this, None)
    }
  }

  override def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    receiver ! flexRequest

    (this, None)
  }
}

object InternalCore {
  def apply(core: EmServiceCore): InternalCore = core match {
    case internal: InternalCore =>
      internal
    case _ =>
      InternalCore(
        core.mode,
        core.lastFinishedTick,
        core.uuidToAgent,
        core.agentToUuid,
        core.uncontrolled,
        core.uuidToInferior,
        core.uuidToParent,
        core.completions,
        core.nextActivation,
        core.allFlexOptions,
      )
  }

}
