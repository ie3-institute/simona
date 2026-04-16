/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.model.em
import edu.ie3.simona.api.data.model.em.{
  EmSetPoint,
  FlexOptions,
  MultiFlexOptions,
}
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.ServiceMessage.EmServiceRegistration
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.flex.PowerLimitFlexOptions
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.ReceiveDataMap
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger

import java.util.UUID
import scala.jdk.CollectionConverters.{MapHasAsScala, MapHasAsJava}

/** Basic service core for an [[ExtEmDataService]].
  * @param uuidToAgent
  *   Map: uuid to em agent reference.
  * @param agentToUuid
  *   Map: em agent reference to uuid.
  * @param uncontrolled
  *   A set of uuids of uncontrolled em models.
  * @param uuidToInferior
  *   A map that contains information about uuids of inferior em agents. This
  *   information is used to determine the disaggregated flex options.
  * @param uuidToParent
  *   A map: uuid to parent uuid.
  * @param completions
  *   ReceiveDataMap: uuid to completions.
  * @param nextActivation
  *   A map: uuid to next activation tick.
  * @param allFlexOptions
  *   Map: uuid to flex options. This map stores all flex options received for
  *   the current tick.
  * @param flexOptions
  *   ReceiveDataMap: uuid to flex option.
  * @param disaggregated
  *   A map: uuid of em agent to boolean. It defines for which em agent we
  *   should return disaggregated flex options.
  * @param sendOptionsToExt
  *   True, if flex options should be sent to the external simulation.
  * @param canHandleSetPoints
  *   True, if the core can sent the received em set points to the agent. It
  *   will only be true, of all em agent are activated for the current tick and
  *   therefore able to process the send set points.
  * @param setPointOption
  *   Option for em set points that needs to be handled at a later time.
  * @param internal
  *   A set of uuids of models that simulated internally.
  */
final case class EmServiceBaseCore(
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
    flexOptions: ReceiveDataMap[UUID, FlexOptions] = ReceiveDataMap.empty,
    disaggregated: Map[UUID, Boolean] = Map.empty,
    sendOptionsToExt: Boolean = false,
    canHandleSetPoints: Boolean = false,
    setPointOption: Option[Map[UUID, EmSetPoint]] = None,
    internal: Set[UUID] = Set.empty,
) extends EmServiceCore {

  def handleRegistration(
      emServiceRegistration: EmServiceRegistration
  ): EmServiceBaseCore = {
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
      completions = completions.addExpectedKey(uuid),
      nextActivation = nextActivation.updated(uuid, -1),
    )
  }

  override def handleExtMessage(tick: Long, extMsg: EmDataMessageFromExt)(using
      log: Logger
  ): (EmServiceBaseCore, Option[EmDataResponseMessageToExt]) = extMsg match {
    case provideEmData: ProvideEmData =>
      if !provideEmData.flexOptions.isEmpty then {
        log.warn(
          s"We received the following data '$provideEmData'. The base service can currently not handle the provided flex options."
        )
      }

      val tick = provideEmData.tick
      val flexRequests = provideEmData.flexRequests.asScala.flatMap {
        case (entity, request) =>
          uuidToAgent.get(entity).map { ref =>
            ref ! FlexActivation(tick)

            entity -> request.disaggregated
          }
      }.toMap

      val updatedState = copy(
        flexOptions = ReceiveDataMap(flexRequests.keySet),
        completions = completions.addExpectedKeys(flexRequests.keySet),
        disaggregated = disaggregated ++ flexRequests,
        sendOptionsToExt = flexRequests.nonEmpty,
      )

      // handle set points
      val setPoints = provideEmData.setPoints().asScala.toMap

      if setPoints.nonEmpty then {

        if canHandleSetPoints then {
          handleSetPoint(tick, setPoints, log)

          (updatedState, None)
        } else {
          val entities = setPoints.keySet

          entities.foreach { entity =>
            uuidToAgent.get(entity) match {
              case Some(ref) =>
                // activate the necessary em agent, this is needed, because an em agent needs to know
                // its current flex option to properly handle the given set point
                ref ! FlexActivation(tick)
              case None =>
                log.warn(s"Received entity: $entity")
            }
          }

          (
            updatedState.copy(
              flexOptions = updatedState.flexOptions.addExpectedKeys(entities),
              completions = updatedState.completions.addExpectedKeys(entities),
              setPointOption = Some(setPoints),
            ),
            None,
          )
        }

      } else (updatedState, None)

    case requestEmCompletion: RequestEmCompletion =>
      // finish tick and return next tick
      val extTick = requestEmCompletion.tick

      if extTick != tick then {
        throw new CriticalFailureException(
          s"Received completion request for tick '$extTick', while being in tick '$tick'."
        )
      } else {
        log.info(s"Request to finish for tick '$tick' received.")

        val nextTick = getMaybeNextTick

        (
          this,
          Some(new EmCompletion(nextTick)),
        )
      }

    case _ =>
      throw new CriticalFailureException(
        s"The EmServiceBaseCore is not able to handle the message: $extMsg"
      )
  }

  override def handleFlexResponse(
      tick: Long,
      flexResponse: FlexResponse,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(using
      log: Logger
  ): (EmServiceBaseCore, Option[EmDataResponseMessageToExt]) = {

    val receiverUuid = receiver match {
      case Right(ref) =>
        ref ! flexResponse
        agentToUuid(ref)
      case Left(uuid) =>
        uuid
    }

    flexResponse match {
      case provideFlexOptions: ProvideFlexOptions =>
        val (updated, updatedAdditional) =
          handleFlexOptions(receiverUuid, provideFlexOptions)

        if updated.isComplete then {
          // we received all flex options

          val data = updated.receivedData

          val updatedData = data.map { case (uuid, flexOption) =>
            disaggregated.get(uuid) match {
              case Some(value) if value =>
                val disaggregatedData = uuidToInferior
                  .getOrElse(uuid, Set.empty)
                  .map(inferior => inferior -> allFlexOptions(inferior))
                  .toMap

                uuid -> new MultiFlexOptions(
                  uuid,
                  disaggregatedData.asJava,
                )

              case _ =>
                uuid -> flexOption
            }
          }

          val updatedCore = copy(
            flexOptions = ReceiveDataMap.empty,
            allFlexOptions = updatedAdditional,
            canHandleSetPoints = true,
          )

          if internal.nonEmpty then {
            internal.map(uuidToAgent).foreach(_ ! IssueNoControl(tick))

            (updatedCore, None)

          } else if sendOptionsToExt then {
            val dataToSend = updatedData.map { case (uuid, option) =>
              uuid -> List(option)
            }

            // we have received an option request, that will now be answered
            (updatedCore, Some(new FlexOptionsResponse(dataToSend.asJava)))

          } else {

            setPointOption match {
              case Some(setPoints) =>
                // we have received new set points, that are not handled yet => we will handle them now
                handleSetPoint(tick, setPoints, log)

                (updatedCore.copy(setPointOption = None), None)
              case None =>
                // we are now able to handle set points, but we have not yet received any
                (updatedCore, None)
            }
          }

        } else {
          log.debug(s"Missing flex options for: ${updated.getExpectedKeys}")

          (
            copy(
              flexOptions = updated,
              allFlexOptions = updatedAdditional,
            ),
            None,
          )
        }

      case completion: FlexCompletion =>
        val (updated, extMsgOption, nextTick, finished) =
          handleCompletion(tick, completion)

        if finished then {
          // the next activations
          val updatedNextActivation =
            nextActivation ++ updated.receivedData.flatMap { case (uuid, msg) =>
              msg.requestAtTick.map(uuid -> _)
            }

          val expectedCompletions = nextTick match {
            case Some(t) =>
              val keys = updatedNextActivation.filter { case (_, activation) =>
                activation == t
              }.keySet
              ReceiveDataMap[UUID, FlexCompletion](keys)
            case None =>
              updated
          }

          val msgToExt = if internal.nonEmpty then {
            Some(new EmCompletion(updatedNextActivation.values.minOption))
          } else extMsgOption

          (
            copy(
              completions = expectedCompletions,
              disaggregated = Map.empty,
              sendOptionsToExt = false,
              canHandleSetPoints = false,
              nextActivation = updatedNextActivation,
              internal = Set.empty,
            ),
            msgToExt,
          )

        } else {
          log.debug(s"Missing completion for: ${updated.getExpectedKeys}")

          (copy(completions = updated), extMsgOption)
        }

      case _ =>
        (this, None)
    }
  }

  override def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(using
      log: Logger
  ): (EmServiceBaseCore, Option[EmDataResponseMessageToExt]) = {
    log.debug(s"$receiver: $flexRequest")
    receiver ! flexRequest

    val uuid = agentToUuid(receiver)
    (copy(completions = completions.addExpectedKey(uuid)), None)
  }

  /** Method to handle flex options.
    * @param receiver
    *   The receiver of the flex options.
    * @param provideFlexOptions
    *   The provided flex options.
    * @return
    *   An updated service core and a map: uuid to flex options
    */
  private def handleFlexOptions(
      receiver: UUID,
      provideFlexOptions: ProvideFlexOptions,
  ): (
      ReceiveDataMap[UUID, FlexOptions],
      Map[UUID, FlexOptions],
  ) = provideFlexOptions match {
    case ProvideFlexOptions(
          modelUuid: UUID,
          PowerLimitFlexOptions(ref, min, max),
        ) =>
      val result = new em.PowerLimitFlexOptions(
        receiver,
        modelUuid,
        min.toQuantity,
        ref.toQuantity,
        max.toQuantity,
      )

      if flexOptions.expects(modelUuid) then {
        (
          flexOptions.addData(modelUuid, result),
          allFlexOptions.updated(modelUuid, result),
        )
      } else {
        (
          flexOptions,
          allFlexOptions.updated(modelUuid, result),
        )
      }

    case _ =>
      (flexOptions, allFlexOptions)
  }

}
