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
  ExtendedFlexOptionsResult,
  FlexOptions,
}
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.ServiceMessage.EmServiceRegistration
import edu.ie3.simona.ontology.messages.flex.FlexType.PowerLimit
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.flex.PowerLimitFlexOptions
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.simona.util.TickUtil.TickLong
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters.{
  ListHasAsScala,
  MapHasAsJava,
  MapHasAsScala,
  SetHasAsScala,
}

/** Basic service core for an [[ExtEmDataService]].
  * @param lastFinishedTick
  *   The last tick that was completed.
  * @param uuidToAgent
  *   Map: uuid to em agent reference.
  * @param agentToUuid
  *   Map: em agent reference to uuid.
  * @param flexOptions
  *   ReceiveDataMap: uuid to flex option result.
  * @param allFlexOptions
  *   Map: uuid to flex option result.
  * @param completions
  *   ReceiveDataMap: uuid to completions.
  * @param structure
  *   A map that contains information about uuids of inferior em agents. This
  *   information is used to determine the disaggregated flex options.
  * @param disaggregated
  *   A map: uuid of em agent to boolean. It defines for which em agent we
  *   should return disaggregated flex optios.
  * @param sendOptionsToExt
  *   True, if flex options should be sent to the external simulation.
  * @param canHandleSetPoints
  *   True, if the core can sent the received em set points to the agent. It
  *   will only be true, of all em agent are activated for the current tick and
  *   therefore able to process the send set points.
  * @param setPointOption
  *   Option for em set points that needs to be handled at a later time.
  */
final case class EmServiceBaseCore(
    override val lastFinishedTick: Long = PRE_INIT_TICK,
    override val uuidToAgent: Map[UUID, ActorRef[EmAgent.Message]] = Map.empty,
    agentToUuid: Map[ActorRef[EmAgent.Message] | ActorRef[FlexResponse], UUID] =
      Map.empty,
    flexOptions: ReceiveDataMap[UUID, FlexOptions] = ReceiveDataMap.empty,
    override val allFlexOptions: Map[UUID, FlexOptions] = Map.empty,
    override val completions: ReceiveDataMap[UUID, FlexCompletion] =
      ReceiveDataMap.empty,
    structure: Map[UUID, Set[UUID]] = Map.empty,
    disaggregated: Map[UUID, Boolean] = Map.empty,
    sendOptionsToExt: Boolean = false,
    canHandleSetPoints: Boolean = false,
    setPointOption: Option[Map[UUID, EmSetPoint]] = None,
    nextActivation: Map[UUID, Long] = Map.empty,
) extends EmServiceCore {

  override def handleRegistration(
      emServiceRegistration: EmServiceRegistration
  ): EmServiceBaseCore = {
    val ref = emServiceRegistration.requestingActor
    val modelUuid = emServiceRegistration.inputUuid
    val parentUuid = emServiceRegistration.parentUuid

    val updatedStructure = parentUuid match {
      case Some(parent) =>
        structure.get(parent) match {
          case Some(subEms) =>
            val allSubEms = subEms + modelUuid
            structure ++ Map(parent -> allSubEms)
          case None =>
            structure ++ Map(parent -> Set(modelUuid))
        }

      case None if !structure.contains(modelUuid) =>
        structure ++ Map(modelUuid -> Set.empty[UUID])

      case _ =>
        // since the given em agent has no parent, no changes to the parent structure are needed
        // the actual em agent is added to the structure later
        structure
    }

    copy(
      uuidToAgent = uuidToAgent + (modelUuid -> ref),
      agentToUuid = agentToUuid + (ref -> modelUuid),
      completions = completions.addExpectedKeys(Set(modelUuid)),
      structure = updatedStructure ++ Map(modelUuid -> Set.empty[UUID]),
      nextActivation = nextActivation.updated(modelUuid, 0),
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
            ref ! FlexActivation(tick, PowerLimit)

            entity -> request.disaggregated
          }
      }.toMap

      val updatedState = copy(
        flexOptions = ReceiveDataMap(flexRequests.keySet),
        disaggregated = disaggregated ++ flexRequests,
        sendOptionsToExt = true,
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
                ref ! FlexActivation(tick, PowerLimit)
              case None =>
                log.warn(s"Received entity: $entity")
            }
          }

          (
            updatedState.copy(
              flexOptions = updatedState.flexOptions.addExpectedKeys(entities),
              setPointOption = Some(setPoints),
            ),
            None,
          )
        }

      } else (updatedState, None)

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
          handleFlexOptions(tick, receiverUuid, provideFlexOptions)

        if updated.isComplete then {
          // we received all flex options

          val data = updated.receivedData

          data.foreach { case (uuid, flexOption) =>
            if disaggregated.contains(uuid) then {
              // we add the disaggregated flex options
              addDisaggregatingFlexOptions(
                flexOption,
                structure.getOrElse(uuid, Set.empty),
              )
            }
          }

          val updatedCore = copy(
            flexOptions = ReceiveDataMap.empty,
            allFlexOptions = updatedAdditional,
            canHandleSetPoints = true,
          )

          if sendOptionsToExt then {
            val dataToSend = data.map { case (uuid, option) =>
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
          log.warn(s"Missing flex options for: ${updated.getExpectedKeys}")

          (
            copy(
              flexOptions = updated,
              allFlexOptions = updatedAdditional,
            ),
            None,
          )
        }

      case completion: FlexCompletion =>
        if tick == INIT_SIM_TICK then {
          receiver match {
            case Left(value) =>
              (copy(lastFinishedTick = tick), None)
            case Right(_) =>
              (this, None)
          }
        } else {
          val (updated, extMsgOption, nextTick, finished) =
            handleCompletion(tick, completion)

          if finished then {
            // the next activations
            val updatedNextActivation =
              nextActivation ++ updated.receivedData.flatMap {
                case (uuid, msg) =>
                  msg.requestAtTick.map(uuid -> _)
              }

            val expectedCompletions = nextTick match {
              case Some(t) =>
                val keys = updatedNextActivation.filter {
                  case (_, activation) => activation == t
                }.keySet
                log.warn(s"Keys: $keys")
                ReceiveDataMap[UUID, FlexCompletion](keys)
              case None =>
                updated
            }

            log.warn(s"$updated")

            (
              copy(
                lastFinishedTick = tick,
                completions = expectedCompletions,
                disaggregated = Map.empty,
                sendOptionsToExt = false,
                canHandleSetPoints = false,
                nextActivation = updatedNextActivation,
              ),
              extMsgOption,
            )

          } else {
            log.warn(s"$updated")

            (copy(completions = updated), extMsgOption)
          }
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

    (this, None)
  }

  /** Method to handle flex options.
    * @param tick
    *   Current tick of the service.
    * @param receiver
    *   The receiver of the flex options.
    * @param provideFlexOptions
    *   The provided flex options.
    * @return
    *   An updated service core and a map: uuid to flex options
    */
  private def handleFlexOptions(
      tick: Long,
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
        println(s"Received expected: $modelUuid")

        (
          flexOptions.addData(modelUuid, result),
          allFlexOptions.updated(modelUuid, result),
        )
      } else {
        println(s"Received unexpected: $modelUuid")

        (
          flexOptions,
          allFlexOptions.updated(modelUuid, result),
        )
      }

    case _ =>
      (flexOptions, allFlexOptions)
  }

}

object EmServiceBaseCore {

  def empty: EmServiceBaseCore = EmServiceBaseCore()
}
