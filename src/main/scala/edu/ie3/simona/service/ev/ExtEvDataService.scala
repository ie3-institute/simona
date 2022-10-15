/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.ev

import akka.actor.{ActorRef, Props}
import edu.ie3.simona.api.data.ev.ExtEvData
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology.EvMovementsMessage.EvcsMovements
import edu.ie3.simona.api.data.ev.ontology.{
  AllDepartedEvsResponse,
  EvMovementsMessage,
  ExtEvMessage,
  ProvideEvcsFreeLots,
  RequestEvcsFreeLots,
  ProvideCurrentPrices,
  RequestCurrentPrices
}
import edu.ie3.simona.api.data.ontology.ExtDataMessage
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleTriggerMessage
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ServiceRegistrationMessage
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData
}
import edu.ie3.simona.service.ev.ExtEvDataService.{
  ExtEvStateData,
  InitExtEvData
}
import edu.ie3.simona.service.{ExtDataSupport, ServiceStateData, SimonaService}

import java.util.UUID
import scala.jdk.CollectionConverters.{
  MapHasAsJava,
  MapHasAsScala,
  SeqHasAsJava
}
import scala.util.{Failure, Success, Try}

object ExtEvDataService {

  def props(scheduler: ActorRef): Props =
    Props(
      new ExtEvDataService(scheduler: ActorRef)
    )

  final case class ExtEvStateData(
      extEvData: ExtEvData,
      uuidToActorRef: Map[UUID, (ActorRef, Long => ScheduleTriggerMessage)] =
        Map.empty,
      extEvMessage: Option[ExtEvMessage] = None,
      freeLots: Map[UUID, Option[Int]] = Map.empty,
      evModelResponses: Map[UUID, Option[List[EvModel]]] = Map.empty,
      currentPrices: Map[UUID, Option[Double]] = Map.empty
  ) extends ServiceBaseStateData

  final case class InitExtEvData(
      extEvData: ExtEvData
  ) extends InitializeServiceStateData

}

class ExtEvDataService(override val scheduler: ActorRef)
    extends SimonaService[ExtEvStateData](scheduler)
    with ExtDataSupport[ExtEvStateData] {

  override def init(
      initServiceData: ServiceStateData.InitializeServiceStateData
  ): Try[
    (
        ExtEvStateData,
        Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]
    )
  ] =
    initServiceData match {
      case InitExtEvData(extEvData) =>
        val evInitializedStateData = ExtEvStateData(
          extEvData
        )

        Success(
          evInitializedStateData,
          None
        )

      case invalidData =>
        Failure(
          new InitializationException(
            s"Provided init data '${invalidData.getClass.getSimpleName}' for EV service are invalid!"
          )
        )
    }

  /** Handle a request to register for information from this service
    *
    * @param registrationMessage
    *   registration message to handle
    * @param serviceStateData
    *   current state data of the actor
    * @return
    *   the service stata data that should be used in the next state (normally
    *   with updated values)
    */
  override def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(implicit
      serviceStateData: ExtEvStateData
  ): Try[ExtEvStateData] =
    registrationMessage match {
      case RegisterForEvDataMessage(evcs, scheduleFunc) =>
        Success(handleRegistrationRequest(sender(), evcs, scheduleFunc))
      case invalidMessage =>
        Failure(
          InvalidRegistrationRequestException(
            "Cannot register an agent for ev movement service with registration " +
              s"request message '${invalidMessage.getClass.getSimpleName}'!"
          )
        )
    }

  /** Try to register the sending agent with its uuid and location type values
    * for ev movement updates
    *
    * @param agentToBeRegistered
    *   the agent that wants to be registered
    * @param evcs
    *   the charging station
    * @param scheduleFunc
    *   function providing the proper ScheduleTriggerMessage for a given tick
    * @param serviceStateData
    *   the current service state data of this service
    * @return
    *   an updated state data of this service that contains registration
    *   information if the registration has been carried out successfully
    */
  private def handleRegistrationRequest(
      agentToBeRegistered: ActorRef,
      evcs: UUID,
      scheduleFunc: Long => ScheduleTriggerMessage
  )(implicit
      serviceStateData: ExtEvStateData
  ): ExtEvStateData = {
    log.debug(
      "Received ev movement service registration from {} for [Evcs:{}]",
      agentToBeRegistered.path.name,
      evcs
    )

    serviceStateData.uuidToActorRef.get(evcs) match {
      case None =>
        // Actor is not registered yet
        agentToBeRegistered ! RegistrationSuccessfulMessage(None)

        serviceStateData.copy(
          uuidToActorRef =
            serviceStateData.uuidToActorRef + (evcs -> (agentToBeRegistered, scheduleFunc))
        )
      case Some(_) =>
        // actor is already registered, do nothing
        log.warning(
          "Sending actor {} is already registered",
          agentToBeRegistered
        )
        serviceStateData
    }
  }

  /** Send out the information to all registered recipients
    *
    * @param tick
    *   current tick data should be announced for
    * @param serviceStateData
    *   the current state data of this service
    * @return
    *   the service stata data that should be used in the next state (normally
    *   with updated values) together with the completion message that is send
    *   in response to the trigger that was sent to start this announcement
    */
  override protected def announceInformation(
      tick: Long
  )(implicit serviceStateData: ExtEvStateData): (
      ExtEvStateData,
      Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]
  ) = {
    serviceStateData.extEvMessage.getOrElse(
      throw ServiceException(
        "ExtEvDataActor was triggered without ExtEvMessage available"
      )
    ) match {
      case _: RequestEvcsFreeLots =>
        requestFreeLots(tick)
      case _: RequestCurrentPrices =>
        requestCurrentPrices(tick)
      case evMovementsMessage: EvMovementsMessage =>
        sendEvMovements(tick, evMovementsMessage.getMovements)
    }
  }

  private def requestFreeLots(tick: Long)(implicit
      serviceStateData: ExtEvStateData
  ): (ExtEvStateData, Option[Seq[ScheduleTriggerMessage]]) = {
    serviceStateData.uuidToActorRef.values.foreach { case (evcsActor, _) =>
      evcsActor ! EvFreeLotsRequest(tick)
    }

    val freeLots: Map[UUID, Option[Int]] =
      serviceStateData.uuidToActorRef.map { case (evcs, _) =>
        evcs -> None
      }

    // if there are no evcs, we're sending response right away
    if (freeLots.isEmpty)
      serviceStateData.extEvData.queueExtResponseMsg(new ProvideEvcsFreeLots())

    (
      serviceStateData.copy(
        extEvMessage = None,
        freeLots = freeLots
      ),
      None
    )
  }

  private def requestCurrentPrices(tick: Long)(implicit
      serviceStateData: ExtEvStateData
  ): (ExtEvStateData, Option[Seq[ScheduleTriggerMessage]]) = {
    val scheduleTriggerMsgs =
      serviceStateData.uuidToActorRef.map {
        case (_, (evcsActor, scheduleFunc)) =>
          evcsActor ! ProvideEvDataMessage(
            tick,
            CurrentPriceRequest
          )

          // schedule activation of participant
          scheduleFunc(tick)
      }

    val currentPrices: Map[UUID, Option[Double]] =
      serviceStateData.uuidToActorRef.map { case (evcs, _) =>
        evcs -> None
      }

    // if there are no evcs, we're sending response right away
    if (currentPrices.isEmpty)
      serviceStateData.extEvData.queueExtResponseMsg(new ProvideCurrentPrices())

    (
      serviceStateData.copy(
        extEvMessage = None,
        currentPrices = currentPrices
      ),
      Option.when(scheduleTriggerMsgs.nonEmpty)(scheduleTriggerMsgs.toSeq)
    )
  }

  private def sendEvMovements(
      tick: Long,
      evMovements: java.util.Map[UUID, EvcsMovements]
  )(implicit
      serviceStateData: ExtEvStateData
  ): (ExtEvStateData, Option[Seq[ScheduleTriggerMessage]]) = {
    val filteredPositionData =
      evMovements.asScala.flatMap { case (evcs, movements) =>
        serviceStateData.uuidToActorRef.get(evcs) match {
          case Some((evcsActor, scheduleFunc)) =>
            Some(evcs, evcsActor, scheduleFunc, movements)
          case None =>
            log.warning(
              "A corresponding actor ref for UUID {} could not be found",
              evcs
            )
            None
        }
      }

    val scheduleTriggerMsgs =
      filteredPositionData.map { case (_, evcsActor, scheduleFunc, movements) =>
        evcsActor ! ProvideEvDataMessage(
          tick,
          EvMovementData(movements)
        )

        // schedule activation of participant
        scheduleFunc(tick)
      }

    val departingVehicles: Map[UUID, Option[List[EvModel]]] =
      filteredPositionData.flatMap { case (evcs, _, _, movements) =>
        val containsDeparture = !movements.getDepartures.isEmpty

        // only when positions message contains departure,
        // we want to wait for the departed vehicles
        Option.when(containsDeparture)(evcs -> None)
      }.toMap

    // if there are no departing evs during this tick,
    // we're sending response right away
    if (departingVehicles.isEmpty)
      serviceStateData.extEvData.queueExtResponseMsg(
        new AllDepartedEvsResponse()
      )

    (
      serviceStateData.copy(
        extEvMessage = None,
        evModelResponses = departingVehicles
      ),
      Option.when(scheduleTriggerMsgs.nonEmpty)(scheduleTriggerMsgs.toSeq)
    )
  }

  override protected def handleDataMessage(
      extMsg: ExtDataMessage
  )(implicit serviceStateData: ExtEvStateData): ExtEvStateData =
    extMsg match {
      case extEvMessage: ExtEvMessage =>
        serviceStateData.copy(
          extEvMessage = Some(extEvMessage)
        )
    }

  override protected def handleDataResponseMessage(
      extResponseMsg: EvResponseMessage
  )(implicit serviceStateData: ExtEvStateData): ExtEvStateData = {
    extResponseMsg match {
      case DepartedEvsResponse(evcs, evModels) =>
        val updatedResponses = serviceStateData.evModelResponses +
          (evcs -> Some(evModels.toList))

        if (updatedResponses.exists { case (_, resp) => resp.isEmpty }) {
          // responses are still incomplete
          serviceStateData.copy(
            evModelResponses = updatedResponses
          )
        } else {
          // all responses received, forward them to external simulation in a bundle
          val departedEvs = updatedResponses.values.flatten.flatten

          serviceStateData.extEvData.queueExtResponseMsg(
            new AllDepartedEvsResponse(departedEvs.toList.asJava)
          )

          serviceStateData.copy(
            evModelResponses = Map.empty
          )
        }
      case FreeLotsResponse(evcs, freeLots) =>
        val updatedFreeLots = serviceStateData.freeLots +
          (evcs -> Some(freeLots))

        if (updatedFreeLots.exists { case (_, resp) => resp.isEmpty }) {
          // responses are still incomplete
          serviceStateData.copy(
            freeLots = updatedFreeLots
          )
        } else {
          // all responses received, forward them to external simulation in a bundle
          val freeLotsResponse = updatedFreeLots.flatMap {
            case (evcs, Some(freeLotsCount)) if freeLotsCount > 0 =>
              Some(evcs -> Integer.valueOf(freeLotsCount))
            case _ =>
              None
          }

          serviceStateData.extEvData.queueExtResponseMsg(
            new ProvideEvcsFreeLots(freeLotsResponse.asJava)
          )

          serviceStateData.copy(
            freeLots = Map.empty
          )
        }
      case CurrentPriceResponse(evcs, currentPrice) =>
        val updatedCurrentPrices = serviceStateData.currentPrices +
          (evcs -> Some(currentPrice))

        if (updatedCurrentPrices.exists { case (_, resp) => resp.isEmpty }) {
          // responses are still incomplete
          serviceStateData.copy(
            currentPrices = updatedCurrentPrices
          )
        } else {
          // all responses received, forward them to external simulation in a bundle
          val currentPricesResponse = updatedCurrentPrices.flatMap {
            case (evcs, Some(currentPrice)) =>
              Some(evcs -> java.lang.Double.valueOf(currentPrice))
            case _ =>
              None
          }

          serviceStateData.extEvData.queueExtResponseMsg(
            new ProvideCurrentPrices(currentPricesResponse.asJava)
          )

          serviceStateData.copy(
            currentPrices = Map.empty
          )
        }
    }
  }
}
