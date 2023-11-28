/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.ev

import org.apache.pekko.actor.{ActorRef, Props}
import edu.ie3.simona.api.data.ev.ExtEvData
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology.{
  EvDataMessageFromExt,
  ProvideArrivingEvs,
  ProvideDepartingEvs,
  ProvideEvcsFreeLots,
  RequestDepartingEvs,
  RequestEvcsFreeLots
}
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleTriggerMessage
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ServiceRegistrationMessage
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
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
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

object ExtEvDataService {

  def props(scheduler: ActorRef): Props =
    Props(
      new ExtEvDataService(scheduler: ActorRef)
    )

  final case class ExtEvStateData(
      extEvData: ExtEvData,
      uuidToActorRef: Map[UUID, ActorRef] = Map.empty[UUID, ActorRef],
      extEvMessage: Option[EvDataMessageFromExt] = None,
      freeLots: Map[UUID, Option[Int]] = Map.empty,
      departingEvResponses: Map[UUID, Option[Seq[EvModel]]] = Map.empty
  ) extends ServiceBaseStateData

  final case class InitExtEvData(
      extEvData: ExtEvData
  ) extends InitializeServiceStateData

  val FALLBACK_EV_MOVEMENTS_STEM_DISTANCE: Long = 3600L
}

class ExtEvDataService(override val scheduler: ActorRef)
    extends SimonaService[ExtEvStateData](scheduler)
    with ExtDataSupport[ExtEvStateData] {

  override def init(
      initServiceData: ServiceStateData.InitializeServiceStateData
  ): Try[
    (
        ExtEvStateData,
        Option[SchedulerMessage.ScheduleTriggerMessage]
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
      case RegisterForEvDataMessage(evcs) =>
        Success(handleRegistrationRequest(sender(), evcs))
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
    * @param serviceStateData
    *   the current service state data of this service
    * @return
    *   an updated state data of this service that contains registration
    *   information if the registration has been carried out successfully
    */
  private def handleRegistrationRequest(
      agentToBeRegistered: ActorRef,
      evcs: UUID
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
            serviceStateData.uuidToActorRef + (evcs -> agentToBeRegistered)
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
      Option[SchedulerMessage.ScheduleTriggerMessage]
  ) = {
    serviceStateData.extEvMessage.getOrElse(
      throw ServiceException(
        "ExtEvDataActor was triggered without ExtEvMessage available"
      )
    ) match {
      case _: RequestEvcsFreeLots =>
        requestFreeLots(tick)
      case departingEvsRequest: RequestDepartingEvs =>
        requestDepartingEvs(tick, departingEvsRequest.departures)
      case arrivingEvsProvision: ProvideArrivingEvs =>
        handleArrivingEvs(tick, arrivingEvsProvision.arrivals)
    }
  }

  private def requestFreeLots(tick: Long)(implicit
      serviceStateData: ExtEvStateData
  ): (ExtEvStateData, Option[ScheduleTriggerMessage]) = {
    serviceStateData.uuidToActorRef.foreach { case (_, evcsActor) =>
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

  private def requestDepartingEvs(
      tick: Long,
      requestedDepartingEvs: java.util.Map[UUID, java.util.List[UUID]]
  )(implicit
      serviceStateData: ExtEvStateData
  ): (ExtEvStateData, Option[ScheduleTriggerMessage]) = {

    val departingEvResponses: Map[UUID, Option[Seq[EvModel]]] =
      requestedDepartingEvs.asScala.flatMap { case (evcs, departingEvs) =>
        serviceStateData.uuidToActorRef.get(evcs) match {
          case Some(evcsActor) =>
            evcsActor ! DepartingEvsRequest(tick, departingEvs.asScala.toSeq)

            Some(evcs -> None)

          case None =>
            log.warning(
              "A corresponding actor ref for UUID {} could not be found",
              evcs
            )

            None
        }
      }.toMap

    // if there are no departing evs during this tick,
    // we're sending response right away
    if (departingEvResponses.isEmpty)
      serviceStateData.extEvData.queueExtResponseMsg(new ProvideDepartingEvs())

    (
      serviceStateData.copy(
        extEvMessage = None,
        departingEvResponses = departingEvResponses
      ),
      None
    )
  }

  private def handleArrivingEvs(
      tick: Long,
      allArrivingEvs: java.util.Map[UUID, java.util.List[EvModel]]
  )(implicit
      serviceStateData: ExtEvStateData
  ): (ExtEvStateData, Option[ScheduleTriggerMessage]) = {

    allArrivingEvs.asScala.foreach { case (evcs, arrivingEvs) =>
      serviceStateData.uuidToActorRef.get(evcs) match {
        case Some(evcsActor) =>
          evcsActor ! ProvideEvDataMessage(
            tick,
            ArrivingEvsData(arrivingEvs.asScala.toSeq)
          )

          // schedule activation of participant
          scheduler ! ScheduleTriggerMessage(
            ActivityStartTrigger(tick),
            evcsActor
          )

        case None =>
          log.warning(
            "A corresponding actor ref for UUID {} could not be found",
            evcs
          )
          None

      }
    }

    (
      serviceStateData.copy(
        extEvMessage = None
      ),
      None
    )
  }

  override protected def handleDataMessage(
      extMsg: DataMessageFromExt
  )(implicit serviceStateData: ExtEvStateData): ExtEvStateData =
    extMsg match {
      case extEvMessage: EvDataMessageFromExt =>
        serviceStateData.copy(
          extEvMessage = Some(extEvMessage)
        )
    }

  override protected def handleDataResponseMessage(
      extResponseMsg: EvResponseMessage
  )(implicit serviceStateData: ExtEvStateData): ExtEvStateData = {
    extResponseMsg match {
      case DepartingEvsResponse(evcs, evModels) =>
        val updatedResponses = serviceStateData.departingEvResponses +
          (evcs -> Some(evModels.toList))

        if (updatedResponses.values.toSeq.contains(None)) {
          // responses are still incomplete
          serviceStateData.copy(
            departingEvResponses = updatedResponses
          )
        } else {
          // all responses received, forward them to external simulation in a bundle
          val departingEvs = updatedResponses.values.flatten.flatten

          serviceStateData.extEvData.queueExtResponseMsg(
            new ProvideDepartingEvs(departingEvs.toList.asJava)
          )

          serviceStateData.copy(
            departingEvResponses = Map.empty
          )
        }
      case FreeLotsResponse(evcs, freeLots) =>
        val updatedFreeLots = serviceStateData.freeLots +
          (evcs -> Some(freeLots))

        if (updatedFreeLots.values.toSeq.contains(None)) {
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
    }
  }
}
