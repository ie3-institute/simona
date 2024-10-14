/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.ev

import edu.ie3.simona.api.data.ev.ExtEvData
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology._
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{CriticalFailureException, InitializationException, ServiceException}
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.ontology.messages.services.DataMessage
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ServiceRegistrationMessage
import edu.ie3.simona.service.ServiceStateData.{InitializeServiceStateData, ServiceBaseStateData}
import edu.ie3.simona.service.ev.ExtEvDataService.{ExtEvStateData, InitExtEvData}
import edu.ie3.simona.service.{ExtDataSupport, ServiceStateData, SimonaService}
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.{ActorContext, ActorRef, Props}

import java.util.UUID
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
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
      freeLots: ReceiveDataMap[UUID, Int] = ReceiveDataMap.empty,
      departingEvResponses: ReceiveDataMap[UUID, Seq[EvModelWrapper]] =
        ReceiveDataMap.empty,
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
        Option[Long],
    )
  ] =
    initServiceData match {
      case InitExtEvData(extEvData) =>
        val evInitializedStateData = ExtEvStateData(
          extEvData
        )

        Success(
          evInitializedStateData,
          None,
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
      evcs: UUID,
  )(implicit
      serviceStateData: ExtEvStateData
  ): ExtEvStateData = {
    log.debug(
      "Received ev movement service registration from {} for [Evcs:{}]",
      agentToBeRegistered.path.name,
      evcs,
    )

    serviceStateData.uuidToActorRef.get(evcs) match {
      case None =>
        // Actor is not registered yet
        // (not sending confirmation message yet, because we're waiting
        // for MobSim to tell us what the first tick is going to be)
        serviceStateData.copy(
          uuidToActorRef =
            serviceStateData.uuidToActorRef + (evcs -> agentToBeRegistered)
        )
      case Some(_) =>
        // actor is already registered, do nothing
        log.warning(
          "Sending actor {} is already registered",
          agentToBeRegistered,
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
  )(implicit serviceStateData: ExtEvStateData, ctx: ActorContext): (
      ExtEvStateData,
      Option[Long],
  ) = {
    def asScala[E]
        : java.util.Map[UUID, java.util.List[E]] => Map[UUID, Seq[E]] = map =>
      map.asScala.view.mapValues(_.asScala.toSeq).toMap

    serviceStateData.extEvMessage.getOrElse(
      throw ServiceException(
        "ExtEvDataService was triggered without ExtEvMessage available"
      )
    ) match {
      case _: RequestCurrentPrices =>
        requestCurrentPrices()
      case _: RequestEvcsFreeLots =>
        requestFreeLots(tick)
      case departingEvsRequest: RequestDepartingEvs =>
        requestDepartingEvs(tick, asScala(departingEvsRequest.departures))
      case arrivingEvsProvision: ProvideArrivingEvs =>
        handleArrivingEvs(
          tick,
          asScala(arrivingEvsProvision.arrivals),
          arrivingEvsProvision.maybeNextTick.toScala.map(Long2long),
        )(
          serviceStateData
        )
    }
  }

  private def requestCurrentPrices()(implicit
      serviceStateData: ExtEvStateData
  ): (ExtEvStateData, Option[Long]) = {
    // currently not supported, return dummy
    val dummyPrice = double2Double(0d)
    val prices = serviceStateData.uuidToActorRef.map { case (evcs, _) =>
      evcs -> dummyPrice
    }
    serviceStateData.extEvData.queueExtResponseMsg(
      new ProvideCurrentPrices(prices.asJava)
    )

    (
      serviceStateData.copy(
        extEvMessage = None
      ),
      None,
    )
  }

  private def requestFreeLots(tick: Long)(implicit
      serviceStateData: ExtEvStateData
  ): (ExtEvStateData, Option[Long]) = {
    serviceStateData.uuidToActorRef.foreach { case (_, evcsActor) =>
      evcsActor ! EvFreeLotsRequest(tick)
    }

    val freeLots =
      serviceStateData.uuidToActorRef.map { case (evcs, _) =>
        evcs
      }.toSet

    // if there are no evcs, we're sending response right away
    if (freeLots.isEmpty)
      serviceStateData.extEvData.queueExtResponseMsg(new ProvideEvcsFreeLots())

    (
      serviceStateData.copy(
        extEvMessage = None,
        freeLots = ReceiveDataMap(freeLots),
      ),
      None,
    )
  }

  private def requestDepartingEvs(
      tick: Long,
      requestedDepartingEvs: Map[UUID, Seq[UUID]],
  )(implicit
      serviceStateData: ExtEvStateData
  ): (ExtEvStateData, Option[Long]) = {

    val departingEvResponses =
      requestedDepartingEvs.flatMap { case (evcs, departingEvs) =>
        serviceStateData.uuidToActorRef.get(evcs) match {
          case Some(evcsActor) =>
            evcsActor ! DepartingEvsRequest(tick, departingEvs)

            Some(evcs)

          case None =>
            log.warning(
              "A corresponding actor ref for UUID {} could not be found",
              evcs,
            )

            None
        }
      }

    // if there are no departing evs during this tick,
    // we're sending response right away
    if (departingEvResponses.isEmpty)
      serviceStateData.extEvData.queueExtResponseMsg(new ProvideDepartingEvs())

    (
      serviceStateData.copy(
        extEvMessage = None,
        departingEvResponses = ReceiveDataMap(departingEvResponses.toSet),
      ),
      None,
    )
  }

  private def handleArrivingEvs(
      tick: Long,
      allArrivingEvs: Map[UUID, Seq[EvModel]],
      maybeNextTick: Option[Long],
  )(implicit
      serviceStateData: ExtEvStateData
  ): (ExtEvStateData, Option[Long]) = {

    if (tick == INIT_SIM_TICK) {

      maybeNextTick.getOrElse(
        throw new CriticalFailureException(
          s"After initialization, a first simulation tick needs to be provided by the external mobility simulation."
        )
      )

      serviceStateData.uuidToActorRef.foreach { case (_, actor) =>
        actor ! RegistrationSuccessfulMessage(
          self,
          maybeNextTick,
        )
      }

    } else {
      serviceStateData.uuidToActorRef.foreach { case (evcs, actor) =>
        val evs =
          allArrivingEvs.getOrElse(evcs, Seq.empty)

        actor ! ProvideEvDataMessage(
          tick,
          self,
          ArrivingEvs(evs.map(EvModelWrapper.apply)),
          maybeNextTick,
        )
      }
    }

    (
      serviceStateData.copy(
        extEvMessage = None
      ),
      // We still don't return the next tick because departures might come earlier
      None,
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
      extResponseMsg: DataMessage
  )(implicit serviceStateData: ExtEvStateData): ExtEvStateData = {
    extResponseMsg match {
      case DepartingEvsResponse(evcs, evModels) =>
        val updatedResponses =
          serviceStateData.departingEvResponses.addData(evcs, evModels)

        if (updatedResponses.nonComplete) {
          // responses are still incomplete
          serviceStateData.copy(
            departingEvResponses = updatedResponses
          )
        } else {
          // all responses received, forward them to external simulation in a bundle
          val departingEvs =
            updatedResponses.receivedData.values.flatten.map(_.unwrap())

          serviceStateData.extEvData.queueExtResponseMsg(
            new ProvideDepartingEvs(departingEvs.toList.asJava)
          )

          serviceStateData.copy(
            departingEvResponses = ReceiveDataMap.empty
          )
        }
      case FreeLotsResponse(evcs, freeLots) =>
        val updatedFreeLots = serviceStateData.freeLots.addData(evcs, freeLots)

        if (updatedFreeLots.nonComplete) {
          // responses are still incomplete
          serviceStateData.copy(
            freeLots = updatedFreeLots
          )
        } else {
          // all responses received, forward them to external simulation in a bundle
          val freeLotsResponse = updatedFreeLots.receivedData
            .filter { case (_, freeLotsCount) =>
              freeLotsCount > 0
            }
            .map { case (evcs, freeLotsCount) =>
              evcs -> int2Integer(freeLotsCount)
            }

          serviceStateData.extEvData.queueExtResponseMsg(
            new ProvideEvcsFreeLots(freeLotsResponse.asJava)
          )

          serviceStateData.copy(
            freeLots = ReceiveDataMap.empty
          )
        }
    }
  }
}
