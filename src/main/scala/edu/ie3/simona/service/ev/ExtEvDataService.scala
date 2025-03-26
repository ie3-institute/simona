/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.ev

import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology._
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{
  CriticalFailureException,
  InitializationException,
  ServiceException,
}
import edu.ie3.simona.model.participant2.evcs.EvModelWrapper
import edu.ie3.simona.ontology.messages.services.EvMessage
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  RegisterForEvDataMessage,
  ServiceRegistrationMessage,
  ServiceResponseMessage,
}
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.{ExtDataSupport, ServiceStateData, SimonaService}
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.slf4j.Logger

import java.util.UUID
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.{Failure, Success, Try}

object ExtEvDataService
    extends SimonaService[EvMessage]
    with ExtDataSupport[EvMessage, EvDataMessageFromExt] {

  override type S = ExtEvStateData

  final case class ExtEvStateData(
      extEvData: ExtEvDataConnection,
      uuidToActorRef: Map[UUID, ActorRef[ParticipantAgent.Request]] = Map.empty,
      extEvMessage: Option[EvDataMessageFromExt] = None,
      freeLots: ReceiveDataMap[UUID, Int] = ReceiveDataMap.empty,
      departingEvResponses: ReceiveDataMap[UUID, Seq[EvModelWrapper]] =
        ReceiveDataMap.empty,
  ) extends ServiceBaseStateData

  final case class InitExtEvData(
      extEvData: ExtEvDataConnection
  ) extends InitializeServiceStateData

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

  override def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(implicit
      serviceStateData: S,
      ctx: ActorContext[EvMessage],
  ): Try[S] =
    registrationMessage match {
      case RegisterForEvDataMessage(requestingActor, evcs) =>
        Success(handleRegistrationRequest(requestingActor, evcs))
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
      agentToBeRegistered: ActorRef[ParticipantAgent.Request],
      evcs: UUID,
  )(implicit
      serviceStateData: ExtEvStateData,
      ctx: ActorContext[EvMessage],
  ): ExtEvStateData = {
    ctx.log.debug(
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
        ctx.log.warn(
          "Sending actor {} is already registered",
          agentToBeRegistered,
        )
        serviceStateData
    }
  }

  override protected def announceInformation(
      tick: Long
  )(implicit
      serviceStateData: S,
      ctx: ActorContext[EvMessage],
  ): (
      S,
      Option[Long],
  ) = {
    def asScala[E]
        : java.util.Map[UUID, java.util.List[E]] => Map[UUID, Seq[E]] = map =>
      map.asScala.view.mapValues(_.asScala.toSeq).toMap

    implicit val log: Logger = ctx.log

    serviceStateData.extEvMessage
      .map {
        case _: RequestCurrentPrices =>
          requestCurrentPrices()
        case _: RequestEvcsFreeLots =>
          requestFreeLots(tick, ctx)
        case departingEvsRequest: RequestDepartingEvs =>
          requestDepartingEvs(
            tick,
            asScala(departingEvsRequest.departures),
            ctx,
          )
        case arrivingEvsProvision: ProvideArrivingEvs =>
          handleArrivingEvs(
            tick,
            asScala(arrivingEvsProvision.arrivals),
            arrivingEvsProvision.maybeNextTick.toScala.map(Long2long),
          )(
            serviceStateData,
            ctx,
          )
      }
      .getOrElse(
        throw ServiceException(
          "ExtEvDataService was triggered without ExtEvMessage available"
        )
      )
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

  private def requestFreeLots(tick: Long, ctx: ActorContext[EvMessage])(implicit
      serviceStateData: ExtEvStateData
  ): (ExtEvStateData, Option[Long]) = {
    serviceStateData.uuidToActorRef.foreach { case (_, evcsActor) =>
      evcsActor ! EvFreeLotsRequest(tick, ctx.self)
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
      ctx: ActorContext[EvMessage],
  )(implicit
      serviceStateData: ExtEvStateData,
      log: Logger,
  ): (ExtEvStateData, Option[Long]) = {

    val departingEvResponses =
      requestedDepartingEvs.flatMap { case (evcs, departingEvs) =>
        serviceStateData.uuidToActorRef.get(evcs) match {
          case Some(evcsActor) =>
            evcsActor ! DepartingEvsRequest(tick, departingEvs, ctx.self)

            Some(evcs)

          case None =>
            log.warn(
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
      serviceStateData: ExtEvStateData,
      ctx: ActorContext[EvMessage],
  ): (ExtEvStateData, Option[Long]) = {

    if (tick == INIT_SIM_TICK) {
      // During initialization, an empty ProvideArrivingEvs message
      // is sent, which includes the first relevant tick

      val nextTick = maybeNextTick.getOrElse(
        throw new CriticalFailureException(
          s"After initialization, a first simulation tick needs to be provided by the external mobility simulation."
        )
      )

      serviceStateData.uuidToActorRef.foreach { case (_, actor) =>
        actor ! RegistrationSuccessfulMessage(
          ctx.self,
          nextTick,
        )
      }

    } else {
      serviceStateData.uuidToActorRef.foreach { case (evcs, actor) =>
        val evs =
          allArrivingEvs.getOrElse(evcs, Seq.empty)

        actor ! DataProvision(
          tick,
          ctx.self,
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
      extMsg: EvDataMessageFromExt
  )(implicit serviceStateData: S): S =
    serviceStateData.copy(extEvMessage = Some(extMsg))

  override protected def handleDataResponseMessage(
      extResponseMsg: ServiceResponseMessage
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
