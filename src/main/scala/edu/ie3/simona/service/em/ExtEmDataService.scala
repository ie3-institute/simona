/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.em.ExtEmData
import edu.ie3.simona.api.data.em.ontology.{
  EmDataMessageFromExt,
  ProvideEmSetPointData,
}
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  IssuePowerControl,
  SetPointFlexRequest,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ExtEmDataServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationSuccessfulMessage,
  WrappedRegistrationSuccessfulMessage,
}
import edu.ie3.simona.ontology.messages.services.{DataMessage, ServiceMessage}
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.em.ExtEmDataService.{
  ExtEmDataStateData,
  InitExtEmData,
}
import edu.ie3.simona.service.{ExtDataSupport, SimonaService}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.{ActorContext, Props, ActorRef => ClassicRef}
import squants.Power
import squants.energy.Kilowatts

import java.util.UUID
import scala.jdk.CollectionConverters.{CollectionHasAsScala, MapHasAsScala}
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

object ExtEmDataService {

  def props(scheduler: ClassicRef): Props =
    Props(
      new ExtEmDataService(scheduler: ClassicRef)
    )

  final case class ExtEmDataStateData(
      extEmData: ExtEmData,
      subscribers: List[UUID] = List.empty,
      uuidToActorRef: Map[UUID, ActorRef[EmAgent.Request]] =
        Map.empty[UUID, ActorRef[EmAgent.Request]], // subscribers in SIMONA
      uuidToAdapterRef: Map[UUID, ActorRef[FlexRequest]] =
        Map.empty[UUID, ActorRef[FlexRequest]], // subscribers in SIMONA
      extEmDataMessage: Option[EmDataMessageFromExt] = None,
  ) extends ServiceBaseStateData

  case class InitExtEmData(
      extEmData: ExtEmData
  ) extends InitializeServiceStateData

  final case class WrappedIssuePowerControl(
      issuePowerControl: IssuePowerControl
  ) extends EmAgent.Request
}

final case class ExtEmDataService(
    override val scheduler: ClassicRef
) extends SimonaService[ExtEmDataStateData](scheduler)
    with ExtDataSupport[ExtEmDataStateData] {

  /** Initialize the concrete service implementation using the provided
    * initialization data. This method should perform all heavyweight tasks
    * before the actor becomes ready. The return values are a) the state data of
    * the initialized service and b) optional triggers that should be send to
    * the [[edu.ie3.simona.scheduler.Scheduler]] together with the completion
    * message that is send in response to the trigger that is send to start the
    * initialization process
    *
    * @param initServiceData
    *   the data that should be used for initialization
    * @return
    *   the state data of this service and optional tick that should be included
    *   in the completion message
    */
  override def init(
      initServiceData: InitializeServiceStateData
  ): Try[(ExtEmDataStateData, Option[Long])] = initServiceData match {
    case InitExtEmData(extEmData) =>
      val emDataInitializedStateData = ExtEmDataStateData(
        extEmData,
        subscribers = extEmData.getControlledEms.asScala.toList,
      )
      Success(
        emDataInitializedStateData,
        None,
      )

    case invalidData =>
      Failure(
        new InitializationException(
          s"Provided init data '${invalidData.getClass.getSimpleName}' for ExtEmDataService are invalid!"
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
  override protected def handleRegistrationRequest(
      registrationMessage: ServiceMessage.ServiceRegistrationMessage
  )(implicit serviceStateData: ExtEmDataStateData): Try[ExtEmDataStateData] =
    registrationMessage match {
      case ExtEmDataServiceRegistrationMessage(
            modelUuid,
            requestingActor,
            flexAdapter,
          ) =>
        Success(
          handleEmRegistrationRequest(modelUuid, requestingActor, flexAdapter)
        )
      case invalidMessage =>
        Failure(
          InvalidRegistrationRequestException(
            s"A primary service provider is not able to handle registration request '$invalidMessage'."
          )
        )
    }

  private def handleEmRegistrationRequest(
      modelUuid: UUID,
      modelActorRef: ActorRef[EmAgent.Request],
      flexAdapterRef: ActorRef[FlexRequest],
  )(implicit serviceStateData: ExtEmDataStateData): ExtEmDataStateData = {
    if (serviceStateData.subscribers.contains(modelUuid)) {
      modelActorRef ! WrappedRegistrationSuccessfulMessage(
        RegistrationSuccessfulMessage(self, None)
      )
      serviceStateData.copy(
        uuidToActorRef =
          serviceStateData.uuidToActorRef + (modelUuid -> modelActorRef),
        uuidToAdapterRef =
          serviceStateData.uuidToAdapterRef + (modelUuid -> flexAdapterRef),
      )
    } else {
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
  override protected def announceInformation(tick: Long)(implicit
      serviceStateData: ExtEmDataStateData,
      ctx: ActorContext,
  ): (ExtEmDataStateData, Option[Long]) = {
    //log.info(s"[$tick] ExtEmDataService should announce information!")
    serviceStateData.extEmDataMessage.getOrElse(
      throw ServiceException(
        "ExtPrimaryDataService was triggered without ExtPrimaryDataMessage available"
      )
    ) match {
      case providedEmData: ProvideEmSetPointData =>
        announceEmData(tick, providedEmData)(
          serviceStateData,
          ctx,
        )
    }
  }

  private def announceEmData(
      tick: Long,
      emDataMessage: ProvideEmSetPointData,
  )(implicit serviceStateData: ExtEmDataStateData, ctx: ActorContext): (
      ExtEmDataStateData,
      Option[Long],
  ) = {
    //log.info(s"[$tick] ExtEmDataService got msg = $emDataMessage!")
    val actorToEmData = emDataMessage.emData.asScala.flatMap {
      case (agent, emDataPerAgent) =>
        serviceStateData.uuidToAdapterRef
          .get(agent)
          .map((_, convertToSetPoint(emDataPerAgent)))
          .orElse {
            log.warning(
              "A corresponding actor ref for UUID {} could not be found",
              agent,
            )
            None
          }
    }

    val maybeNextTick = emDataMessage.maybeNextTick.toScala.map(Long2long)

    if (actorToEmData.nonEmpty) {
      actorToEmData.foreach { case (actor, setPoint) =>
        actor ! SetPointFlexRequest(
          tick,
          setPoint,
          maybeNextTick,
        )
      }
    }
    (
      serviceStateData.copy(extEmDataMessage = None),
      None,
    )
  }

  private def convertToSetPoint(
      value: PValue
  ): Power = {
    Kilowatts(value.getP.get.getValue.doubleValue())
  }

  /** Handle a message from outside the simulation
    *
    * @param extMsg
    *   the external incoming message
    * @param serviceStateData
    *   the current state data of this service
    * @return
    *   the updated state data
    */
  override protected def handleDataMessage(
      extMsg: DataMessageFromExt
  )(implicit serviceStateData: ExtEmDataStateData): ExtEmDataStateData = {
    extMsg match {
      case extEmDataMessage: EmDataMessageFromExt =>
        serviceStateData.copy(
          extEmDataMessage = Some(extEmDataMessage)
        )
    }
  }

  /** Handle a message from inside SIMONA sent to external
    *
    * @param extResponseMsg
    *   the external incoming message
    * @param serviceStateData
    *   the current state data of this service
    * @return
    *   the updated state data
    */
  override protected def handleDataResponseMessage(extResponseMsg: DataMessage)(
      implicit serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = serviceStateData
}
