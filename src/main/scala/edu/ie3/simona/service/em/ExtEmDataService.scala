/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.em.model.{EmSetPointResult, FlexRequestResult}
import edu.ie3.simona.api.data.em.ontology.{RequestEmCompletion, _}
import edu.ie3.simona.api.data.em.{ExtEmDataConnection, NoSetPointValue}
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.EmMessage
import edu.ie3.simona.ontology.messages.services.EmMessage.{WrappedFlexRequest, WrappedFlexResponse}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{RegisterForEmDataService, ServiceRegistrationMessage, ServiceResponseMessage}
import edu.ie3.simona.service.{ExtDataSupport, SimonaService}
import edu.ie3.simona.service.ServiceStateData.{InitializeServiceStateData, ServiceBaseStateData}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.simona.util.{ReceiveDataMap, ReceiveHierarchicalDataMap}
import edu.ie3.util.quantities.PowerSystemUnits.KILOWATT
import edu.ie3.util.quantities.QuantityUtils._
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.slf4j.{Logger, LoggerFactory}
import squants.Power
import squants.energy.Kilowatts
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Power => PsdmPower}
import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsJava, MapHasAsScala}
import scala.jdk.OptionConverters.{RichOption, RichOptional}
import scala.util.{Failure, Success, Try}

object ExtEmDataService
    extends SimonaService[EmMessage]
    with ExtDataSupport[EmMessage] {

  private val log: Logger = LoggerFactory.getLogger(ExtEmDataService.getClass)

  override type S = ExtEmDataStateData

  implicit class SquantsToQuantity(private val value: Power) {
    def toQuantity: ComparableQuantity[PsdmPower] = value.toKilowatts.asKiloWatt
  }

  implicit class quantityToSquants(
      private val value: ComparableQuantity[PsdmPower]
  ) {
    def toSquants: Power = Kilowatts(value.to(KILOWATT).getValue.doubleValue())
  }

  def emServiceResponseAdapter(
      emService: ActorRef[EmMessage],
      receiver: Option[ActorRef[FlexResponse]],
      self: UUID,
  )(implicit ctx: ActorContext[EmAgent.Request]): ActorRef[FlexResponse] = {

    val request = Behaviors.receiveMessagePartial[FlexResponse] {
      case response: FlexResponse =>
        emService ! WrappedFlexResponse(
          response,
          receiver.map(Right(_)).getOrElse(Left(self)),
        )

        Behaviors.same
    }

    ctx.spawn(request, "response-adapter")
  }

  def emServiceRequestAdapter(
      emService: ActorRef[EmMessage],
      receiver: ActorRef[FlexRequest],
  )(implicit ctx: ActorContext[EmAgent.Request]): ActorRef[FlexRequest] = {
    val response = Behaviors.receiveMessagePartial[FlexRequest] {
      case request: FlexRequest =>
        emService ! WrappedFlexRequest(
          request,
          receiver,
        )

        Behaviors.same
    }

    ctx.spawn(response, "request-adapter")
  }

  final case class ExtEmDataStateData(
      extEmDataConnection: ExtEmDataConnection,
      startTime: ZonedDateTime,
      tick: Long = INIT_SIM_TICK,
      emHierarchy: EmHierarchy = EmHierarchy(),
      uuidToFlexAdapter: Map[UUID, ActorRef[FlexRequest]] = Map.empty,
      flexAdapterToUuid: Map[ActorRef[FlexRequest], UUID] = Map.empty,
      extEmDataMessage: Option[EmDataMessageFromExt] = None,
      toSchedule: Map[UUID, ScheduleFlexActivation] = Map.empty,
      flexRequest: ReceiveHierarchicalDataMap[UUID, Boolean] =
        ReceiveHierarchicalDataMap.empty(false),
      flexOptionResponse: ReceiveHierarchicalDataMap[
        UUID,
        (UUID, FlexOptionsResult),
      ] = ReceiveHierarchicalDataMap.empty,
      setPointResponse: ReceiveDataMap[UUID, EmSetPointResult] =
        ReceiveDataMap.empty,
  ) extends ServiceBaseStateData

  final case class EmHierarchy(
      refToUuid: Map[ActorRef[EmAgent.Request], UUID] = Map.empty,
      uuidToRef: Map[UUID, ActorRef[EmAgent.Request]] = Map.empty,
      uuidToFlexResponse: Map[UUID, ActorRef[FlexResponse]] = Map.empty,
      flexResponseToUuid: Map[ActorRef[FlexResponse], UUID] = Map.empty,
  ) {
    def add(model: UUID, ref: ActorRef[EmAgent.Request]): EmHierarchy = copy(
      uuidToRef = uuidToRef + (model -> ref),
      refToUuid = refToUuid + (ref -> model),
    )

    def add(
        model: UUID,
        ref: ActorRef[EmAgent.Request],
        parent: ActorRef[FlexResponse],
    ): EmHierarchy = {

      copy(
        uuidToRef = uuidToRef + (model -> ref),
        refToUuid = refToUuid + (ref -> model),
        uuidToFlexResponse = uuidToFlexResponse + (model -> parent),
        flexResponseToUuid = flexResponseToUuid + (parent -> model),
      )
    }

    def getUuid(ref: ActorRef[FlexResponse]): UUID =
      flexResponseToUuid(ref)

    def getResponseRef(uuid: UUID): Option[ActorRef[FlexResponse]] =
      uuidToFlexResponse.get(uuid)
  }

  case class InitExtEmData(
      extEmData: ExtEmDataConnection,
      startTime: ZonedDateTime,
  ) extends InitializeServiceStateData

  override protected def handleServiceResponse(
      serviceResponse: ServiceResponseMessage
  )(implicit ctx: ActorContext[EmMessage]): Unit = serviceResponse match {
    case WrappedFlexResponse(
          scheduleFlexActivation: ScheduleFlexActivation,
          receiver,
        ) =>
      ctx.log.info(s"Received response message: $scheduleFlexActivation")

      receiver match {
        case Right(ref) =>
          log.info(s"Forwarding the message to: $ref")
          ref ! scheduleFlexActivation
        case Left(_) =>
          log.info(s"Unlocking msg: $scheduleFlexActivation")

          scheduleFlexActivation.scheduleKey.foreach(_.unlock())
      }

  }

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
    case InitExtEmData(extEmDataConnection, startTime) =>
      val emDataInitializedStateData =
        ExtEmDataStateData(extEmDataConnection, startTime)
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
      registrationMessage: ServiceRegistrationMessage
  )(implicit
      serviceStateData: ExtEmDataStateData,
      ctx: ActorContext[EmMessage],
  ): Try[ExtEmDataStateData] =
    registrationMessage match {
      case RegisterForEmDataService(
            modelUuid,
            requestingActor,
            flexAdapter,
            parentEm,
            parentUuid,
          ) =>
        Success(
          handleEmRegistrationRequest(
            modelUuid,
            requestingActor,
            flexAdapter,
            parentEm,
            parentUuid,
          )
        )
      case invalidMessage =>
        Failure(
          InvalidRegistrationRequestException(
            s"An external em service is not able to handle registration request '$invalidMessage'."
          )
        )
    }

  private def handleEmRegistrationRequest(
      modelUuid: UUID,
      modelActorRef: ActorRef[EmAgent.Request],
      flexAdapter: ActorRef[FlexRequest],
      parentEm: Option[ActorRef[FlexResponse]],
      parentUuid: Option[UUID],
  )(implicit serviceStateData: ExtEmDataStateData): ExtEmDataStateData = {
    val hierarchy = serviceStateData.emHierarchy

    val updatedFlexRequest =
      serviceStateData.flexRequest.updateStructure(parentUuid, modelUuid)
    val updatedFlexResponse =
      serviceStateData.flexOptionResponse.updateStructure(parentUuid, modelUuid)

    val updatedHierarchy = parentEm match {
      case Some(parent) =>
        hierarchy.add(modelUuid, modelActorRef, parent)
      case None =>
        hierarchy.add(modelUuid, modelActorRef, modelActorRef)
    }

    if (parentEm.isEmpty) {
      flexAdapter ! FlexActivation(INIT_SIM_TICK)
    }

    serviceStateData.copy(
      emHierarchy = updatedHierarchy,
      uuidToFlexAdapter =
        serviceStateData.uuidToFlexAdapter + (modelUuid -> flexAdapter),
      flexAdapterToUuid =
        serviceStateData.flexAdapterToUuid + (flexAdapter -> modelUuid),
      flexRequest = updatedFlexRequest,
      flexOptionResponse = updatedFlexResponse,
    )
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
      ctx: ActorContext[EmMessage],
  ): (ExtEmDataStateData, Option[Long]) = {
    val updatedTick = serviceStateData.copy(tick = tick)

    val updatedStateData = serviceStateData.extEmDataMessage.getOrElse(
      throw ServiceException(
        "ExtEmDataService was triggered without ExtEmDataMessage available"
      )
    ) match {
      case requestEmCompletion : RequestEmCompletion =>

        if (requestEmCompletion.tick != tick) {
          log.warn(s"Received completion request for tick '${requestEmCompletion.tick}' in tick '$tick'.")
          serviceStateData

        } else {
          ctx.log.info(s"Receive a request for completion for tick '$tick'.")

          val setEms = serviceStateData.setPointResponse.getExpectedKeys

          serviceStateData.uuidToFlexAdapter.foreach { case (uuid, adapter) if !setEms.contains(uuid) =>
              adapter ! IssueNoControl(tick)
          }

          serviceStateData.extEmDataConnection.queueExtResponseMsg(new EmCompletion())

          serviceStateData.copy(setPointResponse = ReceiveDataMap.empty)
        }

      case provideFlexRequests: ProvideFlexRequestData =>
        ctx.log.warn(s"$provideFlexRequests")

        // entities for which flex options are requested
        val emEntities: Set[UUID] = provideFlexRequests
          .flexRequests()
          .asScala
          .flatMap { case (_, v) => v.asScala }
          .toSet
        val uuidToRef = serviceStateData.uuidToFlexAdapter
        val refs = emEntities.map(uuidToRef)

        log.warn(s"Em refs: $refs")
        refs.foreach(_ ! FlexActivation(tick))

        val updatedFlexRequest =
          serviceStateData.flexRequest.addSubKeysToExpectedKeys(emEntities)
        val updatedFlexOptionResponse =
          serviceStateData.flexOptionResponse.addExpectedKeys(emEntities)

        updatedTick.copy(
          extEmDataMessage = None,
          flexRequest = updatedFlexRequest,
          flexOptionResponse = updatedFlexOptionResponse,
        )

      case provideFlexOptions: ProvideEmFlexOptionData =>
        ctx.log.warn(s"$provideFlexOptions")

        announceFlexOptions(provideFlexOptions)(updatedTick, ctx)

      case providedEmData: ProvideEmSetPointData =>
        ctx.log.warn(s"$providedEmData")

        announceEmSetPoints(tick, providedEmData)(updatedTick, ctx)

      case requestEmSetPoints: RequestEmSetPoints =>
        ctx.log.warn(s"$requestEmSetPoints")

        val uuids = requestEmSetPoints.emEntities().asScala.toSet

        updatedTick.copy(
          extEmDataMessage = None,
          setPointResponse = ReceiveDataMap(uuids),
        )
    }

    (updatedStateData, None)
  }

  private def announceFlexOptions(
      provideFlexOptions: ProvideEmFlexOptionData
  )(implicit
      serviceStateData: ExtEmDataStateData,
      ctx: ActorContext[EmMessage],
  ): ExtEmDataStateData = {
    val hierarchy = serviceStateData.emHierarchy

    provideFlexOptions
      .flexOptions()
      .asScala
      .foreach { case (agent, flexOptions) =>
        hierarchy.getResponseRef(agent) match {
          case Some(receiver) =>
            flexOptions.asScala.foreach { case (sender, options) =>
              receiver ! ProvideFlexOptions(
                sender,
                MinMaxFlexOptions(
                  options.pRef.toSquants,
                  options.pMin.toSquants,
                  options.pMax.toSquants,
                ),
              )
            }

          case None =>
            ctx.log.warn(s"No em agent with uuid '$agent' registered!")
        }
      }

    serviceStateData.copy(extEmDataMessage = None)
  }

  private def announceEmSetPoints(
      tick: Long,
      provideEmSetPointData: ProvideEmSetPointData,
  )(implicit
      serviceStateData: ExtEmDataStateData,
      ctx: ActorContext[EmMessage],
  ): ExtEmDataStateData = {

    provideEmSetPointData
      .emData()
      .asScala
      .foreach { case (agent, emSetPoint) =>
        serviceStateData.uuidToFlexAdapter.get(agent) match {
          case Some(receiver) =>
            emSetPoint match {
              case _: NoSetPointValue =>
                receiver ! IssueNoControl(tick)
              case _ =>
                val power =
                  emSetPoint.getP.toScala.map(_.toSquants).getOrElse(zeroKW)

                receiver ! IssuePowerControl(tick, power)
            }

          case None =>
            ctx.log.warn(s"No em agent with uuid '$agent' registered!")
        }
      }

    serviceStateData.copy(extEmDataMessage = None)
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
  )(implicit
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = {
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
  override protected def handleDataResponseMessage(
      extResponseMsg: ServiceResponseMessage
  )(implicit
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = extResponseMsg match {
    case WrappedFlexResponse(flexResponse, receiver) =>
      flexResponse match {
        case scheduleFlexActivation: ScheduleFlexActivation =>
          handleScheduleFlexActivation(scheduleFlexActivation, receiver)

        case options: ProvideFlexOptions =>
          handleFlexProvision(options, receiver)

        case completion: FlexCompletion =>
          receiver.map(_ ! completion)
          log.warn(s"Completion: $completion")

          serviceStateData
      }

    case WrappedFlexRequest(issueFlexControl: IssueFlexControl, receiver) =>
      handleFlexControl(issueFlexControl, receiver)

    case WrappedFlexRequest(flexActivation: FlexActivation, receiver) =>
      handleFlexActivation(flexActivation, receiver)
  }

  private def handleFlexProvision(
      provideFlexOptions: ProvideFlexOptions,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(implicit
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = {

    if (serviceStateData.tick == INIT_SIM_TICK) {
      receiver match {
        case Right(otherRef) =>
          otherRef ! provideFlexOptions

        case Left(self: UUID) =>
          serviceStateData.uuidToFlexAdapter(self) ! IssuePowerControl(
            INIT_SIM_TICK,
            zeroKW,
          )
      }

      serviceStateData
    } else {

      val uuid = receiver match {
        case Right(otherRef) =>
          serviceStateData.emHierarchy.getUuid(otherRef)
        case Left(self: UUID) =>
          self
      }

      val updated = provideFlexOptions match {
        case ProvideFlexOptions(modelUuid, MinMaxFlexOptions(ref, min, max)) =>
          serviceStateData.flexOptionResponse.addData(
            modelUuid,
            (
              uuid,
              new FlexOptionsResult(
                serviceStateData.startTime, // TODO: Fix this
                modelUuid,
                min.toQuantity,
                ref.toQuantity,
                max.toQuantity,
              ),
            ),
          )

        case _ =>
          serviceStateData.flexOptionResponse
      }

      log.warn(s"Updated data map: $updated")

      if (updated.hasCompletedKeys) {
        // all responses received, forward them to external simulation in a bundle

        val (data, updatedFlexOptionResponse) = updated.getFinishedData

        serviceStateData.extEmDataConnection.queueExtResponseMsg(
          new FlexOptionsResponse(
            data.map { case (entity, (_, value)) => entity -> value }.asJava
          )
        )

        serviceStateData.copy(
          flexOptionResponse = updatedFlexOptionResponse
        )
      } else {
        // responses are still incomplete
        serviceStateData.copy(
          flexOptionResponse = updated
        )
      }
    }
  }

  private def handleScheduleFlexActivation(
      scheduleFlexActivation: ScheduleFlexActivation,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(implicit
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = {
    log.warn(s"Flex activation: $scheduleFlexActivation")

    receiver match {
      case Right(ref) =>
        if (scheduleFlexActivation.tick == INIT_SIM_TICK) {
          log.warn(s"$ref: $scheduleFlexActivation")
          ref ! scheduleFlexActivation
        } else {
          log.warn(s"$scheduleFlexActivation not handled!")
        }

      case Left(uuid) =>
        if (scheduleFlexActivation.tick == INIT_SIM_TICK) {
          serviceStateData.uuidToFlexAdapter(uuid) ! FlexActivation(
            INIT_SIM_TICK
          )
        } else {
          log.warn(s"$scheduleFlexActivation not handled!")
        }
    }

    serviceStateData
  }

  private def handleFlexActivation(
      flexActivation: FlexActivation,
      receiver: ActorRef[FlexRequest],
  )(implicit
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = {

    if (flexActivation.tick == INIT_SIM_TICK) {
      receiver ! flexActivation

      serviceStateData
    } else {
      val uuid = serviceStateData.flexAdapterToUuid(receiver)

      log.warn(s"Receiver: $uuid")

      val updated = serviceStateData.flexRequest.addData(
        uuid,
        true,
      )

      log.warn(s"$updated")

      if (updated.hasCompletedKeys) {

        val (dataMap, updatedFlexRequest) = updated.getFinishedData

        log.warn(s"Data to be send: $dataMap")

        val map = dataMap.map { case (key, _) =>
          key ->
            new FlexRequestResult(
              flexActivation.tick.toDateTime(serviceStateData.startTime),
              key,
            )
        }

        serviceStateData.extEmDataConnection.queueExtResponseMsg(new FlexRequestResponse(map.asJava))

        serviceStateData.copy(flexRequest = updatedFlexRequest)
      } else {
        serviceStateData.copy(flexRequest = updated)
      }
    }
  }

  private def handleFlexControl(
      issueFlexControl: IssueFlexControl,
      receiver: ActorRef[FlexRequest],
  )(implicit
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = {
    if (issueFlexControl.tick == INIT_SIM_TICK) {

      receiver ! issueFlexControl
      serviceStateData

    } else {
      val uuid = serviceStateData.flexAdapterToUuid(receiver)

      val updated = issueFlexControl match {
        case IssueNoControl(tick) =>
          serviceStateData.setPointResponse.addData(
            uuid,
            new EmSetPointResult(
              tick.toDateTime(serviceStateData.startTime),
              uuid,
              None.toJava,
            ),
          )

        case IssuePowerControl(tick, setPower) =>
          serviceStateData.setPointResponse.addData(
            uuid,
            new EmSetPointResult(
              tick.toDateTime(serviceStateData.startTime),
              uuid,
              Some(new PValue(setPower.toQuantity)).toJava,
            ),
          )
      }

      if (updated.nonComplete) {
        // responses are still incomplete
        serviceStateData.copy(
          setPointResponse = updated
        )
      } else {
        // all responses received, forward them to external simulation in a bundle

        serviceStateData.extEmDataConnection.queueExtResponseMsg(
          new EmSetPointDataResponse(updated.receivedData.asJava)
        )

        serviceStateData.copy(
          setPointResponse = ReceiveDataMap.empty
        )
      }
    }
  }
}
