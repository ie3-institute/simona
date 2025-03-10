/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.em.model.FlexOptionValue
import edu.ie3.simona.api.data.em.ontology._
import edu.ie3.simona.api.data.em.{ExtEmDataConnection, NoSetPointValue}
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{CriticalFailureException, InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.EmMessage.{WrappedFlexRequest, WrappedFlexResponse}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{DataResponseMessage, RegisterForEmDataService}
import edu.ie3.simona.service.ServiceStateData.{InitializeServiceStateData, ServiceBaseStateData}
import edu.ie3.simona.service.em.ExtEmDataService.{ExtEmDataStateData, InitExtEmData}
import edu.ie3.simona.service.{ExtDataSupport, SimonaService}
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.util.quantities.PowerSystemUnits.KILOWATT
import edu.ie3.util.quantities.QuantityUtils._
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, ActorContext => TypedContext}
import org.apache.pekko.actor.{ActorContext, Props, ActorRef => ClassicRef}
import squants.Power
import squants.energy.Kilowatts
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Power => PsdmPower}
import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsJava, MapHasAsScala}
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

object ExtEmDataService {

  def props(scheduler: ClassicRef)(implicit simulationStart: ZonedDateTime): Props =
    Props(
      new ExtEmDataService(scheduler: ClassicRef)
    )

  def emServiceResponseAdapter(
      emService: ClassicRef,
      receiver: Option[ActorRef[FlexResponse]],
      self: ActorRef[FlexResponse],
  )(implicit ctx: TypedContext[EmAgent.Request]): ActorRef[FlexResponse] = {

    val request = Behaviors.receiveMessagePartial[FlexResponse] {
      case response: FlexResponse =>
        emService ! WrappedFlexResponse(
          response,
          receiver,
          Some(self)
        )

        Behaviors.same
    }

    ctx.spawn(request, "response-adapter")
  }

  def emServiceRequestAdapter(
      emService: ClassicRef,
      receiver: ActorRef[FlexRequest],
  )(implicit ctx: TypedContext[EmAgent.Request]): ActorRef[FlexRequest] = {
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
      emHierarchy: EmHierarchy = EmHierarchy(),
      uuidToFlexAdapter: Map[UUID, ActorRef[FlexRequest]] = Map.empty,
      flexAdapterToUuid: Map[ActorRef[FlexRequest], UUID] = Map.empty,
      extEmDataMessage: Option[EmDataMessageFromExt] = None,
      flexOptionResponse: ReceiveDataMap[UUID, FlexOptionsResult] =
        ReceiveDataMap.empty,
      setPointResponse: ReceiveDataMap[UUID, IssueFlexControl] =
        ReceiveDataMap.empty,
  ) extends ServiceBaseStateData

  final case class EmHierarchy(
      uncontrolledToRef: Map[UUID, ActorRef[EmAgent.Request]] = Map.empty,
      refToUncontrolled: Map[ActorRef[FlexResponse], UUID] = Map.empty,
      controlledToRef: Map[UUID, ActorRef[FlexResponse]] = Map.empty,
      refToControlled: Map[ActorRef[FlexResponse], UUID] = Map.empty,
      parentToControlled: Map[ActorRef[FlexResponse], List[UUID]] = Map.empty,
  ) {
    def add(model: UUID, ref: ActorRef[EmAgent.Request]): EmHierarchy = copy(
      uncontrolledToRef = uncontrolledToRef + (model -> ref),
      refToUncontrolled = refToUncontrolled + (ref -> model),
    )

    def add(
        model: UUID,
        ref: ActorRef[EmAgent.Request],
        parent: ActorRef[FlexResponse],
    ): EmHierarchy = {
      val hierarchy = parentToControlled.getOrElse(parent, List.empty)

      copy(
        controlledToRef = controlledToRef + (model -> ref),
        refToControlled = refToControlled + (ref -> model),
        parentToControlled =
          parentToControlled + (parent -> (hierarchy ++ List(model))),
      )
    }

    def getUuid(ref: ActorRef[FlexResponse]): UUID =
      refToUncontrolled.getOrElse(ref, refToControlled(ref))

    def getResponseRef(uuid: UUID): Option[ActorRef[FlexResponse]] =
      uncontrolledToRef.get(uuid) match {
        case Some(value) =>
          Some(value)
        case None =>
          controlledToRef.get(uuid)
      }

  }

  case class InitExtEmData(
      extEmData: ExtEmDataConnection
  ) extends InitializeServiceStateData
}

final case class ExtEmDataService(
    override val scheduler: ClassicRef
                                 )(
                                 implicit val simulationStart: ZonedDateTime,
) extends SimonaService[ExtEmDataStateData](scheduler)
    with ExtDataSupport[ExtEmDataStateData] {

  implicit class SquantsToQuantity(private val value: Power) {
    def toQuantity: ComparableQuantity[PsdmPower] = value.toKilowatts.asKiloWatt
  }

  implicit class quantityToSquants(
      private val value: ComparableQuantity[PsdmPower]
  ) {
    def toSquants: Power = Kilowatts(value.to(KILOWATT).getValue.doubleValue())
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
    case InitExtEmData(extEmDataConnection) =>
      val emDataInitializedStateData = ExtEmDataStateData(extEmDataConnection)
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
      case RegisterForEmDataService(
            modelUuid,
            requestingActor,
            flexAdapter,
            parentEm,
            _,
          ) =>
        Success(
          handleEmRegistrationRequest(
            modelUuid,
            requestingActor,
            flexAdapter,
            parentEm,
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
  )(implicit serviceStateData: ExtEmDataStateData): ExtEmDataStateData = {
    val hierarchy = serviceStateData.emHierarchy

    val updatedHierarchy = parentEm match {
      case Some(parent) =>
        hierarchy.add(modelUuid, modelActorRef, parent)
      case None =>
        hierarchy.add(modelUuid, modelActorRef)
    }

    serviceStateData.copy(
      emHierarchy = updatedHierarchy,
      uuidToFlexAdapter =
        serviceStateData.uuidToFlexAdapter + (modelUuid -> flexAdapter),
      flexAdapterToUuid =
        serviceStateData.flexAdapterToUuid + (flexAdapter -> modelUuid),
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
      ctx: ActorContext,
  ): (ExtEmDataStateData, Option[Long]) = {
    val updatedStateData = serviceStateData.extEmDataMessage.getOrElse(
      throw ServiceException(
        "ExtEMDataService was triggered without ExtEmDataMessage available"
      )
    ) match {
      case requestEmFlexResults: RequestEmFlexResults =>
        val uuids = requestEmFlexResults.emEntities().asScala.toSet

        val uuidToRef = serviceStateData.uuidToFlexAdapter
        uuids.map(uuidToRef).foreach(_ ! FlexActivation(tick))

        serviceStateData.copy(
          extEmDataMessage = None,
          flexOptionResponse = ReceiveDataMap(uuids),
        )

      case requestEmSetPoints: RequestEmSetPoints =>
        val uuids = requestEmSetPoints.emEntities().asScala.toSet

        serviceStateData.copy(
          extEmDataMessage = None,
          setPointResponse = ReceiveDataMap(uuids),
        )

      case provideFlexOptions: ProvideEmFlexOptionData =>
        announceFlexOptions(provideFlexOptions)

      case providedEmData: ProvideEmSetPointData =>
        announceEmSetPoints(tick, providedEmData)
    }

    (updatedStateData, None)
  }

  private def announceFlexOptions(
      provideFlexOptions: ProvideEmFlexOptionData
  )(implicit
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = {
    val hierarchy = serviceStateData.emHierarchy

    provideFlexOptions
      .flexOptions()
      .asScala
      .foreach { case (agent, flexOption: FlexOptionValue) =>
        hierarchy.getResponseRef(agent) match {
          case Some(receiver) =>
            receiver ! ProvideMinMaxFlexOptions(
              flexOption.sender,
              flexOption.pRef.toSquants,
              flexOption.pMin.toSquants,
              flexOption.pMax.toSquants,
            )

          case None =>
            log.warning(s"No em agent with uuid '$agent' registered!")
        }
      }

    serviceStateData.copy(extEmDataMessage = None)
  }

  private def announceEmSetPoints(
      tick: Long,
      provideEmSetPointData: ProvideEmSetPointData,
  )(implicit serviceStateData: ExtEmDataStateData): ExtEmDataStateData = {

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
            log.warning(s"No em agent with uuid '$agent' registered!")
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
  override protected def handleDataResponseMessage(
      extResponseMsg: DataResponseMessage
  )(implicit
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = extResponseMsg match {
    case WrappedFlexResponse(
          provideFlexOptions: ProvideFlexOptions,
          receiver,
          self
        ) =>

      val ref = receiver.getOrElse(self.getOrElse(
        throw new CriticalFailureException("No receiver defined!")
      ))

      val uuid = serviceStateData.emHierarchy.getUuid(ref)

      val updated = provideFlexOptions match {
        case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>

          serviceStateData.flexOptionResponse.addData(uuid, new FlexOptionsResult(
            simulationStart, // TODO: Fix this
            modelUuid,
            min.toQuantity,
            ref.toQuantity,
            max.toQuantity,
          ))
      }



      if (updated.nonComplete) {
        // responses are still incomplete
        serviceStateData.copy(
          flexOptionResponse = updated
        )
      } else {
        // all responses received, forward them to external simulation in a bundle

        serviceStateData.extEmDataConnection.queueExtResponseMsg(
            new FlexOptionsResponse(
              updated.receivedData.asJava
            )
          )

        serviceStateData.copy(
          flexOptionResponse = ReceiveDataMap.empty
        )
      }

    case WrappedFlexRequest(issueFlexControl: IssueFlexControl, receiver) =>
      val uuid = serviceStateData.flexAdapterToUuid(receiver)

      val updated = serviceStateData.setPointResponse.addData(
        uuid,
        issueFlexControl,
      )

      if (updated.nonComplete) {
        // responses are still incomplete
        serviceStateData.copy(
          setPointResponse = updated
        )
      } else {
        // all responses received, forward them to external simulation in a bundle
        // serviceStateData.extEmDataConnection.queueExtResponseMsg(new EmSetPointDataResponse(updated.receivedData.asJava))

        serviceStateData.copy(
          setPointResponse = ReceiveDataMap.empty
        )
      }

  }
}
