/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.em.NoSetPointValue
import edu.ie3.simona.api.data.em.model.{EmSetPointResult, FlexRequestResult}
import edu.ie3.simona.api.data.em.ontology._
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.EmMessage.WrappedFlexResponse
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  RegisterForEmDataService,
  ServiceResponseMessage,
}
import edu.ie3.simona.service.em.EmCommunicationCore.{DataMap, EmHierarchy}
import edu.ie3.simona.util.ReceiveHierarchicalDataMap
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.Power
import scala.jdk.CollectionConverters.{
  IterableHasAsScala,
  MapHasAsJava,
  MapHasAsScala,
}

final case class EmCommunicationCore(
    hierarchy: EmHierarchy = EmHierarchy(),
    override val uuidToFlexAdapter: Map[UUID, ActorRef[FlexRequest]] =
      Map.empty,
    flexAdapterToUuid: Map[ActorRef[FlexRequest], UUID] = Map.empty,
    uuidToPRef: Map[UUID, ComparableQuantity[Power]] = Map.empty,
    toSchedule: Map[UUID, ScheduleFlexActivation] = Map.empty,
    flexRequestReceived: DataMap[UUID, Boolean] =
      ReceiveHierarchicalDataMap.empty(false),
    flexOptionResponse: DataMap[UUID, (UUID, FlexOptionsResult)] =
      ReceiveHierarchicalDataMap.empty,
    setPointResponse: DataMap[UUID, EmSetPointResult] =
      ReceiveHierarchicalDataMap.empty(false),
    completions: DataMap[UUID, FlexCompletion] =
      ReceiveHierarchicalDataMap.empty(false),
) extends EmServiceCore {

  override def handleRegistration(
      registerMsg: RegisterForEmDataService
  ): EmServiceCore = {
    val uuid = registerMsg.modelUuid
    val ref = registerMsg.requestingActor
    val flexAdapter = registerMsg.flexAdapter
    val parentEm = registerMsg.parentEm
    val parentUuid = registerMsg.parentUuid

    val updatedHierarchy = parentEm match {
      case Some(parent) =>
        hierarchy.add(uuid, ref, parent)
      case None =>
        hierarchy.add(uuid, ref, ref)
    }

    copy(
      hierarchy = updatedHierarchy,
      uuidToFlexAdapter = uuidToFlexAdapter + (uuid -> flexAdapter),
      flexAdapterToUuid = flexAdapterToUuid + (flexAdapter -> uuid),
      flexRequestReceived =
        flexRequestReceived.updateStructure(parentUuid, uuid),
      flexOptionResponse = flexOptionResponse.updateStructure(parentUuid, uuid),
      setPointResponse = setPointResponse.updateStructure(parentUuid, uuid),
      completions = completions.updateStructure(parentUuid, uuid),
    )
  }

  override def handleExtMessage(
      tick: Long,
      extMSg: EmDataMessageFromExt,
  )(implicit
      log: Logger
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = extMSg match {
    case requestEmCompletion: RequestEmCompletion =>
      if (requestEmCompletion.tick != tick) {
        log.warn(
          s"Received completion request for tick '${requestEmCompletion.tick}' in tick '$tick'."
        )
        (this, None)

      } else {
        log.info(s"Receive a request for completion for tick '$tick'.")

        uuidToFlexAdapter.foreach { case (_, adapter) =>
          adapter ! IssueNoControl(tick)
        }

        (EmCommunicationCore.empty, Some(new EmCompletion()))
      }

    case provideFlexRequests: ProvideFlexRequestData =>
      log.info(s"Handling of: $provideFlexRequests")

      // entities for which flex options are requested
      val emEntities: Set[UUID] = provideFlexRequests
        .flexRequests()
        .asScala
        .flatMap { case (_, v) => v.asScala }
        .toSet

      val refs = emEntities.map(uuidToFlexAdapter)

      log.warn(s"Em refs: $refs")
      refs.foreach(_ ! FlexActivation(tick))

      (addSubKeysToExpectedKeys(emEntities), None)

    case provideFlexOptions: ProvideEmFlexOptionData =>
      log.info(s"Handling of: $provideFlexOptions")

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
              log.warn(s"No em agent with uuid '$agent' registered!")
          }
        }

      (this, None)

    case providedSetPoints: ProvideEmSetPointData =>
      handleSetPoint(tick, providedSetPoints, log)

      (this, None)
  }

  override def handleFlexResponse(
      tick: Long,
      flexResponse: FlexResponse,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(implicit
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = flexResponse match {
    case scheduleFlexActivation: ScheduleFlexActivation =>
      log.warn(s"Flex activation: $scheduleFlexActivation")

      if (scheduleFlexActivation.tick == INIT_SIM_TICK) {
        receiver match {
          case Right(ref) =>
            log.warn(s"$ref: $scheduleFlexActivation")
            ref ! scheduleFlexActivation

          case Left(uuid) =>
            uuidToFlexAdapter(uuid) ! FlexActivation(INIT_SIM_TICK)
        }
      } else {
        log.warn(s"$scheduleFlexActivation not handled!")
      }

      (this, None)

    case provideFlexOptions: ProvideFlexOptions =>
      if (tick == INIT_SIM_TICK) {
        receiver match {
          case Right(otherRef) =>
            otherRef ! provideFlexOptions

          case Left(self: UUID) =>
            uuidToFlexAdapter(self) ! IssuePowerControl(
              INIT_SIM_TICK,
              zeroKW,
            )
        }

        (this, None)
      } else {

        val uuid = receiver match {
          case Right(otherRef) =>
            hierarchy.getUuid(otherRef)
          case Left(self: UUID) =>
            self
        }

        val updated = provideFlexOptions match {
          case ProvideFlexOptions(
                modelUuid,
                MinMaxFlexOptions(ref, min, max),
              ) =>
            flexOptionResponse.addData(
              modelUuid,
              (
                uuid,
                new FlexOptionsResult(
                  tick.toDateTime(startTime),
                  modelUuid,
                  min.toQuantity,
                  ref.toQuantity,
                  max.toQuantity,
                ),
              ),
            )

          case _ =>
            flexOptionResponse
        }

        log.warn(s"Updated data map: $updated")

        if (updated.hasCompletedKeys) {
          // all responses received, forward them to external simulation in a bundle

          val (data, updatedFlexOptionResponse) = updated.getFinishedData

          val pRefs = data.map { case (uuid, (_, options)) =>
            uuid -> options.getpRef
          }

          val msgToExt = new FlexOptionsResponse(data.map {
            case (entity, (_, value)) => entity -> value
          }.asJava)

          (
            copy(
              flexOptionResponse = updatedFlexOptionResponse,
              uuidToPRef = uuidToPRef ++ pRefs,
            ),
            Some(msgToExt),
          )

        } else {
          // responses are still incomplete
          (copy(flexOptionResponse = updated), None)
        }
      }

    case FlexResult(modelUuid, result) =>
      log.info(s"Flex result '$result' for model '$modelUuid'.")
      (this, None)

    case completion: FlexCompletion =>
      receiver.map(_ ! completion)
      log.warn(s"Completion: $completion")

      // TODO: Check if necessary
      if (tick != INIT_SIM_TICK) {

        val model = completion.modelUuid

        val updated = completions.addData(model, completion)

        if (updated.allCompleted) {

          val (_, updatedCompletions) = updated.getFinishedData

          // every em agent has sent a completion message
          // send completion message to external simulation
          (copy(completions = updatedCompletions), Some(new EmCompletion()))

        } else {
          (copy(completions = updated), None)
        }
      } else {
        (this, None)
      }
  }

  override def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(implicit
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = flexRequest match {
    case flexActivation @ FlexActivation(tick) =>
      if (tick == INIT_SIM_TICK) {
        receiver ! flexActivation

        (this, None)
      } else {
        val uuid = flexAdapterToUuid(receiver)

        log.warn(s"Receiver: $uuid")

        val updated = flexRequestReceived.addData(
          uuid,
          true,
        )

        log.warn(s"$updated")

        if (updated.hasCompletedKeys) {

          val (dataMap, updatedFlexRequest) = updated.getFinishedData

          log.warn(s"Data to be send: $dataMap")

          val map = dataMap.map { case (key, _) =>
            key ->
              new FlexRequestResult(flexActivation.tick.toDateTime, key)
          }

          (
            copy(flexRequestReceived = updatedFlexRequest),
            Some(new FlexRequestResponse(map.asJava)),
          )

        } else {
          (copy(flexRequestReceived = updated), None)
        }
      }

    case issueFlexControl: IssueFlexControl =>
      if (issueFlexControl.tick == INIT_SIM_TICK) {

        receiver ! issueFlexControl

        (this, None)

      } else {
        val uuid = flexAdapterToUuid(receiver)

        val (time, power) = issueFlexControl match {
          case IssueNoControl(tick) =>
            (tick.toDateTime, new NoSetPointValue(uuidToPRef(uuid)))

          case IssuePowerControl(tick, setPower) =>
            (tick.toDateTime, new PValue(setPower.toQuantity))
        }

        val updated = setPointResponse.addData(
          uuid,
          new EmSetPointResult(time, uuid, power),
        )

        log.warn(s"Updated set point response: $updated")

        if (updated.hasCompletedKeys) {

          val (dataMap, updatedSetPointResponse) = updated.getFinishedData

          (
            copy(setPointResponse = updatedSetPointResponse),
            Some(new EmSetPointDataResponse(dataMap.asJava)),
          )

        } else {
          // responses are still incomplete
          (copy(setPointResponse = updated), None)
        }
      }
  }

  private def addSubKeysToExpectedKeys(keys: Set[UUID]): EmCommunicationCore =
    copy(
      flexRequestReceived = flexRequestReceived.addSubKeysToExpectedKeys(keys),
      flexOptionResponse = flexOptionResponse.addSubKeysToExpectedKeys(keys),
      setPointResponse = setPointResponse.addSubKeysToExpectedKeys(keys),
      completions = completions.addSubKeysToExpectedKeys(keys),
    )
}

object EmCommunicationCore {

  type DataMap[K, V] = ReceiveHierarchicalDataMap[K, V]

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

  def empty: EmCommunicationCore = EmCommunicationCore()
}
