/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.api.data.em.model.{
  EmSetPointResult,
  ExtendedFlexOptionsResult,
  FlexRequestResult,
  NoSetPointValue,
}
import edu.ie3.simona.api.data.em.ontology._
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegisterForEmDataService
import edu.ie3.simona.service.em.EmCommunicationCore.DataMap
import edu.ie3.simona.service.em.EmServiceCore.EmRefMaps
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.simona.util.{ReceiveDataMap, ReceiveHierarchicalDataMap}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import edu.ie3.util.scala.quantities.QuantityConversionUtils.PowerConversionSimona
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
  SetHasAsJava,
}

final case class EmCommunicationCore(
    override val lastFinishedTick: Long = PRE_INIT_TICK,
    override val uuidToFlexAdapter: Map[UUID, ActorRef[FlexRequest]] =
      Map.empty,
    override val completions: ReceiveDataMap[UUID, FlexCompletion] =
      ReceiveDataMap.empty,
    refs: EmRefMaps = EmRefMaps(),
    flexAdapterToUuid: Map[ActorRef[FlexRequest], UUID] = Map.empty,
    uuidToPRef: Map[UUID, ComparableQuantity[Power]] = Map.empty,
    toSchedule: Map[UUID, ScheduleFlexActivation] = Map.empty,
    flexRequestReceived: DataMap[UUID, Boolean] =
      ReceiveHierarchicalDataMap.empty,
    flexOptionResponse: DataMap[UUID, ExtendedFlexOptionsResult] =
      ReceiveHierarchicalDataMap.empty,
    setPointResponse: DataMap[UUID, PValue] = ReceiveHierarchicalDataMap.empty,
) extends EmServiceCore {

  override def handleRegistration(
      registrationMsg: RegisterForEmDataService
  ): EmServiceCore = {
    val uuid = registrationMsg.modelUuid
    val ref = registrationMsg.requestingActor
    val flexAdapter = registrationMsg.flexAdapter
    val parentEm = registrationMsg.parentEm
    val parentUuid = registrationMsg.parentUuid

    val updatedRefs = refs.add(uuid, ref, parentEm, parentUuid)

    copy(
      refs = updatedRefs,
      uuidToFlexAdapter = uuidToFlexAdapter + (uuid -> flexAdapter),
      flexAdapterToUuid = flexAdapterToUuid + (flexAdapter -> uuid),
      flexRequestReceived =
        flexRequestReceived.updateStructure(parentUuid, uuid),
      flexOptionResponse = flexOptionResponse.updateStructure(parentUuid, uuid),
      setPointResponse = setPointResponse.updateStructure(parentUuid, uuid),
      completions = completions.addExpectedKeys(Set(uuid)),
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

        (this, Some(new EmCompletion()))
      }

    case provideFlexRequests: ProvideFlexRequestData =>
      // entities for which flex options are requested
      val emEntities: Set[UUID] = provideFlexRequests
        .flexRequests()
        .asScala
        .map { case (agent, _) => agent }
        .toSet

      val refs = emEntities.map(uuidToFlexAdapter)

      refs.foreach(_ ! FlexActivation(tick))

      (
        copy(
          flexRequestReceived =
            flexRequestReceived.addSubKeysToExpectedKeys(emEntities),
          flexOptionResponse =
            flexOptionResponse.addSubKeysToExpectedKeys(emEntities),
          setPointResponse =
            setPointResponse.addSubKeysToExpectedKeys(emEntities),
          completions = completions.addExpectedKeys(emEntities),
        ),
        None,
      )

    case provideFlexOptions: ProvideEmFlexOptionData =>
      provideFlexOptions
        .flexOptions()
        .asScala
        .foreach { case (agent, flexOptions) =>
          refs.getResponse(agent) match {
            case Some(receiver) =>
              flexOptions.asScala.foreach { option =>
                receiver ! ProvideFlexOptions(
                  option.sender,
                  MinMaxFlexOptions(
                    option.pRef.toSquants,
                    option.pMin.toSquants,
                    option.pMax.toSquants,
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

        val receiverUuid = receiver match {
          case Right(otherRef) =>
            refs.getUuid(otherRef)
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
              new ExtendedFlexOptionsResult(
                tick.toDateTime(startTime),
                modelUuid,
                receiverUuid,
                min.toQuantity,
                ref.toQuantity,
                max.toQuantity,
              ),
            )

          case _ =>
            flexOptionResponse
        }

        if (updated.hasCompletedKeys) {
          // all responses received, forward them to external simulation in a bundle

          val (data, updatedFlexOptionResponse) = updated.getFinishedData

          val pRefs = data.map { case (uuid, options) =>
            uuid -> options.getpRef
          }

          val msgToExt = new FlexOptionsResponse(data.map {
            case (entity, value) => entity -> value
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

    case _: FlexResult =>
      (this, None)

    case completion: FlexCompletion =>
      receiver.map(_ ! completion)

      val updated = completions.addData(completion.modelUuid, completion)

      if (updated.isComplete) {
        val allKeys = updated.receivedData.keySet

        val extMsgOption = if (tick != INIT_SIM_TICK) {
          // send completion message to external simulation, if we aren't in the INIT_SIM_TICK
          Some(new EmCompletion())
        } else None

        // every em agent has sent a completion message
        (
          copy(lastFinishedTick = tick, completions = ReceiveDataMap(allKeys)),
          extMsgOption,
        )

      } else (copy(completions = updated), None)

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

        val updated = flexRequestReceived.addData(
          uuid,
          true,
        )

        if (updated.hasCompletedKeys) {

          val (dataMap, _, updatedFlexRequest) =
            updated.getFinishedDataHierarchical

          val map = dataMap.map { case (sender, receivers) =>
            sender -> new FlexRequestResult(
              flexActivation.tick.toDateTime,
              sender,
              receivers.asJava,
            )
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

        val updated = setPointResponse.addData(uuid, power)

        if (updated.hasCompletedKeys) {

          val (structureMap, dataMap, updatedSetPointResponse) =
            updated.getFinishedDataHierarchical

          val setPointResults = structureMap.map { case (sender, receivers) =>
            sender -> new EmSetPointResult(
              time,
              sender,
              receivers
                .map(receiver => receiver -> dataMap(receiver))
                .toMap
                .asJava,
            )
          }

          (
            copy(setPointResponse = updatedSetPointResponse),
            Some(new EmSetPointDataResponse(setPointResults.asJava)),
          )

        } else {
          // responses are still incomplete
          (copy(setPointResponse = updated), None)
        }
      }
  }
}

object EmCommunicationCore {

  type DataMap[K, V] = ReceiveHierarchicalDataMap[K, V]

  def empty: EmCommunicationCore = EmCommunicationCore()
}
