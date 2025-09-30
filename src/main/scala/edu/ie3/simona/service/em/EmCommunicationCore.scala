/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.em.EmAgent.Message
import edu.ie3.simona.api.data.model.em.{EmData, EmSetPoint, ExtendedFlexOptionsResult, FlexOptionRequest}
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.ServiceMessage.EmServiceRegistration
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.flex.{FlexType, FlexibilityMessage, PowerLimitFlexOptions}
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.simona.util.TickUtil.*
import edu.ie3.simona.util.{ReceiveDataMap, ReceiveMultiDataMap}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Try

case class EmCommunicationCore(
    disaggregated: Map[UUID, Boolean] = Map.empty,
    override val lastFinishedTick: Long = PRE_INIT_TICK,
    override val uuidToAgent: Map[UUID, ActorRef[Message]] = Map.empty,
    refToUuid: Map[ActorRef[FlexResponse] | ActorRef[FlexRequest], UUID] =
      Map.empty,
    uuidToInferior: Map[UUID, Seq[UUID]] = Map.empty,
    uuidToParent: Map[UUID, UUID] = Map.empty,
    override val completions: ReceiveDataMap[UUID, FlexCompletion] =
      ReceiveDataMap.empty,
    requestedFlexType: Map[UUID, FlexType] = Map.empty,
    allFlexOptions: Map[UUID, FlexOptionsResult] = Map.empty,
    currentSetPoint: Map[UUID, Power] = Map.empty,
    activatedAgents: Set[UUID] = Set.empty,
    waitingForFlexOptions: Set[UUID] = Set.empty,
    waitingForSetPoint: Set[UUID] = Set.empty,
    expectDataFrom: ReceiveMultiDataMap[UUID, EmData] =
      ReceiveMultiDataMap.empty,
) extends EmServiceCore {

  override def handleRegistration(
      emServiceRegistration: EmServiceRegistration
  ): EmServiceCore = {
    val uuid = emServiceRegistration.inputUuid
    val ref = emServiceRegistration.requestingActor

    val (updatedInferior, updatedUuidToParent) =
      emServiceRegistration.parentUuid match {
        case Some(parent) =>
          val inferior = uuidToInferior.get(parent) match {
            case Some(inferiorUuids) =>
              inferiorUuids.appended(uuid)
            case None =>
              Seq(uuid)
          }

          (
            uuidToInferior.updated(parent, inferior),
            uuidToParent.updated(uuid, parent),
          )
        case None =>
          (uuidToInferior, uuidToParent)
      }

    copy(
      uuidToAgent = uuidToAgent.updated(uuid, ref),
      refToUuid = refToUuid.updated(ref, uuid),
      uuidToInferior = updatedInferior,
      uuidToParent = updatedUuidToParent,
    )
  }

  override def handleExtMessage(tick: Long, extMSg: EmDataMessageFromExt)(using
      log: Logger
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = extMSg match {
    case requestEmCompletion: RequestEmCompletion =>
      // finish tick and return next tick
      val extTick = requestEmCompletion.tick

      if extTick != tick then {
        throw new CriticalFailureException(
          s"Received completion request for tick '$extTick', while being in tick '$tick'."
        )
      } else {
        log.info(s"Request to finish for tick '$tick' received.")

        // deactivate agents by sending an IssueNoControl message
        // activatedAgents.map(uuidToAgent).foreach(_ ! IssueNoControl(tick))

        val nextTick: Option[java.lang.Long] =
          if activatedAgents.nonEmpty then {
            requestEmCompletion.maybeNextTick.toScala
          } else getMaybeNextTick

        (
          copy(lastFinishedTick = tick),
          Some(new EmCompletion(nextTick.toJava)),
        )
      }

    case provideEmData: ProvideEmData =>
      // provide em data
      val extTick = provideEmData.tick

      if extTick != tick then {
        throw new CriticalFailureException(
          s"Received request for tick '$extTick', while being in tick '$tick'."
        )
      } else {
        // handle flex requests
        val requests = provideEmData.flexRequests.asScala
        val activated = requests.flatMap {
          case (uuid, _) if !activatedAgents.contains(uuid) =>
            uuidToAgent.get(uuid).map { agent =>
              agent ! FlexShiftActivation(
                tick,
                requestedFlexType.getOrElse(uuid, FlexType.PowerLimit),
              )

              // uuid -> number of sent flex requests
              uuid -> Try(uuidToInferior(uuid).size).getOrElse(1)
            }
          case _ =>
            None
        }.toMap

        val updatedDisaggregated = disaggregated ++ requests.map {
          case (uuid, request) => uuid -> request.disaggregated
        }

        // handle flex options
        val expectFlexOptions = provideEmData.flexOptions.asScala.flatMap {
          case (receiver, options) if waitingForFlexOptions.contains(receiver) =>
            val agent = uuidToAgent(receiver)

            // send flex options to agent
            options.asScala.foreach { option =>
              agent ! ProvideFlexOptions(
                option.sender,
                PowerLimitFlexOptions(
                  option.pRef.toSquants,
                  option.pMin.toSquants,
                  option.pMax.toSquants,
                ),
              )
            }

            // receiver -> number of received flex options
            Some(receiver -> 1)
          case _ => None
        }.toMap

        // handle set points
        val setPoints = provideEmData.setPoints.asScala
        val expectedSetPoints = setPoints.flatMap {
          case (uuid, setPoint)
              if waitingForSetPoint.contains(uuid) | waitingForFlexOptions
                .contains(uuid) =>
            val agent = uuidToAgent(uuid)

            setPoint.power.toScala.flatMap(
              _.getP.toScala.map(_.toSquants)
            ) match {
              case Some(power) =>
                agent ! IssuePowerControl(extTick, power)

              case None =>
                agent ! IssueNoControl(extTick)
            }

            // sender -> number of set points to send
            Some(uuid -> Try(uuidToInferior(uuid).size).getOrElse(0))
          case _ => None
        }.toMap

        val activatedKeys = activated.keySet
        val flexOptionKeys = expectFlexOptions.keys
        val setPointKeys = expectedSetPoints.keys

        val updatedExpectDataFrom = expectDataFrom
          .addExpectedKeys(activated)
          .addExpectedKeys(expectFlexOptions)
          .addExpectedKeys(expectedSetPoints)
        log.warn(
          s"ExpectDataFrom: $updatedExpectDataFrom, Request: $activated, FlexOption: $expectFlexOptions, SetPoint: $expectedSetPoints"
        )

        val updatedWaitingForFlexOptions = waitingForFlexOptions ++ activatedKeys.filter(uuidToInferior.contains) -- flexOptionKeys -- setPointKeys

        // update state data
        val newState = copy(
          disaggregated = updatedDisaggregated,
          activatedAgents = activatedAgents ++ activatedKeys,
          waitingForFlexOptions = updatedWaitingForFlexOptions,
          waitingForSetPoint =
            waitingForSetPoint ++ flexOptionKeys -- setPointKeys,
          expectDataFrom = updatedExpectDataFrom,
          completions = completions.addExpectedKeys(activatedKeys),
        )

        log.warn(
          s"Activated: ${newState.activatedAgents}; WaitingForFlexOptions: ${newState.waitingForFlexOptions}; WaitingForSetPoints: ${newState.waitingForSetPoint}"
        )

        (newState, None)
      }

    case _: RequestEmFlexResults =>
      // should not happen, this should be done by ProvideFlexRequestData
      log.warn(
        s"Received request for flex results. This is not supported by ${this.getClass}!"
      )

      (this, None)

    case other =>
      log.warn(s"Deprecated message received! Message: $other")

      (this, None)
  }

  override def handleFlexResponse(
      tick: Long,
      flexResponse: FlexResponse,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(using
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    val receiverUuid = receiver match {
      case Left(value) =>
        value
      case Right(ref) =>
        refToUuid(ref)
    }

    flexResponse match {
      case scheduleFlexActivation @ ScheduleFlexActivation(
            modelUuid,
            _,
            scheduleKey,
          ) =>
        if tick == INIT_SIM_TICK then {
          scheduleKey.foreach(_.unlock())

          uuidToAgent(receiverUuid) ! FlexActivation(
            INIT_SIM_TICK,
            FlexType.PowerLimit,
          )

        } else {
          log.warn(s"$scheduleFlexActivation not handled!")
        }

        (this, None)

      case provideFlexOptions @ ProvideFlexOptions(sender, flexOptions) =>
        if tick == INIT_SIM_TICK then {
          uuidToAgent(receiverUuid) ! provideFlexOptions

          (this, None)
        } else {
          // flex option to ext
          val (resultToExt, pRef) = flexOptions match {
            case PowerLimitFlexOptions(ref, min, max) =>
              val flexOptionResult = new ExtendedFlexOptionsResult(
                tick.toDateTime(using startTime),
                sender,
                receiverUuid,
                ref.toQuantity,
                min.toQuantity,
                max.toQuantity,
              )

              if disaggregated.contains(receiverUuid) then {
                uuidToInferior(receiverUuid)
                  .flatMap(allFlexOptions.get)
                  .foreach { result =>
                    flexOptionResult
                      .addDisaggregated(result.getInputModel, result)
                  }
              }

              (flexOptionResult, ref)

            case other =>
              throw CriticalFailureException(
                s"Flex option type '$other' is currently not supported!"
              )
          }

          val updated = expectDataFrom.addData(sender, resultToExt)

          if updated.isComplete then {
            (
              copy(
                allFlexOptions = allFlexOptions.updated(sender, resultToExt),
                currentSetPoint = currentSetPoint.updated(sender, pRef),
                expectDataFrom = ReceiveMultiDataMap.empty,
              ),
              Some(new EmResultResponse(updated.receivedData.asJava)),
            )
          } else {
            (
              copy(
                allFlexOptions = allFlexOptions.updated(sender, resultToExt),
                currentSetPoint = currentSetPoint.updated(sender, pRef),
                expectDataFrom = updated,
              ),
              None,
            )
          }
        }

      case completion @ FlexCompletion(
            sender,
            requestAtNextActivation,
            requestAtTick,
          ) =>
        // the completion can be sent directly to the receiver, since it's not used by the external communication
        uuidToAgent(receiverUuid) ! completion

        if tick == INIT_SIM_TICK then {
          receiver match {
            case Left(value) =>
              (copy(lastFinishedTick = tick), None)
            case Right(_) =>
              (this, None)
          }
        } else {
          val updatedData = completions.addData(sender, completion)

          if updatedData.isComplete then {
            (
              copy(
                lastFinishedTick = tick,
                completions = ReceiveDataMap.empty,
                requestedFlexType = Map.empty,
                allFlexOptions = Map.empty,
                currentSetPoint = Map.empty,
                activatedAgents = Set.empty,
                expectDataFrom = ReceiveMultiDataMap.empty,
              ),
              Some(new EmCompletion(getMaybeNextTick.toJava)),
            )
          } else {
            (copy(completions = updatedData), None)
          }
        }

      // not supported
      case other =>
        log.warn(s"Flex response $other is not supported!")

        (this, None)
    }
  }

  override def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(using
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    val receiverUuid = refToUuid(receiver) // the controlled em
    val sender = uuidToParent(receiverUuid) // the controlling em

    val updated = flexRequest match {
      case FlexActivation(tick, flexType) =>
        // send request to ext
        expectDataFrom.addData(
          sender,
          new FlexOptionRequest(
            receiverUuid,
            sender,
            disaggregated.getOrElse(sender, false),
          ),
        )

      case control: IssueFlexControl =>
        // send set point to ext
        log.warn(
          s"Receiver $receiverUuid got flex control message from $sender"
        )

        val (time, power) = control match {
          case IssueNoControl(tick) =>
            log.warn(s"Set points: $currentSetPoint")

            (
              tick.toDateTime,
              new PValue(currentSetPoint(receiverUuid).toQuantity),
            )

          case IssuePowerControl(tick, setPower) =>
            (tick.toDateTime, new PValue(setPower.toQuantity))

          case other =>
            throw new CriticalFailureException(
              s"Flex control $other is not supported!"
            )
        }

        expectDataFrom.addData(
          sender,
          new EmSetPoint(receiverUuid, sender, power),
        )

      case other =>
        log.warn(s"$other is not supported!")
        expectDataFrom
    }

    if updated.isComplete then {
      (
        copy(expectDataFrom = ReceiveMultiDataMap.empty),
        Some(new EmResultResponse(updated.receivedData.asJava)),
      )
    } else {
      (copy(expectDataFrom = updated), None)
    }
  }

}
