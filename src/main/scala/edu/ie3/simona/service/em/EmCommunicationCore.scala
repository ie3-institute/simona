/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.em.EmAgent.Message
import edu.ie3.simona.api.data.model.em.{
  EmData,
  EmSetPoint,
  ExtendedFlexOptionsResult,
  FlexOptionRequest,
}
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.ServiceMessage.EmServiceRegistration
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.flex.{
  FlexType,
  FlexibilityMessage,
  PowerLimitFlexOptions,
}
import edu.ie3.simona.service.em.EmCommunicationCore.EmAgentState
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
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Try

object EmCommunicationCore {

  final case class EmAgentState(
      private var receivedActivation: Boolean = false,
      private val awaitedFlexOptions: mutable.Set[UUID] = mutable.Set.empty,
      private var awaitedSetPoint: Boolean = false,
      private var waitingForInternal: Boolean = false,
  ) {
    def setReceivedRequest(): Unit = {
      receivedActivation = true
      waitingForInternal = true
      awaitedSetPoint = true
    }

    def addSendRequest(request: UUID): Unit = {
      awaitedFlexOptions.add(request)
    }

    def addSendRequests(requests: Seq[UUID]): Unit = {
      awaitedFlexOptions.addAll(requests)
    }

    def handleReceivedFlexOption(flexOption: UUID): Unit = {
      awaitedFlexOptions.remove(flexOption)
      waitingForInternal = false

      if awaitedFlexOptions.isEmpty then {
        waitingForInternal = true
      }
    }

    def handleReceivedFlexOptions(flexOptions: Seq[UUID]): Unit = {
      flexOptions.foreach(awaitedFlexOptions.remove)
      waitingForInternal = false

      if awaitedFlexOptions.isEmpty then {
        waitingForInternal = true
      }
    }

    def setReceivedSetPoint(): Unit = {
      awaitedSetPoint = false
      waitingForInternal = true
    }

    def setWaitingForInternal(value: Boolean): Unit = {
      waitingForInternal = value
    }

    def getAwaited: Set[UUID] = awaitedFlexOptions.toSet

    def isWaitingForActivation: Boolean = !receivedActivation

    def isWaitingForExtern: Boolean =
      (awaitedFlexOptions.nonEmpty || awaitedSetPoint) && !waitingForInternal

    def isWaitingForSetPoint: Boolean = awaitedSetPoint

    def isWaitingForInternal: Boolean = waitingForInternal

    def isActivated: Boolean = receivedActivation

    def clear(): Unit = {
      receivedActivation = false
      awaitedFlexOptions.clear
      awaitedSetPoint = false
      waitingForInternal = false
    }
  }
}

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
    emStates: Map[UUID, EmAgentState] = Map.empty,
    expectDataFrom: ReceiveMultiDataMap[UUID, EmData] =
      ReceiveMultiDataMap.empty,
    nextActivation: Map[UUID, Long] = Map.empty
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
      emStates = emStates.updated(uuid, EmAgentState()),
      nextActivation = nextActivation.updated(uuid, 0)
    )
  }

  override def handleExtMessage(tick: Long, extMsg: EmDataMessageFromExt)(using
      log: Logger
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = extMsg match {
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
      log.warn(s"Handling ext message: $provideEmData")

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
          case (uuid, _) if emStates(uuid).isWaitingForActivation =>
            uuidToAgent.get(uuid).map { agent =>
              // update the em state
              emStates(uuid).setReceivedRequest()

              agent ! FlexShiftActivation(
                tick,
                requestedFlexType.getOrElse(uuid, FlexType.PowerLimit),
              )

              val count = Try {
                uuidToInferior(uuid).count { id => nextActivation(id) <= tick}
              }.getOrElse(1)

              // uuid -> number of sent flex requests
              uuid -> count
            }
          case _ =>
            None
        }.toMap

        val updatedDisaggregated = disaggregated ++ requests.map {
          case (uuid, request) => uuid -> request.disaggregated
        }

        // handle flex options
        val expectFlexOptions = provideEmData.flexOptions.asScala.flatMap {
          case (receiver, options) if emStates(receiver).isWaitingForExtern =>
            val agent = uuidToAgent(receiver)

            val emState = emStates(receiver)

            // send flex options to agent
            options.asScala.foreach { option =>
              val sender = option.sender

              // update the em state
              emState.handleReceivedFlexOption(sender)

              agent ! ProvideFlexOptions(
                sender,
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
        val expectedSetPoints = provideEmData
          .setPoints()
          .asScala
          .flatMap {
            case (uuid, setPoint) if emStates(uuid).isWaitingForSetPoint =>
              val agent = uuidToAgent(uuid)
              log.warn(s"Receiver of set point: $agent")

              // updates the em state
              emStates(uuid).setReceivedSetPoint()

              setPoint.power.toScala.flatMap(
                _.getP.toScala.map(_.toSquants)
              ) match {
                case Some(power) =>
                  agent ! IssuePowerControl(extTick, power)

                case None =>
                  agent ! IssueNoControl(extTick)
              }

              val count = Try {
                uuidToInferior(uuid).count { id => emStates(id).isActivated }
              }.getOrElse(0)

              // sender -> number of set points to send
              Some(uuid -> count)
            case _ => None
          }
          .toMap

        // check if we need to wait for internal answers
        val msgToExt = getMsgToExtOption

        val activatedKeys = activated.keySet

        val updatedExpectDataFrom = expectDataFrom
          .addExpectedKeys(activated)
          .addExpectedKeys(expectFlexOptions)
          .addExpectedKeys(expectedSetPoints)
        log.warn(
          s"ExpectDataFrom: $updatedExpectDataFrom, Request: $activated, FlexOption: $expectFlexOptions, SetPoint: $expectedSetPoints"
        )

        // update state data
        val newState = copy(
          disaggregated = updatedDisaggregated,
          activatedAgents = activatedAgents ++ activatedKeys,
          expectDataFrom = updatedExpectDataFrom,
          completions = completions.addExpectedKeys(activatedKeys),
        )

        log.warn(s"Activated: ${newState.activatedAgents}")
        log.warn(s"EmStates: ${newState.emStates}")
        log.warn(s"Message to ext: $msgToExt")

        (newState, msgToExt)
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

          if updated.isComplete || updated.hasCompleted then {
            val (data, updatedExpectDataFrom) = updated.getFinished

            // should no longer wait for internal data
            data.keys.foreach(emStates(_).setWaitingForInternal(false))
            log.warn(s"Updated EmStates: $emStates")

            (
              copy(
                allFlexOptions = allFlexOptions.updated(sender, resultToExt),
                currentSetPoint = currentSetPoint.updated(sender, pRef),
                expectDataFrom = updatedExpectDataFrom,
              ),
              Some(new EmResultResponse(data.asJava)),
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
        emStates(sender).setWaitingForInternal(false)

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
            emStates.foreach(_._2.clear())
            log.warn(s"Cleared EmStates: $emStates")

            // the next activations
            val additionalActivation = updatedData.receivedData.flatMap { case (uuid, msg) =>
              msg.requestAtTick.map(uuid -> _)
            }

            (
              copy(
                lastFinishedTick = tick,
                completions = ReceiveDataMap.empty,
                requestedFlexType = Map.empty,
                allFlexOptions = Map.empty,
                currentSetPoint = Map.empty,
                activatedAgents = Set.empty,
                expectDataFrom = ReceiveMultiDataMap.empty,
                nextActivation = nextActivation ++ additionalActivation,
              ),
              Some(new EmCompletion(getMaybeNextTick.toJava)),
            )
          } else {
            val msgToExt = getMsgToExtOption
            log.warn(s"Not finished! Expected: ${updatedData.getExpectedKeys}")
            log.warn(s"EmStates: $emStates")
            log.warn(s"Message to ext: $msgToExt")

            (copy(completions = updatedData), msgToExt)
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
        // update the em state => waiting for external flex option provision
        emStates(sender).addSendRequest(receiverUuid)

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
        emStates(receiverUuid).setWaitingForInternal(false)

        // send set point to ext
        log.warn(
          s"Receiver $receiverUuid got flex control message from $sender"
        )

        val (_, power) = control match {
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
      val data = updated.receivedData

      // should no longer wait for internal data
      data.keys.foreach { uuid => emStates(uuid).setWaitingForInternal(false) }
      log.warn(s"Updated EmStates: $emStates")

      (
        copy(expectDataFrom = ReceiveMultiDataMap.empty),
        Some(new EmResultResponse(data.asJava)),
      )
    } else {
      val msgToExt = getMsgToExtOption
      log.warn(s"Not finished! Expected: ${updated.getExpectedKeys}")
      log.warn(s"EmStates: $emStates")
      log.warn(s"Message to ext: $msgToExt")

      (copy(expectDataFrom = updated), msgToExt)
    }
  }

  private def getMsgToExtOption(using
      log: Logger
  ): Option[EmDataResponseMessageToExt] = {
    if emStates.exists(_._2.isWaitingForInternal) then {
      None
    } else {
      val awaited = emStates.filter((_, x) => x.isWaitingForExtern).map {
        case (uuid, state) => uuid -> state.getAwaited
      }

      log.info(s"Waiting for external data: $awaited")

      if awaited.isEmpty then None
      else Some(new EmResultResponse(Map.empty.asJava))
    }
  }
}
