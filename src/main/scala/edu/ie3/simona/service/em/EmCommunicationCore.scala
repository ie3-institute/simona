/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.em.EmAgent.Message
import edu.ie3.simona.api.data.model.em
import edu.ie3.simona.api.data.model.em.{
  EmCommunicationMessage,
  EmData,
  EmSetPoint,
  FlexOptionRequest,
  FlexOptions as ExtFlexOptions,
}
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.ServiceMessage.EmServiceRegistration
import edu.ie3.simona.ontology.messages.flex.FlexOptions.fromExt
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.service.em.EmCommunicationCore.EmAgentState
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.{ReceiveDataMap, ReceiveMultiDataMap}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger

import java.util.{OptionalLong, UUID}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.math.max
import scala.util.Try

object EmCommunicationCore {

  def apply(core: EmServiceCore): EmCommunicationCore = {
    val uuidToAgent = core.uuidToAgent

    EmCommunicationCore(
      uuidToAgent,
      core.agentToUuid,
      core.uncontrolled,
      core.uuidToInferior,
      core.uuidToParent,
      core.completions,
      core.nextActivation,
      uuidToAgent.keys.map(uuid => uuid -> EmAgentState()).toMap,
    )
  }

  final case class EmAgentState(
      private var receivedActivation: Boolean = false,
      private val awaitedFlexOptions: mutable.Set[UUID] = mutable.Set.empty,
      private var awaitedSetPoint: Boolean = false,
      private var waitingForInternal: Boolean = false,
      private var waitingForRelease: Boolean = false,
  ) {
    def setReceivedRequest(): Unit = {
      receivedActivation = true
      waitingForInternal = true
      awaitedSetPoint = true
    }

    def setWaitingForRelease(): Unit = {
      waitingForRelease = true
    }

    def setReceivedRelease(): Unit = {
      receivedActivation = true
      waitingForInternal = false
      waitingForRelease = false
    }

    def addSendRequest(request: UUID): Unit = {
      awaitedFlexOptions.add(request)
    }

    def handleReceivedFlexOption(flexOption: UUID): Unit = {
      awaitedFlexOptions.remove(flexOption)

      if awaitedFlexOptions.isEmpty then {
        waitingForInternal = true
      } else {
        waitingForInternal = false
      }
    }

    def handleReceivedFlexOptions(flexOptions: Seq[UUID]): Unit = {
      flexOptions.foreach(awaitedFlexOptions.remove)

      if awaitedFlexOptions.isEmpty then {
        waitingForInternal = true
      } else {
        waitingForInternal = false
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

    def isWaitingForRelease: Boolean = waitingForRelease

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
    override val uuidToAgent: Map[UUID, ActorRef[Message]] = Map.empty,
    override val agentToUuid: Map[
      ActorRef[FlexRequest] | ActorRef[FlexResponse],
      UUID,
    ] = Map.empty,
    override val uncontrolled: Set[UUID] = Set.empty,
    override val uuidToInferior: Map[UUID, Set[UUID]] = Map.empty,
    override val uuidToParent: Map[UUID, UUID] = Map.empty,
    override val completions: ReceiveDataMap[UUID, FlexCompletion] =
      ReceiveDataMap.empty,
    override val nextActivation: Map[UUID, Long] = Map.empty,
    emStates: Map[UUID, EmAgentState] = Map.empty,
    expectDataFrom: ReceiveMultiDataMap[UUID, EmData] =
      ReceiveMultiDataMap.empty,
) extends EmServiceCore {

  def handleRegistration(
      emServiceRegistration: EmServiceRegistration
  ): EmServiceCore = {
    val uuid = emServiceRegistration.inputUuid
    val ref = emServiceRegistration.requestingActor

    val (updatedUncontrolled, updatedInferior, updatedUuidToParent) =
      emServiceRegistration.parentUuid match {
        case Some(parent) =>
          val inferior = uuidToInferior.get(parent) match {
            case Some(inferiorUuids) =>
              inferiorUuids ++ Seq(uuid)
            case None =>
              Set(uuid)
          }

          (
            uncontrolled,
            uuidToInferior.updated(parent, inferior),
            uuidToParent.updated(uuid, parent),
          )
        case None =>
          (uncontrolled + uuid, uuidToInferior, uuidToParent)
      }

    copy(
      uuidToAgent = uuidToAgent.updated(uuid, ref),
      agentToUuid = agentToUuid.updated(ref, uuid),
      uncontrolled = updatedUncontrolled,
      uuidToInferior = updatedInferior,
      uuidToParent = updatedUuidToParent,
      nextActivation = nextActivation.updated(uuid, 0),
      emStates = emStates.updated(uuid, EmAgentState()),
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

        val nextTick: OptionalLong = if emStates.exists(_._2.isActivated) then {
          requestEmCompletion.maybeNextTick
        } else getMaybeNextTick(tick)

        (
          this,
          Some(new EmCompletion(nextTick)),
        )
      }

    case internal: EmSimulationInternal =>
      // will be handled by an internal core
      log.warn(
        s"Forwarding message to base core. This should only happen, if the simulation shall be finished."
      )

      EmServiceBaseCore(this).handleExtMessage(tick, internal)

    case provideEmData: ProvideEmData =>
      log.debug(s"Handling ext message: $provideEmData")
      val extTick = provideEmData.tick

      // handling of requests
      val flexRequests = provideEmData.flexRequests.asScala

      val requestMapping = flexRequests.flatMap { case (uuid, request) =>
        if emStates(uuid).isWaitingForActivation then {

          uuidToAgent.get(uuid).map { agent =>
            // update the em state
            emStates(uuid).setReceivedRequest()

            agent ! FlexActivation(tick, request.disaggregated, true)

            val count = uuidToInferior(uuid).size

            // uuid -> number of sent flex requests
            uuid -> count
          }
        } else None
      }.toMap

      // handling of set points
      val setPointMapping = provideEmData
        .setPoints()
        .asScala
        .flatMap { case (receiver, setPoint) =>
          val agent = uuidToAgent(receiver)

          // updates the em state
          emStates(receiver).setReceivedSetPoint()

          setPoint.power.toScala.flatMap(
            _.getP.toScala.map(_.toSquants)
          ) match {
            case Some(power) =>
              agent ! IssuePowerControl(extTick, power)

            case None =>
              agent ! IssueNoControl(extTick)
          }

          val count = Try {
            uuidToInferior(receiver).count { id => emStates(id).isActivated }
          }.getOrElse(0)

          // sender -> number of set points to send
          Some(receiver -> count)
        }
        .toMap

      /* update internal state */
      val mapping = requestMapping ++ setPointMapping

      val updatedExpectDataFrom = expectDataFrom.addExpectedKeys(mapping)

      // check if we need to wait for internal answers
      val msgToExt = getMsgToExtOption

      // update state data
      val newState = copy(
        expectDataFrom = updatedExpectDataFrom,
        completions = completions.addExpectedKeys(mapping.keySet),
      )

      // log.warn(s"EmStates: ${newState.emStates}")
      // log.warn(s"Message to ext: $msgToExt")

      (newState, msgToExt)

    case comMsg: EmCommunicationMessages =>
      val messages = comMsg.messages.asScala
      val extTick = comMsg.tick

      val mapping = messages.flatMap { msg =>
        val receiver = msg.receiver
        val sender = msg.sender

        msg.content match {
          case request: FlexOptionRequest =>
            uuidToAgent.get(receiver) match {
              case Some(agent) =>
                // update the em state
                emStates(receiver).setReceivedRequest()

                agent ! FlexActivation(tick, request.disaggregated, true)

                val count = max(
                  Try {
                    uuidToInferior(receiver).count { id =>
                      nextActivation(id) <= tick
                    }
                  }.getOrElse(1),
                  1,
                )

                // uuid -> number of sent flex requests
                Some(receiver -> count)

              case None =>
                log.warn(s"Cannot send flex request to receiver '$receiver'.")
                None
            }

          case flexOption: ExtFlexOptions =>
            val agent = uuidToAgent(receiver)
            val emState = emStates(receiver)

            // update the em state
            emState.handleReceivedFlexOption(sender)

            agent ! ProvideFlexOptions(
              sender,
              fromExt(flexOption),
            )

            // receiver -> number of received flex options
            Some(receiver -> 1)

          case setPoint: EmSetPoint =>
            val agent = uuidToAgent(receiver)

            // updates the em state
            emStates(receiver).setReceivedSetPoint()

            setPoint.power.toScala.flatMap(
              _.getP.toScala.map(_.toSquants)
            ) match {
              case Some(power) =>
                agent ! IssuePowerControl(extTick, power)

              case None =>
                agent ! IssueNoControl(extTick)
            }

            val count = Try {
              uuidToInferior(receiver).count { id => emStates(id).isActivated }
            }.getOrElse(0)

            // sender -> number of set points to send
            Some(receiver -> count)

          case other =>
            log.warn(s"Cannot handle content: $other")
            None

        }
      }.toMap

      // check if we need to wait for internal answers
      val msgToExt = None

      // update state data
      val newState = copy(
        expectDataFrom = expectDataFrom.addExpectedKeys(mapping),
        completions = completions.addExpectedKeys(mapping.keySet),
      )

      (newState, msgToExt)

    case other =>
      log.warn(s"Unsupported message received! Message: $other")

      (this, None)
  }

  override def handleFlexResponse(
      tick: Long,
      flexResponse: FlexResponse,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    val receiverUuid = receiver match {
      case Left(value) =>
        value
      case Right(ref) =>
        agentToUuid(ref)
    }

    flexResponse match {
      case scheduleFlexActivation @ ScheduleFlexActivation(
            modelUuid,
            _,
            scheduleKey,
          ) =>
        log.warn(s"$scheduleFlexActivation not handled!")
        (this, None)

      case ProvideFlexOptions(sender, flexOptions) =>
        // flex option to ext
        val resultToExt = flexOptions.toExt(receiverUuid, sender)

        // wrap the result, if sender and receiver are not the same, since we want to use ext communication
        val msg = if receiverUuid != sender then {
          new EmCommunicationMessage(receiverUuid, sender, resultToExt)
        } else resultToExt

        val updated = expectDataFrom.addData(sender, msg)

        if updated.isComplete || updated.hasCompleted then {
          val (data, updatedExpectDataFrom) = updated.getFinished

          // should no longer wait for internal data
          data.keys.foreach(emStates(_).setWaitingForInternal(false))

          (
            copy(expectDataFrom = updatedExpectDataFrom),
            Some(new EmResultResponse(data.asJava)),
          )
        } else {
          (
            copy(expectDataFrom = updated),
            None,
          )
        }

      case completion @ FlexCompletion(
            sender,
            requestAtNextActivation,
            requestAtTick,
          ) if tick != INIT_SIM_TICK =>
        // the completion can be sent directly to the receiver, since it's not used by the external communication
        uuidToAgent(receiverUuid) ! completion
        emStates(sender).setWaitingForInternal(false)

        val updatedData = completions.addData(sender, completion)

        if updatedData.isComplete then {
          emStates.foreach(_._2.clear())
          log.warn(s"Cleared EmStates: $emStates")

          // the next activations
          val additionalActivation = updatedData.receivedData.flatMap {
            case (uuid, msg) =>
              msg.requestAtTick.map(uuid -> _)
          }

          (
            copy(
              completions = ReceiveDataMap.empty,
              expectDataFrom = ReceiveMultiDataMap.empty,
              nextActivation = nextActivation ++ additionalActivation,
            ),
            Some(new EmCompletion(getMaybeNextTick(tick))),
          )
        } else {
          val msgToExt = getMsgToExtOption
          // log.warn(s"Not finished! Expected: ${updatedData.getExpectedKeys}")

          (copy(completions = updatedData), msgToExt)
        }

      case completion: FlexCompletion =>
        receiver.foreach(_ ! completion)
        (this, None)

      // not supported
      case other =>
        log.warn(s"Flex response $other is not supported!")

        (this, None)
    }
  }

  override def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    val receiverUuid = agentToUuid(receiver) // the controlled em
    val sender = uuidToParent(receiverUuid) // the controlling em

    val updated = flexRequest match {
      case FlexActivation(tick, disaggregated, _) =>
        // update the em state => waiting for external flex option provision
        emStates(sender).addSendRequest(receiverUuid)

        // send request to ext
        expectDataFrom.addData(
          sender,
          new EmCommunicationMessage(
            receiverUuid,
            sender,
            new FlexOptionRequest(receiverUuid, disaggregated),
          ),
        )

      case control: IssueFlexControl =>
        val state = emStates(receiverUuid)

        if state.isWaitingForRelease then {
          // we are waiting for release, therefore, we are not sending data to ext

          state.setReceivedRelease()
          receiver ! control

          // since we don't expect data, we simply return this store
          expectDataFrom

        } else {
          state.setWaitingForInternal(false)

          // send set point to ext
          val power = control match {
            case IssueNoControl(tick) =>
              new PValue(null)

            case IssuePowerControl(tick, setPower) =>
              new PValue(setPower.toQuantity)

            case other =>
              throw new CriticalFailureException(
                s"Flex control $other is not supported!"
              )
          }

          expectDataFrom.addData(
            sender,
            new EmCommunicationMessage(
              receiverUuid,
              sender,
              new EmSetPoint(receiverUuid, power),
            ),
          )
        }

      case other =>
        log.warn(s"$other is not supported!")
        expectDataFrom
    }

    if updated.isComplete || updated.hasCompleted then {
      val (data, updatedExpectDataFrom) = updated.getFinished

      // should no longer wait for internal data
      data.keys.foreach(emStates(_).setWaitingForInternal(false))

      (
        copy(expectDataFrom = updatedExpectDataFrom),
        Some(new EmResultResponse(data.asJava)),
      )
    } else {
      (
        copy(expectDataFrom = updated),
        None,
      )
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
