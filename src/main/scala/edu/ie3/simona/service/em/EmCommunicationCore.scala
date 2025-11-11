/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.em.EmAgent.Message
import edu.ie3.simona.api.data.connection.ExtEmDataConnection.EmMode
import edu.ie3.simona.api.data.model.em
import edu.ie3.simona.api.data.model.em.*
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.flex.{FlexType, FlexibilityMessage, PowerLimitFlexOptions}
import edu.ie3.simona.service.em.EmCommunicationCore.EmAgentState
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.SimonaConstants.PRE_INIT_TICK
import edu.ie3.simona.util.{ReceiveDataMap, ReceiveMultiDataMap}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger
import squants.Power

import java.util.UUID
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.{RichOption, RichOptional}
import scala.math.max
import scala.util.Try

object EmCommunicationCore {

  def apply(core: EmServiceCore): EmCommunicationCore = {
    val uuidToAgent = core.uuidToAgent

    EmCommunicationCore(
      core.mode,
      core.lastFinishedTick,
      uuidToAgent,
      core.agentToUuid,
      core.uuidToInferior,
      core.uuidToParent,
      core.completions,
      core.nextActivation,
      core.allFlexOptions,
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
    override val mode: EmMode,
    override val lastFinishedTick: Long = PRE_INIT_TICK,
    override val uuidToAgent: Map[UUID, ActorRef[Message]] = Map.empty,
    override val agentToUuid: Map[
      ActorRef[FlexRequest] | ActorRef[FlexResponse],
      UUID,
    ] = Map.empty,
    override val uuidToInferior: Map[UUID, Set[UUID]] = Map.empty,
    override val uuidToParent: Map[UUID, UUID] = Map.empty,
    override val completions: ReceiveDataMap[UUID, FlexCompletion] =
      ReceiveDataMap.empty,
    override val nextActivation: Map[UUID, Long] = Map.empty,
    override val allFlexOptions: Map[UUID, FlexOptions] = Map.empty,
    emStates: Map[UUID, EmAgentState] = Map.empty,
    disaggregated: Map[UUID, Boolean] = Map.empty,
    requestedFlexType: Map[UUID, FlexType] = Map.empty,
    currentSetPoint: Map[UUID, Power] = Map.empty,
    activatedAgents: Set[UUID] = Set.empty,
    expectDataFrom: ReceiveMultiDataMap[UUID, EmData] =
      ReceiveMultiDataMap.empty,
) extends EmServiceCore {

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

        val nextTick: java.util.Optional[java.lang.Long] =
          if activatedAgents.nonEmpty then {
            requestEmCompletion.maybeNextTick
          } else getMaybeNextTick.map(long2Long).toJava

        (
          copy(lastFinishedTick = tick),
          Some(new EmCompletion(nextTick)),
        )
      }

    case provideEmData: ProvideEmData =>
      log.warn(s"Handling ext message: $provideEmData")
      val extTick = provideEmData.tick

      // handling of requests
      val (flexRelease, flexRequest) =
        provideEmData.flexRequests.asScala.partition { case (_, request) =>
          request.releaseControl()
        }

      flexRelease.keys.foreach { uuid =>
        log.warn(s"Release control for: $uuid")

        val inferior = uuidToInferior(uuid)

        uuidToAgent.get(uuid).foreach { agent =>
          // update the em states of the inferior
          inferior.flatMap(emStates.get).foreach(_.setWaitingForRelease())

          agent ! IssueNoControl(tick)
        }
      }

      val requestMapping = flexRequest.keys.flatMap { uuid =>
        if emStates(uuid).isWaitingForActivation then {

          uuidToAgent.get(uuid).map { agent =>
            // update the em state
            emStates(uuid).setReceivedRequest()

            agent ! FlexShiftActivation(
              tick,
              requestedFlexType.getOrElse(uuid, FlexType.PowerLimit),
            )

            log.warn(s"Inferior: ${uuidToInferior.get(uuid)}")

            val count = max(
              Try {
                uuidToInferior(uuid).count { id =>
                  nextActivation(id) <= tick
                }
              }.getOrElse(1),
              1,
            )

            // uuid -> number of sent flex requests
            uuid -> count
          }
        } else None
      }.toMap

      val updatedDisaggregated = disaggregated ++ flexRequest.map {
        case (uuid, request) => uuid -> request.disaggregated.booleanValue
      }.toMap

      // handling of set points
      val setPointMapping = provideEmData
        .setPoints()
        .asScala
        .flatMap { case (receiver, setPoint) =>
          val agent = uuidToAgent(receiver)
          log.warn(s"Receiver of set point: $agent")

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
      log.warn(s"ExpectDataFrom: $updatedExpectDataFrom")

      // check if we need to wait for internal answers
      val msgToExt = getMsgToExtOption

      // update state data
      val newState = copy(
        disaggregated = updatedDisaggregated,
        expectDataFrom = updatedExpectDataFrom,
        completions = completions.addExpectedKeys(mapping.keySet),
      )

      log.warn(s"EmStates: ${newState.emStates}")
      log.warn(s"Message to ext: $msgToExt")

      (newState, msgToExt)

    case comMsg: EmCommunicationMessages =>
      log.warn(s"Handling ext message: $comMsg")

      val messages = comMsg.messages.asScala
      val extTick = comMsg.tick

      val mapping = messages.flatMap { msg =>
        val receiver = msg.receiver
        val sender = msg.sender

        msg.content match {
          case _: FlexOptionRequest =>
            uuidToAgent.get(receiver) match {
              case Some(agent) =>
                // update the em state
                emStates(receiver).setReceivedRequest()

                agent ! FlexShiftActivation(
                  tick,
                  requestedFlexType.getOrElse(receiver, FlexType.PowerLimit),
                )

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

          case flexOption: FlexOptions =>
            val agent = uuidToAgent(receiver)
            val emState = emStates(receiver)

            // update the em state
            emState.handleReceivedFlexOption(sender)

            flexOption match {
              case options: em.PowerLimitFlexOptions =>
                // send flex options to agent
                agent ! ProvideFlexOptions(
                  sender,
                  PowerLimitFlexOptions(
                    options.pRef.toSquants,
                    options.pMin.toSquants,
                    options.pMax.toSquants,
                  ),
                )
              case other =>
                log.warn(s"Cannot handle flex option: $other")
            }

            // receiver -> number of received flex options
            Some(receiver -> 1)

          case flexOptions: FlexOptionsResult =>
            val agent = uuidToAgent(receiver)

            val emState = emStates(receiver)

            // update the em state
            emState.handleReceivedFlexOption(sender)

            // send flex options to agent
            agent ! ProvideFlexOptions(
              sender,
              PowerLimitFlexOptions(
                flexOptions.getpRef.toSquants,
                flexOptions.getpMin.toSquants,
                flexOptions.getpMax.toSquants,
              ),
            )

            // receiver -> number of received flex options
            Some(receiver -> 1)

          case setPoint: EmSetPoint =>
            val agent = uuidToAgent(receiver)
            log.warn(s"Receiver of set point: $agent")

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
      }.toMap

      val updatedExpectDataFrom = expectDataFrom.addExpectedKeys(mapping)

      log.warn(s"ExpectDataFrom: $updatedExpectDataFrom, Changes: $mapping")

      // check if we need to wait for internal answers
      val msgToExt = getMsgToExtOption

      // update state data
      val newState = copy(
        expectDataFrom = updatedExpectDataFrom,
        completions = completions.addExpectedKeys(mapping.keySet),
      )

      log.warn(s"EmStates: ${newState.emStates}")
      log.warn(s"Message to ext: $msgToExt")

      (newState, msgToExt)

    case other =>
      log.warn(s"Deprecated message received! Message: $other")

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
      case scheduleFlexActivation@ScheduleFlexActivation(
      modelUuid,
      _,
      scheduleKey,
      ) =>
        log.warn(s"$scheduleFlexActivation not handled!")
        (this, None)

      case ProvideFlexOptions(sender, flexOptions) =>
        // flex option to ext
        val (resultToExt, pRef) = flexOptions match {
          case PowerLimitFlexOptions(ref, min, max) =>
            val flexOptionResult = new em.PowerLimitFlexOptions(
              receiverUuid,
              sender,
              ref.toQuantity,
              min.toQuantity,
              max.toQuantity,
            )

            if disaggregated.contains(receiverUuid) then {
              uuidToInferior(receiverUuid)
                .flatMap(allFlexOptions.get)
                .foreach { result =>
                  val model = result match {
                    case options: em.EnergyBoundariesFlexOptions =>
                      options.model
                    case options: em.PowerLimitFlexOptions =>
                      options.model
                  }

                  flexOptionResult.addDisaggregated(model, result)
                }
            }

            (flexOptionResult, ref)

          case other =>
            throw CriticalFailureException(
              s"Flex option type '$other' is currently not supported!"
            )
        }

        // wrap the result, if sender and receiver are not the same, since we want to use ext communication
        val msg = if receiverUuid != sender then {
          new EmCommunicationMessage(receiverUuid, sender, resultToExt)
        } else resultToExt

        val updated = expectDataFrom.addData(sender, msg)

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


      case completion@FlexCompletion(
      sender,
      requestAtNextActivation,
      requestAtTick,
      ) =>
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
              lastFinishedTick = tick,
              completions = ReceiveDataMap.empty,
              requestedFlexType = Map.empty,
              allFlexOptions = Map.empty,
              currentSetPoint = Map.empty,
              activatedAgents = Set.empty,
              expectDataFrom = ReceiveMultiDataMap.empty,
              nextActivation = nextActivation ++ additionalActivation,
            ),
            Some(new EmCompletion(getMaybeNextTick.map(long2Long).toJava)),
          )
        } else {
          val msgToExt = getMsgToExtOption
          log.warn(s"Not finished! Expected: ${updatedData.getExpectedKeys}")
          log.warn(s"EmStates: $emStates")
          log.warn(s"Message to ext: $msgToExt")

          (copy(completions = updatedData), msgToExt)
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
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    val receiverUuid = agentToUuid(receiver) // the controlled em
    val sender = uuidToParent(receiverUuid) // the controlling em

    val updated = flexRequest match {
      case FlexActivation(tick, flexType) =>
        // update the em state => waiting for external flex option provision
        emStates(sender).addSendRequest(receiverUuid)

        // send request to ext
        expectDataFrom.addData(
          sender,
          new EmCommunicationMessage(
            receiverUuid,
            sender,
            new FlexOptionRequest(
              receiverUuid,
              disaggregated.getOrElse(sender, false),
            ),
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
          log.warn(
            s"Receiver $receiverUuid got flex control message from $sender"
          )

          val power = control match {
            case IssueNoControl(tick) =>
              log.warn(s"Set points: $currentSetPoint")
              new PValue(currentSetPoint(receiverUuid).toQuantity)

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
