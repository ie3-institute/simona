/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.FlexConversion
import edu.ie3.simona.api.data.model.em.{
  SetPoint,
  FlexOptions as ExtFlexOptions,
}
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  EmFlexMessage,
  EmServiceRegistration,
  ServiceResponseMessage,
}
import edu.ie3.simona.ontology.messages.flex.FlexType.PowerLimit
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.service.DataTimeType.Current
import edu.ie3.simona.service.em.EmServiceCore.EmAgentState
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.QuantityUtils.asMegaWatt
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger
import squants.Power
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.{OptionalLong, UUID}
import javax.measure.quantity.Power as PsdmPower
import scala.collection.mutable
import scala.jdk.OptionConverters.RichOptionalLong

/** Trait for all em service cores.
  */
abstract class EmServiceCore(
    val emUnitsToRegister: Set[UUID],
    val uuidToAgent: Map[UUID, ActorRef[EmAgent.Message]],
    val agentToUuid: Map[ActorRef[FlexRequest] | ActorRef[FlexResponse], UUID],
    val uncontrolled: Set[UUID],
    val uuidToInferior: Map[UUID, Set[UUID]],
    val uuidToParent: Map[UUID, UUID],
    val completions: ReceiveDataMap[UUID, FlexCompletion],
    val nextActivation: Map[UUID, Long],
    val allFlexOptions: Map[UUID, ExtFlexOptions],
    val emStates: Map[UUID, EmAgentState],
) {

  /** Extension to convert a squants power value to a psdm power value.
    */
  extension (value: Power) {
    def toQuantity: ComparableQuantity[PsdmPower] = value.toMegawatts.asMegaWatt
  }

  given Conversion[OptionalLong, Option[Long]] =
    (x: OptionalLong) => x.toScala
  given Conversion[Option[Long], OptionalLong] = {
    case Some(value) => OptionalLong.of(value)
    case None        => OptionalLong.empty
  }

  final def init(): Unit = {
    uncontrolled.foreach(uuidToAgent(_) ! FlexInit(PowerLimit, Current))
  }

  /** Method to handle a registration message.
    *
    * @param emServiceRegistration
    *   The registration to handle.
    * @return
    *   An updated service core.
    */
  def handleRegistration(
      emServiceRegistration: EmServiceRegistration
  ): EmServiceCore = {
    val uuid = emServiceRegistration.inputUuid
    val ref = emServiceRegistration.requestingActor

    val (
      updatedUncontrolled,
      updatedInferior,
      updatedUuidToParent,
      updatedCompletions,
    ) =
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
            completions,
          )
        case None =>
          (
            uncontrolled + uuid,
            uuidToInferior,
            uuidToParent,
            completions.addExpectedKey(uuid),
          )
      }

    updated(
      emUnitsToRegister = emUnitsToRegister.excl(uuid),
      uuidToAgent = uuidToAgent.updated(uuid, ref),
      agentToUuid = agentToUuid.updated(ref, uuid),
      uncontrolled = updatedUncontrolled,
      uuidToInferior = updatedInferior,
      uuidToParent = updatedUuidToParent,
      completions = updatedCompletions,
      nextActivation = nextActivation.updated(uuid, 0),
      emStates = emStates.updated(uuid, EmAgentState()),
    )
  }

  def updated(
      emUnitsToRegister: Set[UUID],
      uuidToAgent: Map[UUID, ActorRef[EmAgent.Message]],
      agentToUuid: Map[ActorRef[FlexRequest] | ActorRef[FlexResponse], UUID],
      uncontrolled: Set[UUID],
      uuidToInferior: Map[UUID, Set[UUID]],
      uuidToParent: Map[UUID, UUID],
      completions: ReceiveDataMap[UUID, FlexCompletion],
      nextActivation: Map[UUID, Long],
      emStates: Map[UUID, EmAgentState],
  ): EmServiceCore

  /** Method to handle the received message from the external simulation.
    * @param tick
    *   Current tick of the service.
    * @param extMsg
    *   The message from external.
    * @param log
    *   Logger for logging messages.
    * @return
    *   An updated service core and an option for a message that should be sent
    *   to the external simulation.
    */
  def handleExtMessage(
      tick: Long,
      extMsg: EmDataMessageFromExt,
  )(using
      log: Logger
  ): (EmServiceCore, Option[EmDataResponseMessageToExt])

  /** Method to handle data response messages from the em agents.
    * @param tick
    *   Current tick of the service.
    * @param responseMsg
    *   To handle.
    * @param startTime
    *   The start time of the simulation.
    * @param log
    *   Logger for logging messages.
    * @return
    *   An updated service core and an option for a message that should be sent
    *   to the external simulation.
    */
  final def handleDataResponseMessage(
      tick: Long,
      responseMsg: ServiceResponseMessage,
  )(using
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = responseMsg match {
    case EmFlexMessage(flexRequest: FlexRequest, receiver) =>
      log.debug(s"$receiver <- $flexRequest")

      receiver match {
        case ref: ActorRef[FlexRequest] =>
          if tick == INIT_SIM_TICK then {
            ref ! flexRequest

            (this, None)
          } else {
            handleFlexRequest(flexRequest, ref)
          }

        case _ =>
          // should not happen
          log.warn(s"No receiver found for msg: $flexRequest")
          (this, None)
      }

    case EmFlexMessage(flexResponse: FlexResponse, receiver) =>
      log.debug(s"$receiver <- $flexResponse")

      receiver match {
        case uuid: UUID =>
          handleFlexResponse(tick, flexResponse, Left(uuid))

        case ref: ActorRef[FlexResponse] =>
          if tick == INIT_SIM_TICK then {
            ref ! flexResponse
            (this, None)
          } else {
            handleFlexResponse(tick, flexResponse, Right(ref))
          }
      }
  }

  /** Method to handle the set points provided by the external simulation.
    * @param tick
    *   Current tick of the service.
    * @param setPoints
    *   The set points to handle.
    * @param log
    *   Logger for logging messages.
    */
  final def handleSetPoint(
      tick: Long,
      setPoints: Map[UUID, SetPoint],
      log: Logger,
  ): Unit = {
    setPoints.foreach { case (agent, setPoint) =>
      uuidToAgent.get(agent) match {
        case Some(receiver) =>
          receiver ! FlexConversion.convert(tick, setPoint)

        case None =>
          log.warn(s"No em agent with uuid '$agent' registered!")
      }
    }
  }

  /** Method to handle flex responses from the em agents.
    * @param tick
    *   Current tick of the service.
    * @param flexResponse
    *   From the agent to handle.
    * @param receiver
    *   The receiver of the agent.
    * @param log
    *   Logger for logging messages.
    * @return
    *   An updated service core and an option for a message that should be sent
    *   to the external simulation.
    */
  def handleFlexResponse(
      tick: Long,
      flexResponse: FlexResponse,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt])

  /** Method to handle flex requests to the em agents.
    * @param flexRequest
    *   That is sent to an agents.
    * @param receiver
    *   Of the flex request.
    * @param log
    *   Logger for logging messages.
    * @return
    *   An updated service core and an option for a message that should be sent
    *   to the external simulation.
    */
  def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt])

  /** Method to handle received completion messages.
    * @param tick
    *   Current tick of the service.
    * @param completion
    *   To handle.
    * @return
    *   The updated ReceiveDataMap an option for or a message that should be
    *   sent to the external simulation and a boolean that tells, if all
    *   completions have been received.
    */
  final def handleCompletion(tick: Long, completion: FlexCompletion): (
      ReceiveDataMap[UUID, FlexCompletion],
      Option[EmDataResponseMessageToExt],
      Option[Long],
      Boolean,
  ) = {
    val uuid = completion.modelUuid

    if completions.expects(uuid) then {
      val updated = completions.addData(uuid, completion)

      if updated.isComplete then {
        val (extMsgOption, nextTickOption) = if tick != INIT_SIM_TICK then {
          // send completion message to external simulation, if we aren't in the INIT_SIM_TICK
          val option = getMaybeNextTick(tick)

          (Some(new EmCompletion(option)), option)
        } else (None, None)

        // every em agent has sent a completion message
        (updated, extMsgOption, nextTickOption, true)

      } else (updated, None, None, false)
    } else (completions, None, None, false)
  }

  /** Method to calculate the next tick option.
    * @return
    *   An option for the next activation tick.
    */
  final def getMaybeNextTick(tick: Long): Option[Long] = {
    val allActivations = completions.receivedData.flatMap {
      case (_, completion) =>
        completion.requestAtTick
    } ++ nextActivation.values.filter(_ > tick)

    allActivations.minOption
  }
}

object EmServiceCore {

  final case class EmAgentState(
      private var receivedActivation: Boolean = false,
      private var disaggregated: Boolean = false,
      private val awaitedFlexOptions: mutable.Set[UUID] = mutable.Set.empty,
      private var awaitedSetPoint: Boolean = false,
      private var waitingForInternal: Boolean = false,
      private var waitingForRelease: Boolean = false,
  ) {
    def setReceivedRequest(value: Boolean = false): Unit = {
      receivedActivation = true
      disaggregated = value
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

    def sentDisaggregated: Boolean = disaggregated

    def isWaitingForActivation: Boolean = !receivedActivation

    def isWaitingForExtern: Boolean =
      (awaitedFlexOptions.nonEmpty || awaitedSetPoint) && !waitingForInternal

    def isWaitingForSetPoint: Boolean = awaitedSetPoint

    def isWaitingForRelease: Boolean = waitingForRelease

    def isWaitingForInternal: Boolean = waitingForInternal

    def isActivated: Boolean = receivedActivation

    def clear(): Unit = {
      receivedActivation = false
      disaggregated = false
      awaitedFlexOptions.clear
      awaitedSetPoint = false
      waitingForInternal = false
    }
  }
}
