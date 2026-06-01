/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.FlexConversion
import edu.ie3.simona.api.FlexConversion.convertOptions
import edu.ie3.simona.api.data.connection.ExtEmDataConnection
import edu.ie3.simona.api.data.connection.ExtEmDataConnection.EmMode
import edu.ie3.simona.api.data.model.em.{
  EmData,
  FlexOptionRequest,
  SetPoint,
  FlexOptions as ExtFlexOptions,
}
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  EmFlexMessage,
  EmServiceRegistration,
  ServiceResponseMessage,
}
import edu.ie3.simona.ontology.messages.flex.FlexType.PowerLimit
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.service.DataTimeType.Current
import edu.ie3.simona.service.em.EmServiceCore.EmAgentState
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.SimonaConstants.{
  FIRST_TICK_IN_SIMULATION,
  INIT_SIM_TICK,
}
import edu.ie3.simona.util.{ReceiveDataMap, ReceiveMultiDataMap}
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger

import java.time.ZonedDateTime
import java.util.{OptionalLong, UUID}
import scala.collection.mutable
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.RichOptionalLong

/** Basic service core for an [[ExtEmDataService]].
  *
  * @param mode
  *   The mode of the em service core.
  * @param scheduler
  *   Actor reference to the SIMONA scheduler.
  * @param sendDataToExt
  *   True, if em data should be sent to the external simulation.
  * @param uuidToAgent
  *   Map: uuid to em agent reference.
  * @param agentToUuid
  *   Map: em agent reference to uuid.
  * @param uncontrolled
  *   A set of uuids of uncontrolled em models.
  * @param uuidToInferior
  *   A map that contains information about uuids of inferior em agents. This
  *   information is used to determine the disaggregated flex options.
  * @param uuidToParent
  *   A map: uuid to parent uuid.
  * @param completions
  *   ReceiveDataMap: uuid to completions.
  * @param nextActivation
  *   A map: uuid to next activation tick.
  * @param allFlexOptions
  *   A map: uuid to flex options
  * @param emStates
  *   A map: uuid to em agent state.
  * @param emDataStore
  *   ReceiveMultiDataMap: uuid to flex option.
  * @param internal
  *   A set of uuids of models that simulated internally.
  * @param canHandleSetPoints
  *   True, if the core can sent the received em set points to the agent. It
  *   will only be true, of all em agent are activated for the current tick and
  *   therefore able to process the send set points.
  * @param setPointOption
  *   Option for em set points that needs to be handled at a later time.
  */
case class EmServiceCore(
    mode: ExtEmDataConnection.EmMode,
    scheduler: ActorRef[SchedulerMessage],
    sendDataToExt: Boolean = false,
    uuidToAgent: Map[UUID, ActorRef[EmAgent.Message]] = Map.empty,
    agentToUuid: Map[ActorRef[FlexRequest] | ActorRef[FlexResponse], UUID] =
      Map.empty,
    uncontrolled: Set[UUID] = Set.empty,
    uuidToInferior: Map[UUID, Set[UUID]] = Map.empty,
    uuidToParent: Map[UUID, UUID] = Map.empty,
    completions: ReceiveDataMap[UUID, FlexCompletion] = ReceiveDataMap.empty,
    nextActivation: Map[UUID, Long] = Map.empty,
    allFlexOptions: Map[UUID, ExtFlexOptions] = Map.empty,
    emStates: Map[UUID, EmAgentState] = Map.empty,
    emDataStore: ReceiveMultiDataMap[UUID, EmData] = ReceiveMultiDataMap.empty,
    internal: Set[UUID] = Set.empty,
    canHandleSetPoints: Boolean = false,
    setPointOption: Option[Map[UUID, SetPoint]] = None,
) {

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

    copy(
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
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = extMsg match {
    case internal: EmSimulationInternal =>
      simulateInternal(tick, internal)

    case requestEmCompletion: RequestEmCompletion =>
      handleExtCompletion(tick, requestEmCompletion)

    case provideEmData: ProvideEmData =>
      checkTick(tick, provideEmData.tick)

      if !provideEmData.flexOptions.isEmpty then {
        log.warn(
          s"We received the following data '$provideEmData'. The base service can currently not handle the provided flex options."
        )
      }

      val flexRequests =
        handleExtFlexRequests(tick, provideEmData.flexRequests.asScala)

      val updatedState = copy(
        emDataStore = emDataStore.addExpectedKeys(flexRequests),
        completions = completions.addExpectedKeys(flexRequests),
        sendDataToExt = flexRequests.nonEmpty,
      )

      // handle set points
      val setPoints = provideEmData.setPoints().asScala.toMap

      if setPoints.nonEmpty then {

        if canHandleSetPoints then {
          handleSetPoint(tick, setPoints, log)

          (updatedState, None)
        } else {
          val entities = setPoints.keySet

          entities.foreach { entity =>
            uuidToAgent.get(entity) match {
              case Some(ref) =>
                // activate the necessary em agent, this is needed, because an em agent needs to know
                // its current flex option to properly handle the given set point
                ref ! FlexActivation(tick)
              case None =>
                log.warn(s"Received entity: $entity")
            }
          }

          (
            updatedState.copy(
              emDataStore = updatedState.emDataStore.addExpectedKeys(entities),
              completions = updatedState.completions.addExpectedKeys(entities),
              setPointOption = Some(setPoints),
            ),
            None,
          )
        }

      } else (updatedState, None)

    case other =>
      throw new CriticalFailureException(
        s"The EmServiceBaseCore is not able to handle the message: $extMsg"
      )
  }

  /** Method for checking the ticks.
    * @param tick
    *   Current tick of SIMONA.
    * @param extTick
    *   Current tick of the external simulation.
    */
  private def checkTick(tick: Long, extTick: Long): Unit =
    if tick != extTick then {
      throw new CriticalFailureException(
        s"Simulations out of sync. SIMONA at tick $tick, external simulation at tick $extTick."
      )
    }

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
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {

    val receiverUuid = receiver match {
      case Right(ref) =>
        ref ! flexResponse
        agentToUuid(ref)
      case Left(uuid) =>
        uuid
    }

    flexResponse match {
      case scheduleFlexActivation @ ScheduleFlexActivation(
            modelUuid,
            tick,
            scheduleKey,
          ) if tick < FIRST_TICK_IN_SIMULATION =>
        receiver match {
          case Left(uuid) =>
            scheduler ! ScheduleActivation(
              uuidToAgent(uuid),
              tick,
              scheduleKey,
            )

          case Right(ref) =>
            ref ! scheduleFlexActivation
        }

        (this, None)

      case provideFlexOptions: ProvideFlexOptions =>
        handleFlexOptionProvision(
          tick,
          receiverUuid,
          provideFlexOptions,
        )

      case completion: FlexCompletion =>
        handleCompletion(tick, completion)

      case _ =>
        (this, None)
    }
  }

  /** Method to handle a request to simulate internally.
    * @param tick
    *   The current simulation tick.
    * @param internal
    *   The request to handle.
    * @return
    *   An updated service core and an option for a message that should be sent
    *   to the external simulation.
    */
  private def simulateInternal(
      tick: Long,
      internal: EmSimulationInternal,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    // the service should simulate the tick internal
    val internalTick = internal.tick
    checkTick(tick, internalTick)

    val uuids = uncontrolled
      .filter { uuid => nextActivation(uuid) == internalTick }
      .map { uuid =>
        uuidToAgent(uuid) ! FlexActivation(internalTick)
        uuid
      }

    (
      copy(
        completions = completions.addExpectedKeys(uuids),
        emDataStore = emDataStore.addExpectedKeys(uuids),
        internal = uuids,
      ),
      None,
    )
  }

  /** Method to handle an external em service completion request.
    *
    * @param tick
    *   The current simulation tick.
    * @param requestEmCompletion
    *   The request to handle.
    * @return
    *   An updated service core and an option for a message that should be sent
    *   to the external simulation.
    */
  private def handleExtCompletion(
      tick: Long,
      requestEmCompletion: RequestEmCompletion,
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    // finish tick and return next tick
    val extTick = requestEmCompletion.tick
    checkTick(tick, extTick)

    if extTick != tick then {
      throw new CriticalFailureException(
        s"Received completion request for tick '$extTick', while being in tick '$tick'."
      )
    } else {
      log.info(s"Request to finish for tick '$tick' received.")

      val nextTick: OptionalLong = if emStates.exists(_._2.isActivated) then {
        requestEmCompletion.maybeNextTick
      } else getMaybeNextTick(tick)

      (
        this,
        Some(new EmCompletion(nextTick)),
      )
    }
  }

  /** Method to handle external flex requests.
    * @param tick
    *   For which the request should be handled.
    * @param flexRequests
    *   The requests to handle.
    * @return
    *   A set of uuids of activated em agents.
    */
  private def handleExtFlexRequests(
      tick: Long,
      flexRequests: Iterable[(UUID, FlexOptionRequest)],
  ): Set[UUID] = {
    val agents = if tick == 0 then {
      uuidToAgent
    } else {
      uuidToAgent.filter { case (uuid, _) => nextActivation(uuid) <= tick }
    }

    flexRequests.flatMap { case (uuid, request) =>
      handleExtFlexRequest(tick, uuid, request, agents)
    }.toSet
  }

  /** Method to handle an external flex request.
    *
    * @param tick
    *   For which the request should be handled.
    * @param receiver
    *   The receiver uuid of the request.
    * @param request
    *   The request to handle.
    * @param agents
    *   A map: uuid to activatable em agents.
    * @return
    *   An option for an uuid. The option is [[None]] if no agent was activated.
    */
  private def handleExtFlexRequest(
      tick: Long,
      receiver: UUID,
      request: FlexOptionRequest,
      agents: Map[UUID, ActorRef[EmAgent.Message]],
  ): Option[UUID] =
    if emStates(receiver).isWaitingForActivation then {
      agents.get(receiver).map { ref =>
        // update the em state
        emStates(receiver).setReceivedRequest(request.disaggregated)

        ref ! FlexActivation(tick)
        receiver
      }

    } else None

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
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    log.debug(s"$receiver: $flexRequest")
    receiver ! flexRequest

    val uuid = agentToUuid(receiver)
    (
      copy(completions = completions.addExpectedKey(uuid)),
      None,
    )
  }

  /** Method to handle flex options.
    *
    * @param tick
    *   The current tick of the simulation.
    * @param receiver
    *   The receiver of the flex options.
    * @param provideFlexOptions
    *   The provided flex options.
    * @return
    *   An updated service core and a map: uuid to flex options
    */
  private def handleFlexOptionProvision(
      tick: Long,
      receiver: UUID,
      provideFlexOptions: ProvideFlexOptions,
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    val updated = provideFlexOptions match {
      case ProvideFlexOptions(modelUuid: UUID, fo) =>
        val result = convertOptions(fo, receiver, modelUuid)

        if emDataStore.expects(modelUuid) then {
          emDataStore.addData(modelUuid, result)
        } else emDataStore

      case _ => emDataStore
    }

    if updated.isComplete then {
      // we received all flex options

      val (data, updatedStore) = updated.getFinished

      val updatedCore = copy(
        emDataStore = updatedStore,
        canHandleSetPoints = true,
      )

      if internal.nonEmpty then {
        internal.map(uuidToAgent).foreach(_ ! IssueNoControl(tick))

        (updatedCore, None)

      } else if sendDataToExt then {
        val dataToSend = data

        // we have received an option request, that will now be answered
        (updatedCore, Some(new EmResultResponse(dataToSend.asJava)))

      } else {

        setPointOption match {
          case Some(setPoints) =>
            // we have received new set points, that are not handled yet => we will handle them now
            handleSetPoint(tick, setPoints, log)

            (updatedCore.copy(setPointOption = None), None)
          case None =>
            // we are now able to handle set points, but we have not yet received any
            (updatedCore, None)
        }
      }

    } else {
      log.debug(s"Missing flex options for: ${updated.getExpectedKeys}")

      (copy(emDataStore = updated), None)
    }
  }

  /** Method to handle received completion messages.
    *
    * @param tick
    *   Current tick of the service.
    * @param completion
    *   To handle.
    * @return
    *   The updated ReceiveDataMap an option for or a message that should be
    *   sent to the external simulation and a boolean that tells, if all
    *   completions have been received.
    */
  private def handleCompletion(
      tick: Long,
      completion: FlexCompletion,
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    val (updated, extMsgOption, finished) = {
      val uuid = completion.modelUuid

      if completions.expects(uuid) then {
        val updated = completions.addData(uuid, completion)

        if updated.isComplete then {
          val extMsgOption = if tick != INIT_SIM_TICK then {
            // send completion message to external simulation, if we aren't in the INIT_SIM_TICK
            val option = getMaybeNextTick(tick, completion.requestAtTick)

            Some(new EmCompletion(option))
          } else None

          // every em agent has sent a completion message
          (updated, extMsgOption, true)

        } else (updated, None, false)
      } else (completions, None, false)
    }

    if finished then {
      if tick < FIRST_TICK_IN_SIMULATION then {
        uncontrolled.foreach(uuid => scheduler ! Completion(uuidToAgent(uuid)))
      }

      // the next activations
      val updatedNextActivation =
        nextActivation ++ updated.receivedData.flatMap { case (uuid, msg) =>
          msg.requestAtTick.map(uuid -> _)
        }

      val updatedStateData = copy(
        completions = ReceiveDataMap.empty,
        sendDataToExt = false,
        canHandleSetPoints = false,
        nextActivation = updatedNextActivation,
        internal = Set.empty,
      )

      val msgToExt = if internal.nonEmpty then {
        Some(new EmCompletion(updatedStateData.getMaybeNextTick(tick)))
      } else extMsgOption

      log.info(s"Em service completed for tick: $tick")

      (updatedStateData, msgToExt)

    } else {
      log.debug(s"Missing completion for: ${updated.getExpectedKeys}")

      (copy(completions = updated), extMsgOption)
    }
  }

  /** Method to calculate the next tick option.
    * @return
    *   An option for the next activation tick.
    */
  private final def getMaybeNextTick(
      tick: Long,
      option: Option[Long] = None,
  ): Option[Long] = {
    val allActivations = completions.receivedData.flatMap {
      case (_, completion) =>
        completion.requestAtTick
    } ++ nextActivation.values.filter(_ > tick) ++ option

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
