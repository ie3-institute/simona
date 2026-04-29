/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.FlexConversion
import edu.ie3.simona.api.FlexConversion.{convert, convertOptions}
import edu.ie3.simona.api.data.connection.ExtEmDataConnection
import edu.ie3.simona.api.data.connection.ExtEmDataConnection.EmMode
import edu.ie3.simona.api.data.model.em.{DisaggregatedFlexOptions, EmCommunicationMessage, EmData, FlexOptionRequest, SetPoint, FlexOptions as ExtFlexOptions}
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{Completion, ScheduleActivation}
import edu.ie3.simona.ontology.messages.ServiceMessage.{EmFlexMessage, EmServiceRegistration, ServiceResponseMessage}
import edu.ie3.simona.ontology.messages.flex.FlexType.PowerLimit
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.service.DataTimeType.Current
import edu.ie3.simona.service.em.EmServiceCore.EmAgentState
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.SimonaConstants.{FIRST_TICK_IN_SIMULATION, INIT_SIM_TICK}
import edu.ie3.simona.util.{ReceiveDataMap, ReceiveMultiDataMap}
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger

import java.time.ZonedDateTime
import java.util.{OptionalLong, UUID}
import scala.collection.mutable
import scala.jdk.CollectionConverters.{CollectionHasAsScala, MapHasAsJava, MapHasAsScala}
import scala.jdk.OptionConverters.RichOptionalLong
import scala.math.max
import scala.util.Try

/** Basic service core for an [[ExtEmDataService]].
  *
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
  * @param sendDataToExt
  *   True, if em data should be sent to the external simulation.
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
                          emUnitsToRegister: Set[UUID],
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
                          sendDataToExt: Boolean = true,
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
      // the service should simulate the tick internal
      val internalTick = internal.tick

      val uuids = uncontrolled
        .filter { uuid => nextActivation(uuid) == internalTick }
        .map { uuid =>
          uuidToAgent(uuid) ! FlexActivation(tick)
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

    case requestEmCompletion: RequestEmCompletion =>
      // finish tick and return next tick
      val extTick = requestEmCompletion.tick

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

    case provideEmData: ProvideEmData if mode == EmMode.EM_COMMUNICATION =>
      log.debug(s"Handling ext message: $provideEmData")

      // handling of requests
      val flexRequests = provideEmData.flexRequests.asScala

      val requestMapping = flexRequests.flatMap { case (uuid, request) =>
        if emStates(uuid).isWaitingForActivation then {

          uuidToAgent.get(uuid).map { agent =>
            // update the em state
            emStates(uuid).setReceivedRequest(request.disaggregated)

            agent ! FlexActivation(tick, true)

            val count = Try(uuidToInferior(uuid).size).getOrElse(0)

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

          agent ! FlexConversion.convert(tick, setPoint)

          val count = Try {
            uuidToInferior(receiver).count { id => emStates(id).isActivated }
          }.getOrElse(0)

          // sender -> number of set points to send
          Some(receiver -> count)
        }
        .toMap

      /* update internal state */
      val mapping = requestMapping ++ setPointMapping

      val updatedExpectDataFrom = emDataStore.addExpectedKeys(mapping)

      // check if we need to wait for internal answers
      val msgToExt = getMsgToExtOption

      // update state data
      val newState = copy(
        emDataStore = updatedExpectDataFrom,
        completions = completions.addExpectedKeys(mapping.keySet),
      )

      log.warn(s"Updated store: ${newState.emDataStore.getExpected}")

      (newState, msgToExt)

    case provideEmData: ProvideEmData =>
      if !provideEmData.flexOptions.isEmpty then {
        log.warn(
          s"We received the following data '$provideEmData'. The base service can currently not handle the provided flex options."
        )
      }

      val agents = if tick == 0 then {
        uuidToAgent
      } else {
        uuidToAgent.filter { case (uuid, _) => nextActivation(uuid) <= tick }
      }

      val flexRequests = provideEmData.flexRequests.asScala.flatMap {
        case (entity, _) =>
          agents.get(entity).map { ref =>
            ref ! FlexActivation(tick)
            entity
          }
      }.toSet

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

    case comMsg: EmCommunicationMessages if mode == EmMode.EM_COMMUNICATION =>
      val messages = comMsg.messages.asScala

      val mapping = messages.flatMap { msg =>
        val receiver = msg.receiver
        val sender = msg.sender

        msg.content match {
          case request: FlexOptionRequest =>
            uuidToAgent.get(receiver) match {
              case Some(agent) =>
                // update the em state
                emStates(receiver).setReceivedRequest(request.disaggregated)

                agent ! FlexActivation(tick, true)

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
              convertOptions(flexOption),
            )

            // receiver -> number of received flex options
            Some(receiver -> 1)

          case setPoint: SetPoint =>
            val agent = uuidToAgent(receiver)

            // updates the em state
            emStates(receiver).setReceivedSetPoint()

            agent ! convert(tick, setPoint)

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
        emDataStore = emDataStore.addExpectedKeys(mapping),
        completions = completions.addExpectedKeys(mapping.keySet),
      )

      log.warn(s"Old store: ${emDataStore.getExpected}")
      log.warn(s"Mapping: $mapping")
      log.warn(s"Store: ${newState.emDataStore.getExpected}")

      (newState, msgToExt)

    case other =>
      throw new CriticalFailureException(
        s"The EmServiceBaseCore is not able to handle the message: $extMsg"
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
              scheduleKey
            )

          case Right(ref) =>
            ref ! scheduleFlexActivation
        }

        (this, None)

      case provideFlexOptions @ ProvideFlexOptions(sender, flexOptions)
          if mode == EmMode.EM_COMMUNICATION =>
        if internal.nonEmpty then {
          receiver match {
            case Left(uuid) =>
              uuidToAgent(uuid) ! IssueNoControl(tick)
            case Right(ref) =>
              ref ! provideFlexOptions
          }

          (this, None)

        } else {
          // flex option to ext
          val convertedOption = flexOptions.toExt(receiverUuid, sender)

          val resultToExt = if emStates(sender).sentDisaggregated then {
            val disaggregatedOptions = uuidToInferior(receiverUuid)
              .map { uuid =>
                uuid -> allFlexOptions(uuid)
              }
              .toMap
              .asJava

            new DisaggregatedFlexOptions(receiverUuid, disaggregatedOptions)
          } else convertedOption

          // wrap the result, if sender and receiver are not the same, since we want to use ext communication
          val msg = if receiverUuid != sender then {
            new EmCommunicationMessage(receiverUuid, sender, resultToExt)
          } else resultToExt

          val updated = emDataStore.addData(sender, msg)

          if updated.isComplete || updated.hasCompleted then {
            val (data, updatedExpectDataFrom) = updated.getFinished

            // should no longer wait for internal data
            data.keys.foreach(emStates(_).setWaitingForInternal(false))

            (
              copy(
                emDataStore = updatedExpectDataFrom,
                allFlexOptions = allFlexOptions.updated(sender, convertedOption),
              ),
              Some(new EmResultResponse(data.asJava)),
            )
          } else {
            (
              copy(
                emDataStore = updated,
                allFlexOptions = allFlexOptions.updated(sender, convertedOption),
              ),
              None,
            )
          }
        }

      case provideFlexOptions: ProvideFlexOptions =>
        val updated = handleFlexOptions(receiverUuid, provideFlexOptions)

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

      case completion @ FlexCompletion(modelUuid, _, _) if mode == EmMode.EM_COMMUNICATION =>
        val agent = uuidToAgent(receiverUuid)

        if tick < FIRST_TICK_IN_SIMULATION && uncontrolled.contains(modelUuid) then {
          scheduler ! Completion(agent)
        }

        // the completion can be sent directly to the receiver, since it's not used by the external communication
        agent ! completion
        emStates(modelUuid).setWaitingForInternal(false)

        val updatedData = completions.addData(modelUuid, completion)

        if updatedData.isComplete then {
          emStates.foreach(_._2.clear())

          // the next activations
          val additionalActivation = updatedData.receivedData.flatMap {
            case (uuid, msg) =>
              msg.requestAtTick.map(uuid -> _)
          }

          (
            copy(
              completions = ReceiveDataMap.empty,
              emDataStore = ReceiveMultiDataMap.empty,
              nextActivation = nextActivation ++ additionalActivation,
              internal = Set.empty,
            ),
            Some(new EmCompletion(getMaybeNextTick(tick))),
          )
        } else {
          (copy(completions = updatedData), getMsgToExtOption)
        }

      case completion: FlexCompletion =>
        val (updated, extMsgOption, _, finished) =
          handleCompletion(tick, completion)

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

      case _ =>
        (this, None)
    }
  }

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
    if mode == EmMode.BASE then {

      log.debug(s"$receiver: $flexRequest")
      receiver ! flexRequest

      val uuid = agentToUuid(receiver)
      (copy(completions = completions.addExpectedKey(uuid)), None)

    } else {
      val receiverUuid = agentToUuid(receiver) // the controlled em
      val sender = uuidToParent(receiverUuid) // the controlling em

      if internal.nonEmpty then {
        receiver ! flexRequest

        (copy(completions = completions.addExpectedKey(receiverUuid)), None)

      } else {

        val updated = flexRequest match {
          case flexInit: FlexInit =>
            receiver ! flexInit
            emDataStore

          case FlexActivation(tick, _) =>
            // update the em state => waiting for external flex option provision
            emStates(sender).addSendRequest(receiverUuid)

            // send request to ext
            emDataStore.addData(
              sender,
              new EmCommunicationMessage(
                receiverUuid,
                sender,
                new FlexOptionRequest(receiverUuid),
              ),
            )

          case control: IssueFlexControl =>
            val state = emStates(receiverUuid)

            if state.isWaitingForRelease then {
              // we are waiting for release, therefore, we are not sending data to ext

              state.setReceivedRelease()
              receiver ! control

              // since we don't expect data, we simply return this store
              emDataStore

            } else {
              state.setWaitingForInternal(false)

              // send set point to ext
              emDataStore.addData(
                sender,
                new EmCommunicationMessage(
                  receiverUuid,
                  sender,
                  control.toExt(receiverUuid),
                ),
              )
            }

          case other =>
            log.warn(s"$other is not supported!")
            emDataStore
        }

        if updated.isComplete || updated.hasCompleted then {
          val (data, updatedExpectDataFrom) = updated.getFinished

          // should no longer wait for internal data
          data.keys.foreach(emStates(_).setWaitingForInternal(false))

          (
            copy(emDataStore = updatedExpectDataFrom),
            Some(new EmResultResponse(data.asJava)),
          )
        } else {
          (
            copy(emDataStore = updated),
            None,
          )
        }
      }

    }
  }

  /** Method to handle flex options.
    *
    * @param receiver
    *   The receiver of the flex options.
    * @param provideFlexOptions
    *   The provided flex options.
    * @return
    *   An updated service core and a map: uuid to flex options
    */
  private def handleFlexOptions(
      receiver: UUID,
      provideFlexOptions: ProvideFlexOptions,
  ): ReceiveMultiDataMap[UUID, EmData] = provideFlexOptions match {
    case ProvideFlexOptions(modelUuid: UUID, fo) =>
      val result = fo.toExt(receiver, modelUuid)

      if emDataStore.expects(modelUuid) then {
        emDataStore.addData(modelUuid, result)
      } else emDataStore

    case _ => emDataStore
  }

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
  private final def getMaybeNextTick(tick: Long): Option[Long] = {
    val allActivations = completions.receivedData.flatMap {
      case (_, completion) =>
        completion.requestAtTick
    } ++ nextActivation.values.filter(_ > tick)

    allActivations.minOption
  }

  private def getMsgToExtOption: Option[EmDataResponseMessageToExt] = {
    if emStates.exists(_._2.isWaitingForInternal) then {
      None
    } else {
      val awaited = emStates.filter((_, x) => x.isWaitingForExtern).map {
        case (uuid, state) => uuid -> state.getAwaited
      }

      if awaited.isEmpty then None
      else Some(new EmResultResponse(Map.empty.asJava))
    }
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
