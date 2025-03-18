/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.results

import edu.ie3.datamodel.models.result.connector.LineResult
import edu.ie3.datamodel.models.result.system.{
  FlexOptionsResult,
  SystemParticipantResult,
}
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.api.data.results.ExtResultDataConnection
import edu.ie3.simona.api.data.results.ontology.{
  ProvideResultEntities,
  RequestResultEntities,
  ResultDataMessageFromExt,
}
import edu.ie3.simona.event.listener.DelayedStopHelper
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters._

object ExtResultDataProvider {

  trait Request

  final case class WrappedActivation(activation: Activation) extends Request

  /** ExtSimulation -> ExtResultDataProvider */
  final case class WrappedResultDataMessageFromExt(
      extResultDataMessageFromExt: ResultDataMessageFromExt
  ) extends Request
  final case class WrappedScheduleServiceActivationAdapter(
      scheduleServiceActivationMsg: ScheduleServiceActivation
  ) extends Request

  final case class RequestDataMessageAdapter(
      sender: ActorRef[ActorRef[ResultDataMessageFromExt]]
  ) extends Request

  final case class RequestScheduleActivationAdapter(
      sender: ActorRef[ActorRef[ScheduleServiceActivation]]
  ) extends Request

  /** ResultEventListener -> ExtResultDataProvider */
  final case class ResultResponseMessage(
      result: ResultEntity
  ) extends Request {
    def tick(implicit startTime: ZonedDateTime): Long =
      TimeUtil.withDefaults.zonedDateTimeDifferenceInSeconds(
        startTime,
        result.getTime,
      )
  }

  /** ExtResultDataProvider -> ExtResultDataProvider */
  final case class ResultRequestMessage(
      currentTick: Long
  ) extends Request

  final case class Create(
      initializeStateData: InitExtResultData,
      unlockKey: ScheduleKey,
  ) extends Request

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  def apply(
      scheduler: ActorRef[SchedulerMessage],
      startTime: ZonedDateTime,
  ): Behavior[Request] = Behaviors.withStash(5000000) { buffer =>
    Behaviors.setup[Request] { ctx =>
      val activationAdapter: ActorRef[Activation] =
        ctx.messageAdapter[Activation](msg => WrappedActivation(msg))
      val resultDataMessageFromExtAdapter: ActorRef[ResultDataMessageFromExt] =
        ctx.messageAdapter[ResultDataMessageFromExt](msg =>
          WrappedResultDataMessageFromExt(msg)
        )
      val scheduleServiceActivationAdapter
          : ActorRef[ScheduleServiceActivation] =
        ctx.messageAdapter[ScheduleServiceActivation](msg =>
          WrappedScheduleServiceActivationAdapter(msg)
        )

      uninitialized(
        scheduler,
        activationAdapter,
        resultDataMessageFromExtAdapter,
        scheduleServiceActivationAdapter,
        buffer,
        startTime,
      )
    }
  }

  private def uninitialized(implicit
      scheduler: ActorRef[SchedulerMessage],
      activationAdapter: ActorRef[Activation],
      resultDataMessageFromExtAdapter: ActorRef[ResultDataMessageFromExt],
      scheduleServiceActivationAdapter: ActorRef[ScheduleServiceActivation],
      buffer: StashBuffer[Request],
      startTime: ZonedDateTime,
  ): Behavior[Request] = Behaviors.receiveMessagePartial {
    case RequestDataMessageAdapter(sender) =>
      sender ! resultDataMessageFromExtAdapter
      Behaviors.same

    case RequestScheduleActivationAdapter(sender) =>
      sender ! scheduleServiceActivationAdapter
      Behaviors.same

    case Create(
          initializeStateData: InitExtResultData,
          unlockKey: ScheduleKey,
        ) =>
      scheduler ! ScheduleActivation(
        activationAdapter,
        INIT_SIM_TICK,
        Some(unlockKey),
      )

      initializing(initializeStateData)
  }

  private def initializing(
      initServiceData: InitExtResultData
  )(implicit
      scheduler: ActorRef[SchedulerMessage],
      activationAdapter: ActorRef[Activation],
      resultDataMessageFromExtAdapter: ActorRef[ResultDataMessageFromExt],
      buffer: StashBuffer[Request],
      startTime: ZonedDateTime,
  ): Behavior[Request] = {
    Behaviors.receivePartial {
      case (_, WrappedActivation(Activation(INIT_SIM_TICK))) =>
        val initGridSubscribers =
          initServiceData.extResultData.getGridResultDataAssets.asScala.toList
        val initParticipantSubscribers =
          initServiceData.extResultData.getParticipantResultDataAssets.asScala.toList
        val initFlexOptionSubscribers =
          initServiceData.extResultData.getFlexOptionAssets.asScala.toList

        var initResultScheduleMap = Map.empty[Long, Set[UUID]]
        initResultScheduleMap =
          initResultScheduleMap + (0L -> (initParticipantSubscribers ++ initFlexOptionSubscribers).toSet) // First result for system participants expected for tick 0
        initResultScheduleMap =
          initResultScheduleMap + (initServiceData.powerFlowResolution -> initGridSubscribers.toSet) // First result for grid expected for tick powerflowresolution

        val resultInitializedStateData = ExtResultStateData(
          extResultData = initServiceData.extResultData,
          powerFlowResolution = initServiceData.powerFlowResolution,
          currentTick = INIT_SIM_TICK,
          extResultSchedule = ExtResultSchedule(
            scheduleMap = initResultScheduleMap
          ),
        )
        scheduler ! Completion(
          activationAdapter,
          None,
        )
        idle(resultInitializedStateData)
    }
  }

  private def idle(serviceStateData: ExtResultStateData)(implicit
      scheduler: ActorRef[SchedulerMessage],
      activationAdapter: ActorRef[Activation],
      resultDataMessageFromExtAdapter: ActorRef[ResultDataMessageFromExt],
      buffer: StashBuffer[Request],
      startTime: ZonedDateTime,
  ): Behavior[Request] = Behaviors
    .receivePartial[Request] {
      case (ctx, WrappedActivation(activation: Activation)) =>
        var updatedStateData = serviceStateData.handleActivation(activation)
        // ctx.log.info(s"+++++++ Received Activation for tick ${updatedStateData.currentTick} +++++++")

        serviceStateData.extResultsMessage.getOrElse(
          throw ServiceException(
            "ExtResultDataService was triggered without ResultDataMessageFromExt available"
          ) // this should not be possible because the external simulation schedules this service
        ) match {
          case msg: RequestResultEntities => // ExtResultDataProvider wurde aktiviert und es wurden Nachrichten von ExtSimulation angefragt
            // ctx.log.info(s"[${updatedStateData.currentTick}] [requestResults] resultStorage = ${updatedStateData.resultStorage}\n extResultScheduler ${updatedStateData.extResultScheduler}")
            val currentTick = updatedStateData.currentTick
            if (msg.tick == currentTick) { // check, if we are in the right tick
              // ctx.log.info(s"[${updatedStateData.currentTick}] RequestResultEntities with message = $msg")
              // ctx.log.info(s"[${updatedStateData.currentTick}] RequestResultEntities for ${msg.requestedResults()}")

              // Search for too old schedules
              val shiftedSchedule =
                serviceStateData.extResultSchedule.shiftPastTicksToCurrentTick(
                  currentTick
                )

              val requestedKeys = msg.requestedResults().asScala
              val expectedKeys = shiftedSchedule
                .getExpectedKeys(
                  currentTick
                )
                .intersect(msg.requestedResults().asScala.toSet)
              // ctx.log.info(s"[${updatedStateData.currentTick}] [requestResults] Expected Keys = $expectedKeys")
              val receiveDataMap =
                ReceiveDataMap[UUID, ResultEntity](expectedKeys)
              val updatedSchedule =
                shiftedSchedule.handleActivationWithRequest(
                  currentTick,
                  msg.requestedResults().asScala,
                )

              // ctx.log.info(s"[${updatedStateData.currentTick}] [requestResults] updatedSchedule = $updatedSchedule \n receiveDataMap = $receiveDataMap")

              if (receiveDataMap.isComplete) {
                // --- There are no expected results for this tick! Send the send right away!
                // ctx.log.info(s"[requestResults] tick ${msg.tick} -> ReceiveDataMap is complete \n requestedKeys = $requestedKeys -> send it right away: \n ${serviceStateData.resultStorage}")
                val filteredStorage =
                  serviceStateData.resultStorage.filter(entry =>
                    requestedKeys.toSet.contains(entry._1)
                  )
                // ctx.log.info(s"\u001b[0;34m[${serviceStateData.currentTick}] receiveDataMap = $receiveDataMap,\nexpectedKeys = ${receiveDataMap.receivedData.keySet},\nfilteredStorage = $filteredStorage\u001b[0;0m")

                serviceStateData.extResultData.queueExtResponseMsg(
                  new ProvideResultEntities(
                    filteredStorage.asJava
                  )
                )
                updatedStateData = updatedStateData.copy(
                  extResultsMessage = None,
                  receiveDataMap = None,
                  extResultSchedule = updatedSchedule,
                  extRequestedResultKeys = List.empty,
                )
                scheduler ! Completion(activationAdapter, None)
              } else {
                // We got an activation and we are waiting for some results -> trigger ourself to process
                // ctx.log.info(s"[requestResults] receiveDataMap was built -> now sending ResultRequestMessage")
                ctx.self ! ResultRequestMessage(msg.tick)
                updatedStateData = updatedStateData.copy(
                  extResultsMessage = None,
                  receiveDataMap = Some(receiveDataMap),
                  extResultSchedule = updatedSchedule,
                  extRequestedResultKeys = requestedKeys,
                )
              }
            } else {
              throw ServiceException(
                s"Results for the wrong tick ${msg.tick} requested! We are currently in tick ${updatedStateData.currentTick}"
              )
            }
        }
        // scheduler ! Completion(activationAdapter, None)
        idle(updatedStateData)

      case (
            _,
            scheduleServiceActivationMsg: WrappedScheduleServiceActivationAdapter,
          ) =>
        scheduler ! ScheduleActivation(
          activationAdapter,
          scheduleServiceActivationMsg.scheduleServiceActivationMsg.tick,
          Some(
            scheduleServiceActivationMsg.scheduleServiceActivationMsg.unlockKey
          ),
        )
        Behaviors.same

      case (
            _,
            resultDataMessageFromExt: WrappedResultDataMessageFromExt,
          ) => // Received a request for results before activation -> save it and answer later
        idle(
          serviceStateData.copy(
            extResultsMessage =
              Some(resultDataMessageFromExt.extResultDataMessageFromExt)
          )
        )

      case (
            _,
            extResultResponseMsg: ResultResponseMessage,
          ) => // Received result from ResultEventListener
        serviceStateData.receiveDataMap.fold {
          // result arrived before activation -> stash them away
          buffer.stash(extResultResponseMsg)
          idle(serviceStateData)
        } { dataMap =>
          if (
            dataMap.getExpectedKeys.contains(
              extResultResponseMsg.result.getInputModel
            )
          ) { // Received a result for external entity
            // ctx.log.info(s"[${serviceStateData.currentTick}] Process ResultsResponseMsg = ${extResultResponseMsg.result.getInputModel}\n receiveDataMap ${serviceStateData.receiveDataMap}\n MsgTick=${extResultResponseMsg.tick}, ServiceStateDataTick=${serviceStateData.currentTick}, nextTick = ${extResultResponseMsg.nextTick}")

            if (
              extResultResponseMsg.tick == serviceStateData.currentTick | extResultResponseMsg.tick == -1L
            ) { // Received a result for the current tick -> process it
              // FIXME Not expected results are unconsidered
              val updatedReceiveDataMap = dataMap.addData(
                extResultResponseMsg.result.getInputModel,
                extResultResponseMsg.result,
              )

              // ctx.log.info("[hDRM] AddData to RecentResults -> updatedReceivedResults = " + updatedReceiveDataMap)

              val updatedResultStorage =
                serviceStateData.resultStorage + (extResultResponseMsg.result.getInputModel -> extResultResponseMsg.result)
              val updatedResultSchedule =
                serviceStateData.extResultSchedule.handleResult(
                  extResultResponseMsg,
                  extResultResponseMsg.tick + serviceStateData.powerFlowResolution,
                )
              // ctx.log.info(s"[hDRM] updatedResultSchedule = $updatedResultSchedule")
              // ctx.log.info(s"[hDRM] updatedResultStorage = $updatedResultStorage")

              if (updatedReceiveDataMap.nonComplete) { // There are still results missing...
                // ctx.log.info(s"[${serviceStateData.currentTick}] There are still results missing...")
                idle(
                  serviceStateData.copy(
                    receiveDataMap = Some(updatedReceiveDataMap),
                    resultStorage = updatedResultStorage,
                    extResultSchedule = updatedResultSchedule,
                  )
                )
              } else { // all responses received, forward them to external simulation in a bundle
                // ctx.log.info(s"\u001b[0;34m[${serviceStateData.currentTick}] Got all ResultResponseMessage -> Now forward to external simulation in a bundle: $updatedResultStorage\u001b[0;0m")
                serviceStateData.extResultData.queueExtResponseMsg(
                  new ProvideResultEntities(updatedResultStorage.asJava)
                )
                // ctx.log.info("++++++++++++++++++ sended ExtResultData +++++++++++++++++++++++")
                scheduler ! Completion(activationAdapter, None)
                idle(
                  serviceStateData.copy(
                    receiveDataMap = None,
                    resultStorage = updatedResultStorage,
                    extResultSchedule = updatedResultSchedule,
                  )
                )
              }
            } else { // Received a result for another tick -> ignore it
              idle(serviceStateData)
            }
          } else { // Received a result for internal entity -> ignore it
            idle(serviceStateData)
          }
        }

      case (
            ctx,
            msg: ResultRequestMessage,
          ) => // Received internal result request -> unstash messages
        ctx.self ! msg
        buffer.unstashAll(idle(serviceStateData))

      case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
        DelayedStopHelper.handleMsg((ctx, msg))
    }

  private def checkResultType(
      result: ResultEntity,
      serviceStateData: ExtResultStateData,
  ): Boolean = {
    val uuid = result.getInputModel
    result match {
      case _: FlexOptionsResult =>
        if (serviceStateData.extResultData.getFlexOptionAssets.contains(uuid)) {
          true
        } else {
          false
        }
      case _: SystemParticipantResult =>
        if (
          serviceStateData.extResultData.getParticipantResultDataAssets
            .contains(uuid)
        ) {
          true
        } else {
          false
        }
      case _: NodeResult | _: LineResult =>
        if (
          serviceStateData.extResultData.getGridResultDataAssets.contains(uuid)
        ) {
          true
        } else {
          false
        }
      case _ => false
    }
  }

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  final case class ExtResultStateData(
      extResultData: ExtResultDataConnection,
      powerFlowResolution: Long,
      currentTick: Long,
      extResultSchedule: ExtResultSchedule,
      extResultsMessage: Option[ResultDataMessageFromExt] = None,
      resultStorage: Map[UUID, ResultEntity] = Map.empty,
      receiveDataMap: Option[ReceiveDataMap[UUID, ResultEntity]] = None,
      extRequestedResultKeys: Iterable[UUID] = List.empty,
  ) {
    def handleActivation(activation: Activation): ExtResultStateData = {
      copy(
        currentTick = activation.tick
      )
    }
  }
  final case class InitExtResultData(
      extResultData: ExtResultDataConnection,
      powerFlowResolution: Long,
  )

  final case class ExtResultSchedule(
      scheduleMap: Map[Long, Set[UUID]] = Map.empty,
      unscheduledList: Set[UUID] = Set.empty,
  ) {
    def shiftPastTicksToCurrentTick(
        currentTick: Long
    ): ExtResultSchedule = {
      // Sammle alle Sets von Keys, deren Schlüssel kleiner als currentTick
      val (toMerge, remaining) = scheduleMap.partition { case (tick, _) =>
        tick < currentTick
      }

      // Kombiniere die Sets zu einem einzigen Set
      val mergedSet = toMerge.values.flatten.toSet

      // Aktualisiere den scheduleMap mit dem neuen Set für currentTick
      val updatedScheduleMap = remaining.updated(
        currentTick,
        scheduleMap.getOrElse(currentTick, Set.empty) ++ mergedSet,
      )

      // Rückgabe eines neuen ExtResultSchedule mit dem aktualisierten scheduleMap
      copy(
        scheduleMap = updatedScheduleMap
      )
    }

    def getExpectedKeys(tick: Long): Set[UUID] = {
      scheduleMap.getOrElse(
        tick,
        Set(),
      ) ++ unscheduledList
    }

    private def getScheduledKeys(tick: Long): Set[UUID] = {
      scheduleMap.getOrElse(tick, Set[UUID]())
    }

    def handleActivation(tick: Long): ExtResultSchedule = {
      copy(
        scheduleMap = scheduleMap.-(tick)
      )
    }

    def handleActivationWithRequest(
        tick: Long,
        keys: Iterable[UUID],
    ): ExtResultSchedule = {
      val remainingKeys =
        scheduleMap.get(tick).map(_.diff(keys.toSet)).getOrElse(Set.empty)
      if (remainingKeys.isEmpty) {
        copy(
          scheduleMap = scheduleMap.-(tick)
        )
      } else {
        copy(
          scheduleMap = scheduleMap.updated(tick, remainingKeys)
        )
      }
    }

    def handleResult(
        msg: ResultResponseMessage,
        nextTick: Long,
    ): ExtResultSchedule = {
      copy(
        scheduleMap = scheduleMap.updated(
          nextTick,
          getScheduledKeys(nextTick) + msg.result.getInputModel,
        )
      )
    }
  }

}
