package edu.ie3.simona.service.results

import edu.ie3.datamodel.models.result.{ModelResultEntity, ResultEntity}
import edu.ie3.datamodel.models.result.system.PvResult
import edu.ie3.simona.api.data.results.ExtResultData
import edu.ie3.simona.api.data.results.ontology.{ProvideResultEntities, RequestResultEntities, ResultDataMessageFromExt}
import edu.ie3.simona.event.listener.DelayedStopHelper
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.SchedulerMessage.{Completion, ScheduleActivation}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.{ActorRef, Behavior, PostStop}
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}

import java.util.UUID
import scala.collection.immutable.Set
import scala.jdk.CollectionConverters._

object ExtResultDataProvider {

  trait Request

  final case class WrappedActivation(activation: Activation) extends Request

  /** ExtSimulation -> ExtResultDataProvider */
  final case class WrappedResultDataMessageFromExt(extResultDataMessageFromExt: ResultDataMessageFromExt) extends Request
  final case class WrappedScheduleServiceActivationAdapter(scheduleServiceActivationMsg: ScheduleServiceActivation) extends Request

  final case class RequestDataMessageAdapter(sender: ActorRef[ActorRef[ResultDataMessageFromExt]]) extends Request

  final case class RequestScheduleActivationAdapter(sender: ActorRef[ActorRef[ScheduleServiceActivation]]) extends Request


  /** ResultEventListener -> ExtResultDataProvider */
  final case class ResultResponseMessage(
                                          result: ModelResultEntity,
                                          tick: Long,
                                          nextTick: Option[Long]
                                        )
    extends Request

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
            scheduler: ActorRef[SchedulerMessage]
           ): Behavior[Request] =  Behaviors.withStash(5000) { buffer =>
    Behaviors.setup[Request] { ctx =>
      val activationAdapter: ActorRef[Activation] = ctx.messageAdapter[Activation](msg => WrappedActivation(msg))
      val resultDataMessageFromExtAdapter: ActorRef[ResultDataMessageFromExt] = ctx.messageAdapter[ResultDataMessageFromExt](msg => WrappedResultDataMessageFromExt(msg))
      val scheduleServiceActivationAdapter: ActorRef[ScheduleServiceActivation] = ctx.messageAdapter[ScheduleServiceActivation](msg => WrappedScheduleServiceActivationAdapter(msg))

      uninitialized(scheduler, activationAdapter, resultDataMessageFromExtAdapter, scheduleServiceActivationAdapter, buffer)
    }
  }


  private def uninitialized(
                             implicit scheduler: ActorRef[SchedulerMessage],
                             activationAdapter: ActorRef[Activation],
                             resultDataMessageFromExtAdapter: ActorRef[ResultDataMessageFromExt],
                             scheduleServiceActivationAdapter: ActorRef[ScheduleServiceActivation],
                             buffer: StashBuffer[Request],
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
      scheduler ! ScheduleActivation(activationAdapter,
        INIT_SIM_TICK,
        Some(unlockKey))

    initializing(initializeStateData)
  }

  private def initializing(
                            initServiceData: InitExtResultData
                          )(
                            implicit scheduler: ActorRef[SchedulerMessage],
                            activationAdapter: ActorRef[Activation],
                            resultDataMessageFromExtAdapter: ActorRef[ResultDataMessageFromExt],
                            buffer: StashBuffer[Request]): Behavior[Request] = {
    Behaviors.receivePartial {
      case (ctx, WrappedActivation(Activation(INIT_SIM_TICK))) =>
        val initGridSubscribers = initServiceData.extResultData.getGridResultDataAssets.asScala.toList
        val initParticipantSubscribers = initServiceData.extResultData.getParticipantResultDataAssets.asScala.toList

        var initResultSchedule = Map.empty[Long, Set[UUID]]
        initResultSchedule = initResultSchedule + (0L -> initParticipantSubscribers.toSet)
        initResultSchedule = initResultSchedule + (initServiceData.extResultData.getPowerFlowResolution.longValue() -> initGridSubscribers.toSet)

        val resultInitializedStateData = ExtResultStateData(
          extResultData = initServiceData.extResultData,
          currentTick = INIT_SIM_TICK,
          extResultScheduler = initResultSchedule
        )
        scheduler ! Completion(
          activationAdapter,
          None
        )
        idle(resultInitializedStateData)
    }

  }

  private def idle(serviceStateData: ExtResultStateData)(
    implicit scheduler: ActorRef[SchedulerMessage],
    activationAdapter: ActorRef[Activation],
    resultDataMessageFromExtAdapter: ActorRef[ResultDataMessageFromExt],
    buffer: StashBuffer[Request],
  ): Behavior[Request] = Behaviors
    .receivePartial[Request] {
      case (ctx, WrappedActivation(activation: Activation)) =>
        var updatedStateData = serviceStateData.handleActivation(activation)
        //ctx.log.info(s"+++++++ Received Activation for tick ${updatedStateData.currentTick} +++++++")

        serviceStateData.extResultsMessage.getOrElse(
          throw ServiceException(
            "ExtResultDataService was triggered without ResultDataMessageFromExt available"
          )   // this should not be possible because the external simulation schedules this service
        ) match {
          case msg: RequestResultEntities =>      // ExtResultDataProvider wurde aktiviert und es wurden Nachrichten von ExtSimulation angefragt
            //ctx.log.info(s"[${updatedStateData.currentTick}] [requestResults] resultStorage = ${updatedStateData.resultStorage}\n extResultScheduler ${updatedStateData.extResultScheduler}")

            if (msg.tick == updatedStateData.currentTick) { // check, if we are in the right tick
              var updatedSchedule = serviceStateData.extResultScheduler
              val expectedKeys = serviceStateData.extResultScheduler.getOrElse(
                activation.tick,
                Set()
              ) ++ serviceStateData.extResultScheduler.getOrElse(-2L, Set())
              val receiveDataMap = ReceiveDataMap[UUID, ModelResultEntity](expectedKeys)
              updatedSchedule = updatedSchedule.-(activation.tick)

              //ctx.log.info(s"[${updatedStateData.currentTick}] [requestResults] updatedSchedule = $updatedSchedule \n receiveDataMap = $receiveDataMap")

              if (receiveDataMap.isComplete) {
                // --- There are no expected results for this tick! Send the send right away!
                //ctx.log.info(s"[requestResults] tick ${msg.tick} -> ReceiveDataMap is complete -> send it right away: ${serviceStateData.resultStorage}")

                serviceStateData.extResultData.queueExtResponseMsg(
                  new ProvideResultEntities(serviceStateData.resultStorage.asJava)
                )
                //ctx.log.info("++++++++++++++++++ sended ExtResultData +++++++++++++++++++++++")
                updatedStateData = updatedStateData.copy(
                  extResultsMessage = None,
                  receiveDataMap = None,
                  extResultScheduler = updatedSchedule
                )
              } else {
                //ctx.log.info(s"[requestResults] receiveDataMap was built -> now sending ResultRequestMessage")
                ctx.self ! ResultRequestMessage(msg.tick)
                updatedStateData = updatedStateData.copy(
                  extResultsMessage = None,
                  receiveDataMap = Some(receiveDataMap),
                  extResultScheduler = updatedSchedule
                )
              }
            } else {
              throw ServiceException(s"Results for the wrong tick ${msg.tick} requested! We are currently in tick ${updatedStateData.currentTick}")
            }

        }

        scheduler ! Completion(activationAdapter, None)
        idle(updatedStateData)

      case (_, scheduleServiceActivationMsg: WrappedScheduleServiceActivationAdapter) =>
        scheduler ! ScheduleActivation(
          activationAdapter,
          scheduleServiceActivationMsg.scheduleServiceActivationMsg.tick,
          Some(scheduleServiceActivationMsg.scheduleServiceActivationMsg.unlockKey),
        )
        Behaviors.same

      case (ctx, extRequestResultEntitiesMsg: WrappedResultDataMessageFromExt) =>
        //ctx.log.info("Received WrappedResultDataMessageFromExt")
        idle(
          serviceStateData.copy(
            extResultsMessage = Some(extRequestResultEntitiesMsg.extResultDataMessageFromExt)
          ))

      case (ctx, extResultResponseMsg: ResultResponseMessage) =>
        if (serviceStateData.receiveDataMap.isDefined) {
          // process dataResponses
          if (serviceStateData.receiveDataMap.getOrElse(throw new Exception("There is no activation yet! Receive Data Map does not exist!")).getExpectedKeys.contains(extResultResponseMsg.result.getInputModel)) {
            //ctx.log.info(s"[${serviceStateData.currentTick}] Process ResultsResponseMsg = ${extResultResponseMsg.result.getInputModel}\n receiveDataMap ${serviceStateData.receiveDataMap}\n MsgTick=${extResultResponseMsg.tick}, ServiceStateDataTick=${serviceStateData.currentTick}, nextTick = ${extResultResponseMsg.nextTick}")

            // --- Add received results to receiveDataMap

            if (extResultResponseMsg.tick == -4L || extResultResponseMsg.tick == serviceStateData.currentTick) {    //FIXME Not expected results are unconsidered
              val updatedReceiveDataMap = serviceStateData
                .receiveDataMap
                .getOrElse(
                  throw new Exception("noMap")
                ).addData(
                  extResultResponseMsg.result.getInputModel,
                  extResultResponseMsg.result
                )

              //ctx.log.info("[hDRM] AddData to RecentResults -> updatedReceivedResults = " + updatedReceiveDataMap)

              // --- Update ResultStorage and Schedule

              val updatedResultStorage = serviceStateData.resultStorage + (extResultResponseMsg.result.getInputModel -> extResultResponseMsg.result)
              var updatedResultSchedule = serviceStateData.extResultScheduler
              //ctx.log.info(s"[hDRM] updatedResultSchedule = $updatedResultSchedule")

              updatedResultSchedule = extResultResponseMsg.nextTick.fold {
                updatedResultSchedule.updated(
                  -3L,
                  updatedResultSchedule.getOrElse(-3L, Set[UUID]()) + extResultResponseMsg.result.getInputModel
                )
              } {
                newTick =>
                  //ctx.log.info(s"[hDRM] update schedule = $newTick, uuid = ${extResultResponseMsg.result.getInputModel}")
                  updatedResultSchedule.updated(
                    newTick,
                    updatedResultSchedule.getOrElse(newTick, Set[UUID]()) + extResultResponseMsg.result.getInputModel
                  )
              }

              //ctx.log.info(s"[hDRM] updatedResultSchedule = $updatedResultSchedule")
              //ctx.log.info(s"[hDRM] updatedResultStorage = $updatedResultStorage")

              // --- Check, if all expected results has been received

              if (updatedReceiveDataMap.nonComplete) {
                //ctx.log.info(s"[${serviceStateData.currentTick}] There are still results missing...")
                // There are still results missing...
                idle(serviceStateData.copy(
                  receiveDataMap = Some(updatedReceiveDataMap),
                  resultStorage = updatedResultStorage,
                  extResultScheduler = updatedResultSchedule
                ))
              } else {
                ctx.log.info(s"\u001b[0;34m[${serviceStateData.currentTick}] Got all ResultResponseMessage -> Now forward to external simulation in a bundle: $updatedResultStorage\u001b[0;0m")
                // all responses received, forward them to external simulation in a bundle
                serviceStateData.extResultData.queueExtResponseMsg(
                  new ProvideResultEntities(updatedResultStorage.asJava)
                )
                //ctx.log.info("++++++++++++++++++ sended ExtResultData +++++++++++++++++++++++")
                idle(serviceStateData.copy(
                  receiveDataMap = None,
                  resultStorage = updatedResultStorage,
                  extResultScheduler = updatedResultSchedule
                ))
              }
            } else {
              idle(serviceStateData)
            }
          } else {
            idle(serviceStateData)
          }
        } else {
          // the results arrived too early -> stash them away
          buffer.stash(extResultResponseMsg)
          idle(serviceStateData)
        }

      case (ctx, msg: ResultRequestMessage) =>
        //ctx.log.info(s"[handleDataResponseMessage] Received ResultRequestMessage $msg -> Now unstash all buffered messages!")
        buffer.unstashAll(idle(serviceStateData))

      case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
        DelayedStopHelper.handleMsg((ctx, msg))
    }

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  final case class ExtResultStateData(
                                       extResultData: ExtResultData,
                                       currentTick: Long,
                                       extResultsMessage: Option[ResultDataMessageFromExt] = None,
                                       resultStorage: Map[UUID, ModelResultEntity] = Map.empty,
                                       extResultScheduler: Map[Long, Set[UUID]] = Map.empty,
                                       receiveDataMap: Option[ReceiveDataMap[UUID, ModelResultEntity]] = None,
                                      ) {
    def handleActivation(activation: Activation): ExtResultStateData = {
      copy(
        currentTick = activation.tick
      )
    }
  }
  final case class InitExtResultData(
                                      extResultData: ExtResultData
                                    )
}