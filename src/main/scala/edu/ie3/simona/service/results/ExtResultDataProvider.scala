package edu.ie3.simona.service.results

import edu.ie3.datamodel.models.result.ResultEntity
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
                                          result: ResultEntity,
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
      //ctx.log.info("Starting initialization!")
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
      case (_, WrappedActivation(Activation(INIT_SIM_TICK))) =>
        val initGridSubscribers = initServiceData.extResultData.getGridResultDataAssets.asScala.toList
        val initParticipantSubscribers = initServiceData.extResultData.getParticipantResultDataAssets.asScala.toList

        var initResultStorage = Map.empty[UUID, (Option[ResultEntity], Option[Long])]
        initParticipantSubscribers.foreach(
         uuid => initResultStorage = initResultStorage + (uuid -> (None, Some(0L)))
        )
        initGridSubscribers.foreach(
          uuid => initResultStorage = initResultStorage + (uuid -> (None, Some(initServiceData.extResultData.getPowerFlowResolution)))
        )
        val resultInitializedStateData = ExtResultStateData(
         extResultData = initServiceData.extResultData,
         gridSubscribers = initGridSubscribers,
         participantSubscribers = initParticipantSubscribers,
         resultStorage = initResultStorage
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
        //ctx.log.info("Received Activation")
        var updatedStateData = serviceStateData

        serviceStateData.extResultsMessage.getOrElse(
          throw ServiceException(
            "ExtResultDataService was triggered without ResultDataMessageFromExt available"
          )
        ) match {
          case msg: RequestResultEntities =>
            //ctx.log.info(s"[requestResults] for tick ${msg.tick} and resultStorage ${serviceStateData.resultStorage}")
            var receiveDataMap = ReceiveDataMap.empty[UUID, ResultEntity]
            if (activation.tick == 0L) {
              receiveDataMap = ReceiveDataMap[UUID, ResultEntity](serviceStateData.participantSubscribers.toSet)
            } else {
              receiveDataMap = ReceiveDataMap[UUID, ResultEntity]((serviceStateData.participantSubscribers ++ serviceStateData.gridSubscribers).toSet)
            }
            //ctx.log.info(s"[requestResults] tick ${msg.tick} -> created a receivedatamap " + receiveDataMap)
            /*
            serviceStateData.resultStorage.foreach({
              case (uuid, (res, t)) =>
                //ctx.log.info(s"[requestResults] tick = ${msg.tick}, uuid = $uuid, and time = ${t.getOrElse("Option")}, result = ${res.getOrElse("Option")}")
                if (t.getOrElse(-1L) != msg.tick) { //wenn nicht in diesem Tick gefragt, nehme Wert aus ResultDataStorage
                  receiveDataMap = receiveDataMap.addData(
                    uuid,
                    res.getOrElse(
                      throw new Exception("noResult")
                    )
                  )
                  //ctx.log.info(s"[requestResults] tick ${msg.tick} -> added to receivedatamap " + receiveDataMap)
                }
            })

             */

            //ctx.log.info(s"[requestResults] tick ${msg.tick} -> requestResults for " + receiveDataMap)

            var resultList = List.empty[ResultEntity]
            if (receiveDataMap.isComplete) {
              if (receiveDataMap.getExpectedKeys.nonEmpty) {
                serviceStateData.resultStorage.values.foreach(
                  result => resultList = resultList :+ result._1.getOrElse(
                    throw new RuntimeException("There is no result!")
                  )
                )
              }
              //ctx.log.info(s"[requestResults] tick ${msg.tick} -> ReceiveDataMap is complete -> send it right away: " + resultList)
              // all responses received, forward them to external simulation in a bundle
              serviceStateData.extResultData.queueExtResponseMsg(
                new ProvideResultEntities(resultList.asJava)
              )
              updatedStateData = serviceStateData.copy(
                extResultsMessage = None,
                recentResults = None)

            } else {
              //ctx.log.info(s"[requestResults] receiveDataMap was built -> now sending ResultRequestMessage")
              ctx.self ! ResultRequestMessage(msg.tick)
              updatedStateData = serviceStateData.copy(
                                                  extResultsMessage = None,
                                                  recentResults = Some(receiveDataMap)
                                                )
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
        //ctx.log.info("[handleDataResponseMessage] Received ResultsResponseMessage")

        if (serviceStateData.recentResults.isDefined) {
          // process dataResponses
          if (serviceStateData.recentResults.getOrElse(throw new Exception("no Receive Data Map!")).getExpectedKeys.contains(extResultResponseMsg.result.getInputModel)) {
          //if (serviceStateData.participantSubscribers.contains(extResultResponseMsg.result.getInputModel) || serviceStateData.gridSubscribers.contains(extResultResponseMsg.result.getInputModel)) {
            //ctx.log.info("[handleDataResponseMessage] Received ResultsResponseMessage with content " + extResultResponseMsg)
            //ctx.log.info("[handleDataResponseMessage] RecentResults " + serviceStateData.recentResults)
            val updatedReceivedResults = serviceStateData.recentResults.getOrElse(throw new Exception("noMap")).addData(extResultResponseMsg.result.getInputModel, extResultResponseMsg.result)
            //ctx.log.info("[handleDataResponseMessage] AddData to RecentResults -> updatedReceivedResults = " + updatedReceivedResults)
            val updatedResultStorage =
              serviceStateData.resultStorage + (extResultResponseMsg.result.getInputModel -> (Some(extResultResponseMsg.result), extResultResponseMsg.nextTick))
            if (updatedReceivedResults.nonComplete) {
              // all responses received, forward them to external simulation in a bundle
              idle(serviceStateData.copy(
                recentResults = Some(updatedReceivedResults),
                resultStorage = updatedResultStorage
              ))
            } else {
              var resultList = List.empty[ResultEntity]
              updatedReceivedResults.receivedData.values.foreach(
                result => resultList = resultList :+ result
              )
              // all responses received, forward them to external simulation in a bundle
              serviceStateData.extResultData.queueExtResponseMsg(
                new ProvideResultEntities(resultList.asJava)
              )
              //log.info("[handleDataResponseMessage] Got all ResultResponseMessage -> Now forward to external simulation in a bundle: " + resultList)
              idle(serviceStateData.copy(
                resultStorage = updatedResultStorage,
                recentResults = None
              ))
            }
          } else {
            idle(serviceStateData)
          }
        } else {
          // the results arrived too early -> stash them away
          buffer.stash(extResultResponseMsg)
          idle(serviceStateData)
        }
      case (_, msg: ResultRequestMessage) =>
        buffer.unstashAll(idle(serviceStateData))

      case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
        DelayedStopHelper.handleMsg((ctx, msg))
    }

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  final case class ExtResultStateData(
                                        extResultData: ExtResultData,
                                        gridSubscribers: List[UUID] = List.empty,
                                        participantSubscribers: List[UUID] = List.empty,
                                        extResultsMessage: Option[ResultDataMessageFromExt] = None,
                                        resultStorage: Map[UUID, (Option[ResultEntity], Option[Long])] = Map.empty,       // UUID -> Result, nextTick
                                        maybeNextActivationTick: Option[Long] = None,
                                        recentResults: Option[ReceiveDataMap[UUID, ResultEntity]] = None,
                                      )
  final case class InitExtResultData(
                                      extResultData: ExtResultData
                                    )
}