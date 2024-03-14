/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.results

import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.agent.grid.GridAgentMessage
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.api.data.results.ExtResultData
import edu.ie3.simona.api.data.results.ontology.{ProvideResultEntities, RequestResultEntities, ResultDataMessageFromExt}
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.ontology.messages.services.DataMessage
import edu.ie3.simona.ontology.messages.services.ResultMessage.{ResultRequestMessage, ResultResponseMessage}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ServiceRegistrationMessage
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.ServiceStateData.{InitializeServiceStateData, ServiceBaseStateData}
import edu.ie3.simona.service.results.ExtResultDataService.{ExtResultsStateData, InitExtResultData}
import edu.ie3.simona.service.{ExtDataSupport, SimonaService}
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq
import org.apache.pekko.actor.typed.scaladsl.StashBuffer
import org.apache.pekko.actor.{ActorContext, ActorRef, Props}

import scala.collection.immutable.{Map, SortedSet}
import java.util.UUID
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.util.{Failure, Success, Try}

object ExtResultDataService {
  def props(scheduler: ActorRef): Props =
    Props(
      new ExtResultDataService(scheduler: ActorRef)
    )

  final case class ExtResultsStateData(
                                        extResultsData: ExtResultData,
                                        subscribers: List[UUID] = List.empty,
                                        extResultsMessage: Option[ResultDataMessageFromExt] = None,
                                        resultStorage: Map[UUID, (Option[ResultEntity], Option[Long])] = Map.empty,       // UUID -> Result, nextTick
                                        maybeNextActivationTick: Option[Long] = None,
                                        recentResults: ReceiveDataMap[UUID, ResultEntity] = ReceiveDataMap.empty,
                                        receivedResults: Int = 0,
                                        resultSink: List[ResultEntity] = List.empty,
                                        unlockKey: Option[ScheduleKey] = None,
                                        sendedMessage: Boolean = true,
                                        buffer: StashBuffer[ResultResponseMessage],
  ) extends ServiceBaseStateData

  final case class InitExtResultData(
      extResultsData: ExtResultData
  ) extends InitializeServiceStateData
}

class ExtResultDataService(override val scheduler: ActorRef)
    extends SimonaService[ExtResultsStateData](scheduler)
    with ExtDataSupport[ExtResultsStateData] {

  override def init(
      initServiceData: InitializeServiceStateData
  ): Try[(ExtResultsStateData, Option[Long])] = {
    initServiceData match {
      case InitExtResultData(extResultsData) =>
        val initSubscribers = List(
          UUID.fromString("de8cfef5-7620-4b9e-9a10-1faebb5a80c0"),
          UUID.fromString("2560c371-f420-4c2a-b4e6-e04c11b64c03"))
        val resultInitializedStateData = ExtResultsStateData(
          extResultsData = extResultsData,
          subscribers = initSubscribers,
          resultStorage = Map(
            UUID.fromString("de8cfef5-7620-4b9e-9a10-1faebb5a80c0") -> (None, Some(0)),
            UUID.fromString("2560c371-f420-4c2a-b4e6-e04c11b64c03") -> (None, Some(0)),
          ),
          recentResults = ReceiveDataMap(initSubscribers.toSet)
        )
        Success(resultInitializedStateData, None)

      case invalidData =>
        Failure(
          new InitializationException(
            s"Provided init data '${invalidData.getClass.getSimpleName}' for EV service are invalid!"
          )
        )
    }
  }

  override protected def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(implicit serviceStateData: ExtResultsStateData): Try[ExtResultsStateData] =
    Failure(
      ServiceException(
        "For this service is no registration possible!"
      )
    )

  override protected def announceInformation(
      tick: Long
  )(implicit
      serviceStateData: ExtResultsStateData,
      ctx: ActorContext,
  ): (ExtResultsStateData, Option[Long]) = {
    serviceStateData.extResultsMessage.getOrElse(
      throw ServiceException(
        "ExtResultDataService was triggered without ResultDataMessageFromExt available"
      )
    ) match {
      case _: RequestResultEntities =>
        requestResults(tick)
    }
  }

  /** Handle a message from outside the simulation
    *
    * @param extMsg
    *   the external incoming message
    * @param serviceStateData
    *   the current state data of this service
    * @return
    *   the updated state data
    */
  override protected def handleDataMessage(
      extMsg: DataMessageFromExt
  )(implicit
      serviceStateData: ExtResultsStateData
  ): ExtResultsStateData = extMsg match {
    case extResultsMessageFromExt: ResultDataMessageFromExt =>
      serviceStateData.copy(
        extResultsMessage = Some(extResultsMessageFromExt)
      )
  }

  /** Handle a message from inside SIMONA sent to external
    *
    * @param extResponseMsg
    *   the external incoming message
    * @param serviceStateData
    *   the current state data of this service
    * @return
    *   the updated state data
    */
  override protected def handleDataResponseMessage(
      extResponseMsg: DataMessage
  )(implicit serviceStateData: ExtResultsStateData): ExtResultsStateData = {
    extResponseMsg match {
      case ResultResponseMessage(result, nextTick) =>
        if (serviceStateData.subscribers.contains(result.getInputModel)) {
          log.info("[handleDataResponseMessage] Received ResultsResponseMessage with content " + extResponseMsg)
          log.info("[handleDataResponseMessage] RecentResults " + serviceStateData.recentResults)
          val updatedReceivedResults = serviceStateData.recentResults.addData(result.getInputModel, result)
          log.info("[handleDataResponseMessage] AddData to RecentResults -> updatedReceivedResults = " + updatedReceivedResults)
          val updatedResultStorage =
            serviceStateData.resultStorage + (result.getInputModel -> (Some(result), nextTick))
          if (updatedReceivedResults.nonComplete) {
            // all responses received, forward them to external simulation in a bundle
            serviceStateData.copy(
              recentResults = updatedReceivedResults,
              resultStorage = updatedResultStorage
            )
          } else {

            var resultList = List.empty[ResultEntity]


            updatedReceivedResults.receivedData.values.foreach(
              result => resultList = resultList :+ result
            )

            // all responses received, forward them to external simulation in a bundle
            serviceStateData.extResultsData.queueExtResponseMsg(
              new ProvideResultEntities(resultList.asJava)
            )
            log.info("[handleDataResponseMessage] Got all ResultResponseMessage -> Now forward to external simulation in a bundle: " + resultList)
            serviceStateData.copy(
              resultStorage = updatedResultStorage,
              recentResults = ReceiveDataMap(serviceStateData.subscribers.toSet)
            )
            /*

            sendResultData(updatedResultStorage)
            self ! ResultRequestMessage(null)
              serviceStateData.copy(
                recentResults = updatedReceivedResults,
                resultStorage = updatedResultStorage
              )
            */
          }
        } else {
            serviceStateData
        }
    }
  }

  private def requestResults(
      tick: Long
  )(implicit
      serviceStateData: ExtResultsStateData
  ): (ExtResultsStateData, Option[Long]) = {
    log.info(s"[requestResults] for tick $tick and resultStorage ${serviceStateData.resultStorage}")
    var receiveDataMap = ReceiveDataMap[UUID, ResultEntity](serviceStateData.subscribers.toSet)
    log.info(s"[requestResults] tick $tick -> created a receivedatamap " + receiveDataMap)
    serviceStateData.resultStorage.foreach({
      case (uuid, (res, t)) =>
        log.info(s"[requestResults] tick = $tick, uuid = $uuid, and time = ${t.getOrElse("Option")}, result = ${res.getOrElse("Option")}")
          if (t.getOrElse(-1) != tick) { //wenn nicht in diesem Tick gefragt, nehme Wert aus ResultDataStorage
            receiveDataMap = receiveDataMap.addData(
              uuid,
              res.getOrElse(
                throw new Exception("noResult")
              )
            )
            log.info(s"[requestResults] tick $tick -> added to receivedatamap " + receiveDataMap)
          }
    })

    log.info(s"[requestResults] tick $tick -> requestResults for " + receiveDataMap)

    if (receiveDataMap.isComplete) {
      var resultList = List.empty[ResultEntity]

      serviceStateData.resultStorage.values.foreach(
        result => resultList = resultList :+ result._1.getOrElse(
          throw new RuntimeException("There is no result!")
        )
      )

      log.info(s"[requestResults] tick $tick -> ReceiveDataMap is complete -> send it right away: " + resultList)
      // all responses received, forward them to external simulation in a bundle
      serviceStateData.extResultsData.queueExtResponseMsg(
        new ProvideResultEntities(resultList.asJava)
      )
      (serviceStateData.copy(
        extResultsMessage = None,
        recentResults = ReceiveDataMap(serviceStateData.subscribers.toSet)), None)
    } else {
    (
      serviceStateData.copy(
      extResultsMessage = None,
      recentResults = receiveDataMap
      ), None)}
  }

  private def sendResultData(
                              resultStorage: Map[UUID, (Option[ResultEntity], Option[Long])]
                            )(implicit
                               serviceStateData: ExtResultsStateData
  ): Unit = {
    var resultList = List.empty[ResultEntity]

    resultStorage.values.foreach(
      result => resultList = resultList :+ result._1.getOrElse(
        throw new RuntimeException("There is no result!")
      )
    )

    log.info("sendResultData " + resultList)
    // all responses received, forward them to external simulation in a bundle
    serviceStateData.extResultsData.queueExtResponseMsg(
      new ProvideResultEntities(resultList.asJava)
    )
  }
}

