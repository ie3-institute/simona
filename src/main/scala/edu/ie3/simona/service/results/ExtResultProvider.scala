/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.results

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.api.data.connection.ExtResultDataConnection
import edu.ie3.simona.api.ontology.DataMessageFromExt
import edu.ie3.simona.api.ontology.results.{
  ProvideResultEntities,
  RequestResultEntities,
  ResultDataMessageFromExt,
}
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  ResultResponseMessage,
  ServiceRegistrationMessage,
  ServiceResponseMessage,
}
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.{ExtDataSupport, SimonaService}
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.ActorContext

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import scala.util.{Failure, Success, Try}

@deprecated
object ExtResultProvider extends SimonaService with ExtDataSupport {

  override type S = ExtResultStateData

  final case class ExtResultStateData(
      extResultDataConnection: ExtResultDataConnection,
      powerFlowResolution: Long,
      currentTick: Long = INIT_SIM_TICK,
      extResultSchedule: ExtResultSchedule,
      extResultsMessage: Option[ResultDataMessageFromExt] = None,
      receiveDataMap: ReceiveDataMap[UUID, ResultEntity] = ReceiveDataMap.empty,
      resultStorage: Map[UUID, ResultEntity] = Map.empty,
      startTime: ZonedDateTime,
  ) extends ServiceBaseStateData

  final case class InitExtResultData(
      extResultDataConnection: ExtResultDataConnection,
      powerFlowResolution: Long,
      startTime: ZonedDateTime,
  ) extends InitializeServiceStateData

  override def init(
      initServiceData: InitializeServiceStateData
  ): Try[(ExtResultStateData, Option[Long])] = initServiceData match {
    case InitExtResultData(
          extResultDataConnection,
          powerFlowResolution,
          startTime,
        ) =>
      val initGridSubscribers =
        extResultDataConnection.getGridResultDataAssets.asScala
      val initParticipantSubscribers =
        extResultDataConnection.getParticipantResultDataAssets.asScala
      val initFlexOptionSubscribers =
        extResultDataConnection.getFlexOptionAssets.asScala

      // First result for system participants expected for tick 0
      // First result for grid expected for tick power flow resolution
      val initResultScheduleMap = Map(
        0L -> (initParticipantSubscribers ++ initFlexOptionSubscribers).toSet
      ) ++ Map(powerFlowResolution -> initGridSubscribers.toSet)

      Success(
        ExtResultStateData(
          extResultDataConnection,
          powerFlowResolution,
          extResultSchedule = ExtResultSchedule(initResultScheduleMap),
          startTime = startTime,
        ),
        None,
      )

    case invalidData =>
      Failure(
        new InitializationException(
          s"Provided init data '${invalidData.getClass.getSimpleName}' for ExtResultProvider are invalid!"
        )
      )
  }

  override protected def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(using
      serviceStateData: ExtResultStateData,
      ctx: ActorContext[Message],
  ): Try[ExtResultStateData] = {
    // this should not happen
    ctx.log.warn(
      s"Received registration request '$registrationMessage', but no registration is required for ExtResultProvider!"
    )
    Success(serviceStateData)
  }

  override protected def announceInformation(tick: Long)(using
      serviceStateData: ExtResultStateData,
      ctx: ActorContext[Message],
  ): (ExtResultStateData, Option[Long]) = {

    val extMsg = serviceStateData.extResultsMessage.getOrElse(
      // this should not be possible because the external simulation schedules this service
      throw ServiceException(
        "ExtResultDataService was triggered without ResultDataMessageFromExt available"
      )
    )

    val updatedStateData = extMsg match {
      case request: RequestResultEntities =>
        if (request.tick != tick) {
          ctx.log.warn(
            s"Received result request for tick '${request.tick}', but current simulation tick is '$tick'!"
          )

          serviceStateData
        } else {
          // handle result request

          val currentTick = tick
          val requestedKeys = request.requestedResults.asScala.toSet

          // Search for too old schedules
          val shiftedSchedule =
            serviceStateData.extResultSchedule.shiftPastTicksToCurrentTick(
              currentTick
            )

          val expectedKeys = shiftedSchedule
            .getExpectedKeys(currentTick)
            .intersect(requestedKeys)

          val receiveDataMap =
            serviceStateData.receiveDataMap.addExpectedKeys(expectedKeys)

          val updatedSchedule = shiftedSchedule.handleActivationWithRequest(
            currentTick,
            requestedKeys,
          )

          val currentStorage = serviceStateData.resultStorage
          val storageKeys = expectedKeys.filter(currentStorage.contains)

          val updated = storageKeys.foldLeft(receiveDataMap) {
            case (dataMap, key) =>
              dataMap.addData(key, currentStorage(key))
          }

          if (updated.isComplete) {

            serviceStateData.extResultDataConnection.queueExtResponseMsg(
              new ProvideResultEntities(updated.receivedData.values.toList.asJava)
            )

            serviceStateData.copy(
              currentTick = tick,
              extResultSchedule = updatedSchedule,
              extResultsMessage = None,
              receiveDataMap = ReceiveDataMap.empty,
              resultStorage = currentStorage.removedAll(storageKeys),
            )
          } else {

            serviceStateData.copy(
              currentTick = tick,
              extResultSchedule = updatedSchedule,
              extResultsMessage = None,
              receiveDataMap = updated,
              resultStorage = currentStorage.removedAll(storageKeys),
            )
          }
        }

      case unsupported =>
        ctx.log.warn(s"Received unsupported external message: $unsupported")

        serviceStateData.copy(extResultsMessage = None)
    }

    (updatedStateData, None)
  }

  override protected def handleDataMessage(
      extMsg: DataMessageFromExt
  )(using serviceStateData: ExtResultStateData): ExtResultStateData =
    extMsg match {
      case extMsg: ResultDataMessageFromExt =>
        serviceStateData.copy(
          extResultsMessage = Some(extMsg)
        )
    }

  override protected def handleDataResponseMessage(
      extResponseMsg: ServiceResponseMessage
  )(using serviceStateData: ExtResultStateData): ExtResultStateData =
    extResponseMsg match {
      case ResultResponseMessage(results) =>
        val receiveDataMap = serviceStateData.receiveDataMap

        val participantResults = results.map {
          case res: SystemParticipantResult =>
            res.getInputModel -> res
        }.toMap

        if (receiveDataMap.getExpectedKeys.isEmpty) {

          // we currently expect no result,
          // save received result in storage
          serviceStateData.copy(
            resultStorage = serviceStateData.resultStorage ++ participantResults
          )

        } else {
          // we expect results,

          val (expectedModels, otherModels) = participantResults.keys.partition(
            receiveDataMap.getExpectedKeys.contains
          )

          // storing the result, that were not requested
          val updatedResultStorage =
            serviceStateData.resultStorage ++ otherModels
              .map(model => model -> participantResults(model))
              .toMap

          val updated = expectedModels.foldLeft(receiveDataMap) {
            case (dataMap, model) =>
              dataMap.addData(model, participantResults(model))
          }

          if (updated.isComplete) {

            serviceStateData.extResultDataConnection.queueExtResponseMsg(
              new ProvideResultEntities(updated.receivedData.values.toList.asJava)
            )

            serviceStateData.copy(
              receiveDataMap = ReceiveDataMap.empty,
              resultStorage = updatedResultStorage,
            )
          } else {

            serviceStateData.copy(
              receiveDataMap = updated,
              resultStorage = updatedResultStorage,
            )
          }
        }
    }
}
