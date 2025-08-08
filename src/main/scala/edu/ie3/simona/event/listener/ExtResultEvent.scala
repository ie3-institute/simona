/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.connector.Transformer3WResult
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.api.data.connection.{
  ExtResultDataConnection,
  ExtResultListener,
}
import edu.ie3.simona.api.ontology.DataMessageFromExt
import edu.ie3.simona.api.ontology.results.{
  ProvideResultEntities,
  RequestResultEntities,
  ResultDataMessageFromExt,
}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.*
import edu.ie3.simona.event.listener.ResultEventListener.{
  AggregatedTransformer3wResult,
  Transformer3wKey,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.slf4j.Logger

import java.time.ZonedDateTime
import java.util
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success}

object ExtResultEvent {

  type Message = ResultEvent | DelayedStopHelper.StoppingMsg

  final case class ProviderState(
      scheduler: ActorRef[SchedulerMessage],
      connection: ExtResultDataConnection,
      resultStore: Map[UUID, ResultEntity] = Map.empty,
      threeWindingResults: Map[
        Transformer3wKey,
        AggregatedTransformer3wResult,
      ] = Map.empty,
      extMessage: Option[ResultDataMessageFromExt] = None,
      simStartTime: ZonedDateTime,
      gridAssets: List[UUID] = List.empty,
  ) {
    def updateResultData(
        updatedThreeWindingResults: Map[
          Transformer3wKey,
          AggregatedTransformer3wResult,
        ],
        results: Seq[ResultEntity],
    ): ProviderState = {
      val updateStore =
        resultStore ++ results.map(res => res.getInputModel -> res).toMap

      copy(
        threeWindingResults = updatedThreeWindingResults,
        resultStore = updateStore,
      )
    }
  }

  def listener(
      connection: ExtResultListener,
      threeWindingResults: Map[
        Transformer3wKey,
        AggregatedTransformer3wResult,
      ] = Map.empty,
  ): Behavior[Message] = Behaviors.receivePartial[Message] {
    case (ctx, resultEvent: ResultEvent) =>
      val (updatedThreeWinding, results) =
        handleResultEvent(resultEvent, threeWindingResults)(using ctx.log)

      connection.queueExtResponseMsg(
        new ProvideResultEntities(results.toList.asJava)
      )

      listener(connection, updatedThreeWinding)

    case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
      DelayedStopHelper.handleMsg((ctx, msg))
  }

  def provider(
      connection: ExtResultDataConnection,
      scheduler: ActorRef[SchedulerMessage],
      simStartTime: ZonedDateTime,
  ): Behavior[Message | DataMessageFromExt | Activation] = {
    val gridResults = connection.getGridResultDataAssets.asScala
    val participantResults = connection.getParticipantResultDataAssets.asScala
    val flexOptionResults = connection.getFlexOptionAssets.asScala

    val assets = (gridResults ++ participantResults ++ flexOptionResults).toSet

    val stateData =
      ProviderState(
        scheduler,
        connection,
        simStartTime = simStartTime,
        gridAssets = gridResults.toList,
      )

    val resultFilter: ResultEntity => Boolean =
      assets.contains.compose(_.getInputModel)

    provider(stateData)(using resultFilter)
  }

  private def provider(stateData: ProviderState)(using
      resultFilter: ResultEntity => Boolean
  ): Behavior[Message | DataMessageFromExt | Activation] =
    Behaviors.receivePartial[Message | DataMessageFromExt | Activation] {
      case (ctx, resultEvent: ResultEvent) =>
        val (updatedThreeWinding, results) =
          handleResultEvent(
            resultEvent,
            stateData.threeWindingResults,
            resultFilter,
          )(using ctx.log)

        val updatedState =
          stateData.updateResultData(updatedThreeWinding, results)

        // reactivate this service, if we have an unanswered request
        stateData.extMessage.foreach { case extMsg: RequestResultEntities =>
          ctx.self ! Activation(extMsg.tick)
        }

        provider(updatedState)

      case (_, messageFromExt: ResultDataMessageFromExt) =>
        // save ext message
        provider(stateData.copy(extMessage = Some(messageFromExt)))

      case (ctx, ScheduleServiceActivation(tick, unlockKey)) =>
        stateData.scheduler ! ScheduleActivation(
          ctx.self,
          tick,
          Some(unlockKey),
        )

        Behaviors.same

      case (ctx, Activation(tick)) =>
        // handle ext message

        val extMsg = stateData.extMessage.getOrElse(
          // this should not be possible because the external simulation schedules this provider
          throw CriticalFailureException(
            "ExtResultDataService was triggered without ResultDataMessageFromExt available"
          )
        )

        extMsg match {
          case requestResultEntities: RequestResultEntities =>
            val uuids =
              new util.ArrayList[UUID](requestResultEntities.requestedResults)
            val receivedTick = requestResultEntities.tick

            // TODO: Check received tick
            // receivedTick == tick

            if (receivedTick == 0) {
              // we can't send grid results for the tick 0
              // therefore, we remove them
              uuids.removeAll(stateData.gridAssets.asJava)
            }

            val results = stateData.resultStore

            if (results.nonEmpty) {
              val foundResults = uuids.asScala
                .flatMap(uuid => results.get(uuid).map(data => uuid -> data))
                .toMap

              val updatedData = results.removedAll(foundResults.keys)

              // check if there are unanswered requests
              uuids.removeAll(foundResults.keySet.asJava)

              val updatedStateData = if (uuids.isEmpty) {
                // send results to ext
                stateData.connection.queueExtResponseMsg(
                  new ProvideResultEntities(updatedData.values.toList.asJava)
                )

                // tell the scheduler that we are finished
                stateData.scheduler ! Completion(ctx.self)

                stateData.copy(
                  resultStore = updatedData,
                  extMessage = None,
                )
              } else {
                val updatedRequest =
                  new RequestResultEntities(receivedTick, uuids)

                stateData.copy(
                  resultStore = updatedData,
                  extMessage = Some(updatedRequest),
                )
              }

              provider(updatedStateData)

            } else if (results.isEmpty && uuids.isEmpty) {
              // send no results to ext
              stateData.connection.queueExtResponseMsg(
                ProvideResultEntities.empty()
              )

              // tell the scheduler that we are finished
              stateData.scheduler ! Completion(ctx.self)

              provider(stateData.copy(extMessage = None))
            } else {
              ctx.log.warn(s"Could not find results! Waiting ...")
              Behaviors.same
            }

          case other =>
            ctx.log.warn(s"Cannot handle external result message: $other")
            Behaviors.same
        }

      case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
        DelayedStopHelper.handleMsg((ctx, msg))

    }

  private def handleResultEvent(
      resultEvent: ResultEvent,
      threeWindingResults: Map[Transformer3wKey, AggregatedTransformer3wResult],
      resultFilter: ResultEntity => Boolean = _ => true,
  )(using
      log: Logger
  ): (Map[Transformer3wKey, AggregatedTransformer3wResult], Seq[ResultEntity]) =
    resultEvent match {
      case ParticipantResultEvent(systemParticipantResult) =>
        (threeWindingResults, Seq(systemParticipantResult).filter(resultFilter))

      case PowerFlowResultEvent(
            nodeResults,
            switchResults,
            lineResults,
            transformer2wResults,
            partialTransformer3wResults,
            congestionResults,
          ) =>
        // handling of three winding transformers
        val (updatedResults, transformer3wResults) =
          handleThreeWindingTransformers(
            partialTransformer3wResults,
            threeWindingResults,
          )

        val results: Seq[ResultEntity] =
          transformer3wResults ++ nodeResults ++ switchResults ++ lineResults ++ transformer2wResults ++ congestionResults

        (updatedResults, results.filter(resultFilter))

      case ThermalResultEvent(thermalResult) =>
        (threeWindingResults, Seq(thermalResult).filter(resultFilter))

      case FlexOptionsResultEvent(flexOptionsResult) =>
        (threeWindingResults, Seq(flexOptionsResult).filter(resultFilter))
    }

  private def handleThreeWindingTransformers(
      transformer3wResults: Iterable[PartialTransformer3wResult],
      threeWindingResults: Map[Transformer3wKey, AggregatedTransformer3wResult],
  )(using log: Logger) = transformer3wResults.foldLeft(
    threeWindingResults,
    Seq.empty[Transformer3WResult],
  ) { case ((allPartialResults, allResults), result) =>
    val key = Transformer3wKey(result.input, result.time)
    // retrieve existing partial result or use empty one
    val partialResult =
      allPartialResults.getOrElse(
        key,
        AggregatedTransformer3wResult.EMPTY,
      )
    // add partial result
    partialResult.add(result).map { updatedResult =>
      if (updatedResult.ready) {
        // if result is complete, we can write it out
        updatedResult.consolidate match {
          case Failure(exception) =>
            log.warn(
              "Failure when handling partial Transformer3w result",
              exception,
            )
            // on failure, we just continue with previous results
            (allPartialResults, allResults)
          case Success(res) =>
            (allPartialResults.removed(key), allResults.appended(res))
        }

      } else {
        // if result is not complete yet, just update it
        (allPartialResults + (key -> updatedResult), allResults)
      }
    } match {
      case Success(results) => results
      case Failure(exception) =>
        log.warn(
          "Failure when handling partial Transformer3w result",
          exception,
        )
        // on failure, we just continue with previous results
        (allPartialResults, allResults)
    }
  }
}
