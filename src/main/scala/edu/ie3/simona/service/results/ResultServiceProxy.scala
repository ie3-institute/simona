/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.results

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.connector.Transformer3WResult
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.*
import edu.ie3.simona.event.listener.DelayedStopHelper
import edu.ie3.simona.event.listener.ResultEventListener.{
  AggregatedTransformer3wResult,
  Transformer3wKey,
}
import edu.ie3.simona.ontology.messages.RequestResult
import edu.ie3.simona.service.ServiceStateData.ServiceBaseStateData
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}
import org.apache.pekko.actor.typed.{ActorRef, Behavior, PostStop}
import org.slf4j.Logger

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Success}

object ResultServiceProxy {

  type Message = ResultEvent | RequestResult | ExpectResult |
    DelayedStopHelper.StoppingMsg

  final case class ExpectResult(assets: UUID | Seq[UUID], tick: Long)

  private final case class ResultServiceStateData(
      listeners: Seq[ActorRef[ResultEvent.ResultResponse]],
      simStartTime: ZonedDateTime,
      currentTick: Long = INIT_SIM_TICK,
      threeWindingResults: Map[
        Transformer3wKey,
        AggregatedTransformer3wResult,
      ] = Map.empty,
      gridResults: Map[UUID, Iterable[ResultEntity]] = Map.empty,
      results: Map[UUID, Iterable[ResultEntity]] = Map.empty,
      waitingForResults: Map[UUID, Long] = Map.empty,
  ) extends ServiceBaseStateData {
    def notifyListener(results: Map[UUID, Iterable[ResultEntity]]): Unit =
      listeners.foreach(_ ! ResultResponse(results))

    def notifyListener(result: ResultEntity): Unit =
      listeners.foreach(
        _ ! ResultResponse(Map(result.getInputModel -> List(result)))
      )

    def isWaiting(uuids: Iterable[UUID], tick: Long): Boolean = {
      uuids.exists { uuid =>
        waitingForResults.get(uuid) match {
          case Some(nextTick) if nextTick <= tick => true
          case _                                  => false
        }
      }
    }

    def updateTick(tick: Long): ResultServiceStateData =
      copy(currentTick = tick)

    def waitForResult(expectResult: ExpectResult): ResultServiceStateData =
      expectResult.assets match {
        case uuid: UUID =>
          copy(waitingForResults =
            waitingForResults.updated(uuid, expectResult.tick)
          )
        case uuids: Seq[UUID] =>
          val tick = expectResult.tick

          copy(waitingForResults =
            waitingForResults ++ uuids.map(uuid => uuid -> tick).toMap
          )
      }

    def addResult(result: ResultEntity): ResultServiceStateData = {
      val uuid = result.getInputModel
      val tick = result.getTime.toTick(using simStartTime)

      val updatedWaitingForResults =
        if waitingForResults.get(uuid).contains(tick) then {
          waitingForResults.removed(uuid)
        } else waitingForResults

      val updatedResults = results.get(uuid) match {
        case Some(values) =>
          val updatedValues = values
            .map { value => value.getClass -> value }
            .toMap
            .updated(result.getClass, result)
            .values

          results.updated(uuid, updatedValues)

        case None =>
          results.updated(uuid, Iterable(result))
      }

      copy(
        results = updatedResults,
        waitingForResults = updatedWaitingForResults,
      )
    }

    def getResults(uuids: Seq[UUID]): Map[UUID, Iterable[ResultEntity]] = {
      uuids.flatMap { uuid =>
        gridResults.get(uuid) match {
          case Some(values) =>
            Some(uuid -> values)
          case None =>
            results.get(uuid).map { res => uuid -> res }
        }
      }.toMap
    }

  }

  def apply(
      listeners: Seq[ActorRef[ResultEvent.ResultResponse]],
      simStartTime: ZonedDateTime,
      bufferSize: Int = 10000,
  ): Behavior[Message] = Behaviors.withStash(bufferSize) { buffer =>
    idle(ResultServiceStateData(listeners, simStartTime))(using buffer)
  }

  private def idle(
      stateData: ResultServiceStateData
  )(using
      buffer: StashBuffer[Message]
  ): Behavior[Message] = Behaviors
    .receivePartial[Message] {
      case (_, expectResult: ExpectResult) =>
        idle(stateData.waitForResult(expectResult))

      case (ctx, resultEvent: ResultEvent) =>
        // ctx.log.warn(s"Received results: $resultEvent")

        // handles the event and updates the state data
        val updatedStateData =
          handleResultEvent(resultEvent, stateData)(using ctx.log)

        // un-stash received requests
        buffer.unstashAll(idle(updatedStateData))
      case (ctx, requestResultMessage: RequestResult) =>
        val requestedResults = requestResultMessage.requestedResults
        val tick = requestResultMessage.tick

        if stateData.isWaiting(requestedResults, tick) then {
          // ctx.log.warn(s"Cannot answer request: $requestedResults")

          buffer.stash(requestResultMessage)
        } else {

          requestResultMessage.replyTo ! ResultResponse(
            stateData.getResults(requestedResults)
          )
        }

        Behaviors.same

      case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
        DelayedStopHelper.handleMsg((ctx, msg))
    }
    .receiveSignal { case (ctx, PostStop) =>
      ctx.log.debug(
        "Shutdown initiated.\n\tThe following three winding results are not comprehensive and are not " +
          "handled in sinks:{}\n\tWaiting until writing result data is completed ...",
        stateData.threeWindingResults.keys
          .map { case Transformer3wKey(model, zdt) =>
            s"model '$model' at $zdt"
          }
          .mkString("\n\t\t"),
      )

      Behaviors.same
    }

  private def handleResultEvent(
      resultEvent: ResultEvent,
      stateData: ResultServiceStateData,
  )(using log: Logger): ResultServiceStateData = resultEvent match {
    case PowerFlowResultEvent(
          nodeResults,
          switchResults,
          lineResults,
          transformer2wResults,
          partialTransformer3wResults,
          congestionResults,
          nextTick,
        ) =>
      // handling of three winding transformers
      val (updatedResults, transformer3wResults) =
        handleThreeWindingTransformers(
          partialTransformer3wResults,
          stateData.threeWindingResults,
        )

      val gridResults =
        (transformer3wResults ++ nodeResults ++ switchResults ++ lineResults ++ transformer2wResults ++ congestionResults)
          .groupBy(_.getInputModel)

      // notify listener
      stateData.notifyListener(gridResults)

      stateData.copy(
        gridResults = stateData.gridResults ++ gridResults,
        threeWindingResults = updatedResults,
        waitingForResults =
          stateData.waitingForResults.removedAll(gridResults.keys),
      )

    case ParticipantResultEvent(systemParticipantResult) =>
      // notify listener
      stateData.notifyListener(systemParticipantResult)

      stateData.addResult(systemParticipantResult)

    case ThermalResultEvent(thermalResult) =>
      // notify listener
      stateData.notifyListener(thermalResult)

      stateData.addResult(thermalResult)

    case FlexOptionsResultEvent(flexOptionsResult) =>
      // notify listener
      stateData.notifyListener(flexOptionsResult)

      stateData.addResult(flexOptionsResult)
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
      if updatedResult.ready then {
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
