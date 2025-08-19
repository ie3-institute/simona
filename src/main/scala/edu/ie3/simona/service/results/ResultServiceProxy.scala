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
import edu.ie3.simona.ontology.messages.RequestResultMessage
import edu.ie3.simona.service.ServiceStateData.ServiceBaseStateData
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}
import org.apache.pekko.actor.typed.{ActorRef, Behavior, PostStop}
import org.slf4j.Logger

import java.util.UUID
import scala.util.{Failure, Success}

object ResultServiceProxy {

  type Message = ResultEvent | RequestResultMessage |
    DelayedStopHelper.StoppingMsg

  private final case class ResultServiceStateData(
      listeners: Seq[ActorRef[ResultEvent.ResultResponse]],
      nextTicks: Map[UUID, Long] = Map.empty,
      resultMapping: Map[UUID, ResultEntity] = Map.empty,
      threeWindingResults: Map[
        Transformer3wKey,
        AggregatedTransformer3wResult,
      ] = Map.empty,
  ) extends ServiceBaseStateData {
    def notifyListener(results: List[ResultEntity]): Unit =
      listeners.foreach(_ ! ResultResponse(results))
  }

  def apply(
      listeners: Seq[ActorRef[ResultEvent.ResultResponse]],
      bufferSize: Int = 10000,
  ): Behavior[Message] = Behaviors.withStash(bufferSize) { buffer =>
    idle(ResultServiceStateData(listeners))(using buffer)
  }

  private def idle(
      stateData: ResultServiceStateData
  )(using
      buffer: StashBuffer[Message]
  ): Behavior[Message] = Behaviors
    .receivePartial[Message] {
      case (ctx, resultEvent: ResultEvent) =>
        // handles the event and updates the state data
        val updatedStateData =
          handleResultEvent(resultEvent, stateData)(using ctx.log)

        // un-stash received requests
        buffer.unstashAll(idle(updatedStateData))
      case (ctx, requestResultMessage: RequestResultMessage) =>
        val requestedResults = requestResultMessage.requestedResults
        val tick = requestResultMessage.tick

        val nextTicks = stateData.nextTicks
        val results = stateData.resultMapping

        val allResultsPresent = requestedResults.forall(results.contains)
        val allResultsUpToDate = requestedResults.forall { uuid =>
          nextTicks.get(uuid) match {
            case Some(value) => value < tick
            case None        => true
          }
        }

        if allResultsPresent && allResultsUpToDate then {
          val res = requestedResults.map(results).toList
          ctx.log.debug(s"Answering message: $requestResultMessage")

          requestResultMessage.replyTo ! ResultResponse(res)
        } else {
          buffer.stash(requestResultMessage)
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
    case ParticipantResultEvent(systemParticipantResult, maybeNextTick) =>
      // notify listener
      stateData.notifyListener(List(systemParticipantResult))

      val uuid = systemParticipantResult.getInputModel

      val nextTicks = stateData.nextTicks

      val updatedNextTicks = maybeNextTick match {
        case Some(value) =>
          nextTicks.updated(uuid, value)
        case None =>
          nextTicks
      }

      stateData.copy(
        nextTicks = updatedNextTicks,
        resultMapping = stateData.resultMapping.updated(
          uuid,
          systemParticipantResult,
        ),
      )

    case PowerFlowResultEvent(
          nodeResults,
          switchResults,
          lineResults,
          transformer2wResults,
          partialTransformer3wResults,
          congestionResults,
          maybeNextTick,
        ) =>
      // handling of three winding transformers
      val (updatedResults, transformer3wResults) =
        handleThreeWindingTransformers(
          partialTransformer3wResults,
          stateData.threeWindingResults,
        )

      val results =
        (transformer3wResults ++ nodeResults ++ switchResults ++ lineResults ++ transformer2wResults ++ congestionResults).map {
          res => res.getInputModel -> res
        }.toMap

      // notify listener
      stateData.notifyListener(results.values.toList)

      val nextTicks = stateData.nextTicks

      val updatedNextTicks = maybeNextTick match {
        case Some(value) =>
          nextTicks ++ results.keys.map(key => key -> value).toMap

        case None =>
          nextTicks
      }

      stateData.copy(
        nextTicks = updatedNextTicks,
        resultMapping = stateData.resultMapping ++ results,
        threeWindingResults = updatedResults,
      )

    case ThermalResultEvent(thermalResult, maybeNextTick) =>
      // notify listener
      stateData.notifyListener(List(thermalResult))

      val uuid = thermalResult.getInputModel

      val nextTicks = stateData.nextTicks

      val updatedNextTicks = maybeNextTick match {
        case Some(value) =>
          nextTicks.updated(uuid, value)
        case None =>
          nextTicks
      }

      stateData.copy(
        nextTicks = updatedNextTicks,
        resultMapping = stateData.resultMapping
          .updated(uuid, thermalResult),
      )

    case FlexOptionsResultEvent(flexOptionsResult, maybeNextTick) =>
      // notify listener
      stateData.notifyListener(List(flexOptionsResult))

      val uuid = flexOptionsResult.getInputModel

      val nextTicks = stateData.nextTicks

      val updatedNextTicks = maybeNextTick match {
        case Some(value) =>
          nextTicks.updated(uuid, value)
        case None =>
          nextTicks
      }

      stateData.copy(
        nextTicks = updatedNextTicks,
        resultMapping = stateData.resultMapping
          .updated(uuid, flexOptionsResult),
      )
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
