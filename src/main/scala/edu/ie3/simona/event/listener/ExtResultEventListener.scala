/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.api.data.connection.ExtResultListener
import edu.ie3.simona.api.ontology.results.ProvideResultEntities
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.*
import edu.ie3.simona.event.listener.DelayedStopHelper.StoppingMsg
import edu.ie3.simona.event.listener.ResultEventListener.{
  AggregatedTransformer3wResult,
  Transformer3wKey,
}
import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.util.{Failure, Success}

object ExtResultEventListener {

  type Message = ResultEvent | DelayedStopHelper.StoppingMsg

  def apply(
      connection: ExtResultListener,
      threeWindingResults: Map[
        Transformer3wKey,
        AggregatedTransformer3wResult,
      ] = Map.empty,
  ): Behavior[Message] = Behaviors.receivePartial[Message] {
    case (_, ParticipantResultEvent(systemParticipantResult)) =>
      connection.queueExtResponseMsg(
        new ProvideResultEntities(systemParticipantResult)
      )
      Behaviors.same

    case (
          ctx,
          PowerFlowResultEvent(
            nodeResults,
            switchResults,
            lineResults,
            transformer2wResults,
            transformer3wResults,
            congestionResults,
          ),
        ) =>
      // handle all results except the three winding transformer results
      val results: Iterable[ResultEntity] =
        nodeResults ++ switchResults ++ lineResults ++ transformer2wResults ++ congestionResults
      connection.queueExtResponseMsg(
        new ProvideResultEntities(results.toList.asJava)
      )

      // handling of three winding transformers
      val updatedResults = transformer3wResults.foldLeft(threeWindingResults) {
        case (allResults, result) =>
          val key = Transformer3wKey(result.input, result.time)
          // retrieve existing partial result or use empty one
          val partialResult =
            allResults.getOrElse(
              key,
              AggregatedTransformer3wResult.EMPTY,
            )
          // add partial result
          partialResult.add(result).map { updatedResult =>
            if (updatedResult.ready) {
              // if result is complete, we can write it out
              updatedResult.consolidate.foreach(res =>
                connection.queueExtResponseMsg(new ProvideResultEntities(res))
              )
              // also remove partial result from map
              allResults.removed(key)
            } else {
              // if result is not complete yet, just update it
              allResults + (key -> updatedResult)
            }
          } match {
            case Success(results) => results
            case Failure(exception) =>
              ctx.log.warn(
                "Failure when handling partial Transformer3w result",
                exception,
              )
              // on failure, we just continue with previous results
              allResults
          }
      }

      ExtResultEventListener(connection, updatedResults)

    case (_, ThermalResultEvent(thermalResult)) =>
      connection.queueExtResponseMsg(new ProvideResultEntities(thermalResult))
      Behaviors.same

    case (_, FlexOptionsResultEvent(flexOptionsResult)) =>
      connection.queueExtResponseMsg(
        new ProvideResultEntities(flexOptionsResult)
      )
      Behaviors.same

    case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
      DelayedStopHelper.handleMsg((ctx, msg))
  }
}
