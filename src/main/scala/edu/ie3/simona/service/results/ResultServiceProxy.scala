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
import edu.ie3.simona.ontology.messages.ResultMessage.{
  RequestResult,
  ResultResponse,
}
import edu.ie3.simona.service.ServiceStateData.ServiceBaseStateData
import edu.ie3.simona.service.results.Transformer3wResultSupport.{
  AggregatedTransformer3wResult,
  Transformer3wKey,
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}
import org.apache.pekko.actor.typed.{ActorRef, Behavior, PostStop}
import org.slf4j.Logger

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Success}

object ResultServiceProxy {

  type Message = ResultEvent | RequestResult | ExpectResult | AddListener |
    DelayedStopHelper.StoppingMsg

  /** Message send to the [[ResultServiceProxy]]. This message will inform the
    * proxy which assets will provide results for the specified tick.
    *
    * @param assets
    *   One or more assets that will send results.
    * @param tick
    *   For which the results will be sent.
    * @param waitForSetPoint
    *   True, if we need to wait for an em set point.
    */
  final case class ExpectResult(
      assets: UUID | Seq[UUID],
      tick: Long,
      waitForSetPoint: Boolean = false,
  )

  /** Method for adding a listener to the proxy.
    *
    * @param listener
    *   That should be added.
    */
  final case class AddListener(listener: ActorRef[ResultResponse])

  /** State data of the [[ResultServiceProxy]].
    *
    * @param listeners
    *   A sequence of listeners. The proxy will forward all results to these
    *   listeners.
    * @param simStartTime
    *   The start time of the simulation. This is used to convert the tick into
    *   a [[ZonedDateTime]].
    * @param currentTick
    *   The current tick of the result proxy.
    * @param threeWindingResults
    *   A map that contains partial three-winding transformer results.
    * @param results
    *   A map: uuid to other results. This is for participant and em results.
    * @param waitingForResults
    *   A map: uuid to tick. For each result uuid a tick, for which the next
    *   result will be provided, is saved.
    * @param withoutUpdate
    *   A set containing all model uuids for which no update were received.
    * @param requiresSetPoint
    *   A set of participant uuid. The proxy will not wait for those results
    *   before answering a result request. (Note: This is necessary, if an
    *   external simulation is using an
    *   [[edu.ie3.simona.api.data.connection.ExtEmDataConnection]] and an
    *   [[edu.ie3.simona.api.data.connection.ExtResultDataConnection]]
    *   simultaneously.)
    */
  private final case class ResultServiceStateData(
      listeners: Seq[ActorRef[ResultResponse]],
      simStartTime: ZonedDateTime,
      currentTick: Long = INIT_SIM_TICK,
      threeWindingResults: Map[
        Transformer3wKey,
        AggregatedTransformer3wResult,
      ] = Map.empty,
      results: Map[UUID, Map[Class[? <: ResultEntity], ResultEntity]] =
        Map.empty,
      waitingForResults: Map[UUID, Long] = Map.empty,
      withoutUpdate: Set[UUID] = Set.empty,
      requiresSetPoint: Set[UUID] = Set.empty,
  ) extends ServiceBaseStateData {

    /** This method is used to forward results to all known listeners.
      *
      * @param results
      *   That should be sent.
      */
    def notifyListener(results: Map[UUID, Iterable[ResultEntity]]): Unit =
      if results.nonEmpty then listeners.foreach(_ ! ResultResponse(results))

    /** This method is used to forward one result to all known listeners.
      *
      * @param result
      *   That should be sent.
      */
    def notifyListener(result: ResultEntity): Unit =
      listeners.foreach(
        _ ! ResultResponse(Map(result.getInputModel -> List(result)))
      )

    /** Checks if the proxy is waiting for the given uuids at the specified
      * tick.
      *
      * @param uuids
      *   That should be checked.
      * @param tick
      *   That is used.
      * @return
      *   True, if the proxy is waiting for at least one of the uuid for the
      *   given tick.
      */
    def isWaiting(uuids: Iterable[UUID], tick: Long): Boolean = {
      uuids.exists { uuid =>
        waitingForResults.get(uuid) match {
          case Some(nextTick) =>
            nextTick <= tick && !requiresSetPoint.contains(uuid)
          case _ => false
        }
      }
    }

    /** Method for updating the tick of the state data.
      *
      * @param tick
      *   The updated tick.
      * @return
      *   A copy of the state data with update information.
      */
    def updateTick(tick: Long): ResultServiceStateData =
      copy(currentTick = tick)

    /** Method for adding a [[ExpectResult]] information.
      *
      * @param expectResult
      *   That should be considered.
      * @return
      *   A copy of the state data with update information.
      */
    def waitForResult(expectResult: ExpectResult): ResultServiceStateData = {
      expectResult.assets match {
        case uuid: UUID =>
          val updated = copy(waitingForResults =
            waitingForResults.updated(uuid, expectResult.tick)
          )

          if expectResult.waitForSetPoint then {
            updated.copy(requiresSetPoint = requiresSetPoint + uuid)
          } else updated

        case uuids: Seq[UUID] =>
          val tick = expectResult.tick

          val updated = copy(waitingForResults =
            waitingForResults ++ uuids.map(uuid => uuid -> tick).toMap
          )

          if expectResult.waitForSetPoint then {
            updated.copy(requiresSetPoint = requiresSetPoint ++ uuids)
          } else updated
      }
    }

    /** Method for adding a result to the state data.
      *
      * @param result
      *   That should be added.
      * @return
      *   A copy of the state data with update information.
      */
    def addResult(
        result: ResultEntity
    ): ResultServiceStateData = {
      val uuid = result.getInputModel
      val tick = result.getTime.toTick(using simStartTime)

      val updatedWaitingForResults =
        if waitingForResults.get(uuid).contains(tick) then {
          waitingForResults.removed(uuid)
        } else waitingForResults

      val (updatedResults, hasResultChanged) = results.get(uuid) match {
        case Some(oldResults) =>
          filterUnchangedResults(result, oldResults) match {
            case Some((resultClass, res)) =>
              (
                results.updated(uuid, oldResults.updated(resultClass, res)),
                true,
              )
            case None =>
              (results, false)
          }

        case None =>
          (results.updated(uuid, Map(result.getClass -> result)), true)
      }

      val updated = if hasResultChanged then {
        withoutUpdate.excl(uuid)
      } else {
        withoutUpdate.incl(uuid)
      }

      copy(
        results = updatedResults,
        waitingForResults = updatedWaitingForResults,
        requiresSetPoint = requiresSetPoint.excl(uuid),
        withoutUpdate = updated,
      )
    }

    /** Method for extracting results.
      *
      * @param uuids
      *   For which results should be returned.
      * @param sendUnchangedResults
      *   If [[false]] only results that have changed will be returned.
      * @return
      *   A map: uuid to results.
      */
    def getResults(
        uuids: Seq[UUID],
        sendUnchangedResults: Boolean,
    ): Map[UUID, List[ResultEntity]] = {
      uuids.flatMap { uuid =>
        if !withoutUpdate.contains(uuid) || sendUnchangedResults then {
          // we only sent result, if either there was an update or we explicitly requested to send unchanged results
          results.get(uuid).map(res => uuid -> res.values.toList)
        } else None
      }.toMap
    }

  }

  /** Used to create a [[ResultServiceProxy]].
    *
    * @param listeners
    *   A list of all known listeners.
    * @param simStartTime
    *   The start time of the simulation. This is used to convert the tick into
    *   a [[ZonedDateTime]].
    * @param bufferSize
    *   The size of the used message buffer. (Default: 10000)
    * @return
    *   A new behavior.
    */
  def apply(
      listeners: Seq[ActorRef[ResultResponse]],
      simStartTime: ZonedDateTime,
      bufferSize: Int = 10000,
  ): Behavior[Message] = Behaviors.withStash(bufferSize) { buffer =>
    idle(ResultServiceStateData(listeners, simStartTime))(using buffer)
  }

  /** The idle behavior of the [[ResultServiceProxy]].
    *
    * @param stateData
    *   The current state data.
    * @param buffer
    *   A buffer for messages.
    * @return
    *   A new behavior.
    */
  private def idle(
      stateData: ResultServiceStateData
  )(using
      buffer: StashBuffer[Message]
  ): Behavior[Message] = Behaviors
    .receivePartial[Message] {
      case (_, AddListener(listener)) =>
        idle(stateData.copy(listeners = stateData.listeners.appended(listener)))

      case (_, expectResult: ExpectResult) =>
        idle(stateData.waitForResult(expectResult))

      case (ctx, resultEvent: ResultEvent) =>
        // handles the event and updates the state data
        val updatedStateData =
          handleResultEvent(resultEvent, stateData)(using ctx.log)

        // un-stash received requests
        buffer.unstashAll(idle(updatedStateData))
      case (_, requestResultMessage: RequestResult) =>
        val requestedResults = requestResultMessage.requestedResults
        val tick = requestResultMessage.tick

        if stateData.isWaiting(requestedResults, tick) then {

          buffer.stash(requestResultMessage)
        } else {

          requestResultMessage.replyTo ! ResultResponse(
            stateData.getResults(
              requestedResults,
              requestResultMessage.sendUnchangedResults,
            )
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

  /** Method for handling and updating the state data after a result event was
    * received.
    *
    * @param resultEvent
    *   The event to process.
    * @param stateData
    *   The current state data.
    * @param log
    *   A logger for logging.
    * @return
    *   The updated state data.
    */
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
        ) =>
      // handling of three winding transformers
      val (updatedThreeWindingResults, transformer3wResults) =
        handleThreeWindingTransformers(
          partialTransformer3wResults,
          stateData.threeWindingResults,
        )

      val (allResults, changedResults, notUpdated) =
        (transformer3wResults ++ nodeResults ++ switchResults ++ lineResults ++ transformer2wResults ++ congestionResults)
          .groupBy(_.getInputModel)
          .foldLeft(
            stateData.results,
            Map.empty[UUID, Seq[ResultEntity]],
            Seq.empty[UUID],
          ) { case ((allResults, changed, notChanged), (model, results)) =>
            allResults.get(model) match {
              case Some(oldResults) =>
                val changedResults = filterUnchangedResults(results, oldResults)

                if changedResults.nonEmpty then {
                  (
                    allResults.updated(model, oldResults ++ changedResults),
                    changed.updated(model, results),
                    notChanged,
                  )
                } else {
                  (allResults, changed, notChanged.appended(model))
                }

              case None =>
                (
                  allResults.updated(
                    model,
                    results.map(res => res.getClass -> res).toMap,
                  ),
                  changed.updated(model, results),
                  notChanged,
                )
            }
          }

      // notify listener
      stateData.notifyListener(changedResults)

      stateData.copy(
        results = allResults,
        threeWindingResults = updatedThreeWindingResults,
        waitingForResults =
          stateData.waitingForResults.removedAll(allResults.keys),
        withoutUpdate = stateData.withoutUpdate -- allResults.keys ++ notUpdated,
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

  /** Method for handling three-winding results. This is necessary, since a
    * [[PowerFlowResultEvent]] only contains partial results.
    *
    * @param transformer3wResults
    *   An iterable of partial results.
    * @param threeWindingResults
    *   A map: [[Transformer3wKey]] to [[AggregatedTransformer3wResult]].
    * @param log
    *   A logger for logging.
    * @return
    *   An updated map as well as a sequence of completed three-winding
    *   transformer results.
    */
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

  /** Compares the new results with the old ones. If a new result does not match
    * any of the old results, the new result is returned.
    *
    * @param results
    *   To compare.
    * @param oldResults
    *   For comparison.
    * @return
    *   A map containing all results that have changed.
    */
  private[results] def filterUnchangedResults(
      results: Iterable[ResultEntity],
      oldResults: Map[Class[? <: ResultEntity], ResultEntity],
  ): Map[Class[? <: ResultEntity], ResultEntity] =
    results.flatMap(filterUnchangedResults(_, oldResults)).toMap

  /** Compares the new result to the old results. If a new result matched any
    * old result, the new result is returned.
    *
    * @param result
    *   To compare.
    * @param oldResults
    *   For comparison
    * @return
    *   An option for the changed result or None.
    */
  private[results] def filterUnchangedResults(
      result: ResultEntity,
      oldResults: Map[Class[? <: ResultEntity], ResultEntity],
  ): Option[(Class[? <: ResultEntity], ResultEntity)] = {
    val resultClass = result.getClass

    oldResults.get(resultClass) match {
      case Some(oldResult) =>
        // Temporarily change time for comparison, then revert
        val oldTime = oldResult.getTime
        oldResult.setTime(result.getTime)
        val equal = oldResult == result
        oldResult.setTime(oldTime)

        // if equal save old result else save new result
        if equal then None else Some(resultClass -> result)
      case None =>
        Some(resultClass -> result)
    }

  }
}
