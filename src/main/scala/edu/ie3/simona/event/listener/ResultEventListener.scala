/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import edu.ie3.datamodel.io.processor.result.ResultEntityProcessor
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.event.Event
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent,
  PowerFlowResultEvent,
  ThermalResultEvent,
}
import edu.ie3.simona.exceptions.{
  FileHierarchyException,
  ProcessResultEventException,
}
import edu.ie3.simona.io.result._
import edu.ie3.simona.ontology.messages.StopMessage
import edu.ie3.simona.util.ResultFileHierarchy
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{Behavior, PostStop}
import org.slf4j.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object ResultEventListener extends Transformer3wResultSupport {

  trait ResultMessage extends Event

  private final case class SinkResponse(
      response: Map[Class[_], ResultEntitySink]
  ) extends ResultMessage

  private final case class Failed(ex: Exception) extends ResultMessage

  private case object StopTimeout extends ResultMessage

  /** [[ResultEventListener]] base data containing all information the listener
    * needs
    *
    * @param classToSink
    *   a map containing the sink for each class that should be processed by the
    *   listener
    */
  private final case class BaseData(
      classToSink: Map[Class[_], ResultEntitySink],
      threeWindingResults: Map[
        Transformer3wKey,
        AggregatedTransformer3wResult,
      ] = Map.empty,
  )

  /** Initialize the sinks for this listener based on the provided collection
    * with the model names as strings. It generates one sink for each model
    * class.
    *
    * @param resultFileHierarchy
    *   The result file hierarchy
    * @return
    *   mapping of the model class to the sink for this model class
    */
  private def initializeSinks(
      resultFileHierarchy: ResultFileHierarchy
  ): Iterable[Future[(Class[_], ResultEntitySink)]] = {
    resultFileHierarchy.resultSinkType match {
      case _: ResultSinkType.Csv =>
        resultFileHierarchy.resultEntitiesToConsider
          .map(resultClass => {
            resultFileHierarchy.rawOutputDataFilePaths
              .get(resultClass)
              .map(Future.successful)
              .getOrElse(
                Future.failed(
                  new FileHierarchyException(
                    s"Unable to get file path for result class '${resultClass.getSimpleName}' from output file hierarchy! " +
                      s"Available file result file paths: ${resultFileHierarchy.rawOutputDataFilePaths}"
                  )
                )
              )
              .flatMap { fileName =>
                if (fileName.endsWith(".csv") || fileName.endsWith(".csv.gz")) {
                  Future {
                    (
                      resultClass,
                      ResultEntityCsvSink(
                        fileName.replace(".gz", ""),
                        new ResultEntityProcessor(resultClass),
                        fileName.endsWith(".gz"),
                      ),
                    )
                  }
                } else {
                  Future.failed(
                    new ProcessResultEventException(
                      s"Invalid output file format for file $fileName provided. Currently only '.csv' or '.csv.gz' is supported!"
                    )
                  )
                }
              }
          })
      case ResultSinkType.InfluxDb1x(url, database, scenario) =>
        // creates one connection per result entity that should be processed
        resultFileHierarchy.resultEntitiesToConsider
          .map(resultClass =>
            ResultEntityInfluxDbSink(url, database, scenario).map(
              (resultClass, _)
            )
          )

      case ResultSinkType.Kafka(
            topicNodeRes,
            runId,
            bootstrapServers,
            schemaRegistryUrl,
            linger,
          ) =>
        val classes: Iterable[Class[_ <: ResultEntity]] = Set(
          classOf[NodeResult] // currently, only NodeResults are sent out
        )
        classes.map(clz =>
          Future.successful(
            (
              clz,
              ResultEntityKafkaSink[NodeResult](
                topicNodeRes,
                runId,
                bootstrapServers,
                schemaRegistryUrl,
                linger,
              ),
            )
          )
        )
    }
  }

  /** Handle the given result and possibly update the state data
    *
    * @param resultEntity
    *   Result entity to handle
    * @param baseData
    *   Base data
    * @return
    *   The possibly update base data
    */
  private def handleResult(
      resultEntity: ResultEntity,
      baseData: BaseData,
      log: Logger,
  ): BaseData = {
    handOverToSink(resultEntity, baseData.classToSink, log)
    baseData
  }

  /** Handle a partial three winding result properly by adding it to an
    * [[AggregatedTransformer3wResult]] and flushing then possibly completed
    * results. Finally, the base data are updated.
    *
    * @param result
    *   Result entity to handle
    * @param baseData
    *   Base data
    * @return
    *   The possibly update base data
    */
  private def handlePartialTransformer3wResult(
      result: PartialTransformer3wResult,
      baseData: BaseData,
      log: Logger,
  ): BaseData = {
    val key = Transformer3wKey(result.input, result.time)
    // retrieve existing partial result or use empty one
    val partialResult =
      baseData.threeWindingResults.getOrElse(
        key,
        AggregatedTransformer3wResult.EMPTY,
      )
    // add partial result
    val updatedResults = partialResult.add(result).map { updatedResult =>
      if (updatedResult.ready) {
        // if result is complete, we can write it out
        updatedResult.consolidate.foreach {
          handOverToSink(_, baseData.classToSink, log)
        }
        // also remove partial result from map
        baseData.threeWindingResults.removed(key)
      } else {
        // if result is not complete yet, just update it
        baseData.threeWindingResults + (key -> updatedResult)
      }
    } match {
      case Success(results) => results
      case Failure(exception) =>
        log.warn(
          "Failure when handling partial Transformer3w result",
          exception,
        )
        // on failure, we just continue with previous results
        baseData.threeWindingResults
    }

    baseData.copy(threeWindingResults = updatedResults)
  }

  /** Handing over the given result entity to the sink, that might be apparent
    * in the map
    *
    * @param resultEntity
    *   entity to handle
    * @param classToSink
    *   mapping from entity class to sink
    */
  private def handOverToSink(
      resultEntity: ResultEntity,
      classToSink: Map[Class[_], ResultEntitySink],
      log: Logger,
  ): Unit =
    Try {
      classToSink
        .get(resultEntity.getClass)
        .foreach(_.handleResultEntity(resultEntity))
    }.failed.foreach { exception =>
      log.error("Error while writing result event: ", exception)
    }

  def apply(
      resultFileHierarchy: ResultFileHierarchy
  ): Behavior[ResultMessage] = Behaviors.setup[ResultMessage] { ctx =>
    ctx.log.debug("Starting initialization!")
    resultFileHierarchy.resultSinkType match {
      case _: ResultSinkType.Kafka =>
        ctx.log.debug("NodeResults will be processed by a Kafka sink.")
      case _ =>
        ctx.log.debug(
          "Events that will be processed: {}",
          resultFileHierarchy.resultEntitiesToConsider
            .map(_.getSimpleName)
            .mkString(","),
        )
    }

    ctx.pipeToSelf(
      Future.sequence(
        ResultEventListener.initializeSinks(resultFileHierarchy)
      )
    ) {
      case Failure(exception: Exception) => Failed(exception)
      case Success(result)               => SinkResponse(result.toMap)
    }

    init()
  }

  private def init(): Behavior[ResultMessage] = Behaviors.withStash(200) {
    buffer =>
      Behaviors.receive[ResultMessage] {
        case (ctx, SinkResponse(response)) =>
          ctx.log.debug("Initialization complete!")
          buffer.unstashAll(idle(BaseData(response)))

        case (ctx, Failed(ex)) =>
          ctx.log.error("Unable to setup ResultEventListener.", ex)
          Behaviors.stopped

        case (_, msg) =>
          // stash all messages
          buffer.stash(msg)
          Behaviors.same
      }
  }

  private def idle(baseData: BaseData): Behavior[ResultMessage] = Behaviors
    .receive[ResultMessage] {
      case (ctx, ParticipantResultEvent(participantResult)) =>
        val updatedBaseData = handleResult(participantResult, baseData, ctx.log)
        idle(updatedBaseData)

      case (ctx, ThermalResultEvent(thermalResult)) =>
        val updatedBaseData = handleResult(thermalResult, baseData, ctx.log)
        idle(updatedBaseData)

      case (
            ctx,
            PowerFlowResultEvent(
              nodeResults,
              switchResults,
              lineResults,
              transformer2wResults,
              transformer3wResults,
            ),
          ) =>
        val updatedBaseData =
          (nodeResults ++ switchResults ++ lineResults ++ transformer2wResults ++ transformer3wResults)
            .foldLeft(baseData) {
              case (currentBaseData, resultEntity: ResultEntity) =>
                handleResult(resultEntity, currentBaseData, ctx.log)
              case (
                    currentBaseData,
                    partialTransformerResult: PartialTransformer3wResult,
                  ) =>
                handlePartialTransformer3wResult(
                  partialTransformerResult,
                  currentBaseData,
                  ctx.log,
                )
            }
        idle(updatedBaseData)

      case (ctx, FlexOptionsResultEvent(flexOptionsResult)) =>
        val updatedBaseData = handleResult(flexOptionsResult, baseData, ctx.log)
        idle(updatedBaseData)

      case (ctx, _: StopMessage) =>
        ctx.log.debug(
          s"${getClass.getSimpleName} received Stop message, shutting down when no message has been received in 5 seconds."
        )
        ctx.setReceiveTimeout(5.seconds, StopTimeout)
        Behaviors.same

      case (ctx, StopTimeout) =>
        // there have been no messages for 5 seconds, let's end this
        ctx.log.debug(s"${getClass.getSimpleName} is now stopped.")
        Behaviors.stopped

    }
    .receiveSignal { case (ctx, PostStop) =>
      // wait until all I/O has finished
      ctx.log.debug(
        "Shutdown initiated.\n\tThe following three winding results are not comprehensive and are not " +
          "handled in sinks:{}\n\tWaiting until writing result data is completed ...",
        baseData.threeWindingResults.keys
          .map { case Transformer3wKey(model, zdt) =>
            s"model '$model' at $zdt"
          }
          .mkString("\n\t\t"),
      )

      // close sinks concurrently to speed up closing (closing calls might be blocking)
      Await.ready(
        Future.sequence(
          baseData.classToSink.valuesIterator.map(sink =>
            Future {
              sink.close()
            }
          )
        ),
        5.minutes,
      )

      ctx.log.debug("Result I/O completed.")
      ctx.log.debug("Shutdown.")

      Behaviors.same
    }

}
