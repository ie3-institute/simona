/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import akka.actor.typed.{Behavior, ActorRef}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.stream.Materializer
import edu.ie3.datamodel.io.processor.result.ResultEntityProcessor
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{
  ParticipantResultEvent,
  PowerFlowResultEvent
}
import edu.ie3.simona.io.result.{
  ResultEntityCsvSink,
  ResultEntityInfluxDbSink,
  ResultEntityKafkaSink,
  ResultEntitySink,
  ResultSinkType
}
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.exceptions.{
  FileHierarchyException,
  ProcessResultEventException
}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object ResultEventListener extends Transformer3wResultSupport {

  /** Internal [[ResultEventListenerData]]
    */
  sealed trait ResultEventListenerData extends ResultEvent
  
  private final case object Init extends ResultEvent

  private final case class SinkResponse(
      response: Map[Class[_], ResultEntitySink]
  ) extends ResultEvent

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
        AggregatedTransformer3wResult
      ] = Map.empty
  ) extends ResultEventListenerData

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
  )(implicit
      materializer: Materializer
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
                  ResultEntityCsvSink(
                    fileName.replace(".gz", ""),
                    new ResultEntityProcessor(resultClass),
                    fileName.endsWith(".gz")
                  ).map((resultClass, _))
                } else {
                  Future(
                    throw new ProcessResultEventException(
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
            linger
          ) =>
        val clzs: Iterable[Class[_ <: ResultEntity]] = Set(
          classOf[NodeResult] // currently, only NodeResults are sent out
        )
        clzs.map(clz =>
          Future.successful(
            (
              clz,
              ResultEntityKafkaSink[NodeResult](
                topicNodeRes,
                runId,
                bootstrapServers,
                schemaRegistryUrl,
                linger
              )
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
      context: ActorContext[ResultEvent]
  ): BaseData = {
    handOverToSink(resultEntity, baseData.classToSink, context)
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
      context: ActorContext[ResultEvent]
  ): BaseData = {
    val enhancedResults =
      registerPartialTransformer3wResult(
        result,
        baseData.threeWindingResults,
        context
      )
    val uncompletedResults =
      flushComprehensiveResults(enhancedResults, baseData.classToSink, context)
    baseData.copy(threeWindingResults = uncompletedResults)
  }

  /** Register the newly received partial 3 winding transformer result result
    * within the map of yet existing results
    *
    * @param result
    *   Result, that has been received
    * @param threeWindingResults
    *   Collection of all incomplete results
    * @return
    *   Map with added result
    */
  private def registerPartialTransformer3wResult(
      result: PartialTransformer3wResult,
      threeWindingResults: Map[Transformer3wKey, AggregatedTransformer3wResult],
      context: ActorContext[ResultEvent]
  ): Map[Transformer3wKey, AggregatedTransformer3wResult] = {
    val resultKey = Transformer3wKey(result.input, result.time)
    val partialTransformer3wResult =
      threeWindingResults.getOrElse(
        resultKey,
        AggregatedTransformer3wResult.EMPTY
      )
    val updatedTransformer3wResult =
      partialTransformer3wResult.add(result) match {
        case Success(value) => value
        case Failure(exception) =>
          context.log.warn(
            "Cannot handle the given result:\n\t{}",
            exception.getMessage
          )
          partialTransformer3wResult
      }
    threeWindingResults + (resultKey -> updatedTransformer3wResult)
  }

  /** Go through all yet available results and check, if one or more of it are
    * comprehensive. If so, hand them over to the sinks and remove them from the
    * map
    *
    * @param results
    *   Available (possibly) ready results
    * @param classToSink
    *   Mapping from result entity class to applicable sink
    * @return
    *   results without ready ones
    */
  private def flushComprehensiveResults(
      results: Map[Transformer3wKey, AggregatedTransformer3wResult],
      classToSink: Map[Class[_], ResultEntitySink],
      context: ActorContext[ResultEvent]
  ): Map[Transformer3wKey, AggregatedTransformer3wResult] = {
    val comprehensiveResults = results.filter(_._2.ready)
    comprehensiveResults.map(_._2.consolidate).foreach {
      case Success(result) => handOverToSink(result, classToSink, context)
      case Failure(exception) =>
        context.log.warn("Cannot consolidate / write result.\n\t{}", exception)
    }
    results.removedAll(comprehensiveResults.keys)
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
      context: ActorContext[ResultEvent]
  ): Unit =
    Try {
      classToSink
        .get(resultEntity.getClass)
        .foreach(_.handleResultEntity(resultEntity))
    } match {
      case Failure(exception) =>
        context.log.error("Error while writing result event: ", exception)
      case Success(_) =>
    }

  def apply(
      resultFileHierarchy: ResultFileHierarchy,
      supervisor: ActorRef[ResultEvent]
  ): Behavior[ResultEvent] = {
    initialize(Init, resultFileHierarchy, supervisor)
  }

  private def initialize(
      resultEvent: ResultEvent,
      resultFileHierarchy: ResultFileHierarchy,
      supervisor: ActorRef[ResultEvent]
  ): Behavior[ResultEvent] = {
    Behaviors.withStash(10) { buffer =>
      Behaviors.receivePartial[ResultEvent] {
        case (ctx, Init) =>
          implicit val materializer: Materializer = Materializer(ctx)
          ctx.log.debug("Starting initialization!")
          resultFileHierarchy.resultSinkType match {
            case _: ResultSinkType.Kafka =>
              ctx.log.debug("NodeResults will be processed by a Kafka sink.")
            case _ =>
              ctx.log.debug(
                s"Events that will be processed: {}",
                resultFileHierarchy.resultEntitiesToConsider
                  .map(_.getSimpleName)
                  .mkString(",")
              )
          }

          val sinkResponse: Future[SinkResponse] = Future
            .sequence(
              ResultEventListener.initializeSinks(
                resultFileHierarchy
              )
            )
            .map(result => SinkResponse(result.toMap))

          sinkResponse.onComplete { response =>
            initialize(response.get, resultFileHierarchy, supervisor)
          }
          Behaviors.same

        case (ctx, SinkResponse(classToSink)) =>
          // Sink Initialization succeeded
          ctx.log.debug("Initialization complete!")

          // TODO: does not work
          // supervisor ! ServiceInitComplete

          buffer.unstashAll(
            initialize(resultEvent, resultFileHierarchy, supervisor)
          )
          idle(BaseData(classToSink))

        case (_, other: ResultEvent) =>
          buffer.stash(other)
          Behaviors.same
      }
    }
  }

  private def idle(baseData: BaseData): Behavior[ResultEvent] =
    Behaviors.receive { case (ctx, event) =>
      (event, baseData) match {
        case (ParticipantResultEvent(systemParticipantResult), baseData) =>
          val updatedBaseData =
            handleResult(systemParticipantResult, baseData, ctx)
          idle(updatedBaseData)

        case (
              PowerFlowResultEvent(
                nodeResults,
                switchResults,
                lineResults,
                transformer2wResults,
                transformer3wResults
              ),
              baseData
            ) =>
          val updatedBaseData =
            (nodeResults ++ switchResults ++ lineResults ++ transformer2wResults ++ transformer3wResults)
              .foldLeft(baseData) {
                case (currentBaseData, resultEntity: ResultEntity) =>
                  handleResult(resultEntity, currentBaseData, ctx)
                case (
                      currentBaseData,
                      partialTransformerResult: PartialTransformer3wResult
                    ) =>
                  handlePartialTransformer3wResult(
                    partialTransformerResult,
                    currentBaseData,
                    ctx
                  )
              }
          idle(updatedBaseData)
      }
    }

}
