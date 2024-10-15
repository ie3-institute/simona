/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior, PostStop}
import edu.ie3.datamodel.io.processor.result.ResultEntityProcessor
import edu.ie3.datamodel.models.result.{ModelResultEntity, NodeResult, ResultEntity}
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.event.ResultEvent.{FlexOptionsResultEvent, ParticipantResultEvent, PowerFlowResultEvent, ThermalResultEvent}
import edu.ie3.simona.exceptions.{FileHierarchyException, ProcessResultEventException}
import edu.ie3.simona.io.result._
import edu.ie3.simona.service.results.ExtResultDataProvider
import edu.ie3.simona.service.results.ExtResultDataProvider.ResultResponseMessage
import edu.ie3.simona.util.ResultFileHierarchy
import org.slf4j.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object ResultEventListener extends Transformer3wResultSupport {

  trait Request

  private final case class SinkResponse(
      response: Map[Class[_], ResultEntitySink]
  ) extends Request

  private final case class InitFailed(ex: Exception) extends Request

  /** [[ResultEventListener]] base data containing all information the listener
    * needs
    *
    * @param classToSink
    *   a map containing the sink for each class that should be processed by the
    *   listener
    * @param extResultDataService
    *   actor for the external data service
    */
  private final case class BaseData(
      classToSink: Map[Class[_], ResultEntitySink],
      extResultDataService: Option[ActorRef[ExtResultDataProvider.Request]],
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
      log: Logger
  ): BaseData = {
    //log.info("Got Result " + resultEntity)
    handOverToSink(resultEntity, baseData.classToSink, log)
    baseData
  }

  private def handleResultWithTick(
                                    resultEntity: ResultEntity,
                                    baseData: BaseData,
                                    log: Logger,
                                    tick: Long,
                                    nextTick: Option[Long] = None
                                  ): BaseData = {
    //log.info("Got Result " + resultEntity)
    handOverToSink(resultEntity, baseData.classToSink, log)
    if (baseData.extResultDataService.isDefined) {
      handOverToExternalService(
        tick,
        resultEntity,
        baseData.extResultDataService,
        nextTick
      )
    }
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

  private def handOverToExternalService(
                                         tick: Long,
                                         resultEntity: ResultEntity,
                                         extResultDataService: Option[ActorRef[ExtResultDataProvider.Request]],
                                         nextTick: Option[Long] = None
  ): Unit = Try {
    val extResultDataServiceRef = extResultDataService.getOrElse(
      throw new Exception("No external data service registered!")
    )
    resultEntity match {
      case modelResultEntity: ModelResultEntity =>
        extResultDataServiceRef ! ResultResponseMessage(modelResultEntity, tick, nextTick)
      case _ =>
        throw new Exception("Wrong data type!")
    }
  }

  def apply(
      resultFileHierarchy: ResultFileHierarchy,
      extResultDataService: Option[ActorRef[ExtResultDataProvider.Request]] = Option.empty[ActorRef[ExtResultDataProvider.Request]],
  ): Behavior[Request] = Behaviors.setup[Request] { ctx =>
    ctx.log.debug("Starting initialization!")
    resultFileHierarchy.resultSinkType match {
      case _: ResultSinkType.Kafka =>
        ctx.log.debug("NodeResults will be processed by a Kafka sink.")
      case _ =>
        ctx.log.debug(
          s"Events that will be processed: {}",
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
      case Failure(exception: Exception) => InitFailed(exception)
      case Success(result)               => SinkResponse(result.toMap)
    }

    init(extResultDataService)
  }

  private def init(
      extResultDataService: Option[ActorRef[ExtResultDataProvider.Request]]
  ): Behavior[Request] = Behaviors.withStash(200) { buffer =>
    Behaviors.receive[Request] {
      case (ctx, SinkResponse(response)) =>
        ctx.log.debug("Initialization complete!")
        buffer.unstashAll(idle(BaseData(response, extResultDataService)))

      case (ctx, InitFailed(ex)) =>
        ctx.log.error("Unable to setup ResultEventListener.", ex)
        Behaviors.stopped

      case (_, msg) =>
        // stash all messages
        buffer.stash(msg)
        Behaviors.same
    }
  }

  private def idle(baseData: BaseData): Behavior[Request] = Behaviors
    .receivePartial[Request] {
      case (ctx, ParticipantResultEvent(participantResult, tick, nextTick)) =>
        val updatedBaseData = handleResultWithTick(participantResult, baseData, ctx.log, tick, nextTick)
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
              tick,
              nextTick
            ),
          ) =>
        val updatedBaseData =
          (nodeResults ++ switchResults ++ lineResults ++ transformer2wResults ++ transformer3wResults)
            .foldLeft(baseData) {
              case (currentBaseData, resultEntity: ResultEntity) =>
                //ctx.log.info(s"resultEntity = $resultEntity, tick = $tick")
                handleResultWithTick(resultEntity, currentBaseData, ctx.log, tick, Some(nextTick))
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

      case (ctx, FlexOptionsResultEvent(flexOptionsResult, tick)) =>
        val updatedBaseData = handleResultWithTick(flexOptionsResult, baseData, ctx.log, tick)
        idle(updatedBaseData)

      case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
        DelayedStopHelper.handleMsg((ctx, msg))

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
