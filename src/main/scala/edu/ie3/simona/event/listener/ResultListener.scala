/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import edu.ie3.datamodel.io.processor.result.ResultEntityProcessor
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.api.data.connection.ExtResultListener
import edu.ie3.simona.api.ontology.results.ProvideResultEntities
import edu.ie3.simona.exceptions.{
  FileHierarchyException,
  ProcessResultEventException,
}
import edu.ie3.simona.io.result.*
import edu.ie3.simona.ontology.messages.ResultMessage.ResultResponse
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.ResultFileHierarchy
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{Behavior, PostStop}
import org.slf4j.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object ResultListener {

  trait Request

  type Message = Request | ResultResponse

  private final case class SinkResponse(
      response: Map[Class[?], ResultEntitySink]
  ) extends Request

  private final case class InitFailed(ex: Exception) extends Request

  /** [[ResultListener]] base data containing all information the listener needs
    *
    * @param classToSink
    *   a map containing the sink for each class that should be processed by the
    *   listener
    */
  private final case class BaseData(
      classToSink: Map[Class[?], ResultEntitySink]
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
  ): Iterable[Future[(Class[?], ResultEntitySink)]] = {
    resultFileHierarchy.resultSinkType match {
      case csv: ResultSinkType.Csv =>
        val enableCompression = csv.compressOutputs

        resultFileHierarchy.resultEntitiesToConsider.map { resultClass =>
          val filePathOpt =
            resultFileHierarchy.rawOutputDataFilePaths.get(resultClass)

          val filePathFuture = filePathOpt match {
            case Some(fileName) => Future.successful(fileName)
            case None =>
              Future.failed(
                new FileHierarchyException(
                  s"Unable to get file path for result class '${resultClass.getSimpleName}' from output file hierarchy! " +
                    s"Available file result file paths: ${resultFileHierarchy.rawOutputDataFilePaths}"
                )
              )
          }

          filePathFuture.map { fileName =>
            val finalFileName =
              fileName.toString match {
                case name if name.endsWith(".csv.gz") && enableCompression =>
                  name.replace(".gz", "")
                case name if name.endsWith(".csv") => name
                case fileName =>
                  throw new ProcessResultEventException(
                    s"Invalid output file format for file $fileName provided or compression is not activated but filename indicates compression. Currently only '.csv' or '.csv.gz' is supported!"
                  )
              }

            (
              resultClass,
              ResultEntityCsvSink(
                finalFileName,
                new ResultEntityProcessor(resultClass),
                enableCompression,
                csv.bufferOutputs,
                csv.delimiter,
              ),
            )

          }
        }

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
        val classes: Iterable[Class[? <: ResultEntity]] = Set(
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

  /** Handle the given results.
    *
    * @param resultEntities
    *   Results entity to handle.
    * @param baseData
    *   Base data.
    */
  private def handleResults(
      resultEntities: Iterable[ResultEntity],
      baseData: BaseData,
      log: Logger,
  ): Unit =
    resultEntities.foreach(handOverToSink(_, baseData.classToSink, log))

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
      classToSink: Map[Class[?], ResultEntitySink],
      log: Logger,
  ): Unit =
    Try {
      classToSink
        .get(resultEntity.getClass)
        .foreach(_.handleResultEntity(resultEntity))
    }.failed.foreach { exception =>
      log.error("Error while writing result event: ", exception)
    }

  /** Method to create an external result listener.
    *
    * @param connection
    *   Result listener data connection.
    * @return
    *   The behavior of the listener.
    */
  def external(connection: ExtResultListener): Behavior[Message] =
    Behaviors.receivePartial[Message] {
      case (_, ResultResponse(results)) =>
        connection.queueExtResponseMsg(
          new ProvideResultEntities(results.asJava)
        )

        Behaviors.same

      case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
        DelayedStopHelper.handleMsg((ctx, msg))
    }

  def apply(
      resultFileHierarchy: ResultFileHierarchy
  ): Behavior[Message] = Behaviors.setup[Message] { ctx =>
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
        ResultListener.initializeSinks(resultFileHierarchy)
      )
    ) {
      case Failure(exception: Exception) => InitFailed(exception)
      case Success(result)               => SinkResponse(result.toMap)
    }

    init
  }

  private def init: Behavior[Message] = Behaviors.withStash(200) { buffer =>
    Behaviors.receive[Message] {
      case (ctx, SinkResponse(response)) =>
        ctx.log.debug("Initialization complete!")
        buffer.unstashAll(idle(BaseData(response)))

      case (ctx, InitFailed(ex)) =>
        ctx.log.error("Unable to setup ResultEventListener.", ex)
        Behaviors.stopped

      case (_, msg) =>
        // stash all messages
        buffer.stash(msg)
        Behaviors.same
    }
  }

  private def idle(baseData: BaseData): Behavior[Message] = Behaviors
    .receivePartial[Message] {
      case (ctx, ResultResponse(results)) =>
        handleResults(results.values.flatten, baseData, ctx.log)

        Behaviors.same

      case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
        DelayedStopHelper.handleMsg((ctx, msg))

    }
    .receiveSignal { case (ctx, PostStop) =>
      // wait until all I/O has finished
      ctx.log.debug("Shutdown initiated.")

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
