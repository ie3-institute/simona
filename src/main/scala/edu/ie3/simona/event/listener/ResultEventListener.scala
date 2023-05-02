/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import akka.actor._
import akka.pattern.pipe
import edu.ie3.datamodel.io.processor.result.ResultEntityProcessor
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{ParticipantResultEvent, PowerFlowResultEvent}
import edu.ie3.simona.event.listener.ResultEventListener.{AggregatedTransformer3wResult, BaseData, Init, ResultEventListenerData, SinkResponse, Transformer3wKey, UninitializedData}
import edu.ie3.simona.exceptions.{FileHierarchyException, InitializationException, ProcessResultEventException}
import edu.ie3.simona.io.result._
import edu.ie3.simona.logging.SimonaFSMActorLogging
import edu.ie3.simona.ontology.messages.StopMessage
import edu.ie3.simona.util.ResultFileHierarchy
import org.apache.commons.lang3.SerializationUtils
import sourcecode.Text.generate

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object ResultEventListener extends Transformer3wResultSupport {

  /** Internal [[ResultEventListenerData]]
    */
  sealed trait ResultEventListenerData

  /** Data for state [[Uninitialized]]
    */
  private final case object UninitializedData extends ResultEventListenerData

  private final case object Init

  private final case class SinkResponse(
      response: Map[Class[_], ResultEntitySink]
  )

  /** [[ResultEventListener]] base data containing all information the listener
    * needs
    *
    * @param classToSink
    *   a map containing the sink for each class that should be processed by the
    *   listener
    * @param previousResults
    *   a map containing only the previous results
    *
    */
  private final case class BaseData(
      classToSink: Map[Class[_], ResultEntitySink],
      threeWindingResults: Map[
        Transformer3wKey,
        AggregatedTransformer3wResult] = Map.empty,
      previousResults: Map[UUID, ResultEntity] = Map.empty
  ) extends ResultEventListenerData

  def props(
      resultFileHierarchy: ResultFileHierarchy
  ): Props =
    Props(
      new ResultEventListener(
        resultFileHierarchy
      )
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
                        fileName.endsWith(".gz")
                      )
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
}

class ResultEventListener(
    resultFileHierarchy: ResultFileHierarchy
) extends SimonaListener
    with FSM[AgentState, ResultEventListenerData]
    with SimonaFSMActorLogging
    with Stash {

  override def preStart(): Unit = {
    log.debug("Starting initialization!")
    resultFileHierarchy.resultSinkType match {
      case _: ResultSinkType.Kafka =>
        log.debug("NodeResults will be processed by a Kafka sink.")
      case _ =>
        log.debug(
          s"Events that will be processed: {}",
          resultFileHierarchy.resultEntitiesToConsider
            .map(_.getSimpleName)
            .mkString(",")
        )
    }
    self ! Init
  }

  /** Handle the given result and possibly update the state data
    * The state data is compared to the previous corresponding data. Both maps are
    * only updated if the state data has changed.
    * Before comparing both maps, previous results are cloned and the time is adapted to the corresponding time in the state data.
    *
    * @param resultEntity
    *   Result entity to handle
    * @param baseData
    *   Base data
    * @return
    *   The possibly update base data
    */
  def handleResult(resultEntity: ResultEntity, baseData: BaseData): BaseData = {
    val previousResult = baseData.previousResults.get(resultEntity.getUuid)

    val previousResultClone = previousResult.map { previousResult =>
      val resultEntityClone = SerializationUtils.clone(previousResult)
      resultEntityClone.setTime(resultEntity.getTime)
      resultEntityClone
    }

    if (previousResultClone.exists(_.equals(resultEntity))) {

      baseData
    } else {

      val updatedPreviousResults = baseData.previousResults + (resultEntity.getUuid -> SerializationUtils.clone(resultEntity))

    handOverToSink(resultEntity, baseData.classToSink)
      baseData.copy(previousResults = updatedPreviousResults)
    }
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
      baseData: ResultEventListener.BaseData
  ): BaseData = {
    val enhancedResults =
      registerPartialTransformer3wResult(result, baseData.threeWindingResults)
    val uncompletedResults =
      flushComprehensiveResults(enhancedResults, baseData.classToSink)
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
      threeWindingResults: Map[Transformer3wKey, AggregatedTransformer3wResult]
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
          log.warning(
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
      classToSink: Map[Class[_], ResultEntitySink]
  ): Map[Transformer3wKey, AggregatedTransformer3wResult] = {
    val comprehensiveResults = results.filter(_._2.ready)
    comprehensiveResults.map(_._2.consolidate).foreach {
      case Success(result) => handOverToSink(result, classToSink)
      case Failure(exception) =>
        log.warning("Cannot consolidate / write result.\n\t{}", exception)
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
      classToSink: Map[Class[_], ResultEntitySink]
  ): Unit =
    Try {
      classToSink
        .get(resultEntity.getClass)
        .foreach(_.handleResultEntity(resultEntity))
    } match {
      case Failure(exception) =>
        log.error(exception, "Error while writing result event: ")
      case Success(_) =>
    }

  startWith(Uninitialized, UninitializedData)

  when(Uninitialized) {

    case Event(_: ResultEvent, _) =>
      stash()
      stay()

    case Event(StopMessage(_), _) =>
      stash()
      stay()

    case Event(Init, _) =>
      Future
        .sequence(
          ResultEventListener.initializeSinks(
            resultFileHierarchy
          )
        )
        .map(result => SinkResponse(result.toMap))
        .pipeTo(self)
      stay()

    case Event(SinkResponse(classToSink), _) =>
      // Sink Initialization succeeded
      log.debug("Initialization complete!")

      unstashAll()
      goto(Idle) using BaseData(classToSink)

    case Event(Status.Failure(ex), _) =>
      throw new InitializationException("Unable to setup SimonaSim.", ex)

  }

  when(Idle) {
    case Event(
          ParticipantResultEvent(systemParticipantResult),
          baseData: BaseData
        ) =>
      val updateBaseData = handleResult(systemParticipantResult, baseData)
      stay() using updateBaseData

    case Event(
          PowerFlowResultEvent(
            nodeResults,
            switchResults,
            lineResults,
            transformer2wResults,
            transformer3wResults
          ),
          baseData: BaseData
        ) =>
      val updatedBaseData =
        (nodeResults ++ switchResults ++ lineResults ++ transformer2wResults ++ transformer3wResults)
          .foldLeft(baseData) {
            case (currentBaseData, resultEntity: ResultEntity) =>
              handleResult(resultEntity, currentBaseData)
            case (
                  currentBaseData,
                  partialTransformerResult: PartialTransformer3wResult
                ) =>
              handlePartialTransformer3wResult(
                partialTransformerResult,
                currentBaseData
              )
          }
      stay() using updatedBaseData

    case Event(StopMessage(_), _) =>
      // set ReceiveTimeout message to be sent if no message has been received for 5 seconds
      context.setReceiveTimeout(5.seconds)
      stay()

    case Event(ReceiveTimeout, _) =>
      // there have been no messages for 5 seconds, let's end this
      self ! PoisonPill
      stay()
  }

  onTermination { case StopEvent(_, _, baseData: BaseData) =>
    // wait until all I/O has finished
    log.debug(
      "Shutdown initiated.\n\tThe following three winding results are not comprehensive and are not " +
        "handled in sinks:{}\n\tWaiting until writing result data is completed ...",
      baseData.threeWindingResults.keys
        .map { case Transformer3wKey(model, zdt) =>
          s"model '$model' at $zdt"
        }
        .mkString("\n\t\t")
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
      5.minutes
    )

    log.debug("Result I/O completed.")

    log.debug("Shutdown.")
  }

  whenUnhandled { case event =>
    log.error(s"Unhandled event $event in state $stateName.")
    stay()

  }
}
