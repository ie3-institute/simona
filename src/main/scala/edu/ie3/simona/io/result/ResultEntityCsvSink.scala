/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import akka.stream.scaladsl.{FileIO, Source}
import akka.stream.{IOResult, Materializer}
import akka.util.ByteString
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.exceptions.EntityProcessorException
import edu.ie3.datamodel.io.processor.result.ResultEntityProcessor
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.exceptions.{FileIOException, ProcessResultEventException}
import edu.ie3.util.StringUtils
import edu.ie3.util.io.FileIOUtils

import java.nio.file.Paths
import java.nio.file.StandardOpenOption.{
  APPEND,
  CREATE,
  TRUNCATE_EXISTING,
  WRITE
}
import java.{lang, util}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters.CompletionStageOps
import scala.util.{Failure, Success}

/** Sink for [[ResultEntity]] s that can be used to write results into a
  * .csv-file
  *
  * @param outfileName
  *   the filename of the output file (full filepath + file extension)
  * @param resultEntityProcessor
  *   a processor that can process the entity and create a field -> value
  *   mapping of the class that should be processed
  * @param delimiter
  *   the delimiter of the .csv-file
  * @param materializer
  *   the materializer to be used by the stream that writes the output file
  */
final case class ResultEntityCsvSink private (
    outfileName: String,
    resultEntityProcessor: ResultEntityProcessor,
    compressOutputFiles: Boolean,
    delimiter: String = ",",
    bufferSize: Int = 50
)(implicit materializer: Materializer)
    extends ResultEntitySink
    with LazyLogging {

  private val logPrefix: String => String = (content: String) =>
    s"[${outfileName}Sink] $content"

  /** Holds all incomplete tasks to allow await I/O operation termination on
    * closing the sink
    */
  private val incompleteTasks: ArrayBuffer[Future[IOResult]] =
    scala.collection.mutable.ArrayBuffer.empty[Future[IOResult]]

  /** Handling of a [[ResultEntity]] to perform the output writing to the
    * .csv-file. Actually writes the provided result entity into the .csv-file.
    * If the file is not existing yet, it will be created. If it exists already,
    * the result entity data will be appended as a new row.
    *
    * @param resultEntity
    *   the result entity that should be processed
    * @return
    *   a future holding information about the handling process
    */
  def handleResultEntity(resultEntity: ResultEntity): Unit = {
    incompleteTasks.addOne(try {
      val attributeToValue = resultEntityProcessor
        .handleEntity(resultEntity)
        .orElse(new util.LinkedHashMap[String, String]())
        .asScala
        .view

      val columns = resultEntityProcessor.getHeaderElements
      val text = Source.single(if (attributeToValue.nonEmpty) {
        val resString: String =
          columns
            .map { column =>
              attributeToValue.getOrElse(column, "")
            }
            .mkString(",")

        "\n".concat(resString)
      } else {
        "\n"
      })
      val file = Paths.get(outfileName)
      text
        .map(t => ByteString(t))
        .runWith(FileIO.toPath(file, Set(WRITE, APPEND, CREATE)))
    } catch {
      case e: EntityProcessorException =>
        Future.failed[IOResult](
          new FileIOException(
            "Result entity " + resultEntity.getClass.getSimpleName + " is not part of model sink!",
            e
          )
        )
    })

    /* cleanup incomplete tasks - filters out all finished future I/O operations */
    if (incompleteTasks.size > bufferSize)
      incompleteTasks --= incompleteTasks.filter { future =>
        future.value match {
          case Some(Success(_)) => true // keep the finished ones for removal
          case Some(Failure(e)) =>
            logger.error(logPrefix(s"Error writing output data: $e"))
            throw new ProcessResultEventException(e)
          case _ => false //
        }
      }
  }

  /** Creat the initial the .csv-file and write the header in the first row
    *
    * @return
    *   a future with information on the I/O operation
    */
  private def writeHeader(): Future[IOResult] = {
    val headerElements = resultEntityProcessor.getHeaderElements
    val file = Paths.get(outfileName)

    if (file.toFile.exists()) {
      Future.successful[IOResult](IOResult.apply(0))
    } else {
      val text = Source(
        headerElements.view.map(StringUtils.camelCaseToSnakeCase).mkString(",")
      )

      val headerFut = text
        .map(t => ByteString(t))
        .runWith(FileIO.toPath(file, Set(WRITE, TRUNCATE_EXISTING, CREATE)))
      incompleteTasks.addOne(headerFut)
      headerFut
    }
  }

  private def zipAndDel(outFileName: String): Future[lang.Boolean] = {
    // compress the files
    logger.debug(
      logPrefix(
        "Check if some files are selected for compression and start file compression and raw file deletion " +
          "for all selected files ..."
      )
    )

    FileIOUtils.gzip(outfileName).asScala.andThen {
      case Success(_) =>
        logger.debug(logPrefix(s"Compressed $outfileName."))
        FileIOUtils.deleteFileIfExists(outFileName).asScala
      case Failure(_) =>
        Future.failed[IOResult](
          new ProcessResultEventException(
            s"Failed to zip file $outFileName!"
          )
        )
    }
  }

  /** Contains all cleanup operations before closing this sink
    */
  override def close(): Unit = {
    // wait until all I/O futures are completed
    Await.result(
      Future.sequence(incompleteTasks),
      100.minutes
    )

    // compress files if necessary
    if (compressOutputFiles)
      Await.ready(zipAndDel(outfileName), 100.minutes)
  }

}

object ResultEntityCsvSink {

  /** Default constructor to get an instance of [[ResultEntityCsvSource]] incl.
    * creation of the output file with the headers written
    *
    * @param outfileName
    *   the filename of the output file (full filepath + file extension)
    * @param resultEntityProcessor
    *   a processor that can process the entity and create a field -> value
    *   mapping of the class that should be processed
    * @param delimiter
    *   the delimiter of the .csv-file
    * @param bufferSize
    *   maximum size of the buffer of this sink
    * @param materializer
    *   the materializer to be used by the stream that writes the output file
    * @return
    *   instance of [[ResultEntityCsvSource]] to be used to write results
    */
  def apply(
      outfileName: String,
      resultEntityProcessor: ResultEntityProcessor,
      compressOutputFiles: Boolean,
      delimiter: String = ",",
      bufferSize: Int = 50
  )(implicit materializer: Materializer): Future[ResultEntityCsvSink] = {
    val resultEntityCsvSink = new ResultEntityCsvSink(
      outfileName,
      resultEntityProcessor,
      compressOutputFiles,
      delimiter,
      bufferSize
    )

    resultEntityCsvSink.writeHeader().map(_ => resultEntityCsvSink)
  }
}
