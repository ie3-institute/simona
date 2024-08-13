/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.exceptions.EntityProcessorException
import edu.ie3.datamodel.io.processor.result.ResultEntityProcessor
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.exceptions.ProcessResultEventException
import edu.ie3.util.StringUtils
import edu.ie3.util.io.FileIOUtils

import java.io.{BufferedWriter, File, FileWriter, Writer}
import java.lang
import java.nio.file.Path
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
  */
final case class ResultEntityCsvSink private (
    outfileName: String,
    fileWriter: Writer,
    resultEntityProcessor: ResultEntityProcessor,
    compressOutputFiles: Boolean,
    delimiter: String,
) extends ResultEntitySink
    with LazyLogging {

  private val logPrefix: String => String = (content: String) =>
    s"[${outfileName}Sink] $content"

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
    try {
      val attributeToValue = resultEntityProcessor
        .handleEntity(resultEntity)
        .asScala
        .view

      val columns = resultEntityProcessor.getHeaderElements
      val text = if (attributeToValue.nonEmpty) {
        val resString: String =
          columns
            .map { column =>
              attributeToValue.getOrElse(column, "")
            }
            .mkString(",")

        "\n".concat(resString)
      } else {
        "\n"
      }

      fileWriter.write(text)
    } catch {
      case e: EntityProcessorException =>
        throw new ProcessResultEventException("Processing result failed", e)
    }
  }

  /** Create the initial the .csv-file and write the header in the first row
    *
    * @return
    *   a future with information on the I/O operation
    */
  private def writeHeader(): Unit = {
    val text = resultEntityProcessor.getHeaderElements.view
      .map(StringUtils.camelCaseToSnakeCase)
      .mkString(",")

    fileWriter.write(text)
    // flush out the headline immediately
    fileWriter.flush()
  }

  private def zipAndDel(outFileName: String): Future[lang.Boolean] = {
    // compress the files
    logger.debug(
      logPrefix(
        "Check if some files are selected for compression and start file compression and raw file deletion " +
          "for all selected files ..."
      )
    )

    FileIOUtils
      .compressFile(Path.of(outFileName), Path.of(outFileName + ".gz"))
      .asScala
      .andThen {
        case Success(_) =>
          logger.debug(logPrefix(s"Compressed $outfileName."))
          FileIOUtils.deleteFileIfExists(outFileName).asScala
        case Failure(_) =>
          throw new ProcessResultEventException(
            s"Failed to zip file $outFileName!"
          )
      }
  }

  /** Contains all cleanup operations before closing this sink
    */
  override def close(): Unit = {
    // wait until all I/O futures are completed
    fileWriter.flush()
    fileWriter.close()

    // compress files if necessary
    if (compressOutputFiles)
      Await.ready(zipAndDel(outfileName), 100.minutes)
  }

}

object ResultEntityCsvSink {

  /** Default constructor to get an instance of [[ResultEntityCsvSink]] incl.
    * creation of the output file with the headers written
    *
    * @param outfileName
    *   the filename of the output file (full filepath + file extension)
    * @param resultEntityProcessor
    *   a processor that can process the entity and create a field -> value
    *   mapping of the class that should be processed
    * @param delimiter
    *   the delimiter of the .csv-file
    * @return
    *   instance of [[ResultEntityCsvSink]] to be used to write results
    */
  def apply(
      outfileName: String,
      resultEntityProcessor: ResultEntityProcessor,
      compressOutputFiles: Boolean,
      delimiter: String = ",",
  ): ResultEntityCsvSink = {

    val file = new File(outfileName)
    val existedBefore = file.exists()

    val writer = new BufferedWriter(new FileWriter(file, existedBefore), 32768)

    val resultEntityCsvSink = new ResultEntityCsvSink(
      outfileName,
      writer,
      resultEntityProcessor,
      compressOutputFiles,
      delimiter,
    )

    if (!existedBefore)
      resultEntityCsvSink.writeHeader()
    resultEntityCsvSink
  }
}
