/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import java.io.{File, FileInputStream}
import java.util.zip.GZIPInputStream

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.alpakka.csv.scaladsl.{CsvParsing, CsvToMap}
import akka.stream.scaladsl.{
  Balance,
  Flow,
  GraphDSL,
  Merge,
  Sink,
  Source,
  StreamConverters
}
import akka.stream.{FlowShape, Materializer}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.factory.SimpleEntityData
import edu.ie3.datamodel.io.factory.result.SystemParticipantResultFactory
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.exceptions.{FileIOException, ProcessResultEventException}
import org.apache.commons.io.FilenameUtils

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success}

/** Source that reads .csv files containing [[ResultEntity]] s
  *
  * @version 0.1
  * @since 16.01.20
  */
final case class ResultEntityCsvSource(
    resultFactory: SystemParticipantResultFactory,
    linesToSkip: Int,
    concurrentFiles: Int = 10,
    nonIOParallelism: Int = 1337,
    delimiter: String = ",",
    lineDelimiter: String = "\n"
)(
    implicit val system: ActorSystem,
    implicit val materializer: Materializer,
    implicit val ec: ExecutionContextExecutor
) extends LazyLogging {

  private val createMapping: Sink[ResultEntity, Future[
    Map[Class[_ <: ResultEntity], Vector[_ <: ResultEntity]]
  ]] =
    Sink.foldAsync(
      Map.empty[Class[_ <: ResultEntity], Vector[_ <: ResultEntity]]
    )((map, currentEntry) =>
      Future {
        {
          val updatedElementList = map.getOrElse(
            currentEntry.getClass,
            Vector.empty[ResultEntity]
          ) :+ currentEntry
          map + (currentEntry.getClass -> updatedElementList)
        }
      }
    )

  private def parseLine(model: Class[ResultEntity], filePath: String)(
      rowWithHeadline: Map[String, String]
  ): Future[_ <: ResultEntity] = Future {

    val simpleEntityData = new SimpleEntityData(rowWithHeadline.asJava, model)
    resultFactory.get(simpleEntityData).toScala match {
      case Some(resultEntity) =>
        resultEntity
      case None =>
        logger.error(
          s"Unable to parse line in $filePath\nline: ${rowWithHeadline.values}\nWill ignore that line!"
        )
        throw new ProcessResultEventException(
          s"Unable to parse line in $filePath\nline: ${rowWithHeadline.values}\nWill ignore that line!"
        )
    }
  }

  private val parseFile
      : Flow[(Class[_ <: ResultEntity], File), ResultEntity, NotUsed] =
    Flow[(Class[_ <: ResultEntity], File)].flatMapConcat {
      case (model: Class[ResultEntity], file: File) =>
        val inputStream = file.getAbsolutePath match {
          case path: String if path.endsWith(".csv") =>
            new FileInputStream(file)
          case path: String if path.endsWith(".csv.gz") =>
            new GZIPInputStream(new FileInputStream(file))
          case file: String =>
            throw new FileIOException(
              s"Unable to open file with file type " +
                s"${FilenameUtils.getExtension(file)}. Currently only '.csv' or '.csv.gz' is supported!"
            )
        }

        StreamConverters
          .fromInputStream(() => inputStream)
          .via(CsvParsing.lineScanner())
          .via(CsvToMap.toMapAsStrings())
          .mapAsync(parallelism = nonIOParallelism)(
            parseLine(model, file.getPath)
          )
      case (model, file) =>
        throw new FileIOException(
          s"Invalid model $model and file $file provided!"
        )
    }

  private val processSingleFile
      : Flow[(Class[_ <: ResultEntity], File), ResultEntity, NotUsed] =
    Flow[(Class[_ <: ResultEntity], File)]
      .via(parseFile)

  private val balancer = GraphDSL.create() { implicit builder =>
    import GraphDSL.Implicits._

    val balance =
      builder.add(Balance[(Class[_ <: ResultEntity], File)](concurrentFiles))
    val merge = builder.add(Merge[ResultEntity](concurrentFiles))

    (1 to concurrentFiles).foreach { _ =>
      balance ~> processSingleFile ~> merge
    }

    FlowShape(balance.in, merge.out)
  }

  def readFiles(
      filesToRead: Map[Class[_ <: ResultEntity], File]
  ): Future[Map[Class[_ <: ResultEntity], Vector[_ <: ResultEntity]]] = {
    val startTime = System.currentTimeMillis()
    Source(filesToRead).via(balancer).runWith(createMapping).andThen {
      case Success(_) =>
        val elapsedTime = (System
          .currentTimeMillis() - startTime) / 1000.0
        logger.info(s"Import finished in ${elapsedTime}s")
      case Failure(e) =>
        logger.error("Import failed!", e)
    }
  }

}
