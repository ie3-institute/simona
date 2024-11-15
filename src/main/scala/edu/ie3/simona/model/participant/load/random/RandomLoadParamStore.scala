/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.random

import java.io.{InputStreamReader, Reader}
import java.time.{Duration, ZonedDateTime}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.exceptions.FileIOException
import edu.ie3.simona.model.participant.load.DayType
import edu.ie3.simona.model.participant.load.random.RandomLoadParamStore.initializeDayTypeValues
import org.apache.commons.csv.{CSVFormat, CSVRecord}

import scala.jdk.CollectionConverters._

/** Storage for a collection of random load parameters.
  */
final case class RandomLoadParamStore private (reader: Reader) {
  private val parameterMap: Map[DayType.Value, TypeDayParameters] =
    initializeDayTypeValues(reader)

  /** Returns the random load parameters for given time.
    *
    * @param time
    *   the requested time
    * @return
    *   [[RandomLoadParameters]] to use for the given time
    */
  def parameters(time: ZonedDateTime): RandomLoadParameters = {
    val dayType = DayType(time.getDayOfWeek)
    parameterMap
      .getOrElse(
        dayType,
        throw new RuntimeException(
          s"Cannot determine the random load parameters for '$time' (day type '$dayType')."
        ),
      )
      .getQuarterHourParameters(time)
  }
}

case object RandomLoadParamStore extends LazyLogging {
  val resolution: Duration = Duration.ofMinutes(15)

  /** Default value store, that uses information from a file
    * 'random_load_parameters.csv' placed in the resources folder of the project
    * / jar
    */
  private lazy val defaultStore = new RandomLoadParamStore(getDefaultReader)

  /** Default entry point to get the default implementation with the provided
    * default random load parameters
    *
    * @return
    *   Instance of [[RandomLoadParamStore]] with default random load parameters
    */
  def apply(): RandomLoadParamStore = defaultStore

  /** Default entry point to get an instance of [[RandomLoadParamStore]] with
    * customized random load model parameters provided by a specific reader
    * including the files. For the default implementation with the provided
    * default random load parameters use [[apply()]] above.
    *
    * @param reader
    *   the reader containing the information where the file with custom random
    *   load parameters is located
    * @return
    *   instance of [[RandomLoadParamStore]] with custom random load parameters
    */
  def apply(reader: Reader): RandomLoadParamStore =
    new RandomLoadParamStore(reader)

  /** Returns a [[CSVFormat]] with the first line as its header
    */
  private def csvParser: CSVFormat =
    CSVFormat.DEFAULT.builder().setHeader().setSkipHeaderRecord(true).build()

  /** Initializes all type day values by receiving values from provided reader.
    *
    * @param reader
    *   a reader that is providing random load parameters from a CSV file
    */
  def initializeDayTypeValues(
      reader: Reader
  ): Map[DayType.Value, TypeDayParameters] = {
    val parser = csvParser.parse(reader)
    /* records list is an ArrayList */
    val records = parser.getRecords

    val headerElements = List.from(parser.getHeaderNames.asScala)
    val descriptorTree = buildDescriptorTree(headerElements)

    /* Go through all lines of the csv file */
    records.asScala
      .flatMap(record => {
        val quartHour = record.get("quarterHour").toInt

        /* Go through all day types */
        descriptorTree.map { case (dayType, parameterToCol) =>
          try {
            (dayType, quartHour, assembleParameters(record, parameterToCol))
          } catch {
            case e: FileIOException =>
              throw new FileIOException(
                s"Cannot determine random load parameters for day type '$dayType' and quarter hour '$quartHour'",
                e,
              )
          }
        }
      })
      .groupMap { case (dayType, _, _) => dayType } {
        case (_, quarterHour, randomLoadParameters) =>
          (quarterHour, randomLoadParameters)
      } // Group entries by day type
      .map { // For each day type, sort the parameters by quarter hour and build a type day parameter object from it
        case (dayType, quarterHourToParameters) =>
          dayType -> TypeDayParameters(
            quarterHourToParameters.sortBy(_._1).map(_._2).toArray
          )
      }
  }

  /** Builds a descriptor tree, which gives information what to find where in
    * the file. Each headline element breaks down to encoded information about
    * the probability density function parameter and the addressed day type. All
    * headline elements are treated this way and a mapping from day type to
    * column position of parameter is build.
    *
    * @param headerElements
    *   List of headline elements
    * @return
    *   Mapping from day type to a mapping from parameter to column index
    */
  private def buildDescriptorTree(
      headerElements: List[String]
  ): Map[DayType.Value, Map[RandomLoadParameters.Value, Int]] = {
    /* Each header entry encodes a pair of parameter and day type */
    type HeaderKey = (RandomLoadParameters.Value, DayType.Value)

    headerElements
      .filterNot(_ == "quarterHour")
      .zipWithIndex
      .map { case (key, colIndex) =>
        /* Extract parameter and day type mapping from entry */
        val keyRegex = "(k|my|sigma)(\\w{2})".r
        val headerKey: HeaderKey = key match {
          case keyRegex(parameterKey, dayTypeKey) =>
            (RandomLoadParameters(parameterKey), DayType(dayTypeKey))
          case unsupportedKey =>
            throw new IllegalArgumentException(
              s"Cannot extract parameter and day type from head line entry '$unsupportedKey'"
            )
        }
        (headerKey._2, headerKey._1, colIndex)
      }
      .groupMap { case (dayType, _, _) =>
        dayType
      } { case (_, randomLoadParameterKey, colIndex) =>
        (randomLoadParameterKey, colIndex)
      }
      .map { case (dayType, parameterKeyToColIndex) =>
        dayType -> parameterKeyToColIndex.map { case (parameterKey, colIndex) =>
          parameterKey -> colIndex
        }.toMap
      }
  }

  /** Assembles a triple of entries in a [[CSVRecord]] to consistent
    * [[RandomLoadParameters]]
    *
    * @param record
    *   Csv record to receive data from
    * @param parameterToCol
    *   Mapping from parameter to column index
    * @return
    *   A consistent [[RandomLoadParameters]] object
    */
  private def assembleParameters(
      record: CSVRecord,
      parameterToCol: Map[RandomLoadParameters.Value, Int],
  ): RandomLoadParameters = {
    val k = record
      .get(
        parameterToCol.getOrElse(
          RandomLoadParameters.K,
          throw new FileIOException(
            s"Cannot determine column index for random load parameter ${RandomLoadParameters.K}."
          ),
        )
      )
      .toDouble
    val my = record
      .get(
        parameterToCol.getOrElse(
          RandomLoadParameters.MY,
          throw new FileIOException(
            s"Cannot determine column index for random load parameter ${RandomLoadParameters.MY}."
          ),
        )
      )
      .toDouble
    val sigma = record
      .get(
        parameterToCol.getOrElse(
          RandomLoadParameters.SIGMA,
          throw new FileIOException(
            s"Cannot determine column index for random load parameter ${RandomLoadParameters.SIGMA}."
          ),
        )
      )
      .toDouble
    RandomLoadParameters(k, my, sigma)
  }

  /** @return
    *   A reader pointing to the default random load parameter location
    */
  private def getDefaultReader: Reader = {
    logger.info(
      "Loading default random load parameters file 'random_load_parameters.csv' from jar."
    )
    new InputStreamReader(
      this.getClass.getResourceAsStream("/load/random_load_parameters.csv")
    )
  }
}
