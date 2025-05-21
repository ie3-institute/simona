/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.ontology.messages.services.WeatherMessage
import edu.ie3.simona.test.common.input.PvInputTestData
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.apache.commons.csv.{CSVFormat, CSVRecord}
import squants.energy.Megawatts
import squants.motion.MetersPerSecond
import squants.{Kelvin, Power}

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.time.ZonedDateTime
import java.util.zip.GZIPInputStream
import scala.jdk.CollectionConverters.IterableHasAsScala

trait PvModelITHelper extends PvInputTestData {

  private val CSV_FORMAT: CSVFormat =
    CSVFormat.DEFAULT.builder().setHeader().get()

  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2011-01-01T00:00:00Z")

  def getCsvRecords(fileName: String): Iterable[CSVRecord] = {
    val resourcePath = getClass.getResource(fileName).getPath
    val resultsInputData = new File(resourcePath)
    val fileStream = new FileInputStream(resultsInputData)
    val gzipStream = new GZIPInputStream(fileStream)
    val decoder = new InputStreamReader(gzipStream, StandardCharsets.UTF_8)
    val br = new BufferedReader(decoder)

    CSV_FORMAT.parse(br).asScala
  }

  def createPvModels(): Map[String, PvModel] = {
    pvInputsTest.map { inputModel =>
      inputModel.getId -> PvModel.Factory(inputModel).create()
    }.toMap
  }

  def getWeatherData
      : Map[ZonedDateTime, Map[String, WeatherMessage.WeatherData]] = {
    val fileName = "_pv/it/weather.tar.gz"
    val csvRecords: Iterable[CSVRecord] = getCsvRecords(fileName)

    csvRecords.foldLeft(
      Map.empty[ZonedDateTime, Map[String, WeatherMessage.WeatherData]]
    ) { (weatherDataMap, row) =>
      val time = TimeUtil.withDefaults.toZonedDateTime(row.get(0))
      val modelId = row.get(1)

      val weather = WeatherMessage.WeatherData(
        WattsPerSquareMeter(row.get(22).replace("Wh/m²", "").toDouble),
        WattsPerSquareMeter(row.get(21).replace("Wh/m²", "").toDouble),
        Kelvin(0.0),
        MetersPerSecond(0.0),
      )

      val modelToWeatherMap = weatherDataMap.getOrElse(
        time,
        Map.empty[String, WeatherMessage.WeatherData],
      )
      weatherDataMap.updated(time, modelToWeatherMap.updated(modelId, weather))
    }
  }

  def getResultsData: Map[ZonedDateTime, Map[String, Power]] = {
    val fileName = "_pv/it/results.tar.gz"
    val csvRecords: Iterable[CSVRecord] = getCsvRecords(fileName)

    val headers = Array(
      "Datetime",
      "pv_east_1",
      "pv_east_2",
      "pv_south_1",
      "pv_south_2",
      "pv_south_3",
      "pv_south_4",
      "pv_west_1",
      "pv_west_2",
    )

    csvRecords
      .filterNot(row => row.get(0).startsWith("\u0000"))
      .map { row =>
        val time = TimeUtil.withDefaults.toZonedDateTime(row.get(0))
        val modelToPowerMap = headers.tail.zipWithIndex.collect {
          case (modelId, i) =>
            val rawValue = row.get(i + 1)
            modelId -> Megawatts(rawValue.toDouble)
        }.toMap
        time -> modelToPowerMap
      }
      .toMap
  }
}
