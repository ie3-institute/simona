/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.simona.ontology.messages.services.WeatherMessage
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.apache.commons.csv.{CSVFormat, CSVRecord}
import org.apache.kafka.common.serialization.Serdes.UUID
import squants.energy.Megawatts
import squants.motion.MetersPerSecond
import squants.space.Degrees
import squants.{Angle, Kelvin, Power}
import tech.units.indriya.quantity.Quantities

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}
import java.nio.file.{Files, Path}
import java.time.ZonedDateTime
import java.util.zip.GZIPInputStream
import scala.collection.convert.ImplicitConversions.`iterator asScala`
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.reflect.runtime.universe.Try
import java.util.UUID
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ZERO_POWER.p
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.quantity.Quantities.getQuantity

trait PvModelITHelper {

  private val CSV_FORMAT = CSVFormat.DEFAULT.builder().setHeader().build()
  implicit val angleTolerance: Angle = Degrees(1e-5)

  def getCsvRecords(fileName: String): java.util.Iterator[CSVRecord] = {
    val resultsInputData = new File(this.getClass.getResource(fileName).getFile)
    val fileStream = new FileInputStream(resultsInputData)
    val gzipStream = new GZIPInputStream(fileStream)
    val decoder = new InputStreamReader(gzipStream, "UTF-8")
    val br = new BufferedReader(decoder)
    CSVFormat.DEFAULT.parse(br).iterator()
  }

  def parsePvInputCSV(line: String): Option[PvInput] = {
    val nodeInput = new NodeInput(
      UUID.fromString("85f8b517-8a2d-4c20-86c6-3ff3c5823e6d"),
      "NodeInputModel for PvModel Test",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      getQuantity(1, PU),
      false,
      p,
      GermanVoltageLevelUtils.MV_20KV,
      11,
    )

    val fields = line.split(";")
    if (fields.length >= 15) {
      Try {
        new PvInput(
          UUID.fromString(fields(0)),
          fields(6),
          new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
          OperationTime.notLimited(),
          nodeInput,
          CosPhiFixed.CONSTANT_CHARACTERISTIC,
          null,
          fields(8).toInt,
          Quantities.getQuantity(fields(2).toDouble, StandardUnits.AZIMUTH),
          Quantities.getQuantity(fields(4).toDouble, StandardUnits.EFFICIENCY),
          Quantities.getQuantity(
            fields(5).toDouble,
            StandardUnits.SOLAR_ELEVATION_ANGLE,
          ),
          fields(12).toDouble,
          fields(7).toDouble,
          fields(9).toBoolean,
          Quantities.getQuantity(fields(12).toDouble, StandardUnits.S_RATED),
          fields(3).toDouble,
        )
      }.toOption
    } else {
      None
    }
  }

  def getCreatePvModels: Map[String, PvModel] = {

    val csvPath = Path.of(getClass.getResource("_pv/it/grid_data.csv").toURI)
    val lines = Files.readAllLines(csvPath).asScala
    val pvPlants: Set[PvInput] = lines.drop(1).flatMap(parsePvInputCSV).toSet
    val simulationStartDate =
      TimeUtil.withDefaults.toZonedDateTime("2011-01-01T00:00:00Z")
    val simulationEndDate =
      TimeUtil.withDefaults.toZonedDateTime("2012-01-01T00:00:00Z")

    val pvModels = mutable.Map[String, PvModel]()

    pvPlants.foreach { inputModel =>
      val model = PvModel(
        inputModel,
        1d,
        simulationStartDate,
        simulationEndDate,
      )
      pvModels.put(inputModel.id, model)
    }
    pvModels.toMap
  }

  def getWeatherData
      : Map[ZonedDateTime, Map[String, WeatherMessage.WeatherData]] = {
    val filePath = Path.of("resources/_pv/it/weather.tar.gz")
    val csvRecords = getCsvRecords(filePath.toString)

    csvRecords.foldLeft(
      Map.empty[ZonedDateTime, Map[String, WeatherMessage.WeatherData]]
    ) { (weatherMap, row) =>
      val time = TimeUtil.withDefaults.toZonedDateTime(row.get(0))
      val modelId = row.get(1)
      val weather = WeatherMessage.WeatherData(
        WattsPerSquareMeter(row.get(22).replace("Wh/m²", "").toDouble),
        WattsPerSquareMeter(row.get(21).replace("Wh/m²", "").toDouble),
        Kelvin(0),
        MetersPerSecond(0),
      )
      val modelToWeatherMap = weatherMap.getOrElse(
        time,
        Map.empty[String, WeatherMessage.WeatherData],
      )
      weatherMap + (time -> (modelToWeatherMap + (modelId -> weather)))
    }
  }

  def getResultsData: Map[ZonedDateTime, Map[String, Power]] = {
    val filePath = Path.of("resources/_pv/it/results2.tar.gz")
    val csvRecords = getCsvRecords(filePath.toString)

    csvRecords.foldLeft(Map.empty[ZonedDateTime, Map[String, Power]]) {
      (resultsMap, row) =>
        val time = TimeUtil.withDefaults.toZonedDateTime(row.get(0))

        val powerData = (1 until row.size()).map { i =>
          val modelId = s"pv_${i}"
          val power = Megawatts(row.get(i).toDouble)
          modelId -> power
        }.toMap

        resultsMap + (time -> powerData)
    }
  }
}
