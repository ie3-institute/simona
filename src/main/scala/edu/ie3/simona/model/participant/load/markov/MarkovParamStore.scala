package edu.ie3.simona.model.participant.load.markov

import java.io.{InputStreamReader, Reader}
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.csv.CSVFormat

import scala.jdk.CollectionConverters._

object MarkovParamStore extends LazyLogging {
  def loadDefaultApplianceProbabilities(): Map[String, Double] = {
    val reader = getDefaultReader
    val csvParser = CSVFormat.DEFAULT
      .withDelimiter(';')
      .withFirstRecordAsHeader()
      .parse(reader)

    val records = csvParser.getRecords.asScala
    val probabilitiesMap = records.map { record =>
      val applianceCategory = record.get("appliance_category")
      val usageProbability = record.get("usage_probability").toDouble
      (applianceCategory, usageProbability)
    }.toMap

    reader.close()
    probabilitiesMap
  }

  private def getDefaultReader: Reader = {
    logger.info(
      "test"
    )
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/probabilities/usage_probabilities/usage_probabilities.csv")
    )
  }
}
