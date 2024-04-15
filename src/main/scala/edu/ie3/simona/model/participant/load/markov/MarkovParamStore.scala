package edu.ie3.simona.model.participant.load.markov

import java.io.{InputStreamReader, Reader}
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.csv.CSVFormat

import scala.jdk.CollectionConverters._

object MarkovParamStore extends LazyLogging {

  // Usage Probabilities
  def main(args: Array[String]): Unit = {

    val probabilitiesMap = Usage_Probabilities()
    println("Test Funktion: Geladene Gerätewahrscheinlichkeiten:")
    probabilitiesMap.foreach { case (appliance, probability) =>
      println(s"$appliance -> $probability")
    }
  }

  def Usage_Probabilities(): Map[String, Double] = {
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
      "Markov Usage_Probabilities parameters file 'usage_probability.csv' from jar."
    )
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/probabilities/usage_probabilities/usage_probabilities.csv")
    )
  }

  // Switch On Probabilities


}
