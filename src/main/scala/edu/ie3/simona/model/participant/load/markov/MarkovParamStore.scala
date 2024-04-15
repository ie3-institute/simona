package edu.ie3.simona.model.participant.load.markov

import edu.ie3.simona.model.participant.load.markov.ApplianceCategory

import java.io.{InputStreamReader, Reader}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.model.participant.load.markov.SwitchOnProbabilityKey.SwitchOnProbabilityKey
import org.apache.commons.csv.CSVFormat

import scala.jdk.CollectionConverters._

object MarkovParamStore extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val probabilitiesMap = Usage_Probabilities()
    println("Test Funktion: Geladene Gerätewahrscheinlichkeiten:")
    probabilitiesMap.foreach { case (appliance, probability) =>
      println(s"$appliance -> $probability")
    }

    val averageHHMap = Average_HH()
    println("Test Funktion: Durchschnittliche Haushaltsnutzungszeiten:")
    averageHHMap.foreach { case (appliance, value) =>
      println(s"$appliance -> $value")
    }
  }
  // Usage Probabilities
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

  // Average HH
  private def Average_HH(): Map[String, Double] = {
    val reader = getDefaultReaderForAverageHH
    val csvParser = CSVFormat.DEFAULT
      .withDelimiter(';')
      .parse(reader)

    val records = csvParser.getRecords.asScala

    val averageHHMap = records.headOption match {
      case Some(headerRecord) =>
        val applianceNames = headerRecord.iterator().asScala.toSeq
        val valuesRecord = records.drop(1).headOption.getOrElse(csvParser.iterator().next())
        val averageHHValues = valuesRecord.iterator().asScala.map(_.toDouble)
        applianceNames.zip(averageHHValues).toMap
      case None =>
        Map.empty[String, Double]
    }

    reader.close()
    averageHHMap
  }

  private def getDefaultReaderForAverageHH: Reader = {
    logger.info("Markov Average_HH parameters file 'average_hh.csv' from jar.")
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/appliances/average_hh.csv")
    )
  }


}
