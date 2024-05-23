/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.markov

import java.io.{InputStreamReader, Reader}
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.csv.{CSVFormat, CSVParser}

import scala.collection.mutable
import scala.collection.mutable.{Map => MutableMap}
import scala.jdk.CollectionConverters._

/** Storage for a collection of MarkovAgent parameters.
  */

final case class MarkovParamStore() {}

/** MarkovPramStore reads values from CSV files and returns them as Maps, where
  * the keys represent different parameters and the values are the corresponding
  * values.
  */

object MarkovParamStore extends LazyLogging {

  def main(args: Array[String]): Unit = {}

  /** This function reads the usage probabilities from a CSV file and returns
    * them as a Map, where the keys are the appliance categories and the values
    * are the corresponding probabilities.
    */

  // Usage Probabilities

  def Usage_Probabilities(): Map[String, Double] = {
    val reader = getDefaultReader
    val customFormat = CSVFormat.DEFAULT.builder().setHeader().build()
    val csvParser = new CSVParser(reader, customFormat)
    val records = csvParser.iterator().asScala.drop(1)
    val probabilitiesMap = records.map { record =>
      val applianceCategory = record.get("appliance_category")
      val usageProbability = record.get("usage_probability").toDouble
      (applianceCategory, usageProbability)
    }.toMap
    reader.close()
    probabilitiesMap
  }

  /** @return
    *   A reader pointing to the default Usage_Probabilities parameter location
    */

  private def getDefaultReader: Reader = {
    logger.info(
      "Markov Usage_Probabilities parameters file 'usage_probability.csv' from jar."
    )
    new InputStreamReader(
      getClass.getResourceAsStream(
        "/load/markov/probabilities/usage_probabilities/usage_probabilities.csv"
      )
    )
  }

  // Switch On Probabilities

  def sop_Dish_Washer(): mutable.Map[String, Seq[Int]] = {
    val reader = getDefaultReadersop_Dish_Washer
    val customFormat = CSVFormat.DEFAULT.builder().setHeader().build()
    val csvParser = new CSVParser(reader, customFormat)
    val records = csvParser.getRecords.asScala.toSeq
    val header = csvParser.getHeaderNames.asScala.toSeq
    val dish_Washer = mutable.Map[String, Seq[Int]]()
    for (record <- records) {
      for (i <- header.indices) {
        val applianceCategory = header(i)
        val value = record.get(i).toInt
        val existingValues = dish_Washer.getOrElse(applianceCategory, Seq())
        dish_Washer.put(applianceCategory, existingValues :+ value)
      }
    }
    reader.close()
    dish_Washer
  }

  def getDefaultReadersop_Dish_Washer: Reader = {
    logger.info("Markov Income parameters file 'dish_washer.csv' from jar.")
    new InputStreamReader(
      getClass.getResourceAsStream(
        "/load/markov/probabilities/switch_on_probabilities/dish_washer.csv"
      )
    )
  }

  // Average HH

  def Average_HH(): Map[String, Double] = {
    val reader = getDefaultReaderForAverageHH
    val csvParser = CSVFormat.DEFAULT.parse(reader)
    val records = csvParser.getRecords.asScala
    val averageHHMap = records.headOption match {
      case Some(headerRecord) =>
        val applianceNames = headerRecord.iterator().asScala.toSeq
        val valuesRecord =
          records.drop(1).headOption.getOrElse(csvParser.iterator().next())
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

  // By_Type

  def Type(): MutableMap[String, Map[String, Double]] = {
    val reader = getDefaultReaderType
    val customFormat = CSVFormat.DEFAULT.builder().setHeader().build()
    val csvParser = new CSVParser(reader, customFormat)
    val records = csvParser.getRecords.asScala.toSeq
    val typeMap = MutableMap[String, Map[String, Double]]()
    records.foreach { record =>
      val typeCategory = record.get(0)
      val appliancesMap = MutableMap[String, Double]()
      val header = csvParser.getHeaderNames.asScala.drop(1)
      header.zipWithIndex.foreach { case (appliance, index) =>
        val value = record.get(index + 1).toDouble
        appliancesMap += (appliance -> value)
      }
      typeMap += (typeCategory -> appliancesMap.toMap)
    }
    reader.close()
    typeMap
  }

  private def getDefaultReaderType: Reader = {
    logger.info("Markov Income parameters file 'by_Type.csv' from jar.")
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/appliances/by_Type.csv")
    )
  }

  // By Income

  def income(): MutableMap[String, Map[String, Double]] = {
    val reader = getDefaultReaderIncome
    val customFormat = CSVFormat.DEFAULT.builder().setHeader().build()
    val csvParser = new CSVParser(reader, customFormat)
    val records = csvParser.getRecords.asScala.toSeq
    val incomeMap = MutableMap[String, Map[String, Double]]()
    records.foreach { record =>
      val incomeCategory = record.get(0)
      val appliancesMap = MutableMap[String, Double]()
      val header = csvParser.getHeaderNames.asScala.drop(1)
      header.zipWithIndex.foreach { case (appliance, index) =>
        val value = record.get(index + 1).toDouble
        appliancesMap += (appliance -> value)
      }
      incomeMap += (incomeCategory -> appliancesMap.toMap)
    }
    reader.close()
    incomeMap
  }

  private def getDefaultReaderIncome: Reader = {
    logger.info("Markov Income parameters file 'by_income.csv' from jar.")
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/appliances/by_income.csv")
    )
  }

  // By Inhabitants

  def inhabitants(): MutableMap[String, Map[String, Double]] = {
    val reader = getDefaultReaderInhabitants
    val customFormat = CSVFormat.DEFAULT.builder().setHeader().build()
    val csvParser = new CSVParser(reader, customFormat)
    val records = csvParser.getRecords.asScala.toSeq
    val inhabitantsMap = MutableMap[String, Map[String, Double]]()
    records.foreach { record =>
      val inhabitantCategory = record.get(0)
      val appliancesMap = MutableMap[String, Double]()
      val header = csvParser.getHeaderNames.asScala.drop(1)
      header.zipWithIndex.foreach { case (appliance, index) =>
        val value = record.get(index + 1).toDouble
        appliancesMap += (appliance -> value)
      }
      inhabitantsMap += (inhabitantCategory -> appliancesMap.toMap)
    }
    reader.close()
    inhabitantsMap
  }

  private def getDefaultReaderInhabitants: Reader = {
    println("Reading by_inhabitants.csv file.")
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/appliances/by_inhabitants.csv")
    )
  }

  // Load_TS

  def load_TS(): mutable.Map[String, Seq[Int]] = {
    val reader = getDefaultReaderLoadTS
    val customFormat = CSVFormat.DEFAULT.builder().setHeader().build()
    val csvParser = new CSVParser(reader, customFormat)
    val records = csvParser.getRecords.asScala.toSeq
    val header = csvParser.getHeaderNames.asScala.toSeq
    val loadTS = mutable.Map[String, Seq[Int]]()
    for (record <- records) {
      for (i <- header.indices) {
        val applianceCategory = header(i)
        val value = record.get(i).toInt
        val existingValues = loadTS.getOrElse(applianceCategory, Seq())
        loadTS.put(applianceCategory, existingValues :+ value)
      }
    }
    reader.close()
    loadTS
  }

  def getDefaultReaderLoadTS: Reader = {
    logger.info("Markov Income parameters file 'load_ts.csv' from jar.")
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/appliances/load_ts.csv")
    )
  }
}
