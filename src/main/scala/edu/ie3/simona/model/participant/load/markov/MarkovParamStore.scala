package edu.ie3.simona.model.participant.load.markov


import java.io.{InputStreamReader, Reader}
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.csv.CSVFormat

import scala.collection.mutable.{Map => MutableMap}
import scala.jdk.CollectionConverters._

final case class MarkovParamStore() {

}

object MarkovParamStore extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val probabilitiesMap = Usage_Probabilities()
    println("Test Function: Usage_Probabilities:")
    probabilitiesMap.foreach { case (appliance, probability) =>
      println(s"$appliance -> $probability")
    }

    val averageHHMap = Average_HH()
    println("Test Function: Average:")
    averageHHMap.foreach { case (appliance, value) =>
      println(s"$appliance -> $value")
    }

    val FlatMap = Flat()
    println("Test Function: Flat:")
    FlatMap.foreach { case (appliance, value) =>
      println(s"$appliance -> $value")
    }

    val HouseMap = House()
    println("Test Function: House:")
    HouseMap.foreach { case (appliance, value) =>
      println(s"$appliance -> $value")
    }

    val incomeMap = income()
    println("Test Function: Income:")
    incomeMap.foreach { case (incomeCategory, appliancesMap) =>
      println(s"Income Category: $incomeCategory")
      appliancesMap.foreach { case (appliance, probability) =>
        println(s"  $appliance -> $probability")
      }
    }

    val inhabitantsMap = inhabitants()
    println("Test Function: Inhabitants:")
    inhabitantsMap.foreach { case (inhabitantsCategory, appliancesMap) =>
      println(s"inhabitants Category: $inhabitantsCategory")
      appliancesMap.foreach { case (appliance, probability) =>
        println(s"  $appliance -> $probability")
      }
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
  def Average_HH(): Map[String, Double] = {
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

  // By Flat // By House
  def Flat(): Map[String, Double] = {
    val reader = getDefaultReaderForFlat
    val csvParser = CSVFormat.DEFAULT
      .withDelimiter(';')
      .parse(reader)

    val records = csvParser.getRecords.asScala

    val FlatMap = records.headOption match {
      case Some(headerRecord) =>
        val applianceNames = headerRecord.iterator().asScala.toSeq
        val valuesRecord = records.drop(1).headOption.getOrElse(csvParser.iterator().next())
        val FlatValues = valuesRecord.iterator().asScala.map(_.toDouble)
        applianceNames.zip(FlatValues).toMap
      case None =>
        Map.empty[String, Double]
    }

    reader.close()
    FlatMap
  }

  def House(): Map[String, Double] = {
    val reader = getDefaultReaderForHouse
    val csvParser = CSVFormat.DEFAULT
      .withDelimiter(';')
      .parse(reader)

    val records = csvParser.getRecords.asScala

    val HouseMap = records.headOption match {
      case Some(headerRecord) =>
        val applianceNames = headerRecord.iterator().asScala.toSeq
        val valuesRecord = records.drop(1).headOption.getOrElse(csvParser.iterator().next())
        val HouseValues = valuesRecord.iterator().asScala.map(_.toDouble)
        applianceNames.zip(HouseValues).toMap
      case None =>
        Map.empty[String, Double]
    }

    reader.close()
    HouseMap
  }

  private def getDefaultReaderForFlat: Reader = {
    logger.info("Markov Flat parameters file 'flat.csv.csv' from jar.")
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/appliances/flat.csv")
    )
  }
  private def getDefaultReaderForHouse: Reader = {
    logger.info("Markov House parameters file 'house.csv.csv' from jar.")
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/appliances/house.csv")
    )
  }

  // By Income
  def income(): MutableMap[String, Map[String, Double]] = {
    val reader = getDefaultReaderIncome
    val csvParser = CSVFormat.DEFAULT.withDelimiter(';').withFirstRecordAsHeader().parse(reader)
    val records = csvParser.getRecords.asScala.toSeq

    val incomeMap = MutableMap[String, Map[String, Double]]()

    records.foreach { record =>
      val incomeCategory = record.get(0)
      val appliancesMap = MutableMap[String, Double]()

      for (i <- 1 until record.size()) {
        val appliance = csvParser.getHeaderNames.get(i)
        val value = record.get(i).toDouble
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
    val csvParser = CSVFormat.DEFAULT.withDelimiter(';').withFirstRecordAsHeader().parse(reader)
    val records = csvParser.getRecords.asScala.toSeq

    val inhabitantsMap = MutableMap[String, Map[String, Double]]()

    records.foreach { record =>
      val inhabitantCategory = record.get(0)
      val appliancesMap = MutableMap[String, Double]()

      for (i <- 1 until record.size()) {
        val appliance = csvParser.getHeaderNames.get(i)
        val value = record.get(i).toDouble
        appliancesMap += (appliance -> value)
      }

      inhabitantsMap += (inhabitantCategory -> appliancesMap.toMap)
    }
    reader.close()
    inhabitantsMap
  }

  private def getDefaultReaderInhabitants: Reader = {
    logger.info("Markov Inhabitants parameters file 'by_inhabitants.csv' from jar.")
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/appliances/by_inhabitants.csv")
    )
  }

  // Load TS

  def LoadTS(): List[Map[String, Map[Int, Double]]] = {
    val reader = getDefaultReaderLoad_TS
    val csvParser = CSVFormat.DEFAULT.withDelimiter(';').withFirstRecordAsHeader().parse(reader)
    val records = csvParser.getRecords.asScala.toList

    val loadTSList = records.map { record =>
      val loadTSMap = MutableMap[String, Map[Int, Double]]()



      loadTSMap.toMap
    }

    reader.close()
    loadTSList
  }

  private def getDefaultReaderLoad_TS: Reader = {
    logger.info("Markov Load_TS parameters file 'by_inhabitants.csv' from jar.")
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/appliances/load_ts.csv")
    )
  }


}


