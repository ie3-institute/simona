package edu.ie3.simona.model.participant.load.markov


import java.io.{File, InputStreamReader, Reader}
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.csv.CSVFormat

import scala.collection.mutable
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

    val typeMap = Type()
    println("Test Function: Type:")
    typeMap.foreach { case (typeCategory, appliancesMap) =>
      println(s"type Category: $typeCategory")
      appliancesMap.foreach { case (appliance, probability) =>
        println(s"  $appliance -> $probability")
      }
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

    def printLoadTSData(loadTSData: mutable.Map[String, Seq[Int]]): Unit = {
      println("Gerätedaten:")
      loadTSData.foreach { case (appliance, values) =>
        println(s"$appliance: ${values.mkString(", ")}")
      }
    }

    val loadTSData = Load_TS()
    printLoadTSData(loadTSData)

    val dishWasher = SOP(getDefaultReaderForSOP("/load/markov/probabilities/switch_on_probabilities/dish_washer.csv"))
    println("Test Function: dish_washer")
    dishWasher.foreach { case (appliance, values) =>
      println(s"$appliance: ${values.mkString(", ")}")
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

  def SOP(reader: Reader): mutable.Map[String, Seq[Double]] = {
    val csvParser = CSVFormat.DEFAULT.parse(reader)
    val records = csvParser.getRecords.asScala.toSeq
    val header = csvParser.getHeaderNames.asScala.toSeq
    val switchonprob = mutable.Map[String, Seq[Double]]()

    for (record <- records) {
      for (i <- header.indices) {
        val applianceCategory = header(i)
        val value = record.get(i).toDouble
        val existingValues = switchonprob.getOrElse(applianceCategory, Seq())
        switchonprob.put(applianceCategory, existingValues :+ value)
      }
    }
    reader.close()
    switchonprob
  }

  def getDefaultReaderForSOP(filePath: String): Reader = {
    logger.info("Markov Average_HH parameters folder 'Switch on Probabilities' from jar.")
    new InputStreamReader(
      getClass.getResourceAsStream(filePath)
    )
  }


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

  // By_Type
  def Type(): MutableMap[String, Map[String, Double]] = {
    val reader = getDefaultReaderType
    val csvParser = CSVFormat.DEFAULT.withDelimiter(';').withFirstRecordAsHeader().parse(reader)
    val records = csvParser.getRecords.asScala.toSeq

    val TypeMap = MutableMap[String, Map[String, Double]]()

    records.foreach { record =>
      val TypeCategory = record.get(0)
      val appliancesMap = MutableMap[String, Double]()

      for (i <- 1 until record.size()) {
        val appliance = csvParser.getHeaderNames.get(i)
        val value = record.get(i).toDouble
        appliancesMap += (appliance -> value)
      }

      TypeMap += (TypeCategory -> appliancesMap.toMap)
    }
    reader.close()
    TypeMap

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

  //Load_TS
  def Load_TS(): mutable.Map[String, Seq[Int]] = {
    val reader = getDefaultReaderLoadTS
    val csvParser = CSVFormat.DEFAULT.withDelimiter(';').withFirstRecordAsHeader().parse(reader)
    val records = csvParser.getRecords.asScala.toSeq
    val header = csvParser.getHeaderNames.asScala.toSeq
    val load_TS = mutable.Map[String, Seq[Int]]()

    for (record <- records) {
      for (i <- header.indices) {
        val applianceCategory = header(i)
        val value = record.get(i).toInt
        val existingValues = load_TS.getOrElse(applianceCategory, Seq())
        load_TS.put(applianceCategory, existingValues :+ value)
      }
    }
    reader.close()
    load_TS
  }
    def getDefaultReaderLoadTS: Reader = {
    logger.info("Markov Income parameters file 'load_ts.csv' from jar.")
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/appliances/load_ts.csv")
    )
  }



}


