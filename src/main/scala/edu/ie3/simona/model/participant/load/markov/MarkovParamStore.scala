package edu.ie3.simona.model.participant.load.markov


import java.io.{File, FileReader, InputStreamReader, Reader}
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.csv.{CSVFormat, CSVParser}

import scala.collection.mutable
import scala.collection.mutable.{Map => MutableMap}
import scala.jdk.CollectionConverters._
import scala.io.Source

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



    def printSOP(SOPData: mutable.Map[String, Seq[Double]]): Unit = {
      println("Gerätedaten:")
      SOPData.foreach { case (appliance, values) =>
        println(s"$appliance: ${values.mkString(", ")}")
      }
    }

   val SOPData = SOP(getDefaultReaderSOP("/load/markov/probabilities/switch_on_probabilities/dish_washer.csv"))
   printSOP(SOPData)



  }
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



  private def getDefaultReader: Reader = {
    logger.info(
      "Markov Usage_Probabilities parameters file 'usage_probability.csv' from jar."
    )
    new InputStreamReader(
      getClass.getResourceAsStream("/load/markov/probabilities/usage_probabilities/usage_probabilities.csv")
    )
  }

  // Switch On Probabilities

  def SOP(filePath: String): mutable.Map[String, Seq[Double]] = {
    val reader = getDefaultReaderSOP(filePath)
    val csvParser = CSVFormat.DEFAULT.withDelimiter(';').withFirstRecordAsHeader().parse(reader)
    val records = csvParser.getRecords.asScala.toSeq
    val header = csvParser.getHeaderNames.asScala.toSeq
    val sop = mutable.Map[String, Seq[Double]]()

    for (record <- records) {
      for (i <- header.indices) {
        val applianceCategory = header(i)
        val value = record.get(i).toDouble
        val existingValues = sop.getOrElse(applianceCategory, Seq())
        sop.put(applianceCategory, existingValues :+ value)
      }
    }
    reader.close()
    sop
  }

  import java.io.{File, FileReader, Reader}

  def getDefaultReaderSOP(filePath: String): Reader = {
    logger.info(s"Markov Income parameters file '$filePath' from jar.")
    val file = new File(getClass.getResource(filePath).toURI)
    new FileReader(file)
  }




  // Average HH
  def Average_HH(): Map[String, Double] = {
    val reader = getDefaultReaderForAverageHH
    val csvParser = CSVFormat.DEFAULT.parse(reader)
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

  //Load_TS
  def Load_TS(): mutable.Map[String, Seq[Int]] = {
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


