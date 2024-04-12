import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.csv.{CSVFormat, CSVRecord}

import java.io.{FileInputStream, InputStreamReader, Reader}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object MarkovParamStore extends LazyLogging {

  def main(args: Array[String]): Unit = {
    val filePath = "/load/markov/probabilities/usage_probabilities.csv"
    val data = readCSV(filePath)
    Test(data)
  }

  def readCSV(filePath: String): Map[String, Double] = {
    val reader: Reader = new InputStreamReader(new FileInputStream(filePath))
    val csvFormat: CSVFormat = CSVFormat.DEFAULT.withHeader().withDelimiter(';')
    val csvRecords: Iterable[CSVRecord] = csvFormat.parse(reader).asScala
    val dataMap = mutable.Map[String, Double]()

    for (record <- csvRecords) {
      val applianceCategory = record.get("appliance_category")
      val usageProbabilityStr = record.get("usage_probability")
      try {
        val usageProbability = usageProbabilityStr.toDouble
        dataMap.put(applianceCategory, usageProbability)
      } catch {
        case _: NumberFormatException => logger.warn(s"Invalid usage probability format for $applianceCategory: $usageProbabilityStr")
      }
    }
    dataMap.toMap
  }

  def Test(data: Map[String, Double]): Unit = {
    data.foreach { case (applianceCategory, usageProbability) =>
      println(s"Appliance Category: $applianceCategory, Usage Probability: $usageProbability")
    }
  }
}
