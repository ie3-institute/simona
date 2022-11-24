/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.io

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.io.FlexSignalFromExcel.TimeSeriesType

import scala.util.{Failure, Success}

class FlexSignalFromExcelSpec extends UnitSpec {
  "Reading flexibility signal from file" should {
    val file = getClass.getResource("flexSignal.xlsx").getFile
    val resultTrial = FlexSignalFromExcel.flexSignals(file)

    "be successful" in {
      resultTrial.isSuccess shouldBe true
    }

    "provide the correct nodes" in {
      resultTrial match {
        case Success(result) =>
          result.keys should contain allOf ("node0", "node1")
        case Failure(exception) =>
          fail(
            "Reading flex signal from excel file failed with the following exception.",
            exception
          )
      }
    }

    "provide all expected time series with correct amount of entries" in {
      resultTrial match {
        case Success(result) =>
          result.foreach { case (_, tsTypeToTs) =>
            tsTypeToTs.keys should contain allElementsOf TimeSeriesType.values
            tsTypeToTs.values.foreach(timeSeries =>
              timeSeries.getEntries.size() shouldBe 8
            )
          }
        case Failure(exception) =>
          fail(
            "Reading flex signal from excel file failed with the following exception.",
            exception
          )
      }
    }
  }
}
