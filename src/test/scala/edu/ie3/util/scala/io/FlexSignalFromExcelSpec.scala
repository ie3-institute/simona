/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.io

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.io.FlexSignalFromExcel.TimeSeriesType
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.{Failure, Success}

class FlexSignalFromExcelSpec extends UnitSpec with TableDrivenPropertyChecks {
  "Reading flexibility signal from file" should {
    val file = getClass.getResource("flexSignal.xlsx").getFile

    val nodeIds = Seq("node0", "node1")
    val combinations = nodeIds.flatMap { nodeId =>
      TimeSeriesType.values.map { seriesType => (nodeId, seriesType) }
    }

    val cases = Table(
      ("nodeId", "timeSeriesType"),
      combinations: _*
    )

    "provide all expected time series with correct amount of entries" in {

      forAll(cases) { case (nodeId, timeSeriesType) =>
        val resultTrial =
          FlexSignalFromExcel.flexSignals(file, nodeId, timeSeriesType)

        resultTrial match {
          case Success(timeSeries) =>
            timeSeries.getEntries.size() shouldBe 8
          case Failure(exception) =>
            fail(
              "Reading flex signal from excel file failed with the following exception.",
              exception,
            )
        }
      }
    }

  }
}
