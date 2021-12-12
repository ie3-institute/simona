/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.io.result.ResultSinkType.{Csv, InfluxDb1x}
import edu.ie3.simona.test.common.UnitSpec

class ResultSinkTypeSpec extends UnitSpec {
  "A ResultSinkType" should {
    "be instantiated correctly when supplying a csv sink" in {
      val conf = SimonaConfig.Simona.Output.Sink(
        csv = Some(
          SimonaConfig.Simona.Output.Sink.Csv(
            fileFormat = ".csv",
            filePrefix = "",
            fileSuffix = ""
          )
        ),
        influxDb1x = None
      )

      inside(ResultSinkType(conf, "testRun")) {
        case Csv(fileFormat, filePrefix, fileSuffix) =>
          fileFormat shouldBe conf.csv.value.fileFormat
          filePrefix shouldBe conf.csv.value.filePrefix
          fileSuffix shouldBe conf.csv.value.fileSuffix
        case _ =>
          fail("Wrong ResultSinkType got instantiated.")
      }
    }

    "be instantiated correctly when supplying an influxDB sink" in {
      val conf = SimonaConfig.Simona.Output.Sink(
        csv = None,
        influxDb1x = Some(
          SimonaConfig.Simona.Output.Sink.InfluxDb1x(
            database = "test",
            port = 1,
            url = "localhost/"
          )
        )
      )
      val runName = "testRun"

      inside(ResultSinkType(conf, runName)) {
        case InfluxDb1x(url, database, scenario) =>
          url shouldBe "localhost:1"
          database shouldBe conf.influxDb1x.value.database
          scenario shouldBe runName
        case _ =>
          fail("Wrong ResultSinkType got instantiated.")
      }
    }

    "fail when more than one sink is supplied" in {
      val conf = SimonaConfig.Simona.Output.Sink(
        csv = Some(
          SimonaConfig.Simona.Output.Sink.Csv(
            fileFormat = ".csv",
            filePrefix = "",
            fileSuffix = ""
          )
        ),
        influxDb1x = Some(
          SimonaConfig.Simona.Output.Sink.InfluxDb1x(
            database = "test",
            port = 1,
            url = "localhost"
          )
        )
      )

      assertThrows[IllegalArgumentException](ResultSinkType(conf, "testRun"))
    }

    "fail when no sink is supplied" in {
      val conf = SimonaConfig.Simona.Output.Sink(
        csv = None,
        influxDb1x = None
      )

      assertThrows[IllegalArgumentException](ResultSinkType(conf, "testRun"))
    }
  }
}
