/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import edu.ie3.simona.config.OutputConfig
import edu.ie3.simona.config.ConfigParams.{
  BaseInfluxDb1xParams,
  PsdmSinkCsvParams,
  ResultKafkaParams,
}
import edu.ie3.simona.io.result.ResultSinkType.{Csv, InfluxDb1x, Kafka}
import edu.ie3.simona.test.common.UnitSpec

import java.util.UUID

class ResultSinkTypeSpec extends UnitSpec {
  "A ResultSinkType" should {
    "be instantiated correctly when supplying a csv sink" in {
      val conf = OutputConfig.Sink(
        csv = Some(
          PsdmSinkCsvParams(
            fileFormat = ".csv",
            filePrefix = "",
            fileSuffix = "",
            isHierarchic = false,
            compressOutputs = false,
          )
        ),
        influxDb1x = None,
        kafka = None,
      )

      inside(ResultSinkType(conf, "testRun")) {
        case Csv(fileFormat, filePrefix, fileSuffix, zipFiles) =>
          fileFormat shouldBe conf.csv.value.fileFormat
          filePrefix shouldBe conf.csv.value.filePrefix
          fileSuffix shouldBe conf.csv.value.fileSuffix
          zipFiles shouldBe conf.csv.value.compressOutputs
        case _ =>
          fail("Wrong ResultSinkType got instantiated.")
      }
    }

    "be instantiated correctly when supplying an influxDB sink" in {
      val conf = OutputConfig.Sink(
        csv = None,
        influxDb1x = Some(
          BaseInfluxDb1xParams(
            database = "test",
            port = 1,
            url = "localhost/",
          )
        ),
        kafka = None,
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

    "be instantiated correctly when supplying a kafka sink" in {
      val conf = OutputConfig.Sink(
        csv = None,
        influxDb1x = None,
        kafka = Some(
          ResultKafkaParams(
            "localhost:9092",
            12,
            "00000000-0000-0000-0000-000000000000",
            "https://reg:123",
            "topic",
          )
        ),
      )
      val runName = "testRun"

      inside(ResultSinkType(conf, runName)) {
        case Kafka(
              topicNodeRes,
              runId,
              bootstrapServers,
              schemaRegistryUrl,
              linger,
            ) =>
          topicNodeRes shouldBe "topic"
          runId shouldBe UUID.fromString("00000000-0000-0000-0000-000000000000")
          bootstrapServers shouldBe "localhost:9092"
          schemaRegistryUrl shouldBe "https://reg:123"
          linger shouldBe 12
        case _ =>
          fail("Wrong ResultSinkType got instantiated.")
      }
    }

    "fail when more than one sink is supplied" in {
      val conf = OutputConfig.Sink(
        csv = Some(
          PsdmSinkCsvParams(
            fileFormat = ".csv",
            filePrefix = "",
            fileSuffix = "",
            isHierarchic = false,
            compressOutputs = false,
          )
        ),
        influxDb1x = Some(
          BaseInfluxDb1xParams(
            database = "test",
            port = 1,
            url = "localhost",
          )
        ),
        kafka = None,
      )

      assertThrows[IllegalArgumentException](ResultSinkType(conf, "testRun"))
    }

    "fail when no sink is supplied" in {
      val conf = OutputConfig.Sink(
        csv = None,
        influxDb1x = None,
        kafka = None,
      )

      assertThrows[IllegalArgumentException](ResultSinkType(conf, "testRun"))
    }
  }
}
