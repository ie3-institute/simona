/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.exceptions.EntityProcessorException
import edu.ie3.datamodel.io.processor.result.ResultEntityProcessor
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.result.system.{PvResult, WecResult}
import edu.ie3.simona.exceptions.ProcessResultEventException
import edu.ie3.simona.test.common.{IOTestCommons, TestKitWithShutdown, UnitSpec}
import edu.ie3.util.TimeUtil
import edu.ie3.util.io.FileIOUtils
import org.apache.pekko.actor.ActorSystem
import tech.units.indriya.quantity.Quantities

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.{CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Files, Paths}
import java.util.UUID
import scala.io.Source
import scala.language.postfixOps

class ResultEntityCsvSinkSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "ResultEntityCsvSinkSpec",
        ConfigFactory
          .parseString(
            """
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="DEBUG"
            |pekko.coordinated-shutdown.phases.actor-system-terminate.timeout = 500s
          """.stripMargin
          ),
      )
    )
    with UnitSpec
    with IOTestCommons {

  "A ResultEntityCsvSink" should {

    "be initialized correctly and write headline to file when called with valid constructor parameters" in {

      createDir(testTmpDir)
      val outFileName = testTmpDir + File.separator + "test.tmp"
      val resultEntityProcessor = new ResultEntityProcessor(classOf[PvResult])

      val resultEntitySink =
        ResultEntityCsvSink(
          outFileName,
          resultEntityProcessor,
          outFileName.endsWith(".gz"),
        )

      resultEntitySink.outfileName shouldBe outFileName
      resultEntitySink.delimiter shouldBe ","
      resultEntitySink.resultEntityProcessor shouldBe resultEntityProcessor
      resultEntitySink.close()

      val outputFile = new File(outFileName)

      assert(outputFile.exists)
      assert(outputFile.isFile)

      val resultFileSource = Source.fromFile(outputFile)

      val resultFileLines = resultFileSource.getLines().toVector

      resultFileLines.size shouldBe 1
      resultFileLines.headOption.getOrElse(
        fail(
          "Cannot get line that should have been written out by sink!"
        )
      ) shouldBe "input_model,p,q,time"

      // close sink to ensure that everything is written out
      resultFileSource.close()

      // cleanup
      FileIOUtils.deleteRecursively(testTmpDir)

    }

    "be initialized correctly and not overwrite the output file if that file already exists" in {

      createDir(testTmpDir)
      val outFileName = testTmpDir + File.separator + "test.tmp"
      val testText = "- this is a test text -"

      val path = Paths.get(outFileName)
      // create output file (should not exist yet at this point)
      Files.write(
        path,
        testText.getBytes(StandardCharsets.UTF_8),
        WRITE,
        TRUNCATE_EXISTING,
        CREATE,
      )

      val resultEntityProcessor = new ResultEntityProcessor(classOf[PvResult])

      val resultEntitySink = ResultEntityCsvSink(
        outFileName,
        resultEntityProcessor,
        outFileName.endsWith(".gz"),
      )

      // close sink to ensure that everything is written out
      resultEntitySink.close()

      val outputFile = new File(outFileName)

      assert(outputFile.exists)
      assert(outputFile.isFile)

      val resultFileSource = Source.fromFile(outputFile)

      val resultFileLines = resultFileSource.getLines().toVector

      // ResultEntityCsvSink should not have overwritten file
      resultFileLines.size shouldBe 1
      resultFileLines.headOption.getOrElse(
        fail(
          "Cannot get line that should have been written out by this test!"
        )
      ) shouldBe testText

      resultFileSource.close()

      // cleanup
      FileIOUtils.deleteRecursively(testTmpDir)

    }

    "write a given valid ResultEntity correctly" in {

      createDir(testTmpDir)
      val outFileName = testTmpDir + File.separator + "test.tmp"
      val resultEntityProcessor = new ResultEntityProcessor(classOf[PvResult])

      val dummyPvResult = new PvResult(
        TimeUtil.withDefaults.toZonedDateTime("2020-01-30T17:26:44Z"),
        UUID.fromString("e5ac84d3-c7a5-4870-a42d-837920aec9bb"),
        Quantities.getQuantity(10, StandardUnits.ACTIVE_POWER_IN),
        Quantities.getQuantity(10, StandardUnits.REACTIVE_POWER_IN),
      )

      val resultEntitySink =
        ResultEntityCsvSink(
          outFileName,
          resultEntityProcessor,
          outFileName.endsWith(".gz"),
        )

      resultEntitySink.handleResultEntity(dummyPvResult)

      // close sink to ensure that everything is written out
      resultEntitySink.close()

      val outputFile = new File(outFileName)

      val resultFileSource = Source.fromFile(outputFile)

      val resultFileLines = resultFileSource.getLines().toVector
      resultFileLines.size shouldBe 2
      resultFileLines.lastOption.getOrElse(
        fail(
          "Cannot get line that should have been written out by the listener!"
        )
      ) shouldBe "e5ac84d3-c7a5-4870-a42d-837920aec9bb,0.01,0.01,2020-01-30T17:26:44Z"

      resultFileSource.close()

      // cleanup
      FileIOUtils.deleteRecursively(testTmpDir)

    }

    "should return a failed future if an invalid ResultEntity is provided" in {

      createDir(testTmpDir)
      val outFileName = testTmpDir + File.separator + "test.tmp"
      val resultEntityProcessor = new ResultEntityProcessor(classOf[PvResult])

      val dummyWecResult = new WecResult(
        TimeUtil.withDefaults.toZonedDateTime("2020-01-30T17:26:44Z"),
        UUID.fromString("e5ac84d3-c7a5-4870-a42d-837920aec9bb"),
        Quantities.getQuantity(10, StandardUnits.ACTIVE_POWER_IN),
        Quantities.getQuantity(10, StandardUnits.REACTIVE_POWER_IN),
      )

      val resultEntitySink =
        ResultEntityCsvSink(
          outFileName,
          resultEntityProcessor,
          outFileName.endsWith(".gz"),
        )

      val exception = intercept[ProcessResultEventException] {
        resultEntitySink.handleResultEntity(dummyWecResult)
      }
      // close sink to ensure that everything is written out
      resultEntitySink.close()

      exception.getMessage shouldBe "Processing result failed"
      exception.getCause.getClass shouldBe classOf[EntityProcessorException]

      // cleanup
      FileIOUtils.deleteRecursively(testTmpDir)

    }

  }

}
