/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import akka.actor.ActorSystem
import akka.stream.scaladsl.FileIO
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.io.processor.result.ResultEntityProcessor
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.result.system.{PvResult, WecResult}
import edu.ie3.simona.exceptions.ProcessResultEventException
import edu.ie3.simona.test.common.{IOTestCommons, TestKitWithShutdown, UnitSpec}
import edu.ie3.util.TimeUtil
import edu.ie3.util.io.FileIOUtils
import tech.units.indriya.quantity.Quantities

import java.io.File
import java.nio.file.Paths
import java.nio.file.StandardOpenOption.{CREATE, TRUNCATE_EXISTING, WRITE}
import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.io.Source
import scala.language.postfixOps

class ResultEntityCsvSinkSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "ResultEntityCsvSinkSpec",
        ConfigFactory
          .parseString(
            """
            |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
            |akka.loglevel="DEBUG"
            |akka.coordinated-shutdown.phases.actor-system-terminate.timeout = 500s
          """.stripMargin
          )
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
        Await.result(
          ResultEntityCsvSink(
            outFileName,
            resultEntityProcessor,
            outFileName.endsWith(".gz")
          ),
          60 seconds
        )

      resultEntitySink.outfileName shouldBe outFileName
      resultEntitySink.delimiter shouldBe ","
      resultEntitySink.resultEntityProcessor shouldBe resultEntityProcessor

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
      ) shouldBe "uuid,input_model,p,q,time"

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
      Await.ready(
        akka.stream.scaladsl.Source
          .single(testText)
          .map(t => ByteString(t))
          .runWith(FileIO.toPath(path, Set(WRITE, TRUNCATE_EXISTING, CREATE))),
        5.seconds
      )

      val resultEntityProcessor = new ResultEntityProcessor(classOf[PvResult])

      ResultEntityCsvSink(
        outFileName,
        resultEntityProcessor,
        outFileName.endsWith(".gz")
      )

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
        UUID.fromString("7f404c4c-fc12-40de-95c9-b5827a40f18b"),
        TimeUtil.withDefaults.toZonedDateTime("2020-01-30 17:26:44"),
        UUID.fromString("e5ac84d3-c7a5-4870-a42d-837920aec9bb"),
        Quantities.getQuantity(10, StandardUnits.ACTIVE_POWER_IN),
        Quantities.getQuantity(10, StandardUnits.REACTIVE_POWER_IN)
      )

      val resultEntitySink =
        Await.result(
          ResultEntityCsvSink(
            outFileName,
            resultEntityProcessor,
            outFileName.endsWith(".gz")
          ),
          60 seconds
        )

      resultEntitySink.handleResultEntity(dummyPvResult)

      // close sink to ensure that everything is written out
      resultEntitySink.close()

      val outputFile = new File(outFileName)

      val resultFileSource = Source.fromFile(outputFile)

      val resultFileLines = resultFileSource.getLines().toVector
      resultFileLines.size shouldBe 2
      resultFileLines.tail.headOption.getOrElse(
        fail(
          "Cannot get line that should have been written out by the listener!"
        )
      ) shouldBe "7f404c4c-fc12-40de-95c9-b5827a40f18b,e5ac84d3-c7a5-4870-a42d-837920aec9bb,0.01,0.01,2020-01-30T17:26:44Z[UTC]"

      resultFileSource.close()

      // cleanup
      FileIOUtils.deleteRecursively(testTmpDir)

    }

    "should return a failed future if an invalid ResultEntity is provided" in {

      createDir(testTmpDir)
      val outFileName = testTmpDir + File.separator + "test.tmp"
      val resultEntityProcessor = new ResultEntityProcessor(classOf[PvResult])

      val dummyWecResult = new WecResult(
        UUID.fromString("7f404c4c-fc12-40de-95c9-b5827a40f18b"),
        TimeUtil.withDefaults.toZonedDateTime("2020-01-30 17:26:44"),
        UUID.fromString("e5ac84d3-c7a5-4870-a42d-837920aec9bb"),
        Quantities.getQuantity(10, StandardUnits.ACTIVE_POWER_IN),
        Quantities.getQuantity(10, StandardUnits.REACTIVE_POWER_IN)
      )

      val resultEntitySink =
        Await.result(
          ResultEntityCsvSink(
            outFileName,
            resultEntityProcessor,
            outFileName.endsWith(".gz"),
            bufferSize = 0
          ),
          60 seconds
        )

      val exception = intercept[ProcessResultEventException] {
        resultEntitySink.handleResultEntity(dummyWecResult)
      }

      exception.getMessage shouldBe "edu.ie3.simona.exceptions.FileIOException: Result entity WecResult is not part of model sink!"

      // cleanup
      FileIOUtils.deleteRecursively(testTmpDir)

    }

  }

}
