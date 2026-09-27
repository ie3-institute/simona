/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import edu.ie3.simona.model.grid.ampacity.LineStateResult
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.testkit.TestKit.awaitCond
import squants.thermal.Celsius

import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, DurationInt}
import scala.jdk.CollectionConverters.*

//FIXME DF temporary till included in PSDM
class AmpacityResultWriterSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with DefaultTestData {

  private val outFile =
    "line_segment_res.csv"

  private val header =
    "time,lineSegmentUuid,lineSegmentTemperature_C,groundTemperature_C"

  private val lineSegmentUuid =
    UUID.fromString("9d62d1dd-a5a2-41e0-aaaa-dfd44365224f")

  private var tempDir: Option[Path] = None

  override protected def afterAll(): Unit = {
    tempDir.foreach { dir =>
      Files
        .walk(dir)
        .iterator()
        .asScala
        .toList
        .reverse
        .foreach(Files.deleteIfExists)
    }
    super.afterAll()
  }

  private def spawnWriter(): ActorRef[AmpacityResultWriter.Message] = {
    val dir = Files.createTempDirectory("ampacityResultWriterSpec")
    tempDir = Some(dir)
    spawn(AmpacityResultWriter(dir))
  }

  private def readLines(): List[String] =
    Files
      .readAllLines(resultFile())
      .iterator()
      .asScala
      .toList

  private def resultFile(): Path =
    tempDir
      .getOrElse(
        throw new IllegalStateException(
          "Temp dir was not initialized by spawnWriter."
        )
      )
      .resolve("rawOutputData")
      .resolve(outFile)

  private def awaitCondition(cond: => Boolean): Unit = {
    if !awaitCond(
        p = cond,
        max = Duration(10, TimeUnit.SECONDS),
        interval = 100.millis,
        noThrow = false,
      )
    then fail("Condition was not met within the timeout.")
  }

  "The ampacity result writer" must {

    "write the header to the line segment result file" in {
      spawnWriter()

      awaitCondition(Files.exists(resultFile()))

      val lines = readLines()

      lines should have size 1
      lines.head should ===(header)
    }

    "write the line and ground temperature of every result" in {
      val writerRef = spawnWriter()

      val time = defaultSimulationStart
      val results = List(
        LineStateResult(
          time,
          lineSegmentUuid,
          Celsius(39d),
          Celsius(20d),
        ),
        LineStateResult(
          time,
          lineSegmentUuid,
          Celsius(41d),
          Celsius(21d),
        ),
      )

      writerRef ! AmpacityResultWriter.WriteLineTemps(results)
      awaitCondition(Files.exists(resultFile()) && readLines().size == 3)

      val lines = readLines()

      lines should have size 3
      lines.head should ===(header)
      lines(1) should ===(
        s"$time,$lineSegmentUuid,${Celsius(39d)},${Celsius(20d)}"
      )
      lines(2) should ===(
        s"$time,$lineSegmentUuid,${Celsius(41d)},${Celsius(21d)}"
      )
    }

    "append the results to an existing file without writing a second header" in {
      val writerRef = spawnWriter()

      val results = List(
        LineStateResult(
          defaultSimulationStart,
          lineSegmentUuid,
          Celsius(39d),
          Celsius(20d),
        )
      )

      writerRef ! AmpacityResultWriter.WriteLineTemps(results)
      awaitCondition(Files.exists(resultFile()) && readLines().size == 2)

      val lines = readLines()

      lines should have size 2
      lines.head should ===(header)
      lines(1) should ===(
        s"${defaultSimulationStart},$lineSegmentUuid,${Celsius(39d)},${Celsius(20d)}"
      )
      lines.count(_ == header) should ===(1)
    }

  }
}
