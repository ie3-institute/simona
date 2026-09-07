/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import edu.ie3.simona.model.grid.ampacity.LineStateResult
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}
import squants.thermal.Celsius
import squants.Temperature

object AmpacityResultWriter {

  sealed trait Message
  final case class WriteLineTemps(results: Iterable[LineStateResult])
      extends Message

  def apply(runOutputDir: Path): Behavior[Message] = Behaviors.setup { ctx =>
    val rawOutputDir = runOutputDir.resolve("rawOutputData")
    Try(Files.createDirectories(rawOutputDir)).failed.foreach(ex =>
      ctx.log.warn("Unable to create rawOutputData dir", ex)
    )
    val outFile = rawOutputDir.resolve("line_segment_res.csv")

    // ensure header exists
    if !Files.exists(outFile) then {
      Try {
        val bw = new BufferedWriter(new FileWriter(outFile.toFile, true))
        try {
          bw.write("time,lineUuid,lineSegmentUuid,lineSegmentTemperature_C")
          bw.newLine()
        } finally bw.close()
      } match {
        case Failure(ex) =>
          ctx.log.error("Unable to create ampacity result file header", ex)
        case Success(_) => // ok
      }
    }

    Behaviors.receiveMessagePartial[Message] { case WriteLineTemps(results) =>
      Try {
        val bw = new BufferedWriter(new FileWriter(outFile.toFile, true))
        try {
          results.foreach { res =>
            val tempC =
              Try(res.lineSegmentTemperature.inCelsius).getOrElse(Double.NaN)
            bw.write(
              s"${res.time},${res.lineUuid},${res.lineSegmentUuid},${tempC}"
            )
            bw.newLine()
          }
        } finally bw.close()
      } match {
        case Failure(ex) =>
          ctx.log.error("Error while writing ampacity results", ex)
        case Success(_) => // ok
      }

      Behaviors.same
    }
  }
}
