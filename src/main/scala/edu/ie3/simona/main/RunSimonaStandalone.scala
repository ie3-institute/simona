/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main

import edu.ie3.simona.config.{ArgsParser, ConfigFailFast, SimonaConfig}
import edu.ie3.simona.main.RunSimona._
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup
import edu.ie3.util.io.FileIOUtils
import org.apache.pekko.actor.typed.scaladsl.AskPattern._
import org.apache.pekko.actor.typed.{ActorSystem, Scheduler}
import org.apache.pekko.util.Timeout

import java.nio.file.Path
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.jdk.FutureConverters.CompletionStageOps
import scala.util.{Failure, Success}

/** Run a standalone simulation of simona
  *
  * @since 01.07.20
  */
object RunSimonaStandalone extends RunSimona[SimonaStandaloneSetup] {

  override implicit val timeout: Timeout = Timeout(12.hours)

  override def setup(args: Array[String]): SimonaStandaloneSetup = {
    // get the config and prepare it with the provided args
    val (arguments, parsedConfig) = ArgsParser.prepareConfig(args)

    // config fail fast check
    val simonaConfig = SimonaConfig(parsedConfig)
    ConfigFailFast.check(parsedConfig, simonaConfig)

    SimonaStandaloneSetup(
      parsedConfig,
      SimonaStandaloneSetup.buildResultFileHierarchy(parsedConfig),
      mainArgs = arguments.mainArgs,
    )
  }

  override def run(simonaSetup: SimonaStandaloneSetup): Boolean = {
    val simonaSim = ActorSystem(
      SimonaSim(simonaSetup),
      name = "Simona",
      config = simonaSetup.typeSafeConfig,
    )

    implicit val scheduler: Scheduler = simonaSim.scheduler

    // run the simulation
    val terminated = simonaSim.ask[SimonaEnded](ref => SimonaSim.Start(ref))

    Await.result(terminated, timeout.duration) match {
      case SimonaEnded(successful) =>
        simonaSim.terminate()

        val config = SimonaConfig(simonaSetup.typeSafeConfig).simona.output

        config.sink.csv.map(_.zipFiles).foreach { zipFiles =>
          if (zipFiles) {
            val rawOutputPath =
              Path.of(simonaSetup.resultFileHierarchy.rawOutputDataDir)

            rawOutputPath.toFile.listFiles().foreach { file =>
              val fileName = file.getName
              val archiveName = fileName.replace(".csv", "")
              val filePath = rawOutputPath.resolve(fileName)

              val compressFuture =
                FileIOUtils
                  .compressFile(filePath, rawOutputPath.resolve(archiveName))
                  .asScala
              compressFuture.onComplete {
                case Success(_) =>
                  FileIOUtils.deleteRecursively(filePath)
                case Failure(exception) =>
                  logger.error(
                    s"Compression of output file to '$archiveName' has failed. Keep raw data.",
                    exception,
                  )
              }
              Await.ready(compressFuture, 5.minutes)
            }
          }
        }

        successful
    }
  }

}
