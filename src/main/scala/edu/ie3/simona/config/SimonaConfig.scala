/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.{Config, ConfigRenderOptions}
import edu.ie3.simona.config.SimonaConfig._
import edu.ie3.util.TimeUtil
import pureconfig._
import pureconfig.error._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._

import java.nio.file.{Files, Path}
import java.time.ZonedDateTime
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.implicitConversions

case class SimonaConfig(
    simulationName: String = "simona",
    time: TimeConfig = TimeConfig(),
    input: InputConfig,
    output: OutputConfig,
    runtime: RuntimeConfig,
    powerflow: PowerFlowConfig,
    gridConfig: GridConfig,
    event: EventConfig = EventConfig(None),
    control: Option[ControlConfig] = None,
) {
  def render(options: ConfigRenderOptions): String =
    SimonaConfig.render(this, options)

}

object SimonaConfig {
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  def apply(filePath: Path): (SimonaConfig, Config) = {
    if (!Files.isReadable(filePath)) {
      throw new IllegalArgumentException(
        s"Config file at $filePath is not readable."
      )
    }
    apply(ConfigSource.file(filePath))
  }

  def apply(confSrc: ConfigObjectSource): (SimonaConfig, Config) = {

    implicit def extract[T](either: Either[ConfigReaderFailures, T]): T =
      either match {
        case Left(readerFailures) =>
          val detailedErrors = readerFailures.toList
            .map {
              case CannotParse(msg, origin) =>
                f"CannotParse => $msg, Origin: $origin \n"
              case _: CannotRead =>
                f"CannotRead => Can not read config source} \n"
              case ConvertFailure(reason, _, path) =>
                f"Convertfailure => Path: $path, Description: ${reason.description} \n"
              case ThrowableFailure(throwable, origin) =>
                f"ThrowableFailure => ${throwable.getMessage}, Origin: $origin \n"
              case failure =>
                f"Unknown failure type => ${failure.toString} \n"
            }
            .mkString("\n")
          throw new RuntimeException(
            s"Unable to load config due to following failures:\n$detailedErrors"
          )
        case Right(conf) => conf
      }

    implicit val reader: ConfigReader[SimonaConfig] = ConfigReader[SimonaConfig]

    val config: Config = confSrc.config()
    val simonaConfig: SimonaConfig = confSrc.at("simona").load[SimonaConfig]

    (simonaConfig, config)
  }

  def render(
      simonaConfig: SimonaConfig,
      options: ConfigRenderOptions,
  ): String = {
    implicit val writer: ConfigWriter[SimonaConfig] = ConfigWriter[SimonaConfig]

    ConfigWriter[SimonaConfig].to(simonaConfig).render(options)
  }

  case class TimeConfig(
      // TODO: remove  date time defaults ?
      startDateTime: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2011-05-01 00:00:00"),
      endDateTime: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2011-05-01 01:00:00"),
      // fixme? : stopOnFailedPowerFlow in PowerFlowConfig?
      stopOnFailedPowerFlow: Boolean = false,
      schedulerReadyCheckWindow: Option[Int] = None,
  )

  final case class PowerFlowConfig(
      maxSweepPowerDeviation: Double,
      sweepTimeOut: FiniteDuration = 30.second,
      newtonraphson: NewtonRaphsonConfig,
      resolution: FiniteDuration = 1.hour,
  )

  final case class NewtonRaphsonConfig(
      epsilon: Seq[Double],
      iterations: Int,
  )

  final case class GridConfig(
      refSystems: Option[Seq[RefSystemConfig]]
  )

  final case class RefSystemConfig(
      sNom: String,
      vNom: String,
      voltLvls: Option[Seq[VoltLvlConfig]],
      gridIds: Option[Seq[String]],
  )

  final case class VoltLvlConfig(
      id: String,
      vNom: String,
  )

  final case class EventConfig(
      listener: Option[Seq[EventListenerConfig]]
  )

  final case class EventListenerConfig(
      fullClassPath: String,
      eventsToProcess: Seq[String],
  )

}
