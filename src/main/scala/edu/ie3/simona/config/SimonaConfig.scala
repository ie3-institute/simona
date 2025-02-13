/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.{Config, ConfigRenderOptions}
import pureconfig._
import pureconfig.error._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._

import java.time.Duration
import scala.language.implicitConversions
import scala.util.Try

final case class SimonaConfig(
    simona: SimonaConfig.Simona
) {
  def render(options: ConfigRenderOptions): String =
    SimonaConfig.render(this, options)
}

object SimonaConfig {
  // pure config start
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  // TODO: replace with finite duration
  implicit def durationConvert: ConfigConvert[Duration] =
    ConfigConvert.viaStringTry(
      str => Try(Duration.parse(("PT" + str).toUpperCase)),
      x => x.toString,
    )

  /** Method to extract a config from a [[pureconfig.ConfigReader.Result]]
    * @param either
    *   that may contain a config
    * @tparam T
    *   type of config
    * @return
    *   the config, or throws an exception
    */
  protected implicit def extract[T](
      either: Either[ConfigReaderFailures, T]
  ): T =
    either match {
      case Left(readerFailures) =>
        val detailedErrors = readerFailures.toList
          .map {
            case CannotParse(msg, origin) =>
              f"CannotParse => $msg, Origin: $origin \n"
            case _: CannotRead =>
              f"CannotRead => Can not read config source} \n"
            case ConvertFailure(reason, _, path) =>
              f"ConvertFailure => Path: $path, Description: ${reason.description} \n"
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

  def apply(typeSafeConfig: Config): SimonaConfig =
    apply(ConfigSource.fromConfig(typeSafeConfig))

  def apply(confSrc: ConfigObjectSource): SimonaConfig =
    confSrc.load[SimonaConfig]

  def render(
      simonaConfig: SimonaConfig,
      options: ConfigRenderOptions,
  ): String = ConfigWriter[SimonaConfig].to(simonaConfig).render(options)

  // pure config end

  final case class RefSystemConfig(
      gridIds: Option[List[String]] = None,
      sNom: String,
      vNom: String,
      voltLvls: Option[List[VoltLvlConfig]] = None,
  )

  final case class TransformerControlGroup(
      measurements: List[String] = List(),
      transformers: List[String] = List(),
      vMax: Double,
      vMin: Double,
  )

  final case class VoltLvlConfig(
      id: String,
      vNom: String,
  )

  final case class Simona(
      control: Option[Simona.Control] = None,
      event: Simona.Event = Simona.Event(),
      gridConfig: Simona.GridConfig = Simona.GridConfig(),
      input: InputConfig,
      output: OutputConfig,
      powerflow: Simona.Powerflow,
      runtime: RuntimeConfig,
      simulationName: String,
      time: Simona.Time = Simona.Time(),
  )
  object Simona {
    final case class Control(
        transformer: List[TransformerControlGroup] = List()
    )

    final case class Event(
        listener: Option[List[Event.Listener$Elm]] = None
    )
    object Event {
      final case class Listener$Elm(
          eventsToProcess: Option[List[String]] = None,
          fullClassPath: String,
      )
    }

    final case class GridConfig(
        refSystems: Option[List[RefSystemConfig]] = None
    )

    final case class Powerflow(
        maxSweepPowerDeviation: Double,
        newtonraphson: Powerflow.Newtonraphson,
        resolution: Duration = Duration.ofHours(1),
        stopOnFailure: Boolean = false,
        sweepTimeout: Duration = Duration.ofSeconds(30),
    )
    object Powerflow {
      final case class Newtonraphson(
          epsilon: List[Double] = List(),
          iterations: Int,
      )
    }

    final case class Time(
        endDateTime: String = "2011-05-01T01:00:00Z",
        schedulerReadyCheckWindow: Option[Int] = None,
        startDateTime: String = "2011-05-01T00:00:00Z",
    )
  }
}
