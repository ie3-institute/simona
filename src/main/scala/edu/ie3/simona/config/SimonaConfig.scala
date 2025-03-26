/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.Config
import edu.ie3.simona.exceptions.CriticalFailureException
import pureconfig._
import pureconfig.error._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.implicitConversions

final case class SimonaConfig(
    simona: SimonaConfig.Simona
)

object SimonaConfig {
  // pure config start
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  def apply(typeSafeConfig: Config): SimonaConfig =
    apply(ConfigSource.fromConfig(typeSafeConfig))

  def apply(confSrc: ConfigObjectSource): SimonaConfig =
    confSrc.load[SimonaConfig] match {
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
        throw new CriticalFailureException(
          s"Unable to load config due to following failures:\n$detailedErrors"
        )
      case Right(conf) => conf
    }

  // pure config end

  /** Case class contains default and individual configs for assets.
    * @param defaultConfig
    *   to use
    * @param individualConfigs
    *   specific configs, that are used instead of the [[defaultConfig]]
    * @tparam T
    *   type of asset config
    */
  final case class AssetConfigs[T](
      defaultConfig: T,
      individualConfigs: List[T] = List.empty,
  )

  sealed trait GridConfigParams {
    val gridIds: Option[List[String]]
    val voltLvls: Option[List[VoltLvlConfig]]
  }

  final case class RefSystemConfig(
      override val gridIds: Option[List[String]] = None,
      sNom: String,
      vNom: String,
      override val voltLvls: Option[List[VoltLvlConfig]] = None,
  ) extends GridConfigParams

  final case class TransformerControlGroup(
      measurements: List[String] = List.empty,
      transformers: List[String] = List.empty,
      vMax: Double,
      vMin: Double,
  )

  final case class VoltLvlConfig(
      id: String,
      vNom: String,
  )

  final case class VoltageLimitsConfig(
      override val gridIds: Option[List[String]] = None,
      vMax: Double,
      vMin: Double,
      override val voltLvls: Option[List[VoltLvlConfig]] = None,
  ) extends GridConfigParams

  final case class Simona(
      congestionManagement: Simona.CongestionManagement =
        Simona.CongestionManagement(),
      control: Option[Simona.Control] = None,
      gridConfig: Simona.GridConfig = Simona.GridConfig(),
      input: InputConfig,
      output: OutputConfig,
      powerflow: Simona.Powerflow,
      runtime: RuntimeConfig,
      simulationName: String,
      time: Simona.Time = Simona.Time(),
  )
  object Simona {
    final case class CongestionManagement(
        enableDetection: Boolean = false,
        timeout: FiniteDuration = 30.seconds,
    )

    final case class Control(
        transformer: List[TransformerControlGroup] = List.empty
    )

    final case class GridConfig(
        refSystems: Option[List[RefSystemConfig]] = None,
        voltageLimits: Option[List[VoltageLimitsConfig]] = None,
    )

    final case class Powerflow(
        maxSweepPowerDeviation: Double,
        newtonraphson: Powerflow.Newtonraphson,
        resolution: FiniteDuration = 1.hours,
        stopOnFailure: Boolean = false,
        sweepTimeout: FiniteDuration = 30.seconds,
    )
    object Powerflow {
      final case class Newtonraphson(
          epsilon: List[Double] = List.empty,
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
