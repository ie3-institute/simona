/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.{Config, ConfigValue}
import edu.ie3.simona.config.SimonaConfig.{
  CongestionManagement,
  Control,
  GridConfig,
  Powerflow,
  Time,
  writer,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.util.TimeUtil
import pureconfig.*
import pureconfig.error.*
import pureconfig.generic.*
import pureconfig.generic.semiauto.deriveConvert

import java.time.ZonedDateTime
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.deriving.Mirror

final case class SimonaConfig(
    congestionManagement: CongestionManagement = CongestionManagement(),
    control: Option[Control] = None,
    gridConfig: GridConfig = GridConfig(),
    input: InputConfig,
    output: OutputConfig,
    powerflow: Option[Powerflow] = None,
    runtime: RuntimeConfig = RuntimeConfig(),
    simulationName: String,
    time: Time,
) derives ConfigConvert {

  /** Returns the values of this config.
    */
  def values: ConfigValue = writer.to(this)
}

object SimonaConfig {
  // pure config start
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  extension (c: ConfigConvert.type)
    inline def derived[A](using m: Mirror.Of[A]): ConfigConvert[A] =
      deriveConvert[A]

  /** Returns a writer for [[SimonaConfig]].
    */
  private def writer: ConfigWriter[SimonaConfig] = ConfigWriter[SimonaConfig]

  def apply(typeSafeConfig: Config): SimonaConfig =
    apply(ConfigSource.fromConfig(typeSafeConfig))

  def apply(confSrc: ConfigObjectSource): SimonaConfig =
    confSrc.at("simona").load[SimonaConfig].getOrThrow

  extension [C](result: ConfigReader.Result[C]) {
    def getOrThrow: C = result match {
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
  }

  // pure config end

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
      derives ConfigConvert

  final case class TransformerControlGroup(
      measurements: List[String] = List.empty,
      transformers: List[String] = List.empty,
      vMax: Double,
      vMin: Double,
  ) derives ConfigConvert

  final case class VoltLvlConfig(
      id: String,
      vNom: String,
  ) derives ConfigConvert

  final case class VoltageLimitsConfig(
      override val gridIds: Option[List[String]] = None,
      vMax: Double,
      vMin: Double,
      override val voltLvls: Option[List[VoltLvlConfig]] = None,
  ) extends GridConfigParams
      derives ConfigConvert

  final case class CongestionManagement(
      enableDetection: Boolean = false,
      enableTransformerTapChange: Boolean = false,
  ) derives ConfigConvert

  final case class Control(
      transformer: List[TransformerControlGroup] = List.empty
  ) derives ConfigConvert

  final case class GridConfig(
      refSystems: Option[List[RefSystemConfig]] = None,
      voltageLimits: Option[List[VoltageLimitsConfig]] = None,
  ) derives ConfigConvert

  final case class Powerflow(
      maxSweepPowerDeviation: Double = 1e-5,
      newtonraphson: Powerflow.Newtonraphson,
      resolution: FiniteDuration = 1.hours,
      stopOnFailure: Boolean = false,
  ) derives ConfigConvert

  object Powerflow {
    final case class Newtonraphson(
        epsilon: List[Double] = List.empty,
        iterations: Int = 50,
    ) derives ConfigConvert
  }

  final case class Time(
      endDateTime: String,
      schedulerReadyCheckWindow: Option[Int] = None,
      startDateTime: String,
  ) derives ConfigConvert {

    val simStartTime: ZonedDateTime =
      TimeUtil.withDefaults.toZonedDateTime(startDateTime)

    val simEndTime: ZonedDateTime =
      TimeUtil.withDefaults.toZonedDateTime(endDateTime)
  }
}
