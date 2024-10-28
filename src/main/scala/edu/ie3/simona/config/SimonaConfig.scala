/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import SimonaConfig._

import scala.concurrent.duration.{Duration, DurationInt}
import pureconfig._
import pureconfig.error.{CannotParse, CannotRead, ConvertFailure, ThrowableFailure}
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._

import java.nio.file.{Files, Path}

case class SimonaConfig(
    simulationName: String = "simona",
    time: TimeConfig = TimeConfig(),
    input: InputConfig,
    output: OutputConfig,
    runtime: RuntimeConfig,
    powerflow: PowerFlowConfig,
    gridConfig: GridConfig,
    event: EventConfig = EventConfig(None)
)

  object SimonaConfig {
    implicit def productHint[T]: ProductHint[T] = ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

    def apply(filePath: Path): SimonaConfig = {
      if (!Files.isReadable(filePath)) {
        throw new IllegalArgumentException(s"Config file at $filePath is not readable.")
      }
      apply(ConfigSource.file(filePath))
    }

    def apply(confSrc: ConfigObjectSource): SimonaConfig = {
     confSrc.at("simona").load[SimonaConfig] match {
        case Left(readerFailures) =>
          val detailedErrors = readerFailures.toList.map {
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
          }.mkString("\n")
          throw new RuntimeException(s"Unable to load config due to following failures:\n$detailedErrors")
        case Right(conf) => conf
      }
    }

  case class TimeConfig(
      // TODO: remove  date time defaults ?
      startDateTime: String = "2011-05-01 00:00:00",
      endDateTime: String = "2011-05-01 01:00:00",
      stopOnFailedPowerFlow: Boolean = false,
      schedulerReadyCheckWindow: Option[Int] = None,
  )

  final case class PowerFlowConfig(
      maxSweepPowerDeviation: Double,
      sweepTimeOut: Duration = 30.second,
      newtonraphson: NewtonRaphsonConfig,
      resolution: Duration = 1.hour,
  )

  final case class NewtonRaphsonConfig(
      epsilon: Seq[Double],
      iterations: Int,
  )

  final case class GridConfig(
      refSystems: Seq[RefSystemConfig]
  )

  final case class RefSystemConfig(
      sNom: String,
      vNom: String,
      voltLvls: Option[Seq[VoltLvlConfig]],
      gridIds: Option[Seq[String]],
  )

  final case class SimpleOutputConfig(
      override val notifier: java.lang.String,
      override val simulationResult: scala.Boolean,
  ) extends BaseOutputConfig(notifier, simulationResult)
  object SimpleOutputConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.SimpleOutputConfig = {
      SimonaConfig.SimpleOutputConfig(
        notifier = $_reqStr(parentPath, c, "notifier", $tsCfgValidator),
        simulationResult =
          $_reqBln(parentPath, c, "simulationResult", $tsCfgValidator),
      )
    }
    private def $_reqBln(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): scala.Boolean = {
      if (c == null) false
      else
        try c.getBoolean(path)
        catch {
          case e: com.typesafe.config.ConfigException =>
            $tsCfgValidator.addBadPath(parentPath + path, e)
            false
        }
    }

    private def $_reqStr(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): java.lang.String = {
      if (c == null) null
      else
        try c.getString(path)
        catch {
          case e: com.typesafe.config.ConfigException =>
            $tsCfgValidator.addBadPath(parentPath + path, e)
            null
        }
    }

  }

  final case class StorageRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: scala.Boolean,
      override val scaling: scala.Double,
      override val uuids: scala.List[java.lang.String],
      initialSoc: scala.Double,
      targetSoc: scala.Option[scala.Double],
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )
  object StorageRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.StorageRuntimeConfig = {
      SimonaConfig.StorageRuntimeConfig(
        initialSoc =
          if (c.hasPathOrNull("initialSoc")) c.getDouble("initialSoc") else 0,
        targetSoc =
          if (c.hasPathOrNull("targetSoc")) Some(c.getDouble("targetSoc"))
          else None,
        calculateMissingReactivePowerWithModel = $_reqBln(
          parentPath,
          c,
          "calculateMissingReactivePowerWithModel",
          $tsCfgValidator,
        ),
        scaling = $_reqDbl(parentPath, c, "scaling", $tsCfgValidator),
        uuids = $_L$_str(c.getList("uuids"), parentPath, $tsCfgValidator),
      )
    }
    private def $_reqBln(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): scala.Boolean = {
      if (c == null) false
      else
        try c.getBoolean(path)
        catch {
          case e: com.typesafe.config.ConfigException =>
            $tsCfgValidator.addBadPath(parentPath + path, e)
            false
        }
    }

    private def $_reqDbl(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): scala.Double = {
      if (c == null) 0
      else
        try c.getDouble(path)
        catch {
          case e: com.typesafe.config.ConfigException =>
            $tsCfgValidator.addBadPath(parentPath + path, e)
            0
        }
    }

  }

  final case class TransformerControlGroup(
      measurements: scala.List[java.lang.String],
      transformers: scala.List[java.lang.String],
      vMax: scala.Double,
      vMin: scala.Double,
  )
  object TransformerControlGroup {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.TransformerControlGroup = {
      SimonaConfig.TransformerControlGroup(
        measurements =
          $_L$_str(c.getList("measurements"), parentPath, $tsCfgValidator),
        transformers =
          $_L$_str(c.getList("transformers"), parentPath, $tsCfgValidator),
        vMax = $_reqDbl(parentPath, c, "vMax", $tsCfgValidator),
        vMin = $_reqDbl(parentPath, c, "vMin", $tsCfgValidator),
      )
    }
    private def $_reqDbl(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): scala.Double = {
      if (c == null) 0
      else
        try c.getDouble(path)
        catch {
          case e: com.typesafe.config.ConfigException =>
            $tsCfgValidator.addBadPath(parentPath + path, e)
            0
        }
    }

  }

  final case class VoltLvlConfig(
      id: String,
      vNom: String,
  )

  final case class EventConfig(
      listener: Option[Seq[EventListenerConfig]]
  )

  final case class EventListenerConfig(
      fullClassPath: String,
      eventsToProcess: Seq[String]
  )
}
