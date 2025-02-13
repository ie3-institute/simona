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
import pureconfig.generic.semiauto.{deriveReader, deriveWriter}

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

  // necessary to prevent StackOverFlowErrors during compilation
  private type RW[T] = (ConfigReader[T], ConfigWriter[T])

  implicit val baseRuntime: RW[BaseRuntimeConfig] = (deriveReader, deriveWriter)
  implicit val loadRuntime: RW[LoadRuntimeConfig] = (deriveReader, deriveWriter)
  implicit val ffiRuntime: RW[FixedFeedInRuntimeConfig] =
    (deriveReader, deriveWriter)
  implicit val pvRuntime: RW[PvRuntimeConfig] = (deriveReader, deriveWriter)
  implicit val wecRuntime: RW[WecRuntimeConfig] = (deriveReader, deriveWriter)
  implicit val evcsRuntime: RW[EvcsRuntimeConfig] = (deriveReader, deriveWriter)
  implicit val emRuntime: RW[EmRuntimeConfig] = (deriveReader, deriveWriter)
  implicit val storageRuntime: RW[StorageRuntimeConfig] =
    (deriveReader, deriveWriter)
  implicit val hpRuntime: RW[HpRuntimeConfig] = (deriveReader, deriveWriter)
  implicit val baseCsv: RW[BaseCsvParams] = (deriveReader, deriveWriter)
  implicit val partBaseOutput: RW[ParticipantBaseOutputConfig] =
    (deriveReader, deriveWriter)
  implicit val primaryDataCsv: RW[PrimaryDataCsvParams] =
    (deriveReader, deriveWriter)
  implicit val resultKafka: RW[ResultKafkaParams] = (deriveReader, deriveWriter)
  implicit val runtimeKafka: RW[RuntimeKafkaParams] =
    (deriveReader, deriveWriter)
  implicit val simpleOutput: RW[SimpleOutputConfig] =
    (deriveReader, deriveWriter)

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

  final case class BaseCsvParams(
      override val csvSep: String,
      override val directoryPath: String,
      override val isHierarchic: Boolean,
  ) extends CsvParams(csvSep, directoryPath, isHierarchic)
  object BaseCsvParams {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.BaseCsvParams = {
      SimonaConfig.BaseCsvParams(
        csvSep = $_reqStr(parentPath, c, "csvSep", $tsCfgValidator),
        directoryPath =
          $_reqStr(parentPath, c, "directoryPath", $tsCfgValidator),
        isHierarchic = $_reqBln(parentPath, c, "isHierarchic", $tsCfgValidator),
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

  sealed abstract class BaseOutputConfig(
      val notifier: String,
      val simulationResult: Boolean,
  )

  sealed abstract class BaseRuntimeConfig(
      val calculateMissingReactivePowerWithModel: Boolean,
      val scaling: Double,
      val uuids: List[String],
  ) extends Serializable

  sealed abstract class CsvParams(
      val csvSep: String,
      val directoryPath: String,
      val isHierarchic: Boolean,
  )

  final case class EmRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
      aggregateFlex: String = "SELF_OPT_EXCL_REG",
      curtailRegenerative: Boolean = false,
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )
  object EmRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.EmRuntimeConfig = {
      SimonaConfig.EmRuntimeConfig(
        aggregateFlex =
          if (c.hasPathOrNull("aggregateFlex")) c.getString("aggregateFlex")
          else "SELF_OPT_EXCL_REG",
        curtailRegenerative =
          c.hasPathOrNull("curtailRegenerative") && c.getBoolean(
            "curtailRegenerative"
          ),
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

  final case class EvcsRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
      chargingStrategy: String = "maxPower",
      lowestEvSoc: Double = 0.2,
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )
  object EvcsRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.EvcsRuntimeConfig = {
      SimonaConfig.EvcsRuntimeConfig(
        chargingStrategy =
          if (c.hasPathOrNull("chargingStrategy"))
            c.getString("chargingStrategy")
          else "maxPower",
        lowestEvSoc =
          if (c.hasPathOrNull("lowestEvSoc")) c.getDouble("lowestEvSoc")
          else 0.2,
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

  final case class FixedFeedInRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )
  object FixedFeedInRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.FixedFeedInRuntimeConfig = {
      SimonaConfig.FixedFeedInRuntimeConfig(
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

  final case class GridOutputConfig(
      lines: Boolean = false,
      nodes: Boolean = false,
      notifier: String,
      switches: Boolean = false,
      transformers2w: Boolean = false,
      transformers3w: Boolean = false,
  )
  object GridOutputConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.GridOutputConfig = {
      SimonaConfig.GridOutputConfig(
        lines = c.hasPathOrNull("lines") && c.getBoolean("lines"),
        nodes = c.hasPathOrNull("nodes") && c.getBoolean("nodes"),
        notifier = $_reqStr(parentPath, c, "notifier", $tsCfgValidator),
        switches = c.hasPathOrNull("switches") && c.getBoolean("switches"),
        transformers2w =
          c.hasPathOrNull("transformers2w") && c.getBoolean("transformers2w"),
        transformers3w =
          c.hasPathOrNull("transformers3w") && c.getBoolean("transformers3w"),
      )
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

  final case class HpRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )
  object HpRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.HpRuntimeConfig = {
      SimonaConfig.HpRuntimeConfig(
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

  sealed abstract class KafkaParams(
      val bootstrapServers: java.lang.String,
      val linger: scala.Int,
      val runId: java.lang.String,
      val schemaRegistryUrl: java.lang.String,
  )

  final case class LoadRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
      modelBehaviour: String,
      reference: String,
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )
  object LoadRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.LoadRuntimeConfig = {
      SimonaConfig.LoadRuntimeConfig(
        modelBehaviour =
          $_reqStr(parentPath, c, "modelBehaviour", $tsCfgValidator),
        reference = $_reqStr(parentPath, c, "reference", $tsCfgValidator),
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

  final case class ParticipantBaseOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean,
      flexResult: Boolean = false,
      powerRequestReply: Boolean,
  ) extends BaseOutputConfig(notifier, simulationResult)
  object ParticipantBaseOutputConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.ParticipantBaseOutputConfig = {
      SimonaConfig.ParticipantBaseOutputConfig(
        flexResult =
          c.hasPathOrNull("flexResult") && c.getBoolean("flexResult"),
        powerRequestReply =
          $_reqBln(parentPath, c, "powerRequestReply", $tsCfgValidator),
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

  final case class PrimaryDataCsvParams(
      override val csvSep: String,
      override val directoryPath: String,
      override val isHierarchic: Boolean,
      timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
  ) extends CsvParams(csvSep, directoryPath, isHierarchic)
  object PrimaryDataCsvParams {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.PrimaryDataCsvParams = {
      SimonaConfig.PrimaryDataCsvParams(
        timePattern =
          if (c.hasPathOrNull("timePattern")) c.getString("timePattern")
          else "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
        csvSep = $_reqStr(parentPath, c, "csvSep", $tsCfgValidator),
        directoryPath =
          $_reqStr(parentPath, c, "directoryPath", $tsCfgValidator),
        isHierarchic = $_reqBln(parentPath, c, "isHierarchic", $tsCfgValidator),
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

  final case class PvRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )
  object PvRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.PvRuntimeConfig = {
      SimonaConfig.PvRuntimeConfig(
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

  final case class RefSystemConfig(
      gridIds: Option[List[String]],
      sNom: String,
      vNom: String,
      voltLvls: Option[List[VoltLvlConfig]],
  )
  object RefSystemConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.RefSystemConfig = {
      SimonaConfig.RefSystemConfig(
        gridIds =
          if (c.hasPathOrNull("gridIds"))
            scala.Some(
              $_L$_str(c.getList("gridIds"), parentPath, $tsCfgValidator)
            )
          else None,
        sNom = $_reqStr(parentPath, c, "sNom", $tsCfgValidator),
        vNom = $_reqStr(parentPath, c, "vNom", $tsCfgValidator),
        voltLvls =
          if (c.hasPathOrNull("voltLvls"))
            scala.Some(
              $_LSimonaConfig_VoltLvlConfig(
                c.getList("voltLvls"),
                parentPath,
                $tsCfgValidator,
              )
            )
          else None,
      )
    }
    private def $_LSimonaConfig_VoltLvlConfig(
        cl: com.typesafe.config.ConfigList,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): scala.List[SimonaConfig.VoltLvlConfig] = {
      import scala.jdk.CollectionConverters._
      cl.asScala
        .map(cv =>
          SimonaConfig.VoltLvlConfig(
            cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
            parentPath,
            $tsCfgValidator,
          )
        )
        .toList
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

  final case class ResultKafkaParams(
      override val bootstrapServers: String,
      override val linger: Int,
      override val runId: String,
      override val schemaRegistryUrl: String,
      topicNodeRes: String,
  ) extends KafkaParams(bootstrapServers, linger, runId, schemaRegistryUrl)
  object ResultKafkaParams {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.ResultKafkaParams = {
      SimonaConfig.ResultKafkaParams(
        topicNodeRes = $_reqStr(parentPath, c, "topicNodeRes", $tsCfgValidator),
        bootstrapServers =
          $_reqStr(parentPath, c, "bootstrapServers", $tsCfgValidator),
        linger = $_reqInt(parentPath, c, "linger", $tsCfgValidator),
        runId = $_reqStr(parentPath, c, "runId", $tsCfgValidator),
        schemaRegistryUrl =
          $_reqStr(parentPath, c, "schemaRegistryUrl", $tsCfgValidator),
      )
    }
    private def $_reqInt(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): scala.Int = {
      if (c == null) 0
      else
        try c.getInt(path)
        catch {
          case e: com.typesafe.config.ConfigException =>
            $tsCfgValidator.addBadPath(parentPath + path, e)
            0
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

  final case class RuntimeKafkaParams(
      override val bootstrapServers: String,
      override val linger: Int,
      override val runId: String,
      override val schemaRegistryUrl: String,
      topic: String,
  ) extends KafkaParams(bootstrapServers, linger, runId, schemaRegistryUrl)
  object RuntimeKafkaParams {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.RuntimeKafkaParams = {
      SimonaConfig.RuntimeKafkaParams(
        topic = $_reqStr(parentPath, c, "topic", $tsCfgValidator),
        bootstrapServers =
          $_reqStr(parentPath, c, "bootstrapServers", $tsCfgValidator),
        linger = $_reqInt(parentPath, c, "linger", $tsCfgValidator),
        runId = $_reqStr(parentPath, c, "runId", $tsCfgValidator),
        schemaRegistryUrl =
          $_reqStr(parentPath, c, "schemaRegistryUrl", $tsCfgValidator),
      )
    }
    private def $_reqInt(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): scala.Int = {
      if (c == null) 0
      else
        try c.getInt(path)
        catch {
          case e: com.typesafe.config.ConfigException =>
            $tsCfgValidator.addBadPath(parentPath + path, e)
            0
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

  final case class SimpleOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean,
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
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
      initialSoc: Double = 0d,
      targetSoc: Option[Double],
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
      measurements: List[String],
      transformers: List[String],
      vMax: Double,
      vMin: Double,
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
  object VoltLvlConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.VoltLvlConfig = {
      SimonaConfig.VoltLvlConfig(
        id = $_reqStr(parentPath, c, "id", $tsCfgValidator),
        vNom = $_reqStr(parentPath, c, "vNom", $tsCfgValidator),
      )
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

  final case class WecRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )
  object WecRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.WecRuntimeConfig = {
      SimonaConfig.WecRuntimeConfig(
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

  final case class Simona(
      control: scala.Option[SimonaConfig.Simona.Control],
      event: SimonaConfig.Simona.Event = Simona.Event(),
      gridConfig: SimonaConfig.Simona.GridConfig,
      input: SimonaConfig.Simona.Input,
      output: SimonaConfig.Simona.Output,
      powerflow: SimonaConfig.Simona.Powerflow,
      runtime: SimonaConfig.Simona.Runtime,
      simulationName: java.lang.String,
      time: SimonaConfig.Simona.Time = Simona.Time(),
  )
  object Simona {
    final case class Control(
        transformer: scala.List[SimonaConfig.TransformerControlGroup]
    )
    object Control {
      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): SimonaConfig.Simona.Control = {
        SimonaConfig.Simona.Control(
          transformer = $_LSimonaConfig_TransformerControlGroup(
            c.getList("transformer"),
            parentPath,
            $tsCfgValidator,
          )
        )
      }
      private def $_LSimonaConfig_TransformerControlGroup(
          cl: com.typesafe.config.ConfigList,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): scala.List[SimonaConfig.TransformerControlGroup] = {
        import scala.jdk.CollectionConverters._
        cl.asScala
          .map(cv =>
            SimonaConfig.TransformerControlGroup(
              cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
              parentPath,
              $tsCfgValidator,
            )
          )
          .toList
      }
    }

    final case class Event(
        listener: scala.Option[
          scala.List[SimonaConfig.Simona.Event.Listener$Elm]
        ] = None
    )
    object Event {
      final case class Listener$Elm(
          eventsToProcess: scala.Option[scala.List[java.lang.String]],
          fullClassPath: java.lang.String,
      )
      object Listener$Elm {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Event.Listener$Elm = {
          SimonaConfig.Simona.Event.Listener$Elm(
            eventsToProcess =
              if (c.hasPathOrNull("eventsToProcess"))
                scala.Some(
                  $_L$_str(
                    c.getList("eventsToProcess"),
                    parentPath,
                    $tsCfgValidator,
                  )
                )
              else None,
            fullClassPath =
              $_reqStr(parentPath, c, "fullClassPath", $tsCfgValidator),
          )
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

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): SimonaConfig.Simona.Event = {
        SimonaConfig.Simona.Event(
          listener =
            if (c.hasPathOrNull("listener"))
              scala.Some(
                $_LSimonaConfig_Simona_Event_Listener$Elm(
                  c.getList("listener"),
                  parentPath,
                  $tsCfgValidator,
                )
              )
            else None
        )
      }
      private def $_LSimonaConfig_Simona_Event_Listener$Elm(
          cl: com.typesafe.config.ConfigList,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): scala.List[SimonaConfig.Simona.Event.Listener$Elm] = {
        import scala.jdk.CollectionConverters._
        cl.asScala
          .map(cv =>
            SimonaConfig.Simona.Event.Listener$Elm(
              cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
              parentPath,
              $tsCfgValidator,
            )
          )
          .toList
      }
    }

    final case class GridConfig(
        refSystems: scala.Option[scala.List[SimonaConfig.RefSystemConfig]]
    )
    object GridConfig {
      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): SimonaConfig.Simona.GridConfig = {
        SimonaConfig.Simona.GridConfig(
          refSystems =
            if (c.hasPathOrNull("refSystems"))
              scala.Some(
                $_LSimonaConfig_RefSystemConfig(
                  c.getList("refSystems"),
                  parentPath,
                  $tsCfgValidator,
                )
              )
            else None
        )
      }
      private def $_LSimonaConfig_RefSystemConfig(
          cl: com.typesafe.config.ConfigList,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): scala.List[SimonaConfig.RefSystemConfig] = {
        import scala.jdk.CollectionConverters._
        cl.asScala
          .map(cv =>
            SimonaConfig.RefSystemConfig(
              cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
              parentPath,
              $tsCfgValidator,
            )
          )
          .toList
      }
    }

    final case class Input(
        grid: SimonaConfig.Simona.Input.Grid,
        primary: SimonaConfig.Simona.Input.Primary = Input.Primary(),
        weather: SimonaConfig.Simona.Input.Weather = Input.Weather(),
    )
    object Input {
      final case class Grid(
          datasource: SimonaConfig.Simona.Input.Grid.Datasource
      )
      object Grid {
        final case class Datasource(
            csvParams: scala.Option[SimonaConfig.BaseCsvParams],
            id: java.lang.String,
        )
        object Datasource {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Input.Grid.Datasource = {
            SimonaConfig.Simona.Input.Grid.Datasource(
              csvParams =
                if (c.hasPathOrNull("csvParams"))
                  scala.Some(
                    SimonaConfig.BaseCsvParams(
                      c.getConfig("csvParams"),
                      parentPath + "csvParams.",
                      $tsCfgValidator,
                    )
                  )
                else None,
              id = $_reqStr(parentPath, c, "id", $tsCfgValidator),
            )
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

        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Input.Grid = {
          SimonaConfig.Simona.Input.Grid(
            datasource = SimonaConfig.Simona.Input.Grid.Datasource(
              if (c.hasPathOrNull("datasource")) c.getConfig("datasource")
              else
                com.typesafe.config.ConfigFactory.parseString("datasource{}"),
              parentPath + "datasource.",
              $tsCfgValidator,
            )
          )
        }
      }

      final case class Primary(
          couchbaseParams: scala.Option[
            SimonaConfig.Simona.Input.Primary.CouchbaseParams
          ] = None,
          csvParams: scala.Option[SimonaConfig.PrimaryDataCsvParams] = None,
          influxDb1xParams: scala.Option[
            SimonaConfig.Simona.Input.Primary.InfluxDb1xParams
          ] = None,
          sqlParams: scala.Option[SimonaConfig.Simona.Input.Primary.SqlParams] =
            None,
      )
      object Primary {
        final case class CouchbaseParams(
            bucketName: String,
            coordinateColumnName: String,
            keyPrefix: String,
            password: String,
            timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
            url: String,
            userName: String,
        )
        object CouchbaseParams {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Input.Primary.CouchbaseParams = {
            SimonaConfig.Simona.Input.Primary.CouchbaseParams(
              bucketName =
                $_reqStr(parentPath, c, "bucketName", $tsCfgValidator),
              coordinateColumnName = $_reqStr(
                parentPath,
                c,
                "coordinateColumnName",
                $tsCfgValidator,
              ),
              keyPrefix = $_reqStr(parentPath, c, "keyPrefix", $tsCfgValidator),
              password = $_reqStr(parentPath, c, "password", $tsCfgValidator),
              timePattern =
                if (c.hasPathOrNull("timePattern")) c.getString("timePattern")
                else "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
              url = $_reqStr(parentPath, c, "url", $tsCfgValidator),
              userName = $_reqStr(parentPath, c, "userName", $tsCfgValidator),
            )
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

        final case class InfluxDb1xParams(
            database: String,
            port: Int,
            timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
            url: String,
        )
        object InfluxDb1xParams {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Input.Primary.InfluxDb1xParams = {
            SimonaConfig.Simona.Input.Primary.InfluxDb1xParams(
              database = $_reqStr(parentPath, c, "database", $tsCfgValidator),
              port = $_reqInt(parentPath, c, "port", $tsCfgValidator),
              timePattern =
                if (c.hasPathOrNull("timePattern")) c.getString("timePattern")
                else "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
              url = $_reqStr(parentPath, c, "url", $tsCfgValidator),
            )
          }
          private def $_reqInt(
              parentPath: java.lang.String,
              c: com.typesafe.config.Config,
              path: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): scala.Int = {
            if (c == null) 0
            else
              try c.getInt(path)
              catch {
                case e: com.typesafe.config.ConfigException =>
                  $tsCfgValidator.addBadPath(parentPath + path, e)
                  0
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

        final case class SqlParams(
            jdbcUrl: String,
            password: String,
            schemaName: String = "public",
            timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
            userName: String,
        )
        object SqlParams {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Input.Primary.SqlParams = {
            SimonaConfig.Simona.Input.Primary.SqlParams(
              jdbcUrl = $_reqStr(parentPath, c, "jdbcUrl", $tsCfgValidator),
              password = $_reqStr(parentPath, c, "password", $tsCfgValidator),
              schemaName =
                if (c.hasPathOrNull("schemaName")) c.getString("schemaName")
                else "public",
              timePattern =
                if (c.hasPathOrNull("timePattern")) c.getString("timePattern")
                else "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
              userName = $_reqStr(parentPath, c, "userName", $tsCfgValidator),
            )
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

        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Input.Primary = {
          SimonaConfig.Simona.Input.Primary(
            couchbaseParams =
              if (c.hasPathOrNull("couchbaseParams"))
                scala.Some(
                  SimonaConfig.Simona.Input.Primary.CouchbaseParams(
                    c.getConfig("couchbaseParams"),
                    parentPath + "couchbaseParams.",
                    $tsCfgValidator,
                  )
                )
              else None,
            csvParams =
              if (c.hasPathOrNull("csvParams"))
                scala.Some(
                  SimonaConfig.PrimaryDataCsvParams(
                    c.getConfig("csvParams"),
                    parentPath + "csvParams.",
                    $tsCfgValidator,
                  )
                )
              else None,
            influxDb1xParams =
              if (c.hasPathOrNull("influxDb1xParams"))
                scala.Some(
                  SimonaConfig.Simona.Input.Primary.InfluxDb1xParams(
                    c.getConfig("influxDb1xParams"),
                    parentPath + "influxDb1xParams.",
                    $tsCfgValidator,
                  )
                )
              else None,
            sqlParams =
              if (c.hasPathOrNull("sqlParams"))
                scala.Some(
                  SimonaConfig.Simona.Input.Primary.SqlParams(
                    c.getConfig("sqlParams"),
                    parentPath + "sqlParams.",
                    $tsCfgValidator,
                  )
                )
              else None,
          )
        }
      }

      final case class Weather(
          datasource: SimonaConfig.Simona.Input.Weather.Datasource =
            Weather.Datasource()
      )
      object Weather {
        final case class Datasource(
            coordinateSource: SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource =
              Datasource.CoordinateSource(),
            couchbaseParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.CouchbaseParams
            ] = None,
            csvParams: scala.Option[SimonaConfig.BaseCsvParams] = None,
            influxDb1xParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.InfluxDb1xParams
            ] = None,
            maxCoordinateDistance: Double = 50000,
            resolution: scala.Option[scala.Long] = None,
            sampleParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.SampleParams
            ] = None,
            scheme: String = "icon",
            sqlParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.SqlParams
            ] = None,
            timestampPattern: scala.Option[java.lang.String] = None,
        )
        object Datasource {
          final case class CoordinateSource(
              csvParams: scala.Option[SimonaConfig.BaseCsvParams] = None,
              gridModel: String = "icon",
              sampleParams: scala.Option[
                SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource.SampleParams
              ] = None,
              sqlParams: scala.Option[
                SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource.SqlParams
              ] = None,
          )
          object CoordinateSource {
            final case class SampleParams(
                use: Boolean = true
            )
            object SampleParams {
              def apply(
                  c: com.typesafe.config.Config,
                  parentPath: java.lang.String,
                  $tsCfgValidator: $TsCfgValidator,
              ): SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource.SampleParams = {
                SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource
                  .SampleParams(
                    use = !c.hasPathOrNull("use") || c.getBoolean("use")
                  )
              }
            }

            final case class SqlParams(
                jdbcUrl: String,
                password: String,
                schemaName: String = "public",
                tableName: String,
                userName: String,
            )
            object SqlParams {
              def apply(
                  c: com.typesafe.config.Config,
                  parentPath: java.lang.String,
                  $tsCfgValidator: $TsCfgValidator,
              ): SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource.SqlParams = {
                SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource
                  .SqlParams(
                    jdbcUrl =
                      $_reqStr(parentPath, c, "jdbcUrl", $tsCfgValidator),
                    password =
                      $_reqStr(parentPath, c, "password", $tsCfgValidator),
                    schemaName =
                      if (c.hasPathOrNull("schemaName"))
                        c.getString("schemaName")
                      else "public",
                    tableName =
                      $_reqStr(parentPath, c, "tableName", $tsCfgValidator),
                    userName =
                      $_reqStr(parentPath, c, "userName", $tsCfgValidator),
                  )
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

            def apply(
                c: com.typesafe.config.Config,
                parentPath: java.lang.String,
                $tsCfgValidator: $TsCfgValidator,
            ): SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource = {
              SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource(
                csvParams =
                  if (c.hasPathOrNull("csvParams"))
                    scala.Some(
                      SimonaConfig.BaseCsvParams(
                        c.getConfig("csvParams"),
                        parentPath + "csvParams.",
                        $tsCfgValidator,
                      )
                    )
                  else None,
                gridModel =
                  if (c.hasPathOrNull("gridModel")) c.getString("gridModel")
                  else "icon",
                sampleParams =
                  if (c.hasPathOrNull("sampleParams"))
                    scala.Some(
                      SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource
                        .SampleParams(
                          c.getConfig("sampleParams"),
                          parentPath + "sampleParams.",
                          $tsCfgValidator,
                        )
                    )
                  else None,
                sqlParams =
                  if (c.hasPathOrNull("sqlParams"))
                    scala.Some(
                      SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource
                        .SqlParams(
                          c.getConfig("sqlParams"),
                          parentPath + "sqlParams.",
                          $tsCfgValidator,
                        )
                    )
                  else None,
              )
            }
          }

          final case class CouchbaseParams(
              bucketName: String,
              coordinateColumnName: String,
              keyPrefix: String,
              password: String,
              url: String,
              userName: String,
          )
          object CouchbaseParams {
            def apply(
                c: com.typesafe.config.Config,
                parentPath: java.lang.String,
                $tsCfgValidator: $TsCfgValidator,
            ): SimonaConfig.Simona.Input.Weather.Datasource.CouchbaseParams = {
              SimonaConfig.Simona.Input.Weather.Datasource.CouchbaseParams(
                bucketName =
                  $_reqStr(parentPath, c, "bucketName", $tsCfgValidator),
                coordinateColumnName = $_reqStr(
                  parentPath,
                  c,
                  "coordinateColumnName",
                  $tsCfgValidator,
                ),
                keyPrefix =
                  $_reqStr(parentPath, c, "keyPrefix", $tsCfgValidator),
                password = $_reqStr(parentPath, c, "password", $tsCfgValidator),
                url = $_reqStr(parentPath, c, "url", $tsCfgValidator),
                userName = $_reqStr(parentPath, c, "userName", $tsCfgValidator),
              )
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

          final case class InfluxDb1xParams(
              database: String,
              port: Int,
              url: String,
          )
          object InfluxDb1xParams {
            def apply(
                c: com.typesafe.config.Config,
                parentPath: java.lang.String,
                $tsCfgValidator: $TsCfgValidator,
            ): SimonaConfig.Simona.Input.Weather.Datasource.InfluxDb1xParams = {
              SimonaConfig.Simona.Input.Weather.Datasource.InfluxDb1xParams(
                database = $_reqStr(parentPath, c, "database", $tsCfgValidator),
                port = $_reqInt(parentPath, c, "port", $tsCfgValidator),
                url = $_reqStr(parentPath, c, "url", $tsCfgValidator),
              )
            }
            private def $_reqInt(
                parentPath: java.lang.String,
                c: com.typesafe.config.Config,
                path: java.lang.String,
                $tsCfgValidator: $TsCfgValidator,
            ): scala.Int = {
              if (c == null) 0
              else
                try c.getInt(path)
                catch {
                  case e: com.typesafe.config.ConfigException =>
                    $tsCfgValidator.addBadPath(parentPath + path, e)
                    0
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

          final case class SampleParams(
              use: Boolean = true
          )
          object SampleParams {
            def apply(
                c: com.typesafe.config.Config,
                parentPath: java.lang.String,
                $tsCfgValidator: $TsCfgValidator,
            ): SimonaConfig.Simona.Input.Weather.Datasource.SampleParams = {
              SimonaConfig.Simona.Input.Weather.Datasource.SampleParams(
                use = !c.hasPathOrNull("use") || c.getBoolean("use")
              )
            }
          }

          final case class SqlParams(
              jdbcUrl: String,
              password: String,
              schemaName: String = "public",
              tableName: String,
              userName: String,
          )
          object SqlParams {
            def apply(
                c: com.typesafe.config.Config,
                parentPath: java.lang.String,
                $tsCfgValidator: $TsCfgValidator,
            ): SimonaConfig.Simona.Input.Weather.Datasource.SqlParams = {
              SimonaConfig.Simona.Input.Weather.Datasource.SqlParams(
                jdbcUrl = $_reqStr(parentPath, c, "jdbcUrl", $tsCfgValidator),
                password = $_reqStr(parentPath, c, "password", $tsCfgValidator),
                schemaName =
                  if (c.hasPathOrNull("schemaName")) c.getString("schemaName")
                  else "public",
                tableName =
                  $_reqStr(parentPath, c, "tableName", $tsCfgValidator),
                userName = $_reqStr(parentPath, c, "userName", $tsCfgValidator),
              )
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

          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Input.Weather.Datasource = {
            SimonaConfig.Simona.Input.Weather.Datasource(
              coordinateSource =
                SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource(
                  if (c.hasPathOrNull("coordinateSource"))
                    c.getConfig("coordinateSource")
                  else
                    com.typesafe.config.ConfigFactory
                      .parseString("coordinateSource{}"),
                  parentPath + "coordinateSource.",
                  $tsCfgValidator,
                ),
              couchbaseParams =
                if (c.hasPathOrNull("couchbaseParams"))
                  scala.Some(
                    SimonaConfig.Simona.Input.Weather.Datasource
                      .CouchbaseParams(
                        c.getConfig("couchbaseParams"),
                        parentPath + "couchbaseParams.",
                        $tsCfgValidator,
                      )
                  )
                else None,
              csvParams =
                if (c.hasPathOrNull("csvParams"))
                  scala.Some(
                    SimonaConfig.BaseCsvParams(
                      c.getConfig("csvParams"),
                      parentPath + "csvParams.",
                      $tsCfgValidator,
                    )
                  )
                else None,
              influxDb1xParams =
                if (c.hasPathOrNull("influxDb1xParams"))
                  scala.Some(
                    SimonaConfig.Simona.Input.Weather.Datasource
                      .InfluxDb1xParams(
                        c.getConfig("influxDb1xParams"),
                        parentPath + "influxDb1xParams.",
                        $tsCfgValidator,
                      )
                  )
                else None,
              maxCoordinateDistance =
                if (c.hasPathOrNull("maxCoordinateDistance"))
                  c.getDouble("maxCoordinateDistance")
                else 50000,
              resolution =
                if (c.hasPathOrNull("resolution"))
                  Some(c.getLong("resolution").longValue())
                else None,
              sampleParams =
                if (c.hasPathOrNull("sampleParams"))
                  scala.Some(
                    SimonaConfig.Simona.Input.Weather.Datasource.SampleParams(
                      c.getConfig("sampleParams"),
                      parentPath + "sampleParams.",
                      $tsCfgValidator,
                    )
                  )
                else None,
              scheme =
                if (c.hasPathOrNull("scheme")) c.getString("scheme")
                else "icon",
              sqlParams =
                if (c.hasPathOrNull("sqlParams"))
                  scala.Some(
                    SimonaConfig.Simona.Input.Weather.Datasource.SqlParams(
                      c.getConfig("sqlParams"),
                      parentPath + "sqlParams.",
                      $tsCfgValidator,
                    )
                  )
                else None,
              timestampPattern =
                if (c.hasPathOrNull("timestampPattern"))
                  Some(c.getString("timestampPattern"))
                else None,
            )
          }
        }

        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Input.Weather = {
          SimonaConfig.Simona.Input.Weather(
            datasource = SimonaConfig.Simona.Input.Weather.Datasource(
              if (c.hasPathOrNull("datasource")) c.getConfig("datasource")
              else
                com.typesafe.config.ConfigFactory.parseString("datasource{}"),
              parentPath + "datasource.",
              $tsCfgValidator,
            )
          )
        }
      }

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): SimonaConfig.Simona.Input = {
        SimonaConfig.Simona.Input(
          grid = SimonaConfig.Simona.Input.Grid(
            if (c.hasPathOrNull("grid")) c.getConfig("grid")
            else com.typesafe.config.ConfigFactory.parseString("grid{}"),
            parentPath + "grid.",
            $tsCfgValidator,
          ),
          primary = SimonaConfig.Simona.Input.Primary(
            if (c.hasPathOrNull("primary")) c.getConfig("primary")
            else com.typesafe.config.ConfigFactory.parseString("primary{}"),
            parentPath + "primary.",
            $tsCfgValidator,
          ),
          weather = SimonaConfig.Simona.Input.Weather(
            if (c.hasPathOrNull("weather")) c.getConfig("weather")
            else com.typesafe.config.ConfigFactory.parseString("weather{}"),
            parentPath + "weather.",
            $tsCfgValidator,
          ),
        )
      }
    }

    final case class Output(
        base: SimonaConfig.Simona.Output.Base,
        flex: Boolean = false,
        grid: SimonaConfig.GridOutputConfig,
        log: SimonaConfig.Simona.Output.Log = Output.Log(),
        participant: SimonaConfig.Simona.Output.Participant,
        sink: SimonaConfig.Simona.Output.Sink = Output.Sink(),
        thermal: SimonaConfig.Simona.Output.Thermal,
    )
    object Output {
      final case class Base(
          addTimestampToOutputDir: Boolean = true,
          dir: String,
      )
      object Base {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Output.Base = {
          SimonaConfig.Simona.Output.Base(
            addTimestampToOutputDir = !c.hasPathOrNull(
              "addTimestampToOutputDir"
            ) || c.getBoolean("addTimestampToOutputDir"),
            dir = $_reqStr(parentPath, c, "dir", $tsCfgValidator),
          )
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

      final case class Log(
          level: String = "INFO"
      )
      object Log {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Output.Log = {
          SimonaConfig.Simona.Output.Log(
            level =
              if (c.hasPathOrNull("level")) c.getString("level") else "INFO"
          )
        }
      }

      final case class Participant(
          defaultConfig: SimonaConfig.ParticipantBaseOutputConfig,
          individualConfigs: scala.List[
            SimonaConfig.ParticipantBaseOutputConfig
          ],
      )
      object Participant {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Output.Participant = {
          SimonaConfig.Simona.Output.Participant(
            defaultConfig = SimonaConfig.ParticipantBaseOutputConfig(
              if (c.hasPathOrNull("defaultConfig")) c.getConfig("defaultConfig")
              else
                com.typesafe.config.ConfigFactory
                  .parseString("defaultConfig{}"),
              parentPath + "defaultConfig.",
              $tsCfgValidator,
            ),
            individualConfigs = $_LSimonaConfig_ParticipantBaseOutputConfig(
              c.getList("individualConfigs"),
              parentPath,
              $tsCfgValidator,
            ),
          )
        }
        private def $_LSimonaConfig_ParticipantBaseOutputConfig(
            cl: com.typesafe.config.ConfigList,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): scala.List[SimonaConfig.ParticipantBaseOutputConfig] = {
          import scala.jdk.CollectionConverters._
          cl.asScala
            .map(cv =>
              SimonaConfig.ParticipantBaseOutputConfig(
                cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                parentPath,
                $tsCfgValidator,
              )
            )
            .toList
        }
      }

      final case class Sink(
          csv: scala.Option[SimonaConfig.Simona.Output.Sink.Csv] = None,
          influxDb1x: scala.Option[SimonaConfig.Simona.Output.Sink.InfluxDb1x] =
            None,
          kafka: scala.Option[SimonaConfig.ResultKafkaParams] = None,
      )
      object Sink {
        final case class Csv(
            compressOutputs: Boolean = false,
            fileFormat: String = ".csv",
            filePrefix: String = "",
            fileSuffix: String = "",
            isHierarchic: Boolean = false,
        )
        object Csv {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Output.Sink.Csv = {
            SimonaConfig.Simona.Output.Sink.Csv(
              compressOutputs =
                c.hasPathOrNull("compressOutputs") && c.getBoolean(
                  "compressOutputs"
                ),
              fileFormat =
                if (c.hasPathOrNull("fileFormat")) c.getString("fileFormat")
                else ".csv",
              filePrefix =
                if (c.hasPathOrNull("filePrefix")) c.getString("filePrefix")
                else "",
              fileSuffix =
                if (c.hasPathOrNull("fileSuffix")) c.getString("fileSuffix")
                else "",
              isHierarchic =
                c.hasPathOrNull("isHierarchic") && c.getBoolean("isHierarchic"),
            )
          }
        }

        final case class InfluxDb1x(
            database: String,
            port: Int,
            url: String,
        )
        object InfluxDb1x {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Output.Sink.InfluxDb1x = {
            SimonaConfig.Simona.Output.Sink.InfluxDb1x(
              database = $_reqStr(parentPath, c, "database", $tsCfgValidator),
              port = $_reqInt(parentPath, c, "port", $tsCfgValidator),
              url = $_reqStr(parentPath, c, "url", $tsCfgValidator),
            )
          }
          private def $_reqInt(
              parentPath: java.lang.String,
              c: com.typesafe.config.Config,
              path: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): scala.Int = {
            if (c == null) 0
            else
              try c.getInt(path)
              catch {
                case e: com.typesafe.config.ConfigException =>
                  $tsCfgValidator.addBadPath(parentPath + path, e)
                  0
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

        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Output.Sink = {
          SimonaConfig.Simona.Output.Sink(
            csv =
              if (c.hasPathOrNull("csv"))
                scala.Some(
                  SimonaConfig.Simona.Output.Sink.Csv(
                    c.getConfig("csv"),
                    parentPath + "csv.",
                    $tsCfgValidator,
                  )
                )
              else None,
            influxDb1x =
              if (c.hasPathOrNull("influxDb1x"))
                scala.Some(
                  SimonaConfig.Simona.Output.Sink.InfluxDb1x(
                    c.getConfig("influxDb1x"),
                    parentPath + "influxDb1x.",
                    $tsCfgValidator,
                  )
                )
              else None,
            kafka =
              if (c.hasPathOrNull("kafka"))
                scala.Some(
                  SimonaConfig.ResultKafkaParams(
                    c.getConfig("kafka"),
                    parentPath + "kafka.",
                    $tsCfgValidator,
                  )
                )
              else None,
          )
        }
      }

      final case class Thermal(
          defaultConfig: SimonaConfig.SimpleOutputConfig,
          individualConfigs: scala.List[SimonaConfig.SimpleOutputConfig],
      )
      object Thermal {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Output.Thermal = {
          SimonaConfig.Simona.Output.Thermal(
            defaultConfig = SimonaConfig.SimpleOutputConfig(
              if (c.hasPathOrNull("defaultConfig")) c.getConfig("defaultConfig")
              else
                com.typesafe.config.ConfigFactory
                  .parseString("defaultConfig{}"),
              parentPath + "defaultConfig.",
              $tsCfgValidator,
            ),
            individualConfigs = $_LSimonaConfig_SimpleOutputConfig(
              c.getList("individualConfigs"),
              parentPath,
              $tsCfgValidator,
            ),
          )
        }
        private def $_LSimonaConfig_SimpleOutputConfig(
            cl: com.typesafe.config.ConfigList,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): scala.List[SimonaConfig.SimpleOutputConfig] = {
          import scala.jdk.CollectionConverters._
          cl.asScala
            .map(cv =>
              SimonaConfig.SimpleOutputConfig(
                cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                parentPath,
                $tsCfgValidator,
              )
            )
            .toList
        }
      }

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): SimonaConfig.Simona.Output = {
        SimonaConfig.Simona.Output(
          base = SimonaConfig.Simona.Output.Base(
            if (c.hasPathOrNull("base")) c.getConfig("base")
            else com.typesafe.config.ConfigFactory.parseString("base{}"),
            parentPath + "base.",
            $tsCfgValidator,
          ),
          flex = c.hasPathOrNull("flex") && c.getBoolean("flex"),
          grid = SimonaConfig.GridOutputConfig(
            if (c.hasPathOrNull("grid")) c.getConfig("grid")
            else com.typesafe.config.ConfigFactory.parseString("grid{}"),
            parentPath + "grid.",
            $tsCfgValidator,
          ),
          log = SimonaConfig.Simona.Output.Log(
            if (c.hasPathOrNull("log")) c.getConfig("log")
            else com.typesafe.config.ConfigFactory.parseString("log{}"),
            parentPath + "log.",
            $tsCfgValidator,
          ),
          participant = SimonaConfig.Simona.Output.Participant(
            if (c.hasPathOrNull("participant")) c.getConfig("participant")
            else com.typesafe.config.ConfigFactory.parseString("participant{}"),
            parentPath + "participant.",
            $tsCfgValidator,
          ),
          sink = SimonaConfig.Simona.Output.Sink(
            if (c.hasPathOrNull("sink")) c.getConfig("sink")
            else com.typesafe.config.ConfigFactory.parseString("sink{}"),
            parentPath + "sink.",
            $tsCfgValidator,
          ),
          thermal = SimonaConfig.Simona.Output.Thermal(
            if (c.hasPathOrNull("thermal")) c.getConfig("thermal")
            else com.typesafe.config.ConfigFactory.parseString("thermal{}"),
            parentPath + "thermal.",
            $tsCfgValidator,
          ),
        )
      }
    }

    final case class Powerflow(
        maxSweepPowerDeviation: scala.Double,
        newtonraphson: SimonaConfig.Simona.Powerflow.Newtonraphson,
        resolution: Duration = Duration.ofHours(1),
        stopOnFailure: Boolean = false,
        sweepTimeout: Duration = Duration.ofSeconds(30),
    )
    object Powerflow {
      final case class Newtonraphson(
          epsilon: List[Double],
          iterations: Int,
      )
      object Newtonraphson {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Powerflow.Newtonraphson = {
          SimonaConfig.Simona.Powerflow.Newtonraphson(
            epsilon =
              $_L$_dbl(c.getList("epsilon"), parentPath, $tsCfgValidator),
            iterations = $_reqInt(parentPath, c, "iterations", $tsCfgValidator),
          )
        }
        private def $_reqInt(
            parentPath: java.lang.String,
            c: com.typesafe.config.Config,
            path: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): scala.Int = {
          if (c == null) 0
          else
            try c.getInt(path)
            catch {
              case e: com.typesafe.config.ConfigException =>
                $tsCfgValidator.addBadPath(parentPath + path, e)
                0
            }
        }

      }

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): SimonaConfig.Simona.Powerflow = {
        SimonaConfig.Simona.Powerflow(
          maxSweepPowerDeviation =
            $_reqDbl(parentPath, c, "maxSweepPowerDeviation", $tsCfgValidator),
          newtonraphson = SimonaConfig.Simona.Powerflow.Newtonraphson(
            if (c.hasPathOrNull("newtonraphson")) c.getConfig("newtonraphson")
            else
              com.typesafe.config.ConfigFactory.parseString("newtonraphson{}"),
            parentPath + "newtonraphson.",
            $tsCfgValidator,
          ),
          resolution =
            if (c.hasPathOrNull("resolution")) c.getDuration("resolution")
            else java.time.Duration.parse("PT1H"),
          stopOnFailure =
            c.hasPathOrNull("stopOnFailure") && c.getBoolean("stopOnFailure"),
          sweepTimeout =
            if (c.hasPathOrNull("sweepTimeout")) c.getDuration("sweepTimeout")
            else java.time.Duration.parse("PT30S"),
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

    final case class Runtime(
        listener: SimonaConfig.Simona.Runtime.Listener =
          SimonaConfig.Simona.Runtime.Listener(),
        participant: SimonaConfig.Simona.Runtime.Participant,
        selected_subgrids: Option[List[Int]],
        selected_volt_lvls: Option[List[SimonaConfig.VoltLvlConfig]],
    )
    object Runtime {
      final case class Listener(
          eventsToProcess: Option[List[String]] = None,
          kafka: Option[SimonaConfig.RuntimeKafkaParams] = None,
      )
      object Listener {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Runtime.Listener = {
          SimonaConfig.Simona.Runtime.Listener(
            eventsToProcess =
              if (c.hasPathOrNull("eventsToProcess"))
                scala.Some(
                  $_L$_str(
                    c.getList("eventsToProcess"),
                    parentPath,
                    $tsCfgValidator,
                  )
                )
              else None,
            kafka =
              if (c.hasPathOrNull("kafka"))
                scala.Some(
                  SimonaConfig.RuntimeKafkaParams(
                    c.getConfig("kafka"),
                    parentPath + "kafka.",
                    $tsCfgValidator,
                  )
                )
              else None,
          )
        }
      }

      final case class Participant(
          em: SimonaConfig.Simona.Runtime.Participant.Em,
          evcs: SimonaConfig.Simona.Runtime.Participant.Evcs,
          fixedFeedIn: SimonaConfig.Simona.Runtime.Participant.FixedFeedIn,
          hp: SimonaConfig.Simona.Runtime.Participant.Hp,
          load: SimonaConfig.Simona.Runtime.Participant.Load,
          pv: SimonaConfig.Simona.Runtime.Participant.Pv,
          requestVoltageDeviationThreshold: Double = 1e-14,
          storage: SimonaConfig.Simona.Runtime.Participant.Storage,
          wec: SimonaConfig.Simona.Runtime.Participant.Wec,
      )
      object Participant {
        final case class Em(
            defaultConfig: SimonaConfig.EmRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.EmRuntimeConfig],
        )
        object Em {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Runtime.Participant.Em = {
            SimonaConfig.Simona.Runtime.Participant.Em(
              defaultConfig = SimonaConfig.EmRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator,
              ),
              individualConfigs = $_LSimonaConfig_EmRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator,
              ),
            )
          }
          private def $_LSimonaConfig_EmRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): scala.List[SimonaConfig.EmRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.EmRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator,
                )
              )
              .toList
          }
        }

        final case class Evcs(
            defaultConfig: SimonaConfig.EvcsRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.EvcsRuntimeConfig],
        )
        object Evcs {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Runtime.Participant.Evcs = {
            SimonaConfig.Simona.Runtime.Participant.Evcs(
              defaultConfig = SimonaConfig.EvcsRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator,
              ),
              individualConfigs = $_LSimonaConfig_EvcsRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator,
              ),
            )
          }
          private def $_LSimonaConfig_EvcsRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): scala.List[SimonaConfig.EvcsRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.EvcsRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator,
                )
              )
              .toList
          }
        }

        final case class FixedFeedIn(
            defaultConfig: SimonaConfig.FixedFeedInRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.FixedFeedInRuntimeConfig],
        )
        object FixedFeedIn {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Runtime.Participant.FixedFeedIn = {
            SimonaConfig.Simona.Runtime.Participant.FixedFeedIn(
              defaultConfig = SimonaConfig.FixedFeedInRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator,
              ),
              individualConfigs = $_LSimonaConfig_FixedFeedInRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator,
              ),
            )
          }
          private def $_LSimonaConfig_FixedFeedInRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): scala.List[SimonaConfig.FixedFeedInRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.FixedFeedInRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator,
                )
              )
              .toList
          }
        }

        final case class Hp(
            defaultConfig: SimonaConfig.HpRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.HpRuntimeConfig],
        )
        object Hp {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Runtime.Participant.Hp = {
            SimonaConfig.Simona.Runtime.Participant.Hp(
              defaultConfig = SimonaConfig.HpRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator,
              ),
              individualConfigs = $_LSimonaConfig_HpRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator,
              ),
            )
          }
          private def $_LSimonaConfig_HpRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): scala.List[SimonaConfig.HpRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.HpRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator,
                )
              )
              .toList
          }
        }

        final case class Load(
            defaultConfig: SimonaConfig.LoadRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.LoadRuntimeConfig],
        )
        object Load {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Runtime.Participant.Load = {
            SimonaConfig.Simona.Runtime.Participant.Load(
              defaultConfig = SimonaConfig.LoadRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator,
              ),
              individualConfigs = $_LSimonaConfig_LoadRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator,
              ),
            )
          }
          private def $_LSimonaConfig_LoadRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): scala.List[SimonaConfig.LoadRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.LoadRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator,
                )
              )
              .toList
          }
        }

        final case class Pv(
            defaultConfig: SimonaConfig.PvRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.PvRuntimeConfig],
        )
        object Pv {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Runtime.Participant.Pv = {
            SimonaConfig.Simona.Runtime.Participant.Pv(
              defaultConfig = SimonaConfig.PvRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator,
              ),
              individualConfigs = $_LSimonaConfig_PvRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator,
              ),
            )
          }
          private def $_LSimonaConfig_PvRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): scala.List[SimonaConfig.PvRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.PvRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator,
                )
              )
              .toList
          }
        }

        final case class Storage(
            defaultConfig: SimonaConfig.StorageRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.StorageRuntimeConfig],
        )
        object Storage {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Runtime.Participant.Storage = {
            SimonaConfig.Simona.Runtime.Participant.Storage(
              defaultConfig = SimonaConfig.StorageRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator,
              ),
              individualConfigs = $_LSimonaConfig_StorageRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator,
              ),
            )
          }
          private def $_LSimonaConfig_StorageRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): scala.List[SimonaConfig.StorageRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.StorageRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator,
                )
              )
              .toList
          }
        }

        final case class Wec(
            defaultConfig: SimonaConfig.WecRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.WecRuntimeConfig],
        )
        object Wec {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): SimonaConfig.Simona.Runtime.Participant.Wec = {
            SimonaConfig.Simona.Runtime.Participant.Wec(
              defaultConfig = SimonaConfig.WecRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator,
              ),
              individualConfigs = $_LSimonaConfig_WecRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator,
              ),
            )
          }
          private def $_LSimonaConfig_WecRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator,
          ): scala.List[SimonaConfig.WecRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.WecRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator,
                )
              )
              .toList
          }
        }

        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator,
        ): SimonaConfig.Simona.Runtime.Participant = {
          SimonaConfig.Simona.Runtime.Participant(
            em = SimonaConfig.Simona.Runtime.Participant.Em(
              if (c.hasPathOrNull("em")) c.getConfig("em")
              else com.typesafe.config.ConfigFactory.parseString("em{}"),
              parentPath + "em.",
              $tsCfgValidator,
            ),
            evcs = SimonaConfig.Simona.Runtime.Participant.Evcs(
              if (c.hasPathOrNull("evcs")) c.getConfig("evcs")
              else com.typesafe.config.ConfigFactory.parseString("evcs{}"),
              parentPath + "evcs.",
              $tsCfgValidator,
            ),
            fixedFeedIn = SimonaConfig.Simona.Runtime.Participant.FixedFeedIn(
              if (c.hasPathOrNull("fixedFeedIn")) c.getConfig("fixedFeedIn")
              else
                com.typesafe.config.ConfigFactory.parseString("fixedFeedIn{}"),
              parentPath + "fixedFeedIn.",
              $tsCfgValidator,
            ),
            hp = SimonaConfig.Simona.Runtime.Participant.Hp(
              if (c.hasPathOrNull("hp")) c.getConfig("hp")
              else com.typesafe.config.ConfigFactory.parseString("hp{}"),
              parentPath + "hp.",
              $tsCfgValidator,
            ),
            load = SimonaConfig.Simona.Runtime.Participant.Load(
              if (c.hasPathOrNull("load")) c.getConfig("load")
              else com.typesafe.config.ConfigFactory.parseString("load{}"),
              parentPath + "load.",
              $tsCfgValidator,
            ),
            pv = SimonaConfig.Simona.Runtime.Participant.Pv(
              if (c.hasPathOrNull("pv")) c.getConfig("pv")
              else com.typesafe.config.ConfigFactory.parseString("pv{}"),
              parentPath + "pv.",
              $tsCfgValidator,
            ),
            requestVoltageDeviationThreshold =
              if (c.hasPathOrNull("requestVoltageDeviationThreshold"))
                c.getDouble("requestVoltageDeviationThreshold")
              else 1e-14,
            storage = SimonaConfig.Simona.Runtime.Participant.Storage(
              if (c.hasPathOrNull("storage")) c.getConfig("storage")
              else com.typesafe.config.ConfigFactory.parseString("storage{}"),
              parentPath + "storage.",
              $tsCfgValidator,
            ),
            wec = SimonaConfig.Simona.Runtime.Participant.Wec(
              if (c.hasPathOrNull("wec")) c.getConfig("wec")
              else com.typesafe.config.ConfigFactory.parseString("wec{}"),
              parentPath + "wec.",
              $tsCfgValidator,
            ),
          )
        }
      }

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): SimonaConfig.Simona.Runtime = {
        SimonaConfig.Simona.Runtime(
          listener = SimonaConfig.Simona.Runtime.Listener(
            if (c.hasPathOrNull("listener")) c.getConfig("listener")
            else com.typesafe.config.ConfigFactory.parseString("listener{}"),
            parentPath + "listener.",
            $tsCfgValidator,
          ),
          participant = SimonaConfig.Simona.Runtime.Participant(
            if (c.hasPathOrNull("participant")) c.getConfig("participant")
            else com.typesafe.config.ConfigFactory.parseString("participant{}"),
            parentPath + "participant.",
            $tsCfgValidator,
          ),
          selected_subgrids =
            if (c.hasPathOrNull("selected_subgrids"))
              scala.Some(
                $_L$_int(
                  c.getList("selected_subgrids"),
                  parentPath,
                  $tsCfgValidator,
                )
              )
            else None,
          selected_volt_lvls =
            if (c.hasPathOrNull("selected_volt_lvls"))
              scala.Some(
                $_LSimonaConfig_VoltLvlConfig(
                  c.getList("selected_volt_lvls"),
                  parentPath,
                  $tsCfgValidator,
                )
              )
            else None,
        )
      }
      private def $_LSimonaConfig_VoltLvlConfig(
          cl: com.typesafe.config.ConfigList,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): scala.List[SimonaConfig.VoltLvlConfig] = {
        import scala.jdk.CollectionConverters._
        cl.asScala
          .map(cv =>
            SimonaConfig.VoltLvlConfig(
              cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
              parentPath,
              $tsCfgValidator,
            )
          )
          .toList
      }
    }

    final case class Time(
        endDateTime: String = "2011-05-01T01:00:00Z",
        schedulerReadyCheckWindow: Option[Int] = None,
        startDateTime: String = "2011-05-01T00:00:00Z",
    )
    object Time {
      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator,
      ): SimonaConfig.Simona.Time = {
        SimonaConfig.Simona.Time(
          endDateTime =
            if (c.hasPathOrNull("endDateTime")) c.getString("endDateTime")
            else "2011-05-01T01:00:00Z",
          schedulerReadyCheckWindow =
            if (c.hasPathOrNull("schedulerReadyCheckWindow"))
              Some(c.getInt("schedulerReadyCheckWindow"))
            else None,
          startDateTime =
            if (c.hasPathOrNull("startDateTime")) c.getString("startDateTime")
            else "2011-05-01T00:00:00Z",
        )
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

  private def $_L$_dbl(
      cl: com.typesafe.config.ConfigList,
      parentPath: java.lang.String,
      $tsCfgValidator: $TsCfgValidator,
  ): scala.List[scala.Double] = {
    import scala.jdk.CollectionConverters._
    cl.asScala.map(cv => $_dbl(cv)).toList
  }
  private def $_L$_int(
      cl: com.typesafe.config.ConfigList,
      parentPath: java.lang.String,
      $tsCfgValidator: $TsCfgValidator,
  ): scala.List[scala.Int] = {
    import scala.jdk.CollectionConverters._
    cl.asScala.map(cv => $_int(cv)).toList
  }
  private def $_L$_str(
      cl: com.typesafe.config.ConfigList,
      parentPath: java.lang.String,
      $tsCfgValidator: $TsCfgValidator,
  ): scala.List[java.lang.String] = {
    import scala.jdk.CollectionConverters._
    cl.asScala.map(cv => $_str(cv)).toList
  }
  private def $_dbl(cv: com.typesafe.config.ConfigValue): scala.Double = {
    val u: Any = cv.unwrapped
    if (
      (cv.valueType != com.typesafe.config.ConfigValueType.NUMBER) ||
      !u.isInstanceOf[java.lang.Number]
    ) throw $_expE(cv, "double")
    u.asInstanceOf[java.lang.Number].doubleValue()
  }

  private def $_expE(
      cv: com.typesafe.config.ConfigValue,
      exp: java.lang.String,
  ) = {
    val u: Any = cv.unwrapped
    new java.lang.RuntimeException(
      s"${cv.origin.lineNumber}: " +
        "expecting: " + exp + " got: " +
        (if (u.isInstanceOf[java.lang.String]) "\"" + u + "\"" else u)
    )
  }

  private def $_int(cv: com.typesafe.config.ConfigValue): scala.Int = {
    val u: Any = cv.unwrapped
    if (
      (cv.valueType != com.typesafe.config.ConfigValueType.NUMBER) ||
      !u.isInstanceOf[Integer]
    ) throw $_expE(cv, "integer")
    u.asInstanceOf[Integer]
  }

  private def $_str(cv: com.typesafe.config.ConfigValue): java.lang.String = {
    java.lang.String.valueOf(cv.unwrapped())
  }

  final class $TsCfgValidator {
    private val badPaths =
      scala.collection.mutable.ArrayBuffer[java.lang.String]()

    def addBadPath(
        path: java.lang.String,
        e: com.typesafe.config.ConfigException,
    ): Unit = {
      badPaths += s"'$path': ${e.getClass.getName}(${e.getMessage})"
    }

    def addInvalidEnumValue(
        path: java.lang.String,
        value: java.lang.String,
        enumName: java.lang.String,
    ): Unit = {
      badPaths += s"'$path': invalid value $value for enumeration $enumName"
    }

    def validate(): Unit = {
      if (badPaths.nonEmpty) {
        throw new com.typesafe.config.ConfigException(
          badPaths.mkString("Invalid configuration:\n    ", "\n    ", "")
        ) {}
      }
    }
  }
}
