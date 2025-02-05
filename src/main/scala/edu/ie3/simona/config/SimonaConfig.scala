/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

final case class SimonaConfig(
    simona: SimonaConfig.Simona
)
object SimonaConfig {
  final case class BaseCsvParams(
      override val csvSep: java.lang.String,
      override val directoryPath: java.lang.String,
      override val isHierarchic: scala.Boolean,
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
      val notifier: java.lang.String,
      val simulationResult: scala.Boolean,
  )

  sealed abstract class BaseRuntimeConfig(
      val calculateMissingReactivePowerWithModel: scala.Boolean,
      val scaling: scala.Double,
      val uuids: scala.List[java.lang.String],
  ) extends java.io.Serializable

  sealed abstract class CsvParams(
      val csvSep: java.lang.String,
      val directoryPath: java.lang.String,
      val isHierarchic: scala.Boolean,
  )

  final case class EmRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: scala.Boolean,
      override val scaling: scala.Double,
      override val uuids: scala.List[java.lang.String],
      aggregateFlex: java.lang.String,
      curtailRegenerative: scala.Boolean,
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
      override val calculateMissingReactivePowerWithModel: scala.Boolean,
      override val scaling: scala.Double,
      override val uuids: scala.List[java.lang.String],
      chargingStrategy: java.lang.String,
      lowestEvSoc: scala.Double,
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
      override val calculateMissingReactivePowerWithModel: scala.Boolean,
      override val scaling: scala.Double,
      override val uuids: scala.List[java.lang.String],
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
      lines: scala.Boolean,
      nodes: scala.Boolean,
      notifier: java.lang.String,
      switches: scala.Boolean,
      transformers2w: scala.Boolean,
      transformers3w: scala.Boolean,
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
      override val calculateMissingReactivePowerWithModel: scala.Boolean,
      override val scaling: scala.Double,
      override val uuids: scala.List[java.lang.String],
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
      override val calculateMissingReactivePowerWithModel: scala.Boolean,
      override val scaling: scala.Double,
      override val uuids: scala.List[java.lang.String],
      modelBehaviour: java.lang.String,
      reference: java.lang.String,
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
      override val notifier: java.lang.String,
      override val simulationResult: scala.Boolean,
      flexResult: scala.Boolean,
      powerRequestReply: scala.Boolean,
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
      override val csvSep: java.lang.String,
      override val directoryPath: java.lang.String,
      override val isHierarchic: scala.Boolean,
      timePattern: java.lang.String,
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
      override val calculateMissingReactivePowerWithModel: scala.Boolean,
      override val scaling: scala.Double,
      override val uuids: scala.List[java.lang.String],
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
      gridIds: scala.Option[scala.List[java.lang.String]],
      sNom: java.lang.String,
      vNom: java.lang.String,
      voltLvls: scala.Option[scala.List[SimonaConfig.VoltLvlConfig]],
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
      override val bootstrapServers: java.lang.String,
      override val linger: scala.Int,
      override val runId: java.lang.String,
      override val schemaRegistryUrl: java.lang.String,
      topicNodeRes: java.lang.String,
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
      override val bootstrapServers: java.lang.String,
      override val linger: scala.Int,
      override val runId: java.lang.String,
      override val schemaRegistryUrl: java.lang.String,
      topic: java.lang.String,
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
      id: java.lang.String,
      vNom: java.lang.String,
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
      override val calculateMissingReactivePowerWithModel: scala.Boolean,
      override val scaling: scala.Double,
      override val uuids: scala.List[java.lang.String],
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
      event: SimonaConfig.Simona.Event,
      gridConfig: SimonaConfig.Simona.GridConfig,
      input: SimonaConfig.Simona.Input,
      output: SimonaConfig.Simona.Output,
      powerflow: SimonaConfig.Simona.Powerflow,
      runtime: SimonaConfig.Simona.Runtime,
      simulationName: java.lang.String,
      time: SimonaConfig.Simona.Time,
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
        ]
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
        extSimDir: scala.Option[java.lang.String],
        grid: SimonaConfig.Simona.Input.Grid,
        primary: SimonaConfig.Simona.Input.Primary,
        weather: SimonaConfig.Simona.Input.Weather,
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
          ],
          csvParams: scala.Option[SimonaConfig.PrimaryDataCsvParams],
          influxDb1xParams: scala.Option[
            SimonaConfig.Simona.Input.Primary.InfluxDb1xParams
          ],
          sqlParams: scala.Option[SimonaConfig.Simona.Input.Primary.SqlParams],
      )
      object Primary {
        final case class CouchbaseParams(
            bucketName: java.lang.String,
            coordinateColumnName: java.lang.String,
            keyPrefix: java.lang.String,
            password: java.lang.String,
            timePattern: java.lang.String,
            url: java.lang.String,
            userName: java.lang.String,
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
            database: java.lang.String,
            port: scala.Int,
            timePattern: java.lang.String,
            url: java.lang.String,
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
            jdbcUrl: java.lang.String,
            password: java.lang.String,
            schemaName: java.lang.String,
            timePattern: java.lang.String,
            userName: java.lang.String,
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
          datasource: SimonaConfig.Simona.Input.Weather.Datasource
      )
      object Weather {
        final case class Datasource(
            coordinateSource: SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource,
            couchbaseParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.CouchbaseParams
            ],
            csvParams: scala.Option[SimonaConfig.BaseCsvParams],
            influxDb1xParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.InfluxDb1xParams
            ],
            maxCoordinateDistance: scala.Double,
            resolution: scala.Option[scala.Long],
            sampleParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.SampleParams
            ],
            scheme: java.lang.String,
            sqlParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.SqlParams
            ],
            timestampPattern: scala.Option[java.lang.String],
        )
        object Datasource {
          final case class CoordinateSource(
              csvParams: scala.Option[SimonaConfig.BaseCsvParams],
              gridModel: java.lang.String,
              sampleParams: scala.Option[
                SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource.SampleParams
              ],
              sqlParams: scala.Option[
                SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource.SqlParams
              ],
          )
          object CoordinateSource {
            final case class SampleParams(
                use: scala.Boolean
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
                jdbcUrl: java.lang.String,
                password: java.lang.String,
                schemaName: java.lang.String,
                tableName: java.lang.String,
                userName: java.lang.String,
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
              bucketName: java.lang.String,
              coordinateColumnName: java.lang.String,
              keyPrefix: java.lang.String,
              password: java.lang.String,
              url: java.lang.String,
              userName: java.lang.String,
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
              database: java.lang.String,
              port: scala.Int,
              url: java.lang.String,
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
              use: scala.Boolean
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
              jdbcUrl: java.lang.String,
              password: java.lang.String,
              schemaName: java.lang.String,
              tableName: java.lang.String,
              userName: java.lang.String,
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
          extSimDir =
            if (c.hasPathOrNull("extSimDir")) Some(c.getString("extSimDir"))
            else None,
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
        flex: scala.Boolean,
        grid: SimonaConfig.GridOutputConfig,
        log: SimonaConfig.Simona.Output.Log,
        participant: SimonaConfig.Simona.Output.Participant,
        sink: SimonaConfig.Simona.Output.Sink,
        thermal: SimonaConfig.Simona.Output.Thermal,
    )
    object Output {
      final case class Base(
          addTimestampToOutputDir: scala.Boolean,
          dir: java.lang.String,
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
          level: java.lang.String
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
          csv: scala.Option[SimonaConfig.Simona.Output.Sink.Csv],
          influxDb1x: scala.Option[SimonaConfig.Simona.Output.Sink.InfluxDb1x],
          kafka: scala.Option[SimonaConfig.ResultKafkaParams],
      )
      object Sink {
        final case class Csv(
            compressOutputs: scala.Boolean,
            fileFormat: java.lang.String,
            filePrefix: java.lang.String,
            fileSuffix: java.lang.String,
            isHierarchic: scala.Boolean,
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
            database: java.lang.String,
            port: scala.Int,
            url: java.lang.String,
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
        resolution: java.time.Duration,
        stopOnFailure: scala.Boolean,
        sweepTimeout: java.time.Duration,
    )
    object Powerflow {
      final case class Newtonraphson(
          epsilon: scala.List[scala.Double],
          iterations: scala.Int,
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
        listener: SimonaConfig.Simona.Runtime.Listener,
        participant: SimonaConfig.Simona.Runtime.Participant,
        selected_subgrids: scala.Option[scala.List[scala.Int]],
        selected_volt_lvls: scala.Option[scala.List[SimonaConfig.VoltLvlConfig]],
    )
    object Runtime {
      final case class Listener(
          eventsToProcess: scala.Option[scala.List[java.lang.String]],
          kafka: scala.Option[SimonaConfig.RuntimeKafkaParams],
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
          requestVoltageDeviationThreshold: scala.Double,
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
        endDateTime: java.lang.String,
        schedulerReadyCheckWindow: scala.Option[scala.Int],
        startDateTime: java.lang.String,
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

    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator,
    ): SimonaConfig.Simona = {
      SimonaConfig.Simona(
        control =
          if (c.hasPathOrNull("control"))
            scala.Some(
              SimonaConfig.Simona.Control(
                c.getConfig("control"),
                parentPath + "control.",
                $tsCfgValidator,
              )
            )
          else None,
        event = SimonaConfig.Simona.Event(
          if (c.hasPathOrNull("event")) c.getConfig("event")
          else com.typesafe.config.ConfigFactory.parseString("event{}"),
          parentPath + "event.",
          $tsCfgValidator,
        ),
        gridConfig = SimonaConfig.Simona.GridConfig(
          if (c.hasPathOrNull("gridConfig")) c.getConfig("gridConfig")
          else com.typesafe.config.ConfigFactory.parseString("gridConfig{}"),
          parentPath + "gridConfig.",
          $tsCfgValidator,
        ),
        input = SimonaConfig.Simona.Input(
          if (c.hasPathOrNull("input")) c.getConfig("input")
          else com.typesafe.config.ConfigFactory.parseString("input{}"),
          parentPath + "input.",
          $tsCfgValidator,
        ),
        output = SimonaConfig.Simona.Output(
          if (c.hasPathOrNull("output")) c.getConfig("output")
          else com.typesafe.config.ConfigFactory.parseString("output{}"),
          parentPath + "output.",
          $tsCfgValidator,
        ),
        powerflow = SimonaConfig.Simona.Powerflow(
          if (c.hasPathOrNull("powerflow")) c.getConfig("powerflow")
          else com.typesafe.config.ConfigFactory.parseString("powerflow{}"),
          parentPath + "powerflow.",
          $tsCfgValidator,
        ),
        runtime = SimonaConfig.Simona.Runtime(
          if (c.hasPathOrNull("runtime")) c.getConfig("runtime")
          else com.typesafe.config.ConfigFactory.parseString("runtime{}"),
          parentPath + "runtime.",
          $tsCfgValidator,
        ),
        simulationName =
          $_reqStr(parentPath, c, "simulationName", $tsCfgValidator),
        time = SimonaConfig.Simona.Time(
          if (c.hasPathOrNull("time")) c.getConfig("time")
          else com.typesafe.config.ConfigFactory.parseString("time{}"),
          parentPath + "time.",
          $tsCfgValidator,
        ),
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

  def apply(c: com.typesafe.config.Config): SimonaConfig = {
    val $tsCfgValidator: $TsCfgValidator = new $TsCfgValidator()
    val parentPath: java.lang.String = ""
    val $result = SimonaConfig(
      simona = SimonaConfig.Simona(
        if (c.hasPathOrNull("simona")) c.getConfig("simona")
        else com.typesafe.config.ConfigFactory.parseString("simona{}"),
        parentPath + "simona.",
        $tsCfgValidator,
      )
    )
    $tsCfgValidator.validate()
    $result
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
