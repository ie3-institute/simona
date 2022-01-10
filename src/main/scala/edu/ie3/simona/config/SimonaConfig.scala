/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

final case class SimonaConfig(
    simona: SimonaConfig.Simona
)
object SimonaConfig {
  final case class BaseOutputConfig(
      notifier: java.lang.String,
      powerRequestReply: scala.Boolean,
      simulationResult: scala.Boolean
  )
  object BaseOutputConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): SimonaConfig.BaseOutputConfig = {
      SimonaConfig.BaseOutputConfig(
        notifier = $_reqStr(parentPath, c, "notifier", $tsCfgValidator),
        powerRequestReply =
          $_reqBln(parentPath, c, "powerRequestReply", $tsCfgValidator),
        simulationResult =
          $_reqBln(parentPath, c, "simulationResult", $tsCfgValidator)
      )
    }
    private def $_reqBln(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
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
        $tsCfgValidator: $TsCfgValidator
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

  sealed abstract class BaseRuntimeConfig(
      val calculateMissingReactivePowerWithModel: scala.Boolean,
      val scaling: scala.Double,
      val uuids: scala.List[java.lang.String]
  ) extends java.io.Serializable

  final case class EvcsRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: scala.Boolean,
      override val scaling: scala.Double,
      override val uuids: scala.List[java.lang.String]
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids
      )
  object EvcsRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): SimonaConfig.EvcsRuntimeConfig = {
      SimonaConfig.EvcsRuntimeConfig(
        calculateMissingReactivePowerWithModel = $_reqBln(
          parentPath,
          c,
          "calculateMissingReactivePowerWithModel",
          $tsCfgValidator
        ),
        scaling = $_reqDbl(parentPath, c, "scaling", $tsCfgValidator),
        uuids = $_L$_str(c.getList("uuids"), parentPath, $tsCfgValidator)
      )
    }
    private def $_reqBln(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
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
        $tsCfgValidator: $TsCfgValidator
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
      override val uuids: scala.List[java.lang.String]
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids
      )
  object FixedFeedInRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): SimonaConfig.FixedFeedInRuntimeConfig = {
      SimonaConfig.FixedFeedInRuntimeConfig(
        calculateMissingReactivePowerWithModel = $_reqBln(
          parentPath,
          c,
          "calculateMissingReactivePowerWithModel",
          $tsCfgValidator
        ),
        scaling = $_reqDbl(parentPath, c, "scaling", $tsCfgValidator),
        uuids = $_L$_str(c.getList("uuids"), parentPath, $tsCfgValidator)
      )
    }
    private def $_reqBln(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
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
        $tsCfgValidator: $TsCfgValidator
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
      transformers3w: scala.Boolean
  )
  object GridOutputConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): SimonaConfig.GridOutputConfig = {
      SimonaConfig.GridOutputConfig(
        lines = c.hasPathOrNull("lines") && c.getBoolean("lines"),
        nodes = c.hasPathOrNull("nodes") && c.getBoolean("nodes"),
        notifier = $_reqStr(parentPath, c, "notifier", $tsCfgValidator),
        switches = c.hasPathOrNull("switches") && c.getBoolean("switches"),
        transformers2w =
          c.hasPathOrNull("transformers2w") && c.getBoolean("transformers2w"),
        transformers3w =
          c.hasPathOrNull("transformers3w") && c.getBoolean("transformers3w")
      )
    }
    private def $_reqStr(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
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

  final case class LoadRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: scala.Boolean,
      override val scaling: scala.Double,
      override val uuids: scala.List[java.lang.String],
      modelBehaviour: java.lang.String,
      reference: java.lang.String
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids
      )
  object LoadRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): SimonaConfig.LoadRuntimeConfig = {
      SimonaConfig.LoadRuntimeConfig(
        modelBehaviour =
          $_reqStr(parentPath, c, "modelBehaviour", $tsCfgValidator),
        reference = $_reqStr(parentPath, c, "reference", $tsCfgValidator),
        calculateMissingReactivePowerWithModel = $_reqBln(
          parentPath,
          c,
          "calculateMissingReactivePowerWithModel",
          $tsCfgValidator
        ),
        scaling = $_reqDbl(parentPath, c, "scaling", $tsCfgValidator),
        uuids = $_L$_str(c.getList("uuids"), parentPath, $tsCfgValidator)
      )
    }
    private def $_reqBln(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
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
        $tsCfgValidator: $TsCfgValidator
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
        $tsCfgValidator: $TsCfgValidator
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
      override val uuids: scala.List[java.lang.String]
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids
      )
  object PvRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): SimonaConfig.PvRuntimeConfig = {
      SimonaConfig.PvRuntimeConfig(
        calculateMissingReactivePowerWithModel = $_reqBln(
          parentPath,
          c,
          "calculateMissingReactivePowerWithModel",
          $tsCfgValidator
        ),
        scaling = $_reqDbl(parentPath, c, "scaling", $tsCfgValidator),
        uuids = $_L$_str(c.getList("uuids"), parentPath, $tsCfgValidator)
      )
    }
    private def $_reqBln(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
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
        $tsCfgValidator: $TsCfgValidator
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
      voltLvls: scala.Option[scala.List[java.lang.String]]
  )
  object RefSystemConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
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
              $_L$_str(c.getList("voltLvls"), parentPath, $tsCfgValidator)
            )
          else None
      )
    }
    private def $_reqStr(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
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

  final case class VoltLvlConfig(
      id: java.lang.String,
      vNom: java.lang.String
  )
  object VoltLvlConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): SimonaConfig.VoltLvlConfig = {
      SimonaConfig.VoltLvlConfig(
        id = $_reqStr(parentPath, c, "id", $tsCfgValidator),
        vNom = $_reqStr(parentPath, c, "vNom", $tsCfgValidator)
      )
    }
    private def $_reqStr(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
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
      override val uuids: scala.List[java.lang.String]
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids
      )
  object WecRuntimeConfig {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): SimonaConfig.WecRuntimeConfig = {
      SimonaConfig.WecRuntimeConfig(
        calculateMissingReactivePowerWithModel = $_reqBln(
          parentPath,
          c,
          "calculateMissingReactivePowerWithModel",
          $tsCfgValidator
        ),
        scaling = $_reqDbl(parentPath, c, "scaling", $tsCfgValidator),
        uuids = $_L$_str(c.getList("uuids"), parentPath, $tsCfgValidator)
      )
    }
    private def $_reqBln(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
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
        $tsCfgValidator: $TsCfgValidator
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
      event: SimonaConfig.Simona.Event,
      gridConfig: SimonaConfig.Simona.GridConfig,
      input: SimonaConfig.Simona.Input,
      output: SimonaConfig.Simona.Output,
      powerflow: SimonaConfig.Simona.Powerflow,
      runtime: SimonaConfig.Simona.Runtime,
      simulationName: java.lang.String,
      time: SimonaConfig.Simona.Time
  )
  object Simona {
    final case class Event(
        listener: scala.Option[
          scala.List[SimonaConfig.Simona.Event.Listener$Elm]
        ]
    )
    object Event {
      final case class Listener$Elm(
          eventsToProcess: scala.Option[scala.List[java.lang.String]],
          fullClassPath: java.lang.String
      )
      object Listener$Elm {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): SimonaConfig.Simona.Event.Listener$Elm = {
          SimonaConfig.Simona.Event.Listener$Elm(
            eventsToProcess =
              if (c.hasPathOrNull("eventsToProcess"))
                scala.Some(
                  $_L$_str(
                    c.getList("eventsToProcess"),
                    parentPath,
                    $tsCfgValidator
                  )
                )
              else None,
            fullClassPath =
              $_reqStr(parentPath, c, "fullClassPath", $tsCfgValidator)
          )
        }
        private def $_reqStr(
            parentPath: java.lang.String,
            c: com.typesafe.config.Config,
            path: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
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
          $tsCfgValidator: $TsCfgValidator
      ): SimonaConfig.Simona.Event = {
        SimonaConfig.Simona.Event(
          listener =
            if (c.hasPathOrNull("listener"))
              scala.Some(
                $_LSimonaConfig_Simona_Event_Listener$Elm(
                  c.getList("listener"),
                  parentPath,
                  $tsCfgValidator
                )
              )
            else None
        )
      }
      private def $_LSimonaConfig_Simona_Event_Listener$Elm(
          cl: com.typesafe.config.ConfigList,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): scala.List[SimonaConfig.Simona.Event.Listener$Elm] = {
        import scala.jdk.CollectionConverters._
        cl.asScala
          .map(cv =>
            SimonaConfig.Simona.Event.Listener$Elm(
              cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
              parentPath,
              $tsCfgValidator
            )
          )
          .toList
      }
    }

    final case class GridConfig(
        refSystems: scala.List[SimonaConfig.RefSystemConfig]
    )
    object GridConfig {
      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): SimonaConfig.Simona.GridConfig = {
        SimonaConfig.Simona.GridConfig(
          refSystems = $_LSimonaConfig_RefSystemConfig(
            c.getList("refSystems"),
            parentPath,
            $tsCfgValidator
          )
        )
      }
      private def $_LSimonaConfig_RefSystemConfig(
          cl: com.typesafe.config.ConfigList,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): scala.List[SimonaConfig.RefSystemConfig] = {
        import scala.jdk.CollectionConverters._
        cl.asScala
          .map(cv =>
            SimonaConfig.RefSystemConfig(
              cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
              parentPath,
              $tsCfgValidator
            )
          )
          .toList
      }
    }

    final case class Input(
        grid: SimonaConfig.Simona.Input.Grid,
        primary: SimonaConfig.Simona.Input.Primary,
        weather: SimonaConfig.Simona.Input.Weather
    )
    object Input {
      final case class Grid(
          datasource: SimonaConfig.Simona.Input.Grid.Datasource
      )
      object Grid {
        final case class Datasource(
            csvParams: scala.Option[
              SimonaConfig.Simona.Input.Grid.Datasource.CsvParams
            ],
            id: java.lang.String
        )
        object Datasource {
          final case class CsvParams(
              csvSep: java.lang.String,
              folderPath: java.lang.String
          )
          object CsvParams {
            def apply(
                c: com.typesafe.config.Config,
                parentPath: java.lang.String,
                $tsCfgValidator: $TsCfgValidator
            ): SimonaConfig.Simona.Input.Grid.Datasource.CsvParams = {
              SimonaConfig.Simona.Input.Grid.Datasource.CsvParams(
                csvSep = $_reqStr(parentPath, c, "csvSep", $tsCfgValidator),
                folderPath =
                  $_reqStr(parentPath, c, "folderPath", $tsCfgValidator)
              )
            }
            private def $_reqStr(
                parentPath: java.lang.String,
                c: com.typesafe.config.Config,
                path: java.lang.String,
                $tsCfgValidator: $TsCfgValidator
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
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Input.Grid.Datasource = {
            SimonaConfig.Simona.Input.Grid.Datasource(
              csvParams =
                if (c.hasPathOrNull("csvParams"))
                  scala.Some(
                    SimonaConfig.Simona.Input.Grid.Datasource.CsvParams(
                      c.getConfig("csvParams"),
                      parentPath + "csvParams.",
                      $tsCfgValidator
                    )
                  )
                else None,
              id = $_reqStr(parentPath, c, "id", $tsCfgValidator)
            )
          }
          private def $_reqStr(
              parentPath: java.lang.String,
              c: com.typesafe.config.Config,
              path: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
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
            $tsCfgValidator: $TsCfgValidator
        ): SimonaConfig.Simona.Input.Grid = {
          SimonaConfig.Simona.Input.Grid(
            datasource = SimonaConfig.Simona.Input.Grid.Datasource(
              if (c.hasPathOrNull("datasource")) c.getConfig("datasource")
              else
                com.typesafe.config.ConfigFactory.parseString("datasource{}"),
              parentPath + "datasource.",
              $tsCfgValidator
            )
          )
        }
      }

      final case class Primary(
          couchbaseParams: scala.Option[
            SimonaConfig.Simona.Input.Primary.CouchbaseParams
          ],
          csvParams: scala.Option[SimonaConfig.Simona.Input.Primary.CsvParams],
          influxDb1xParams: scala.Option[
            SimonaConfig.Simona.Input.Primary.InfluxDb1xParams
          ],
          sqlParams: scala.Option[SimonaConfig.Simona.Input.Primary.SqlParams]
      )
      object Primary {
        final case class CouchbaseParams(
            bucketName: java.lang.String,
            coordinateColumnName: java.lang.String,
            keyPrefix: java.lang.String,
            password: java.lang.String,
            timePattern: java.lang.String,
            url: java.lang.String,
            userName: java.lang.String
        )
        object CouchbaseParams {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Input.Primary.CouchbaseParams = {
            SimonaConfig.Simona.Input.Primary.CouchbaseParams(
              bucketName =
                $_reqStr(parentPath, c, "bucketName", $tsCfgValidator),
              coordinateColumnName = $_reqStr(
                parentPath,
                c,
                "coordinateColumnName",
                $tsCfgValidator
              ),
              keyPrefix = $_reqStr(parentPath, c, "keyPrefix", $tsCfgValidator),
              password = $_reqStr(parentPath, c, "password", $tsCfgValidator),
              timePattern =
                if (c.hasPathOrNull("timePattern")) c.getString("timePattern")
                else "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]'Z'",
              url = $_reqStr(parentPath, c, "url", $tsCfgValidator),
              userName = $_reqStr(parentPath, c, "userName", $tsCfgValidator)
            )
          }
          private def $_reqStr(
              parentPath: java.lang.String,
              c: com.typesafe.config.Config,
              path: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
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

        final case class CsvParams(
            csvSep: java.lang.String,
            folderPath: java.lang.String,
            timePattern: java.lang.String
        )
        object CsvParams {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Input.Primary.CsvParams = {
            SimonaConfig.Simona.Input.Primary.CsvParams(
              csvSep = $_reqStr(parentPath, c, "csvSep", $tsCfgValidator),
              folderPath =
                $_reqStr(parentPath, c, "folderPath", $tsCfgValidator),
              timePattern =
                if (c.hasPathOrNull("timePattern")) c.getString("timePattern")
                else "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]'Z'"
            )
          }
          private def $_reqStr(
              parentPath: java.lang.String,
              c: com.typesafe.config.Config,
              path: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
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
            url: java.lang.String
        )
        object InfluxDb1xParams {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Input.Primary.InfluxDb1xParams = {
            SimonaConfig.Simona.Input.Primary.InfluxDb1xParams(
              database = $_reqStr(parentPath, c, "database", $tsCfgValidator),
              port = $_reqInt(parentPath, c, "port", $tsCfgValidator),
              timePattern =
                if (c.hasPathOrNull("timePattern")) c.getString("timePattern")
                else "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]'Z'",
              url = $_reqStr(parentPath, c, "url", $tsCfgValidator)
            )
          }
          private def $_reqInt(
              parentPath: java.lang.String,
              c: com.typesafe.config.Config,
              path: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
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
              $tsCfgValidator: $TsCfgValidator
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
            timeColumnName: java.lang.String,
            timePattern: java.lang.String,
            userName: java.lang.String,
            weatherTableName: java.lang.String
        )
        object SqlParams {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Input.Primary.SqlParams = {
            SimonaConfig.Simona.Input.Primary.SqlParams(
              jdbcUrl = $_reqStr(parentPath, c, "jdbcUrl", $tsCfgValidator),
              password = $_reqStr(parentPath, c, "password", $tsCfgValidator),
              schemaName =
                if (c.hasPathOrNull("schemaName")) c.getString("schemaName")
                else "public",
              timeColumnName =
                $_reqStr(parentPath, c, "timeColumnName", $tsCfgValidator),
              timePattern =
                if (c.hasPathOrNull("timePattern")) c.getString("timePattern")
                else "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]'Z'",
              userName = $_reqStr(parentPath, c, "userName", $tsCfgValidator),
              weatherTableName =
                $_reqStr(parentPath, c, "weatherTableName", $tsCfgValidator)
            )
          }
          private def $_reqStr(
              parentPath: java.lang.String,
              c: com.typesafe.config.Config,
              path: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
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
            $tsCfgValidator: $TsCfgValidator
        ): SimonaConfig.Simona.Input.Primary = {
          SimonaConfig.Simona.Input.Primary(
            couchbaseParams =
              if (c.hasPathOrNull("couchbaseParams"))
                scala.Some(
                  SimonaConfig.Simona.Input.Primary.CouchbaseParams(
                    c.getConfig("couchbaseParams"),
                    parentPath + "couchbaseParams.",
                    $tsCfgValidator
                  )
                )
              else None,
            csvParams =
              if (c.hasPathOrNull("csvParams"))
                scala.Some(
                  SimonaConfig.Simona.Input.Primary.CsvParams(
                    c.getConfig("csvParams"),
                    parentPath + "csvParams.",
                    $tsCfgValidator
                  )
                )
              else None,
            influxDb1xParams =
              if (c.hasPathOrNull("influxDb1xParams"))
                scala.Some(
                  SimonaConfig.Simona.Input.Primary.InfluxDb1xParams(
                    c.getConfig("influxDb1xParams"),
                    parentPath + "influxDb1xParams.",
                    $tsCfgValidator
                  )
                )
              else None,
            sqlParams =
              if (c.hasPathOrNull("sqlParams"))
                scala.Some(
                  SimonaConfig.Simona.Input.Primary.SqlParams(
                    c.getConfig("sqlParams"),
                    parentPath + "sqlParams.",
                    $tsCfgValidator
                  )
                )
              else None
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
            csvParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.CsvParams
            ],
            influxDb1xParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.InfluxDb1xParams
            ],
            resolution: scala.Option[scala.Long],
            sampleParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.SampleParams
            ],
            scheme: java.lang.String,
            sqlParams: scala.Option[
              SimonaConfig.Simona.Input.Weather.Datasource.SqlParams
            ],
            timestampPattern: scala.Option[java.lang.String]
        )
        object Datasource {
          final case class CoordinateSource(
              csvParams: scala.Option[
                SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource.CsvParams
              ],
              gridModel: java.lang.String,
              sampleParams: scala.Option[
                SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource.SampleParams
              ]
          )
          object CoordinateSource {
            final case class CsvParams(
                csvSep: java.lang.String,
                folderPath: java.lang.String
            )
            object CsvParams {
              def apply(
                  c: com.typesafe.config.Config,
                  parentPath: java.lang.String,
                  $tsCfgValidator: $TsCfgValidator
              ): SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource.CsvParams = {
                SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource
                  .CsvParams(
                    csvSep = $_reqStr(parentPath, c, "csvSep", $tsCfgValidator),
                    folderPath =
                      $_reqStr(parentPath, c, "folderPath", $tsCfgValidator)
                  )
              }
              private def $_reqStr(
                  parentPath: java.lang.String,
                  c: com.typesafe.config.Config,
                  path: java.lang.String,
                  $tsCfgValidator: $TsCfgValidator
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
                  $tsCfgValidator: $TsCfgValidator
              ): SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource.SampleParams = {
                SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource
                  .SampleParams(
                    use = !c.hasPathOrNull("use") || c.getBoolean("use")
                  )
              }
            }

            def apply(
                c: com.typesafe.config.Config,
                parentPath: java.lang.String,
                $tsCfgValidator: $TsCfgValidator
            ): SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource = {
              SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource(
                csvParams =
                  if (c.hasPathOrNull("csvParams"))
                    scala.Some(
                      SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource
                        .CsvParams(
                          c.getConfig("csvParams"),
                          parentPath + "csvParams.",
                          $tsCfgValidator
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
                          $tsCfgValidator
                        )
                    )
                  else None
              )
            }
          }

          final case class CouchbaseParams(
              bucketName: java.lang.String,
              coordinateColumnName: java.lang.String,
              keyPrefix: java.lang.String,
              password: java.lang.String,
              url: java.lang.String,
              userName: java.lang.String
          )
          object CouchbaseParams {
            def apply(
                c: com.typesafe.config.Config,
                parentPath: java.lang.String,
                $tsCfgValidator: $TsCfgValidator
            ): SimonaConfig.Simona.Input.Weather.Datasource.CouchbaseParams = {
              SimonaConfig.Simona.Input.Weather.Datasource.CouchbaseParams(
                bucketName =
                  $_reqStr(parentPath, c, "bucketName", $tsCfgValidator),
                coordinateColumnName = $_reqStr(
                  parentPath,
                  c,
                  "coordinateColumnName",
                  $tsCfgValidator
                ),
                keyPrefix =
                  $_reqStr(parentPath, c, "keyPrefix", $tsCfgValidator),
                password = $_reqStr(parentPath, c, "password", $tsCfgValidator),
                url = $_reqStr(parentPath, c, "url", $tsCfgValidator),
                userName = $_reqStr(parentPath, c, "userName", $tsCfgValidator)
              )
            }
            private def $_reqStr(
                parentPath: java.lang.String,
                c: com.typesafe.config.Config,
                path: java.lang.String,
                $tsCfgValidator: $TsCfgValidator
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

          final case class CsvParams(
              csvSep: java.lang.String,
              folderPath: java.lang.String
          )
          object CsvParams {
            def apply(
                c: com.typesafe.config.Config,
                parentPath: java.lang.String,
                $tsCfgValidator: $TsCfgValidator
            ): SimonaConfig.Simona.Input.Weather.Datasource.CsvParams = {
              SimonaConfig.Simona.Input.Weather.Datasource.CsvParams(
                csvSep = $_reqStr(parentPath, c, "csvSep", $tsCfgValidator),
                folderPath =
                  $_reqStr(parentPath, c, "folderPath", $tsCfgValidator)
              )
            }
            private def $_reqStr(
                parentPath: java.lang.String,
                c: com.typesafe.config.Config,
                path: java.lang.String,
                $tsCfgValidator: $TsCfgValidator
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
              url: java.lang.String
          )
          object InfluxDb1xParams {
            def apply(
                c: com.typesafe.config.Config,
                parentPath: java.lang.String,
                $tsCfgValidator: $TsCfgValidator
            ): SimonaConfig.Simona.Input.Weather.Datasource.InfluxDb1xParams = {
              SimonaConfig.Simona.Input.Weather.Datasource.InfluxDb1xParams(
                database = $_reqStr(parentPath, c, "database", $tsCfgValidator),
                port = $_reqInt(parentPath, c, "port", $tsCfgValidator),
                url = $_reqStr(parentPath, c, "url", $tsCfgValidator)
              )
            }
            private def $_reqInt(
                parentPath: java.lang.String,
                c: com.typesafe.config.Config,
                path: java.lang.String,
                $tsCfgValidator: $TsCfgValidator
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
                $tsCfgValidator: $TsCfgValidator
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
                $tsCfgValidator: $TsCfgValidator
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
              timeColumnName: java.lang.String,
              userName: java.lang.String,
              weatherTableName: java.lang.String
          )
          object SqlParams {
            def apply(
                c: com.typesafe.config.Config,
                parentPath: java.lang.String,
                $tsCfgValidator: $TsCfgValidator
            ): SimonaConfig.Simona.Input.Weather.Datasource.SqlParams = {
              SimonaConfig.Simona.Input.Weather.Datasource.SqlParams(
                jdbcUrl = $_reqStr(parentPath, c, "jdbcUrl", $tsCfgValidator),
                password = $_reqStr(parentPath, c, "password", $tsCfgValidator),
                schemaName =
                  if (c.hasPathOrNull("schemaName")) c.getString("schemaName")
                  else "public",
                timeColumnName =
                  $_reqStr(parentPath, c, "timeColumnName", $tsCfgValidator),
                userName = $_reqStr(parentPath, c, "userName", $tsCfgValidator),
                weatherTableName =
                  $_reqStr(parentPath, c, "weatherTableName", $tsCfgValidator)
              )
            }
            private def $_reqStr(
                parentPath: java.lang.String,
                c: com.typesafe.config.Config,
                path: java.lang.String,
                $tsCfgValidator: $TsCfgValidator
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
              $tsCfgValidator: $TsCfgValidator
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
                  $tsCfgValidator
                ),
              couchbaseParams =
                if (c.hasPathOrNull("couchbaseParams"))
                  scala.Some(
                    SimonaConfig.Simona.Input.Weather.Datasource
                      .CouchbaseParams(
                        c.getConfig("couchbaseParams"),
                        parentPath + "couchbaseParams.",
                        $tsCfgValidator
                      )
                  )
                else None,
              csvParams =
                if (c.hasPathOrNull("csvParams"))
                  scala.Some(
                    SimonaConfig.Simona.Input.Weather.Datasource.CsvParams(
                      c.getConfig("csvParams"),
                      parentPath + "csvParams.",
                      $tsCfgValidator
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
                        $tsCfgValidator
                      )
                  )
                else None,
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
                      $tsCfgValidator
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
                      $tsCfgValidator
                    )
                  )
                else None,
              timestampPattern =
                if (c.hasPathOrNull("timestampPattern"))
                  Some(c.getString("timestampPattern"))
                else None
            )
          }
        }

        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): SimonaConfig.Simona.Input.Weather = {
          SimonaConfig.Simona.Input.Weather(
            datasource = SimonaConfig.Simona.Input.Weather.Datasource(
              if (c.hasPathOrNull("datasource")) c.getConfig("datasource")
              else
                com.typesafe.config.ConfigFactory.parseString("datasource{}"),
              parentPath + "datasource.",
              $tsCfgValidator
            )
          )
        }
      }

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): SimonaConfig.Simona.Input = {
        SimonaConfig.Simona.Input(
          grid = SimonaConfig.Simona.Input.Grid(
            if (c.hasPathOrNull("grid")) c.getConfig("grid")
            else com.typesafe.config.ConfigFactory.parseString("grid{}"),
            parentPath + "grid.",
            $tsCfgValidator
          ),
          primary = SimonaConfig.Simona.Input.Primary(
            if (c.hasPathOrNull("primary")) c.getConfig("primary")
            else com.typesafe.config.ConfigFactory.parseString("primary{}"),
            parentPath + "primary.",
            $tsCfgValidator
          ),
          weather = SimonaConfig.Simona.Input.Weather(
            if (c.hasPathOrNull("weather")) c.getConfig("weather")
            else com.typesafe.config.ConfigFactory.parseString("weather{}"),
            parentPath + "weather.",
            $tsCfgValidator
          )
        )
      }
    }

    final case class Output(
        base: SimonaConfig.Simona.Output.Base,
        grid: SimonaConfig.GridOutputConfig,
        participant: SimonaConfig.Simona.Output.Participant,
        sink: SimonaConfig.Simona.Output.Sink
    )
    object Output {
      final case class Base(
          addTimestampToOutputDir: scala.Boolean,
          dir: java.lang.String
      )
      object Base {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): SimonaConfig.Simona.Output.Base = {
          SimonaConfig.Simona.Output.Base(
            addTimestampToOutputDir = !c.hasPathOrNull(
              "addTimestampToOutputDir"
            ) || c.getBoolean("addTimestampToOutputDir"),
            dir = $_reqStr(parentPath, c, "dir", $tsCfgValidator)
          )
        }
        private def $_reqStr(
            parentPath: java.lang.String,
            c: com.typesafe.config.Config,
            path: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
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

      final case class Participant(
          defaultConfig: SimonaConfig.BaseOutputConfig,
          individualConfigs: scala.List[SimonaConfig.BaseOutputConfig]
      )
      object Participant {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): SimonaConfig.Simona.Output.Participant = {
          SimonaConfig.Simona.Output.Participant(
            defaultConfig = SimonaConfig.BaseOutputConfig(
              if (c.hasPathOrNull("defaultConfig")) c.getConfig("defaultConfig")
              else
                com.typesafe.config.ConfigFactory
                  .parseString("defaultConfig{}"),
              parentPath + "defaultConfig.",
              $tsCfgValidator
            ),
            individualConfigs = $_LSimonaConfig_BaseOutputConfig(
              c.getList("individualConfigs"),
              parentPath,
              $tsCfgValidator
            )
          )
        }
        private def $_LSimonaConfig_BaseOutputConfig(
            cl: com.typesafe.config.ConfigList,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): scala.List[SimonaConfig.BaseOutputConfig] = {
          import scala.jdk.CollectionConverters._
          cl.asScala
            .map(cv =>
              SimonaConfig.BaseOutputConfig(
                cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                parentPath,
                $tsCfgValidator
              )
            )
            .toList
        }
      }

      final case class Sink(
          csv: scala.Option[SimonaConfig.Simona.Output.Sink.Csv],
          influxDb1x: scala.Option[SimonaConfig.Simona.Output.Sink.InfluxDb1x]
      )
      object Sink {
        final case class Csv(
            fileFormat: java.lang.String,
            filePrefix: java.lang.String,
            fileSuffix: java.lang.String
        )
        object Csv {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Output.Sink.Csv = {
            SimonaConfig.Simona.Output.Sink.Csv(
              fileFormat =
                if (c.hasPathOrNull("fileFormat")) c.getString("fileFormat")
                else ".csv",
              filePrefix =
                if (c.hasPathOrNull("filePrefix")) c.getString("filePrefix")
                else "",
              fileSuffix =
                if (c.hasPathOrNull("fileSuffix")) c.getString("fileSuffix")
                else ""
            )
          }
        }

        final case class InfluxDb1x(
            database: java.lang.String,
            port: scala.Int,
            url: java.lang.String
        )
        object InfluxDb1x {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Output.Sink.InfluxDb1x = {
            SimonaConfig.Simona.Output.Sink.InfluxDb1x(
              database = $_reqStr(parentPath, c, "database", $tsCfgValidator),
              port = $_reqInt(parentPath, c, "port", $tsCfgValidator),
              url = $_reqStr(parentPath, c, "url", $tsCfgValidator)
            )
          }
          private def $_reqInt(
              parentPath: java.lang.String,
              c: com.typesafe.config.Config,
              path: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
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
              $tsCfgValidator: $TsCfgValidator
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
            $tsCfgValidator: $TsCfgValidator
        ): SimonaConfig.Simona.Output.Sink = {
          SimonaConfig.Simona.Output.Sink(
            csv =
              if (c.hasPathOrNull("csv"))
                scala.Some(
                  SimonaConfig.Simona.Output.Sink.Csv(
                    c.getConfig("csv"),
                    parentPath + "csv.",
                    $tsCfgValidator
                  )
                )
              else None,
            influxDb1x =
              if (c.hasPathOrNull("influxDb1x"))
                scala.Some(
                  SimonaConfig.Simona.Output.Sink.InfluxDb1x(
                    c.getConfig("influxDb1x"),
                    parentPath + "influxDb1x.",
                    $tsCfgValidator
                  )
                )
              else None
          )
        }
      }

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): SimonaConfig.Simona.Output = {
        SimonaConfig.Simona.Output(
          base = SimonaConfig.Simona.Output.Base(
            if (c.hasPathOrNull("base")) c.getConfig("base")
            else com.typesafe.config.ConfigFactory.parseString("base{}"),
            parentPath + "base.",
            $tsCfgValidator
          ),
          grid = SimonaConfig.GridOutputConfig(
            if (c.hasPathOrNull("grid")) c.getConfig("grid")
            else com.typesafe.config.ConfigFactory.parseString("grid{}"),
            parentPath + "grid.",
            $tsCfgValidator
          ),
          participant = SimonaConfig.Simona.Output.Participant(
            if (c.hasPathOrNull("participant")) c.getConfig("participant")
            else com.typesafe.config.ConfigFactory.parseString("participant{}"),
            parentPath + "participant.",
            $tsCfgValidator
          ),
          sink = SimonaConfig.Simona.Output.Sink(
            if (c.hasPathOrNull("sink")) c.getConfig("sink")
            else com.typesafe.config.ConfigFactory.parseString("sink{}"),
            parentPath + "sink.",
            $tsCfgValidator
          )
        )
      }
    }

    final case class Powerflow(
        maxSweepPowerDeviation: scala.Double,
        newtonraphson: SimonaConfig.Simona.Powerflow.Newtonraphson,
        resolution: java.time.Duration,
        sweepTimeout: java.time.Duration
    )
    object Powerflow {
      final case class Newtonraphson(
          epsilon: scala.List[scala.Double],
          iterations: scala.Int
      )
      object Newtonraphson {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): SimonaConfig.Simona.Powerflow.Newtonraphson = {
          SimonaConfig.Simona.Powerflow.Newtonraphson(
            epsilon =
              $_L$_dbl(c.getList("epsilon"), parentPath, $tsCfgValidator),
            iterations = $_reqInt(parentPath, c, "iterations", $tsCfgValidator)
          )
        }
        private def $_reqInt(
            parentPath: java.lang.String,
            c: com.typesafe.config.Config,
            path: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
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
          $tsCfgValidator: $TsCfgValidator
      ): SimonaConfig.Simona.Powerflow = {
        SimonaConfig.Simona.Powerflow(
          maxSweepPowerDeviation =
            $_reqDbl(parentPath, c, "maxSweepPowerDeviation", $tsCfgValidator),
          newtonraphson = SimonaConfig.Simona.Powerflow.Newtonraphson(
            if (c.hasPathOrNull("newtonraphson")) c.getConfig("newtonraphson")
            else
              com.typesafe.config.ConfigFactory.parseString("newtonraphson{}"),
            parentPath + "newtonraphson.",
            $tsCfgValidator
          ),
          resolution =
            if (c.hasPathOrNull("resolution")) c.getDuration("resolution")
            else java.time.Duration.parse("PT1H"),
          sweepTimeout =
            if (c.hasPathOrNull("sweepTimeout")) c.getDuration("sweepTimeout")
            else java.time.Duration.parse("PT30S")
        )
      }
      private def $_reqDbl(
          parentPath: java.lang.String,
          c: com.typesafe.config.Config,
          path: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
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
        participant: SimonaConfig.Simona.Runtime.Participant,
        selected_subgrids: scala.Option[scala.List[scala.Int]],
        selected_volt_lvls: scala.Option[scala.List[SimonaConfig.VoltLvlConfig]]
    )
    object Runtime {
      final case class Participant(
          evcs: SimonaConfig.Simona.Runtime.Participant.Evcs,
          fixedFeedIn: SimonaConfig.Simona.Runtime.Participant.FixedFeedIn,
          load: SimonaConfig.Simona.Runtime.Participant.Load,
          pv: SimonaConfig.Simona.Runtime.Participant.Pv,
          requestVoltageDeviationThreshold: scala.Double,
          wec: SimonaConfig.Simona.Runtime.Participant.Wec
      )
      object Participant {
        final case class Evcs(
            defaultConfig: SimonaConfig.EvcsRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.EvcsRuntimeConfig]
        )
        object Evcs {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Runtime.Participant.Evcs = {
            SimonaConfig.Simona.Runtime.Participant.Evcs(
              defaultConfig = SimonaConfig.EvcsRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator
              ),
              individualConfigs = $_LSimonaConfig_EvcsRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator
              )
            )
          }
          private def $_LSimonaConfig_EvcsRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): scala.List[SimonaConfig.EvcsRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.EvcsRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator
                )
              )
              .toList
          }
        }

        final case class FixedFeedIn(
            defaultConfig: SimonaConfig.FixedFeedInRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.FixedFeedInRuntimeConfig]
        )
        object FixedFeedIn {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Runtime.Participant.FixedFeedIn = {
            SimonaConfig.Simona.Runtime.Participant.FixedFeedIn(
              defaultConfig = SimonaConfig.FixedFeedInRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator
              ),
              individualConfigs = $_LSimonaConfig_FixedFeedInRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator
              )
            )
          }
          private def $_LSimonaConfig_FixedFeedInRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): scala.List[SimonaConfig.FixedFeedInRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.FixedFeedInRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator
                )
              )
              .toList
          }
        }

        final case class Load(
            defaultConfig: SimonaConfig.LoadRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.LoadRuntimeConfig]
        )
        object Load {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Runtime.Participant.Load = {
            SimonaConfig.Simona.Runtime.Participant.Load(
              defaultConfig = SimonaConfig.LoadRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator
              ),
              individualConfigs = $_LSimonaConfig_LoadRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator
              )
            )
          }
          private def $_LSimonaConfig_LoadRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): scala.List[SimonaConfig.LoadRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.LoadRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator
                )
              )
              .toList
          }
        }

        final case class Pv(
            defaultConfig: SimonaConfig.PvRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.PvRuntimeConfig]
        )
        object Pv {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Runtime.Participant.Pv = {
            SimonaConfig.Simona.Runtime.Participant.Pv(
              defaultConfig = SimonaConfig.PvRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator
              ),
              individualConfigs = $_LSimonaConfig_PvRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator
              )
            )
          }
          private def $_LSimonaConfig_PvRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): scala.List[SimonaConfig.PvRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.PvRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator
                )
              )
              .toList
          }
        }

        final case class Wec(
            defaultConfig: SimonaConfig.WecRuntimeConfig,
            individualConfigs: scala.List[SimonaConfig.WecRuntimeConfig]
        )
        object Wec {
          def apply(
              c: com.typesafe.config.Config,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): SimonaConfig.Simona.Runtime.Participant.Wec = {
            SimonaConfig.Simona.Runtime.Participant.Wec(
              defaultConfig = SimonaConfig.WecRuntimeConfig(
                if (c.hasPathOrNull("defaultConfig"))
                  c.getConfig("defaultConfig")
                else
                  com.typesafe.config.ConfigFactory
                    .parseString("defaultConfig{}"),
                parentPath + "defaultConfig.",
                $tsCfgValidator
              ),
              individualConfigs = $_LSimonaConfig_WecRuntimeConfig(
                c.getList("individualConfigs"),
                parentPath,
                $tsCfgValidator
              )
            )
          }
          private def $_LSimonaConfig_WecRuntimeConfig(
              cl: com.typesafe.config.ConfigList,
              parentPath: java.lang.String,
              $tsCfgValidator: $TsCfgValidator
          ): scala.List[SimonaConfig.WecRuntimeConfig] = {
            import scala.jdk.CollectionConverters._
            cl.asScala
              .map(cv =>
                SimonaConfig.WecRuntimeConfig(
                  cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
                  parentPath,
                  $tsCfgValidator
                )
              )
              .toList
          }
        }

        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): SimonaConfig.Simona.Runtime.Participant = {
          SimonaConfig.Simona.Runtime.Participant(
            evcs = SimonaConfig.Simona.Runtime.Participant.Evcs(
              if (c.hasPathOrNull("evcs")) c.getConfig("evcs")
              else com.typesafe.config.ConfigFactory.parseString("evcs{}"),
              parentPath + "evcs.",
              $tsCfgValidator
            ),
            fixedFeedIn = SimonaConfig.Simona.Runtime.Participant.FixedFeedIn(
              if (c.hasPathOrNull("fixedFeedIn")) c.getConfig("fixedFeedIn")
              else
                com.typesafe.config.ConfigFactory.parseString("fixedFeedIn{}"),
              parentPath + "fixedFeedIn.",
              $tsCfgValidator
            ),
            load = SimonaConfig.Simona.Runtime.Participant.Load(
              if (c.hasPathOrNull("load")) c.getConfig("load")
              else com.typesafe.config.ConfigFactory.parseString("load{}"),
              parentPath + "load.",
              $tsCfgValidator
            ),
            pv = SimonaConfig.Simona.Runtime.Participant.Pv(
              if (c.hasPathOrNull("pv")) c.getConfig("pv")
              else com.typesafe.config.ConfigFactory.parseString("pv{}"),
              parentPath + "pv.",
              $tsCfgValidator
            ),
            requestVoltageDeviationThreshold =
              if (c.hasPathOrNull("requestVoltageDeviationThreshold"))
                c.getDouble("requestVoltageDeviationThreshold")
              else 1e-14,
            wec = SimonaConfig.Simona.Runtime.Participant.Wec(
              if (c.hasPathOrNull("wec")) c.getConfig("wec")
              else com.typesafe.config.ConfigFactory.parseString("wec{}"),
              parentPath + "wec.",
              $tsCfgValidator
            )
          )
        }
      }

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): SimonaConfig.Simona.Runtime = {
        SimonaConfig.Simona.Runtime(
          participant = SimonaConfig.Simona.Runtime.Participant(
            if (c.hasPathOrNull("participant")) c.getConfig("participant")
            else com.typesafe.config.ConfigFactory.parseString("participant{}"),
            parentPath + "participant.",
            $tsCfgValidator
          ),
          selected_subgrids =
            if (c.hasPathOrNull("selected_subgrids"))
              scala.Some(
                $_L$_int(
                  c.getList("selected_subgrids"),
                  parentPath,
                  $tsCfgValidator
                )
              )
            else None,
          selected_volt_lvls =
            if (c.hasPathOrNull("selected_volt_lvls"))
              scala.Some(
                $_LSimonaConfig_VoltLvlConfig(
                  c.getList("selected_volt_lvls"),
                  parentPath,
                  $tsCfgValidator
                )
              )
            else None
        )
      }
      private def $_LSimonaConfig_VoltLvlConfig(
          cl: com.typesafe.config.ConfigList,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): scala.List[SimonaConfig.VoltLvlConfig] = {
        import scala.jdk.CollectionConverters._
        cl.asScala
          .map(cv =>
            SimonaConfig.VoltLvlConfig(
              cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
              parentPath,
              $tsCfgValidator
            )
          )
          .toList
      }
    }

    final case class Time(
        endDateTime: java.lang.String,
        schedulerReadyCheckWindow: scala.Option[scala.Int],
        startDateTime: java.lang.String,
        stopOnFailedPowerFlow: scala.Boolean
    )
    object Time {
      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): SimonaConfig.Simona.Time = {
        SimonaConfig.Simona.Time(
          endDateTime =
            if (c.hasPathOrNull("endDateTime")) c.getString("endDateTime")
            else "2011-05-01 01:00:00",
          schedulerReadyCheckWindow =
            if (c.hasPathOrNull("schedulerReadyCheckWindow"))
              Some(c.getInt("schedulerReadyCheckWindow"))
            else None,
          startDateTime =
            if (c.hasPathOrNull("startDateTime")) c.getString("startDateTime")
            else "2011-05-01 00:00:00",
          stopOnFailedPowerFlow =
            c.hasPathOrNull("stopOnFailedPowerFlow") && c.getBoolean(
              "stopOnFailedPowerFlow"
            )
        )
      }
    }

    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): SimonaConfig.Simona = {
      SimonaConfig.Simona(
        event = SimonaConfig.Simona.Event(
          if (c.hasPathOrNull("event")) c.getConfig("event")
          else com.typesafe.config.ConfigFactory.parseString("event{}"),
          parentPath + "event.",
          $tsCfgValidator
        ),
        gridConfig = SimonaConfig.Simona.GridConfig(
          if (c.hasPathOrNull("gridConfig")) c.getConfig("gridConfig")
          else com.typesafe.config.ConfigFactory.parseString("gridConfig{}"),
          parentPath + "gridConfig.",
          $tsCfgValidator
        ),
        input = SimonaConfig.Simona.Input(
          if (c.hasPathOrNull("input")) c.getConfig("input")
          else com.typesafe.config.ConfigFactory.parseString("input{}"),
          parentPath + "input.",
          $tsCfgValidator
        ),
        output = SimonaConfig.Simona.Output(
          if (c.hasPathOrNull("output")) c.getConfig("output")
          else com.typesafe.config.ConfigFactory.parseString("output{}"),
          parentPath + "output.",
          $tsCfgValidator
        ),
        powerflow = SimonaConfig.Simona.Powerflow(
          if (c.hasPathOrNull("powerflow")) c.getConfig("powerflow")
          else com.typesafe.config.ConfigFactory.parseString("powerflow{}"),
          parentPath + "powerflow.",
          $tsCfgValidator
        ),
        runtime = SimonaConfig.Simona.Runtime(
          if (c.hasPathOrNull("runtime")) c.getConfig("runtime")
          else com.typesafe.config.ConfigFactory.parseString("runtime{}"),
          parentPath + "runtime.",
          $tsCfgValidator
        ),
        simulationName =
          $_reqStr(parentPath, c, "simulationName", $tsCfgValidator),
        time = SimonaConfig.Simona.Time(
          if (c.hasPathOrNull("time")) c.getConfig("time")
          else com.typesafe.config.ConfigFactory.parseString("time{}"),
          parentPath + "time.",
          $tsCfgValidator
        )
      )
    }
    private def $_reqStr(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
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
        $tsCfgValidator
      )
    )
    $tsCfgValidator.validate()
    $result
  }

  private def $_L$_dbl(
      cl: com.typesafe.config.ConfigList,
      parentPath: java.lang.String,
      $tsCfgValidator: $TsCfgValidator
  ): scala.List[scala.Double] = {
    import scala.jdk.CollectionConverters._
    cl.asScala.map(cv => $_dbl(cv)).toList
  }
  private def $_L$_int(
      cl: com.typesafe.config.ConfigList,
      parentPath: java.lang.String,
      $tsCfgValidator: $TsCfgValidator
  ): scala.List[scala.Int] = {
    import scala.jdk.CollectionConverters._
    cl.asScala.map(cv => $_int(cv)).toList
  }
  private def $_L$_str(
      cl: com.typesafe.config.ConfigList,
      parentPath: java.lang.String,
      $tsCfgValidator: $TsCfgValidator
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
      exp: java.lang.String
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
        e: com.typesafe.config.ConfigException
    ): Unit = {
      badPaths += s"'$path': ${e.getClass.getName}(${e.getMessage})"
    }

    def addInvalidEnumValue(
        path: java.lang.String,
        value: java.lang.String,
        enumName: java.lang.String
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
