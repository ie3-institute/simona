/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

final case class ExperimentsConfig(
    simona: ExperimentsConfig.Simona
)
object ExperimentsConfig {
  final case class Experiment(
      config: java.lang.String
  )
  object Experiment {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): ExperimentsConfig.Experiment = {
      ExperimentsConfig.Experiment(
        config = $_reqStr(parentPath, c, "config", $tsCfgValidator)
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

  final case class Simona(
      experiments: ExperimentsConfig.Simona.Experiments
  )
  object Simona {
    final case class Experiments(
        configs: scala.List[ExperimentsConfig.Experiment],
        log: ExperimentsConfig.Simona.Experiments.Log
    )
    object Experiments {
      final case class Log(
          console: scala.Boolean
      )
      object Log {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): ExperimentsConfig.Simona.Experiments.Log = {
          ExperimentsConfig.Simona.Experiments.Log(
            console = c.hasPathOrNull("console") && c.getBoolean("console")
          )
        }
      }

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): ExperimentsConfig.Simona.Experiments = {
        ExperimentsConfig.Simona.Experiments(
          configs = $_LExperimentsConfig_Experiment(
            c.getList("configs"),
            parentPath,
            $tsCfgValidator
          ),
          log = ExperimentsConfig.Simona.Experiments.Log(
            if (c.hasPathOrNull("log")) c.getConfig("log")
            else com.typesafe.config.ConfigFactory.parseString("log{}"),
            parentPath + "log.",
            $tsCfgValidator
          )
        )
      }
      private def $_LExperimentsConfig_Experiment(
          cl: com.typesafe.config.ConfigList,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): scala.List[ExperimentsConfig.Experiment] = {
        import scala.jdk.CollectionConverters._
        cl.asScala
          .map(cv =>
            ExperimentsConfig.Experiment(
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
    ): ExperimentsConfig.Simona = {
      ExperimentsConfig.Simona(
        experiments = ExperimentsConfig.Simona.Experiments(
          if (c.hasPathOrNull("experiments")) c.getConfig("experiments")
          else com.typesafe.config.ConfigFactory.parseString("experiments{}"),
          parentPath + "experiments.",
          $tsCfgValidator
        )
      )
    }
  }

  def apply(c: com.typesafe.config.Config): ExperimentsConfig = {
    val $tsCfgValidator: $TsCfgValidator = new $TsCfgValidator()
    val parentPath: java.lang.String = ""
    val $result = ExperimentsConfig(
      simona = ExperimentsConfig.Simona(
        if (c.hasPathOrNull("simona")) c.getConfig("simona")
        else com.typesafe.config.ConfigFactory.parseString("simona{}"),
        parentPath + "simona.",
        $tsCfgValidator
      )
    )
    $tsCfgValidator.validate()
    $result
  }
  private final class $TsCfgValidator {
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
