/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.{ConfigException, Config => TypesafeConfig}
import edu.ie3.simona.exceptions.InvalidConfigParameterException

import java.io.File
import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object ConfigProvider {

  def deriveConfigs(
      config: TypesafeConfig
  ): Map[SimonaConfig, TypesafeConfig] = {
    // check if config contains identifier for experiments config
    Try(config.getConfigList("simona.experiments.configs")) match {
      case Failure(_: ConfigException.Missing) =>
        // if we cannot find this identifier, we assume a single config and try to parse this
        Map(SimonaConfig(config) -> config)
      case Failure(unknownEx) =>
        throw unknownEx
      case Success(_) =>
        // we have an experiments list, try to parse and get all configs
        ExperimentsConfig(config).simona.experiments.configs
          .map(exCfg => {
            val typesafeCfg = ArgsParser.parseTypesafeConfig(
              deriveConfigLocation(exCfg.config, config.getString("config"))
            )
            Try(SimonaConfig(typesafeCfg)) match {
              case Failure(exception) =>
                // re-throw exception with context
                throw new InvalidConfigParameterException(
                  s"Cannot instantiate SimonaConfig '${exCfg.config}' ."
                ).initCause(exception)
              case Success(simonaConfig) => simonaConfig -> typesafeCfg
            }
          })
          .toMap
    }
  }

  def deriveExperimentsConfig(args: Array[String]): Try[ExperimentsConfig] =
    Try(ExperimentsConfig(ArgsParser.prepareConfig(args)._2))

  private def deriveConfigLocation(
      cfg: String,
      experimentsConfigPath: String
  ): File = {
    val cfgFile = Paths.get(cfg).toFile
    if (!cfgFile.exists()) {
      // try to derive the config path as subfolder of experiments config
      val alternativeCfgPath = Paths
        .get(experimentsConfigPath)
        .getParent
        .toAbsolutePath
        .resolve(cfg)
        .toFile
      if (alternativeCfgPath.exists()) {
        alternativeCfgPath
      } else {
        throw new Exception(s"Config file $cfgFile does not exist!")
      }
    } else {
      cfgFile
    }
  }

}
