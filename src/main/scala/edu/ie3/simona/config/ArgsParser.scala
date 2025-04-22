/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}
import com.typesafe.scalalogging.LazyLogging
import scopt.{OptionParser => scoptOptionParser}

import java.io.File
import java.nio.file.Paths
import scala.jdk.CollectionConverters._

object ArgsParser extends LazyLogging {

  // case class for allowed arguments
  final case class Arguments(
      mainArgs: Array[String],
      configLocation: Option[String] = None,
      config: Option[TypesafeConfig] = None,
  )

  // build the config parser using scopt library
  private def buildParser: scoptOptionParser[Arguments] = {
    new scoptOptionParser[Arguments]("simona") {
      opt[String]("config")
        .action((value, args) => {
          args.copy(
            config = Some(parseTypesafeConfig(value)),
            configLocation = Option(value),
          )
        })
        .validate(value =>
          if (value.trim.isEmpty) failure("config location cannot be empty")
          else success
        )
        .text("Location of the simona config file")
        .minOccurs(1)
    }
  }

  private def parse(
      parser: scoptOptionParser[Arguments],
      args: Array[String],
  ): Option[Arguments] =
    parser.parse(args, init = Arguments(args))

  def parse(args: Array[String]): Option[Arguments] = parse(buildParser, args)

  private def parseTypesafeConfig(fileName: String): TypesafeConfig = {
    val file = Paths.get(fileName).toFile
    if (!file.exists())
      throw new Exception(s"Missing config file on path $fileName")
    parseTypesafeConfig(file)
  }

  private def parseTypesafeConfig(file: File): TypesafeConfig = {
    ConfigFactory
      .parseFile(file)
      .withFallback(
        ConfigFactory.parseMap(
          Map("simona.inputDirectory" -> file.getAbsoluteFile.getParent).asJava
        )
      )
  }

  /** Prepare the config by parsing the provided program arguments
    *
    * @param args
    *   the provided arguments
    * @return
    *   a tuple of the parsed arguments and the result of parsing the provided
    *   config as [[TypesafeConfig]]
    */
  def prepareConfig(args: Array[String]): (Arguments, TypesafeConfig) = {

    val parsedArgs = parse(args) match {
      case Some(pArgs) => pArgs
      case None =>
        System.exit(-1)
        throw new IllegalArgumentException(
          "Unable to parse provided Arguments."
        )
    }

    // check if a config is provided
    val parsedArgsConfig = parsedArgs.config match {
      case None =>
        throw new RuntimeException(
          "Please provide a valid config file via --config <path-to-config-file>."
        )
      case Some(parsedArgsConfig) => parsedArgsConfig
    }

    val argsConfig =
      ConfigFactory.parseString(
        s"""config = "${parsedArgs.configLocation.get.replace("\\", "\\\\")}""""
      )

    // note: this overrides the default config values provided in the config file!
    // THE ORDER OF THE CALLS MATTERS -> the later the call, the more "fallback" -> first config is always the primary one!
    // hence if you add some more program arguments, you have to add them before(!) your default config!
    // see https://github.com/lightbend/config#merging-config-trees for details on merging configs
    val config = argsConfig
      .withFallback(parsedArgsConfig)
      .resolve()

    (parsedArgs, config)
  }

}
