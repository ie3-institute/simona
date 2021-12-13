/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import java.io.File
import java.nio.file.Paths
import com.typesafe.config.{
  ConfigFactory,
  ConfigRenderOptions,
  Config => TypesafeConfig
}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.event.listener.SimonaListenerCompanion
import edu.ie3.util.scala.ReflectionTools
import scopt.{OptionParser => scoptOptionParser}

import scala.jdk.CollectionConverters._

object ArgsParser extends LazyLogging {

  // case class for allowed arguments
  final case class Arguments(
      mainArgs: Array[String],
      configLocation: Option[String] = None,
      config: Option[TypesafeConfig] = None,
      selectedSubnets: Option[String] = None,
      selectedVoltLvls: Option[String] = None,
      tArgs: Option[String] = None
  )

  // build the config parser using scopt library
  private def buildParser: scoptOptionParser[Arguments] = {
    new scoptOptionParser[Arguments]("simona") {
      opt[String]("config")
        .action((value, args) => {
          args.copy(
            config = Some(parseTypesafeConfig(value)),
            configLocation = Option(value)
          )
        })
        .validate(value =>
          if (value.trim.isEmpty) failure("config location cannot be empty")
          else success
        )
        .validate(value =>
          if (value.contains("\\"))
            failure("wrong config path, expected: /, found: \\")
          else success
        )
        .text("Location of the simona config file")
        .required()
      opt[String]("tArgs")
        .action((value, args) => args.copy(tArgs = Some(value)))
        .text(
          "Comma separated list (no whitespaces!) of substitution arguments for simona config."
        )
      opt[String](name = "subnets")
        .action((value, args) => args.copy(selectedSubnets = Some(value)))
        .text("Comma separated list (no whitespaces!) of selected subnets.")
      opt[String](name = "voltlevels")
        .action((value, args) => args.copy(selectedVoltLvls = Some(value)))
        .text("Comma separated list (no whitespaces!) of selected volt levels.")
    }
  }

  private def parse(
      parser: scoptOptionParser[Arguments],
      args: Array[String]
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

  /** Parses the given listener configuration tino a map of
    * [[SimonaListenerCompanion]] to an optional list of it's regarding events
    * to process
    *
    * @param listenerConfigOption
    *   Option to a list of listener definitions from config
    * @return
    *   A mapping from [[SimonaListenerCompanion]] to an Option to a list of
    *   events to process
    */
  def parseListenerConfigOption(
      listenerConfigOption: Option[List[SimonaConfig.Simona.Event.Listener$Elm]]
  ): Map[SimonaListenerCompanion, Option[List[String]]] = {
    val clusterSingletonsWithEvents
        : Map[SimonaListenerCompanion, Option[List[String]]] =
      listenerConfigOption match {
        case Some(listenerElems) =>
          listenerElems.foldLeft(
            Map.empty[SimonaListenerCompanion, Option[List[String]]]
          )((listenerMap, listenerElem) =>
            ReflectionTools
              .resolveClassNameToCompanion(listenerElem.fullClassPath) match {
              case Some(listener: SimonaListenerCompanion) =>
                listenerMap + (listener -> listenerElem.eventsToProcess)
              case nonListenerCompanion =>
                logger.warn(
                  s"Invalid value ${nonListenerCompanion.getClass} for 'event.listener' config parameter!"
                )
                listenerMap
            }
          )
        case None =>
          logger.info(
            "No listener assigned in configuration value 'event.listener'. No event are going to be processed!"
          )
          Map.empty[SimonaListenerCompanion, Option[List[String]]]
      }

    clusterSingletonsWithEvents
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

    val parsedArgs = parse(args).getOrElse {
      throw new IllegalArgumentException(
        "Unable to parse provided Arguments."
      )
    }

    // check if a config is provided
    val parsedArgsConfig = parsedArgs.config.getOrElse {
      throw new RuntimeException(
        "Please provide a valid config file via --config <path-to-config-file>."
      )
    }

    val argsConfig =
      ConfigFactory.parseString(
        s"""config = ${parsedArgs.configLocation.get}
           |simona.runtime_configuration {
           |  selected_subnets = [${parsedArgs.selectedSubnets.getOrElse("")}]
           |  selected_volt_lvls = [${parsedArgs.selectedVoltLvls
          .getOrElse("")}]
           |}
           |""".stripMargin
      )

    val tArgsSubstitution = parsedArgs.tArgs
      .map(_.replace('|', '\n'))
      .map(ConfigFactory.parseString)
      .getOrElse(ConfigFactory.empty())
    if (!tArgsSubstitution.isEmpty) {
      // rendering without comments or whitespaces
      val simonaConfRender =
        tArgsSubstitution
          .root()
          .render(ConfigRenderOptions.concise().setJson(false))
      logger.info(
        s"The following simona configuration has been overwritten by command line arguments (tArgs):\n\t\t$simonaConfRender"
      )
    }

    // note: this overrides the default config values provided in the config file!
    // THE ORDER OF THE CALLS MATTERS -> the later the call, the more "fallback" -> first config is always the primary one!
    // hence if you add some more program arguments, you have to add them before(!) your default config!
    // see https://github.com/lightbend/config#merging-config-trees for details on merging configs
    val config = argsConfig
      .withFallback(tArgsSubstitution)
      .withFallback(parsedArgsConfig)
      .resolve()

    (parsedArgs, config)
  }

}
