/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.event.listener.SimonaListenerCompanion
import edu.ie3.util.scala.ReflectionTools
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
      selectedSubnets: Option[String] = None,
      selectedVoltLvls: Option[String] = None,
      clusterType: Option[ClusterType] = None,
      nodeHost: Option[String] = None,
      nodePort: Option[String] = None,
      seedAddress: Option[String] = None,
      useLocalWorker: Option[Boolean] = None,
      tArgs: Map[String, String] = Map.empty,
      extAddress: Option[String] = None,
      mappingPath: Option[String] = None,
  ) {
    val useCluster: Boolean = clusterType.isDefined
  }

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
      opt[Map[String, String]]("tArgs")
        .action((x, c) => c.copy(tArgs = x))
        .text(
          "Comma separated list (no whitespaces!) of substitution arguments for simona config."
        )
      opt[String](name = "subnets")
        .action((value, args) => args.copy(selectedSubnets = Some(value)))
        .text("Comma separated list (no whitespaces!) of selected subnets.")
      opt[String](name = "voltlevels")
        .action((value, args) => args.copy(selectedVoltLvls = Some(value)))
        .text("Comma separated list (no whitespaces!) of selected volt levels.")
      opt[String]("cluster-type")
        .action((value, args) =>
          args.copy(clusterType = value.trim.toLowerCase match {
            case "master" => Some(MasterNode)
            case "seed"   => Some(SeedNode)
            case _        => None
          })
        )
        .text("If running as a cluster, specify master or seed node.")
      opt[String]("node-host")
        .action((value, args) => args.copy(nodeHost = Option(value)))
        .validate(value =>
          if (value.trim.isEmpty) failure("node-host cannot be empty")
          else success
        )
        .text("Host used to run the remote actor system")
      opt[String]("node-port")
        .action((value, args) => args.copy(nodePort = Option(value)))
        .validate(value =>
          if (value.trim.isEmpty) failure("node-port cannot be empty")
          else success
        )
        .text("Port used to run the remote actor system")
      opt[String]("seed-address")
        .action((value, args) => args.copy(seedAddress = Option(value)))
        .validate(value =>
          if (value.trim.isEmpty) failure("seed-address cannot be empty")
          else success
        )
        .text(
          "Comma separated list (no whitespaces!) of initial addresses used for the rest of the cluster to bootstrap"
        )
      opt[Boolean]("use-local-worker")
        .action((value, args) => args.copy(useLocalWorker = Some(value)))
        .text(
          "Boolean determining whether to use a local worker. " +
            "If cluster is NOT enabled this defaults to true and cannot be false. " +
            "If cluster is specified then this defaults to false and must be explicitly set to true. " +
            "NOTE: For cluster, this will ONLY be checked if cluster-type=master"
        )
      opt[String]("ext-address")
        .action((value, args) => args.copy(extAddress = Option(value)))
        .validate(value =>
          if (value.trim.isEmpty) failure("ext-address cannot be empty")
          else success
        )
        .text(
          "Comma separated list (no whitespaces!) of initial addresses used for the rest of the cluster to bootstrap"
        )
      opt[String]("mapping-path")
        .action((value, args) => args.copy(mappingPath = Option(value)))
        .validate(value =>
          if (value.trim.isEmpty) failure("ext-address cannot be empty")
          else success
        )
        .text(
          "Comma separated list (no whitespaces!) of initial addresses used for the rest of the cluster to bootstrap"
        )

      checkConfig(args =>
        if (
          args.useCluster && (args.nodeHost.isEmpty || args.nodePort.isEmpty || args.seedAddress.isEmpty)
        )
          failure(
            "If using the cluster then node-host, node-port, and seed-address are required"
          )
        else if (args.useCluster && !args.useLocalWorker.getOrElse(true))
          failure(
            "If using the cluster then use-local-worker MUST be true (or unprovided)"
          )
        else success
      )

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

  // sealed trait for cluster type
  sealed trait ClusterType

  case object MasterNode extends ClusterType {
    override def toString = "master"
  }

  case object SeedNode extends ClusterType {
    override def toString = "worker"
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
        s"""config = "${parsedArgs.configLocation.get.replace("\\", "\\\\")}"
           |simona.runtime_configuration {
           |  selected_subnets = [${parsedArgs.selectedSubnets.getOrElse("")}]
           |  selected_volt_lvls = [${parsedArgs.selectedVoltLvls
            .getOrElse("")}]
           |}
           |""".stripMargin
      )

    val tArgsSubstitution = ConfigFactory.parseMap(parsedArgs.tArgs.asJava)

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
