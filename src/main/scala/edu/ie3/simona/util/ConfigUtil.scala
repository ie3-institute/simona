/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.connectors.{CouchbaseConnector, InfluxDbConnector, SqlConnector}
import edu.ie3.datamodel.models.result.connector.{LineResult, SwitchResult, Transformer2WResult, Transformer3WResult}
import edu.ie3.datamodel.io.connectors.{CouchbaseConnector, InfluxDbConnector, SqlConnector}
import edu.ie3.datamodel.models.result.connector.{LineResult, SwitchResult, Transformer2WResult, Transformer3WResult}
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.config.IoConfigUtils.{BaseKafkaParams, BaseSqlParams, CouchbaseParams, CsvParams, SqlParams}
import edu.ie3.simona.config.OutputConfig.{GridOutputConfig, ParticipantOutputConfig, ThermalOutputConfig}
import edu.ie3.simona.config.RuntimeConfig.{BaseRuntimeConfig, RuntimeParticipantsConfig}
import edu.ie3.simona.config.{OutputConfig, SimonaConfig}
import edu.ie3.simona.config.SimonaConfig._
import edu.ie3.simona.event.notifier.{Notifier, NotifierConfig}
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import org.apache.kafka.clients.admin.AdminClient
import org.apache.kafka.common.KafkaException

import java.io.File
import java.util.concurrent.ExecutionException
import java.util.{Properties, UUID}
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try, Using}

object ConfigUtil {

  final case class ParticipantConfigUtil private (
      private val configs: Map[UUID, BaseRuntimeConfig],
      private val defaultConfigs: Map[Class[_], BaseRuntimeConfig],
  ) {

    /** Queries for a [[BaseRuntimeConfig]] of type [[T]], that applies for the
      * given uuid and either returns the config for the requested uuid or the
      * default config for type [[T]].
      *
      * @param uuid
      *   Identifier of the requested load model
      * @return
      *   the requested config or a default value of type [[T]]
      */
    def getOrDefault[T <: BaseRuntimeConfig](
        uuid: UUID
    )(implicit tag: ClassTag[T]): T =
      configs.get(uuid) match {
        case Some(conf: T) => conf
        case _ =>
          defaultConfigs.get(tag.runtimeClass) match {
            case Some(conf: T) => conf
            case _ =>
              throw new RuntimeException(
                s"No config found for $uuid of type ${tag.runtimeClass.getSimpleName}."
              )
          }
      }
  }

  object ParticipantConfigUtil {

    /** Creates a system participant config utility from the given participant
      * configuration. It builds a map from uuid to individual system
      * participants config for faster access.
      *
      * @param subConfig
      *   Configuration sub tree for the behaviour of system participants
      * @return
      *   a matching config utility
      */
    def apply(
        subConfig: RuntimeParticipantsConfig
    ): ParticipantConfigUtil = {
      val default: Map[Class[_], BaseRuntimeConfig] =
        subConfig.asSeq
          .map(_.defaultConfig)
          .map { conf => conf.getClass -> conf }
          .toMap
      val individual =
        buildUuidMapping(
          subConfig.asSeq.flatMap(_.individualConfigs)
        )
      ParticipantConfigUtil(
        individual,
        default,
        /*fixme mh different subConfigs added
        buildUuidMapping(
          Seq(
            subConfig.load.individualConfigs,
            subConfig.fixedFeedIn.individualConfigs,
            subConfig.pv.individualConfigs,
            subConfig.evcs.individualConfigs,
            subConfig.wec.individualConfigs,
            subConfig.storage.individualConfigs,
            subConfig.em.individualConfigs,
          ).flatten
        ),
        Seq(
          subConfig.load.defaultConfig,
          subConfig.fixedFeedIn.defaultConfig,
          subConfig.pv.defaultConfig,
          subConfig.evcs.defaultConfig,
          subConfig.wec.defaultConfig,
          subConfig.hp.defaultConfig,
          subConfig.storage.defaultConfig,
          subConfig.em.defaultConfig,
        ).map { conf => conf.getClass -> conf }.toMap,

         */
      )
    }

    private def buildUuidMapping(
        configs: Seq[BaseRuntimeConfig]
    ): Map[UUID, BaseRuntimeConfig] =
      configs
        .flatMap(modelConfig =>
          modelConfig.uuids
            .map(UUID.fromString(_) -> modelConfig)
        )
        .toMap

  }

  /** A config utility to handle the output configuration for participant
    * models. It holds a map from participant model type to actual config for
    * speeding up processing.
    *
    * @param defaultConfig
    *   Default config to use, when there is no specific one
    * @param configs
    *   Mapping from notifier identifier to it's notifier configuration
    */
  final case class OutputConfigUtil(
      private val defaultConfig: NotifierConfig,
      private val configs: Map[
        NotifierIdentifier.Value,
        NotifierConfig,
      ],
  ) {
    def getOrDefault(
        notifierId: NotifierIdentifier.Value
    ): NotifierConfig =
      configs.getOrElse(notifierId, defaultConfig)

    /** Get all identifiers of [[Notifier]] implementations, that will announce
      * new simulation results
      *
      * @return
      *   A set of applicable notifiers
      */
    def simulationResultIdentifiersToConsider(
        thermal: Boolean
    ): Set[NotifierIdentifier.Value] = {
      if (defaultConfig.simulationResultInfo) {
        val notifiers =
          if (thermal) NotifierIdentifier.getThermalIdentifiers
          else NotifierIdentifier.getParticipantIdentifiers
        /* Generally inform about all simulation results, but not on those, that are explicitly marked */
        notifiers -- configs.flatMap {
          case (
                notifierId,
                NotifierConfig(resultInfo, _, _),
              ) if !resultInfo =>
            Some(notifierId)
          case _ => None
        }
      } else {
        /* Only register those events, that are explicitly marked to be considered */
        configs.flatMap {
          case (
                notifierId,
                NotifierConfig(resultInfo, _, _),
              ) if resultInfo =>
            Some(notifierId)
          case _ => None
        }.toSet
      }
    }

    def simulationResultEntitiesToConsider(
        thermal: Boolean
    ): Set[Class[_ <: ResultEntity]] =
      simulationResultIdentifiersToConsider(thermal).map(notifierId =>
        EntityMapperUtil.getResultEntityClass(notifierId)
      )
  }

  object OutputConfigUtil {
    def apply(
        subConfig: ParticipantOutputConfig
    ): OutputConfigUtil = {
      val defaultConfig = subConfig.defaultConfig match {

        case OutputConfig.BaseOutputConfig(
              _,
              powerRequestReply,
              simulationResult,
              flexResult,
            ) =>
          NotifierConfig(simulationResult, powerRequestReply, flexResult)

      }
      val configMap = subConfig.individualConfigs.map {

        case OutputConfig.BaseOutputConfig(
              notifier,
              powerRequestReply,
              simulationResult,
              flexResult,
            ) =>
          try {
            val id = NotifierIdentifier(notifier)
            id -> NotifierConfig(
              simulationResult,
              powerRequestReply,
              flexResult,
            )
          } catch {
            case e: NoSuchElementException =>
              throw new InvalidConfigParameterException(
                s"Cannot parse $notifier to known result event notifier.",
                e,
              )
          }
      }.toMap
      new OutputConfigUtil(defaultConfig, configMap)
    }

    def apply(
        subConfig: ThermalOutputConfig
    ): OutputConfigUtil = {
      val defaultConfig = subConfig.defaultConfig match {
        case SimpleOutputConfig(_, simulationResult) =>
          NotifierConfig(
            simulationResult,
            powerRequestReply = false,
            flexResult = false,
          )
      }
      val configMap = subConfig.individualConfigs.map {
        case SimpleOutputConfig(notifier, simulationResult) =>
          try {
            val id = NotifierIdentifier(notifier)
            id -> NotifierConfig(
              simulationResult,
              powerRequestReply = false,
              flexResult = false,
            )
          } catch {
            case e: NoSuchElementException =>
              throw new InvalidConfigParameterException(
                s"Cannot parse $notifier to known result event notifier.",
                e,
              )
          }
      }.toMap
      new OutputConfigUtil(defaultConfig, configMap)
    }
  }

  final case class GridOutputConfigUtil(subConfig: GridOutputConfig) {

    /** Determine the set of result entity classes for later consideration based
      * on the grid output configuration
      *
      * @return
      *   Set of result entity classes
      */
    def simulationResultEntitiesToConsider: Set[Class[_ <: ResultEntity]] = {
      val entities = mutable.Set.empty[Class[_ <: ResultEntity]]

      if (subConfig.nodes)
        entities += classOf[NodeResult]
      if (subConfig.lines)
        entities += classOf[LineResult]
      if (subConfig.switches)
        entities += classOf[SwitchResult]
      if (subConfig.transformers2w)
        entities += classOf[Transformer2WResult]
      if (subConfig.transformers3w)
        entities += classOf[Transformer3WResult]

      entities.toSet
    }
  }

  /** Enumeration of known [[Notifier]] implementations including an identifying
    * String, so that they can be identified from e.g. configuration files
    */
  object NotifierIdentifier extends ParsableEnumeration {
    val BioMassPlant: Value = Value("bm")
    val ChpPlant: Value = Value("chp")
    val Em: Value = Value("em")
    val Ev: Value = Value("ev")
    val Evcs: Value = Value("evcs")
    val FixedFeedIn: Value = Value("fixedfeedin")
    val Load: Value = Value("load")
    val PvPlant: Value = Value("pv")
    val Storage: Value = Value("storage")
    val Wec: Value = Value("wec")
    val Hp: Value = Value("hp")
    val House: Value = Value("house")
    val CylindricalStorage: Value = Value("cylindricalstorage")

    /** All participant identifiers */
    def getParticipantIdentifiers: Set[Value] =
      (NotifierIdentifier.values -- getThermalIdentifiers).toSet

    /** All thermal identifiers */
    def getThermalIdentifiers: Set[Value] = Set(
      NotifierIdentifier.House,
      NotifierIdentifier.CylindricalStorage,
    )
  }

  object CsvConfigUtil {

    /** Check basic csv parameter information
      *
      * @param params
      *   Parameters to check
      * @param csvParamsName
      *   Description for what the parameters are intended to be used (for more
      *   descriptive exception messages)
      */
    def checkBaseCsvParams(
        params: CsvParams,
        csvParamsName: String,
    ): Unit = params match {
      case params: CsvParams =>
        if (!(params.csvSep.equals(";") || params.csvSep.equals(",")))
          throw new InvalidConfigParameterException(
            s"The csvSep parameter '${params.csvSep}' for '$csvParamsName' configuration is invalid! Please choose between ';' or ','!"
          )
        if (
          params.directoryPath.isEmpty || !new File(params.directoryPath)
            .exists() || new File(params.directoryPath).isFile
        )
          throw new InvalidConfigParameterException(
            s"The provided directoryPath for .csv-files '${params.directoryPath}' for '$csvParamsName' configuration is invalid! Please correct the path!"
          )
    }

    @deprecated(since = "2.1")
    def checkCsvParams(
        csvParamsName: String,
        csvSep: String,
        folderPath: String,
    ): Unit = {
      if (!(csvSep.equals(";") || csvSep.equals(",")))
        throw new InvalidConfigParameterException(
          s"The csvSep parameter '$csvSep' for '$csvParamsName' configuration is invalid! Please choose between ';' or ','!"
        )
      if (
        folderPath.isEmpty || !new File(folderPath)
          .exists() || new File(folderPath).isFile
      )
        throw new InvalidConfigParameterException(
          s"The provided folderPath for .csv-files '$folderPath' for '$csvParamsName' configuration is invalid! Please correct the path!"
        )
    }
  }

  object DatabaseConfigUtil extends LazyLogging {

    def checkSqlParams(
        sql: SqlParams
    ): Unit = {
      if (!sql.jdbcUrl.trim.startsWith("jdbc:")) {
        throw new InvalidConfigParameterException(
          s"The provided JDBC url '${sql.jdbcUrl}' is invalid! The url should start with 'jdbc:'"
        )
      }
      if (!sql.jdbcUrl.trim.startsWith("jdbc:postgresql://")) {
        logger.warn(
          "It seems like you intend to use the SqlWeatherSource with an other dialect than PostgreSQL. Please be aware that this usage has neither been tested nor been considered in development."
        )
      }
      if (sql.userName.isEmpty)
        throw new InvalidConfigParameterException(
          "User name for SQL weather source cannot be empty"
        )
      if (sql.password.isEmpty)
        logger.info(
          "Password for SQL weather source is empty. This is allowed, but not common. Please check if this an intended setting."
        )
      if (sql.tableName.isEmpty)
        throw new InvalidConfigParameterException(
          "Weather table name for SQL weather source cannot be empty"
        )
      if (sql.schemaName.isEmpty)
        throw new InvalidConfigParameterException(
          "Schema name for SQL weather source cannot be empty"
        )

      /* Try to build a connection */
      Try(
        new SqlConnector(sql.jdbcUrl, sql.userName, sql.password).getConnection
      ) match {
        case Failure(exception) =>
          throw new IllegalArgumentException(
            s"Unable to reach configured SQL database with url '${sql.jdbcUrl}' and user name '${sql.userName}'. Exception: $exception",
            exception,
          )
        case Success(connection) =>
          val validConnection = connection.isValid(5000)
          connection.close()
          if (!validConnection)
            throw new IllegalArgumentException(
              s"Unable to reach configured SQL database with url '${sql.jdbcUrl}' and user name '${sql.userName}'."
            )
          else
            logger.debug(
              s"Successfully pinged SQL database with url '${sql.jdbcUrl}' and user name '${sql.userName}'"
            )
      }
    }

    def checkCouchbaseParams(
        couchbase: CouchbaseParams
    ): Unit = {
      if (couchbase.url.isEmpty)
        throw new InvalidConfigParameterException(
          "URL for Couchbase weather source cannot be empty"
        )
      if (couchbase.userName.isEmpty)
        throw new InvalidConfigParameterException(
          "User name for Couchbase weather source cannot be empty"
        )
      if (couchbase.password.isEmpty)
        throw new InvalidConfigParameterException(
          "Password for Couchbase weather source cannot be empty"
        )
      if (couchbase.bucketName.isEmpty)
        throw new InvalidConfigParameterException(
          "Bucket name for Couchbase weather source cannot be empty"
        )
      if (couchbase.coordinateColumnName.isEmpty)
        throw new InvalidConfigParameterException(
          "Coordinate column for Couchbase weather source cannot be empty"
        )
      if (couchbase.keyPrefix.isEmpty)
        throw new InvalidConfigParameterException(
          "Key prefix for Couchbase weather source cannot be empty"
        )

      /* Try to build a connection */
      Try(
        new CouchbaseConnector(
          couchbase.url,
          couchbase.bucketName,
          couchbase.userName,
          couchbase.password,
        )
      ) match {
        case Failure(exception) =>
          throw new IllegalArgumentException(
            s"Unable to reach configured Couchbase database with url '${couchbase.url}', bucket '${couchbase.bucketName}' and user name '${couchbase.userName}'. Exception: $exception",
            exception,
          )
        case Success(connector) =>
          val validConnection = connector.isConnectionValid
          connector.shutdown()
          if (!validConnection)
            throw new IllegalArgumentException(
              s"Unable to reach configured Couchbase database with url '${couchbase.url}', bucket '${couchbase.bucketName}' and user name '${couchbase.userName}'"
            )
          else
            logger.debug(
              s"Successfully pinged Couchbase database with url '${couchbase.url}', bucket '${couchbase.bucketName}' and user name '${couchbase.userName}'"
            )
      }
    }

    def checkInfluxDb1xParams(
        influxDb1xParamsName: String,
        url: String,
        database: String,
    ): Unit = {
      Try(
        new InfluxDbConnector(url, database).isConnectionValid
      ) match {
        case Failure(exception) =>
          throw new IllegalArgumentException(
            s"Unable to reach configured influxDb1x with url '$url' for '$influxDb1xParamsName' configuration and database '$database'. Exception: $exception",
            exception,
          )
        case Success(validConnection) if !validConnection =>
          throw new IllegalArgumentException(
            s"Unable to reach configured influxDb1x with url '$url' for '$influxDb1xParamsName' configuration and database '$database'."
          )
        case Success(_) => // valid connection, do nothing
          logger.debug(
            s"Successfully pinged influxDb1x with url '$url' for '$influxDb1xParamsName' configuration and s'$database'."
          )
      }
    }

    def checkKafkaParams(
        kafkaParams: BaseKafkaParams,
        topics: Seq[String],
    ): Unit = {
      try {
        UUID.fromString(kafkaParams.runId)
      } catch {
        case e: IllegalArgumentException =>
          throw new InvalidConfigParameterException(
            s"The UUID '${kafkaParams.runId}' cannot be parsed as it is invalid.",
            e,
          )
      }

      val properties = new Properties()
      properties.put("bootstrap.servers", kafkaParams.bootstrapServers)
      properties.put("default.api.timeout.ms", 2000)
      properties.put("request.timeout.ms", 1000)
      Using(AdminClient.create(properties)) { client =>
        val existingTopics = client.listTopics.names().get().asScala
        topics.filterNot(existingTopics.contains)
      } match {
        case Failure(ke: KafkaException) =>
          throw new InvalidConfigParameterException(
            s"Exception creating kafka client for broker ${kafkaParams.bootstrapServers}.",
            ke,
          )
        case Failure(ee: ExecutionException) =>
          throw new InvalidConfigParameterException(
            s"Connection with kafka broker ${kafkaParams.bootstrapServers} failed.",
            ee,
          )
        case Failure(other) =>
          throw new InvalidConfigParameterException(
            s"Checking kafka config failed with unexpected exception.",
            other,
          )
        case Success(missingTopics) if missingTopics.nonEmpty =>
          throw new InvalidConfigParameterException(
            s"Required kafka topics {${missingTopics.mkString}} do not exist."
          )
        case Success(_) =>
        // testing connection succeeded, do nothing
      }
    }
  }

}
