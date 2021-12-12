/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.connectors.{
  CouchbaseConnector,
  InfluxDbConnector,
  SqlConnector
}

import java.util.UUID
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  SwitchResult,
  Transformer2WResult,
  Transformer3WResult
}
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig._
import edu.ie3.simona.event.notifier.{Notifier, ParticipantNotifierConfig}
import edu.ie3.simona.exceptions.InvalidConfigParameterException

import java.io.File
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object ConfigUtil {

  final case class ParticipantConfigUtil private (
      private val configs: Map[UUID, SimonaConfig.BaseRuntimeConfig],
      private val defaultLoadConfig: LoadRuntimeConfig,
      private val defaultFixedFeedInConfig: FixedFeedInRuntimeConfig,
      private val defaultPvConfig: PvRuntimeConfig,
      private val defaultWecConfig: WecRuntimeConfig,
      private val defaultEvcsConfig: EvcsRuntimeConfig
  ) {

    /** Queries for a [[LoadRuntimeConfig]], that applies for the given uuid and
      * either returns the config for the requested uuid or the default config.
      * If the requested uuid is valid, but the return type is not of type
      * [[LoadRuntimeConfig]] the default config for this type is returned.
      *
      * @param uuid
      *   Identifier of the requested load model
      * @return
      *   the requested [[LoadRuntimeConfig]] or a default value
      */
    def getLoadConfigOrDefault(uuid: UUID): LoadRuntimeConfig =
      configs.get(uuid) match {
        case Some(loadConfig: LoadRuntimeConfig) => loadConfig
        case _                                   => defaultLoadConfig
      }

    /** Queries for a [[PvRuntimeConfig]], that applies for the given uuid and
      * either returns the config for the requested uuid or the default config.
      * If the requested uuid is valid, but the return type is not of type
      * [[PvRuntimeConfig]] the default config for this type is returned.
      *
      * @param uuid
      *   Identifier of the requested load model
      * @return
      *   the requested [[PvRuntimeConfig]] or a default value
      */
    def getPvConfigOrDefault(uuid: UUID): PvRuntimeConfig =
      configs.get(uuid) match {
        case Some(pvRuntimeConfig: PvRuntimeConfig) => pvRuntimeConfig
        case _                                      => defaultPvConfig
      }

    def getWecConfigOrDefault(uuid: UUID): WecRuntimeConfig =
      configs.get(uuid) match {
        case Some(wecRuntimeConfig: WecRuntimeConfig) => wecRuntimeConfig
        case _                                        => defaultWecConfig
      }

    /** Queries for a [[FixedFeedInRuntimeConfig]], that applies for the given
      * uuid and either returns the config for the requested uuid or the default
      * config. If the requested uuid is valid, but the return type is not of
      * type [[FixedFeedInRuntimeConfig]] the default config for this type is
      * returned.
      *
      * @param uuid
      *   Identifier of the requested fixed feed in model
      * @return
      *   the requested [[FixedFeedInRuntimeConfig]] or a default value
      */
    def getFixedFeedConfigOrDefault(uuid: UUID): FixedFeedInRuntimeConfig =
      configs.get(uuid) match {
        case Some(ffinConfig: FixedFeedInRuntimeConfig) => ffinConfig
        case _ => defaultFixedFeedInConfig
      }

    /** Queries for a [[EvcsRuntimeConfig]], that applies for the given uuid and
      * either returns the config for the requested uuid or the default config.
      * If the requested uuid is valid, but the return type is not of type
      * [[EvcsRuntimeConfig]] the default config for this type is returned.
      *
      * @param uuid
      *   Identifier of the requested Evcs model
      * @return
      *   the requested [[EvcsRuntimeConfig]] or a default value
      */
    def getEvcsConfigOrDefault(uuid: UUID): EvcsRuntimeConfig =
      configs.get(uuid) match {
        case Some(evcsConfig: EvcsRuntimeConfig) => evcsConfig
        case _                                   => defaultEvcsConfig
      }
  }

  case object ParticipantConfigUtil {

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
        subConfig: SimonaConfig.Simona.Runtime.Participant
    ): ParticipantConfigUtil = {
      new ParticipantConfigUtil(
        buildUuidMapping(
          subConfig.load.individualConfigs ++
            subConfig.fixedFeedIn.individualConfigs ++
            subConfig.pv.individualConfigs ++
            subConfig.evcs.individualConfigs ++
            subConfig.wec.individualConfigs
        ),
        subConfig.load.defaultConfig,
        subConfig.fixedFeedIn.defaultConfig,
        subConfig.pv.defaultConfig,
        subConfig.wec.defaultConfig,
        subConfig.evcs.defaultConfig
      )
    }

    private def buildUuidMapping(
        configs: List[BaseRuntimeConfig]
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
  final case class BaseOutputConfigUtil(
      private val defaultConfig: ParticipantNotifierConfig,
      private val configs: Map[
        NotifierIdentifier.Value,
        ParticipantNotifierConfig
      ]
  ) {
    def getOrDefault(
        notifierId: NotifierIdentifier.Value
    ): ParticipantNotifierConfig =
      configs.getOrElse(notifierId, defaultConfig)

    /** Get all identifiers of [[Notifier]] implementations, that will announce
      * new simulation results
      *
      * @return
      *   A set of applicable notifiers
      */
    def simulationResultIdentifiersToConsider: Set[NotifierIdentifier.Value] =
      if (defaultConfig.simulationResultInfo) {
        /* Generally inform about all simulation results, but not on those, that are explicitly marked */
        NotifierIdentifier.values -- configs.flatMap {
          case (
                notifierId,
                ParticipantNotifierConfig(resultInfo, _)
              ) if !resultInfo =>
            Some(notifierId)
          case _ => None
        }
      } else {
        /* Only register those events, that are explicitly marked to be considered */
        configs.flatMap {
          case (
                notifierId,
                ParticipantNotifierConfig(resultInfo, _)
              ) if resultInfo =>
            Some(notifierId)
          case _ => None
        }.toSet
      }

    def simulationResultEntitiesToConsider: Set[Class[_ <: ResultEntity]] =
      simulationResultIdentifiersToConsider.map(notifierId =>
        EntityMapperUtil.getResultEntityClass(notifierId)
      )
  }

  case object BaseOutputConfigUtil {
    def apply(
        subConfig: SimonaConfig.Simona.Output.Participant
    ): BaseOutputConfigUtil = {
      val defaultConfig = subConfig.defaultConfig match {
        case BaseOutputConfig(_, powerRequestReply, simulationResult) =>
          ParticipantNotifierConfig(simulationResult, powerRequestReply)
      }
      val configMap = subConfig.individualConfigs.map {
        case BaseOutputConfig(notifier, powerRequestReply, simulationResult) =>
          try {
            val id = NotifierIdentifier(notifier)
            id -> ParticipantNotifierConfig(simulationResult, powerRequestReply)
          } catch {
            case e: NoSuchElementException =>
              throw new InvalidConfigParameterException(
                s"Cannot parse $notifier to known result event notifier.",
                e
              )
          }
      }.toMap
      new BaseOutputConfigUtil(defaultConfig, configMap)
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
  case object NotifierIdentifier extends ParsableEnumeration {
    val BioMassPlant: Value = Value("bm")
    val ChpPlant: Value = Value("chp")
    val Ev: Value = Value("ev")
    val Evcs: Value = Value("evcs")
    val FixedFeedIn: Value = Value("fixedfeedin")
    val Load: Value = Value("load")
    val PvPlant: Value = Value("pv")
    val Storage: Value = Value("storage")
    val Wec: Value = Value("wec")
  }

  case object CsvConfigUtil {
    def checkCsvParams(
        csvParamsName: String,
        csvSep: String,
        folderPath: String
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

  case object DatabaseConfigUtil extends LazyLogging {

    def checkSqlParams(
        sql: edu.ie3.simona.config.SimonaConfig.Simona.Input.Weather.Datasource.SqlParams
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
      if (sql.timeColumnName.isEmpty)
        throw new InvalidConfigParameterException(
          "Time column for SQL weather source cannot be empty"
        )
      if (sql.weatherTableName.isEmpty)
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
            s"Unable to reach configured SQL database with url '${sql.jdbcUrl}' and user name '${sql.userName}'. Exception: $exception"
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
        couchbase: edu.ie3.simona.config.SimonaConfig.Simona.Input.Weather.Datasource.CouchbaseParams
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
          couchbase.password
        )
      ) match {
        case Failure(exception) =>
          throw new IllegalArgumentException(
            s"Unable to reach configured Couchbase database with url '${couchbase.url}', bucket '${couchbase.bucketName}' and user name '${couchbase.userName}'. Exception: $exception"
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
        database: String
    ): Unit = {
      Try(
        new InfluxDbConnector(url, database).isConnectionValid
      ) match {
        case Failure(exception) =>
          throw new IllegalArgumentException(
            s"Unable to reach configured influxDb1x with url '$url' for '$influxDb1xParamsName' configuration and database '$database'. Exception: $exception"
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
  }

}
