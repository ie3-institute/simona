/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config.util

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.connectors.{
  CouchbaseConnector,
  InfluxDbConnector,
  SqlConnector
}
import edu.ie3.simona.exceptions.InvalidConfigParameterException

import scala.util.{Failure, Success, Try}

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
