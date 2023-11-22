/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

object IoConfigUtils {

  trait CsvParams {
    val directoryPath: String
    val csvSep: String
  }

  case class BaseCsvParams(
      directoryPath: String,
      csvSep: String
  ) extends CsvParams

  case class PsdmCsvParams(
      directoryPath: String,
      csvSep: String,
      isHierarchic: Boolean
  ) extends CsvParams

  case class TimeStampedDataCsvParams(
      directoryPath: String,
      csvSep: String,
      timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]'Z'"
  ) extends CsvParams

  case class InfluxDb1xParams(
      database: String,
      port: Int,
      url: String,
//      timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]'Z'"
  )

  trait SqlParams {
    val jdbcUrl: String
    val userName: String
    val password: String
    val tableName: String
    val schemaName: String
  }
  case class BaseSqlParams(
      jdbcUrl: String,
      userName: String,
      password: String,
      tableName: String,
      schemaName: String = "public"
  ) extends SqlParams

  case class TimeStampedSqlParams(
      jdbcUrl: String,
      userName: String,
      password: String,
      tableName: String,
      schemaName: String = "public",
      timePattern: String
  ) extends SqlParams

  case class CouchbaseParams(
      url: String,
      bucketName: String,
      userName: String,
      password: String,
      coordinateColumnName: String,
      keyPrefix: String
  )

  trait BaseKafkaParams {
    val runId: String
    val bootstrapServers: String
    val schemaRegistryUrl: String
    val linger: Int
  }

  final case class ResultKafkaParams(
      runId: String,
      bootstrapServers: String,
      schemaRegistryUrl: String,
      linger: Int,
      topicNodeRes: String
  ) extends BaseKafkaParams

  final case class RuntimeKafkaParams(
      runId: String,
      bootstrapServers: String,
      schemaRegistryUrl: String,
      linger: Int,
      topic: String
  ) extends BaseKafkaParams
}
