package edu.ie3.simona.config

import edu.ie3.simona.config.IoConfigUtils.BaseKafkaParams

object IoConfigUtils {

  trait CsvParams {
    val directoryPath: String
    val csvSep: String
  }

  case class BaseCsvParams(
      directoryPath: String,
      csvSep: String,
  ) extends CsvParams

  case class PsdmCsvParams(
      directoryPath: String,
      csvSep: String,
      isHierarchic: Boolean,
  ) extends CsvParams

  case class TimeStampedDataCsvParams(
      directoryPath: String,
      csvSep: String,
      timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]'Z'"
  ) extends CsvParams

  case class InfluxDb1xParams(
      url: String,
      port: Int,
      database: String,
      timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]'Z'"
  )

  case class SQLParams(
      jdbcUrl: String,
      userName: String,
      password: String,
      tableName: String,
      schemaName: String = "public",
  )

  case class CouchbaseParams(
      url: String,
      bucketName: String,
      userName: String,
      password: String,
      coordinateColumnName: String,
      keyPrefix: String,
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
