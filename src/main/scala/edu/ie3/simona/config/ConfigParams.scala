/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import pureconfig.{CamelCase, ConfigConvert, ConfigFieldMapping}
import pureconfig.generic.ProductHint
import pureconfig.generic.semiauto.deriveConvert

import scala.deriving.Mirror

/** Configuration parameters used by the [[SimonaConfig]].
  */
object ConfigParams {
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  extension (c: ConfigConvert.type)
    private inline def derived[A](using m: Mirror.Of[A]): ConfigConvert[A] =
      deriveConvert[A]

  /** Default time pattern: `yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X`
    */
  private val defaultTimePattern = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X"

  /** Sample parameters.
    * @param use
    *   If sample parameters should be used (default: true).
    */
  final case class SampleParams(
      use: Boolean = true
  ) derives ConfigConvert

  /** Basic trait for all csv parameters.
    */
  sealed trait CsvParams {
    val directoryPath: String
    val csvSep: String
    val isHierarchic: Boolean
  }

  /** Basic csv parameters.
    * @param csvSep
    *   The separator used.
    * @param directoryPath
    *   The path of the csv source.
    * @param isHierarchic
    *   True, if a hierarchical structure is used.
    */
  final case class BaseCsvParams(
      override val csvSep: String,
      override val directoryPath: String,
      override val isHierarchic: Boolean,
  ) extends CsvParams
      derives ConfigConvert

  /** Time stamped csv parameters.
    * @param csvSep
    *   The separator used.
    * @param directoryPath
    *   The path of the csv source.
    * @param isHierarchic
    *   True, if a hierarchical structure is used.
    * @param timePattern
    *   Used for the data (default: [[ConfigParams.defaultTimePattern]]).
    */
  final case class TimeStampedCsvParams(
      override val csvSep: String,
      override val directoryPath: String,
      override val isHierarchic: Boolean,
      timePattern: String = defaultTimePattern,
  ) extends CsvParams
      derives ConfigConvert

  /** Csv parameters used by the [[edu.ie3.datamodel.io.sink.CsvFileSink]].
    * @param compressOutputs
    *   If output files should be compressed (default: false).
    * @param fileFormat
    *   That is used (default: .csv).
    * @param filePrefix
    *   Additional file prefix (default: empty).
    * @param fileSuffix
    *   Additional file suffix (default: empty).
    * @param isHierarchic
    *   True, if a hierarchical structure should be used.
    */
  final case class PsdmSinkCsvParams(
      compressOutputs: Boolean = false,
      fileFormat: String = ".csv",
      filePrefix: String = "",
      fileSuffix: String = "",
      isHierarchic: Boolean = false,
  ) derives ConfigConvert

  /** Basic trait for all influxDb1x parameters.
    */
  sealed trait InfluxDb1xParams {
    val database: String
    val port: Int
    val url: String
  }

  /** Basic influxDb1x parameters.
    * @param database
    *   To use.
    * @param port
    *   Of the database.
    * @param url
    *   Of the database.
    */
  final case class BaseInfluxDb1xParams(
      override val database: String,
      override val port: Int,
      override val url: String,
  ) extends InfluxDb1xParams
      derives ConfigConvert

  /** Time stamped influxDb1x parameters.
    * @param database
    *   To use.
    * @param port
    *   Of the database.
    * @param timePattern
    *   Used for the data (default: [[ConfigParams.defaultTimePattern]]).
    * @param url
    *   Of the database.
    */
  final case class TimeStampedInfluxDb1xParams(
      override val database: String,
      override val port: Int,
      timePattern: String = defaultTimePattern,
      override val url: String,
  ) extends InfluxDb1xParams
      derives ConfigConvert

  /** Basic trait for all sql parameters.
    */
  sealed trait SqlParams {
    val jdbcUrl: String
    val userName: String
    val password: String
    val tableName: String = ""
    val schemaName: String
  }

  /** Basic sql parameters.
    * @param jdbcUrl
    *   To the database.
    * @param password
    *   For login.
    * @param schemaName
    *   Name of the schema (default: public).
    * @param tableName
    *   Name of the database table to use.
    * @param userName
    *   For login.
    */
  final case class BaseSqlParams(
      override val jdbcUrl: String,
      override val password: String,
      override val schemaName: String = "public",
      override val tableName: String,
      override val userName: String,
  ) extends SqlParams
      derives ConfigConvert

  /** Time stamped sql parameters. The table name is not used with this,
    * therefore, it is empty.
    * @param jdbcUrl
    *   To the database.
    * @param password
    *   For login.
    * @param schemaName
    *   Name of the schema (default: public).
    * @param timePattern
    *   Used for the data (default: [[ConfigParams.defaultTimePattern]]).
    * @param userName
    *   For login.
    */
  final case class TimeStampedSqlParams(
      override val jdbcUrl: String,
      override val password: String,
      override val schemaName: String = "public",
      timePattern: String = defaultTimePattern,
      override val userName: String,
  ) extends SqlParams
      derives ConfigConvert

  /** Time stamped couchbase parameters.
    * @param bucketName
    *   Name of the specific bucket.
    * @param coordinateColumnName
    *   Name of the column containing coordinates.
    * @param keyPrefix
    *   Prefix for the key.
    * @param password
    *   For login.
    * @param timePattern
    *   Used for the data (default: [[ConfigParams.defaultTimePattern]]).
    * @param url
    *   To the database.
    * @param userName
    *   For login.
    */
  final case class CouchbaseParams(
      bucketName: String,
      coordinateColumnName: String,
      keyPrefix: String,
      password: String,
      timePattern: String = defaultTimePattern,
      url: String,
      userName: String,
  ) derives ConfigConvert

  /** Basic trait for all kafka parameters.
    */
  sealed trait KafkaParams {
    val runId: String
    val bootstrapServers: String
    val schemaRegistryUrl: String
    val linger: Int
  }

  /** Kafka result parameters.
    */
  final case class ResultKafkaParams(
      override val bootstrapServers: String,
      override val linger: Int,
      override val runId: String,
      override val schemaRegistryUrl: String,
      topicNodeRes: String,
  ) extends KafkaParams
      derives ConfigConvert

  /** Kafka runtime parameters.
    */
  final case class RuntimeKafkaParams(
      override val bootstrapServers: String,
      override val linger: Int,
      override val runId: String,
      override val schemaRegistryUrl: String,
      topic: String,
  ) extends KafkaParams
      derives ConfigConvert

}
