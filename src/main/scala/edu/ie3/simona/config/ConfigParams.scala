/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

/** Configuration parameters used by the [[SimonaConfig]].
  */
object ConfigParams {

  /** Default time pattern: `yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X`
    */
  private val defaultTimePattern = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X"

  /** Sample parameters.
    * @param use
    *   if sample parameters should be used (default: true)
    */
  final case class SampleParams(
      use: Boolean = true
  )

  /** Basic trait for all csv parameters.
    */
  sealed trait CsvParams {
    val directoryPath: String
    val csvSep: String
    val isHierarchic: Boolean
  }

  /** Basic csv parameters.
    * @param csvSep
    *   the separator used
    * @param directoryPath
    *   the path of the csv source
    * @param isHierarchic
    *   true, if a hierarchical structure is used
    */
  final case class BaseCsvParams(
      override val csvSep: String,
      override val directoryPath: String,
      override val isHierarchic: Boolean,
  ) extends CsvParams

  /** Time stamped csv parameters.
    * @param csvSep
    *   the separator used
    * @param directoryPath
    *   the path of the csv source
    * @param isHierarchic
    *   true, if a hierarchical structure is used
    * @param timePattern
    *   used for the data (default: [[ConfigParams.defaultTimePattern]])
    */
  final case class TimeStampedCsvParams(
      override val csvSep: String,
      override val directoryPath: String,
      override val isHierarchic: Boolean,
      timePattern: String = defaultTimePattern,
  ) extends CsvParams

  /** Csv parameters used by the [[edu.ie3.datamodel.io.sink.CsvFileSink]].
    * @param compressOutputs
    *   if output files should be compressed (default: false)
    * @param fileFormat
    *   that is used (default: .csv)
    * @param filePrefix
    *   additional file prefix (default: empty)
    * @param fileSuffix
    *   additional file suffix (default: empty)
    * @param isHierarchic
    *   true, if a hierarchical structure should be used
    */
  final case class PsdmSinkCsvParams(
      compressOutputs: Boolean = false,
      fileFormat: String = ".csv",
      filePrefix: String = "",
      fileSuffix: String = "",
      isHierarchic: Boolean = false,
  )

  /** Basic trait for all influxDb1x parameters.
    */
  sealed trait InfluxDb1xParams {
    val database: String
    val port: Int
    val url: String
  }

  /** Basic influxDb1x parameters.
    * @param database
    *   to use
    * @param port
    *   of the database
    * @param url
    *   of the database
    */
  final case class BaseInfluxDb1xParams(
      override val database: String,
      override val port: Int,
      override val url: String,
  ) extends InfluxDb1xParams

  /** Time stamped influxDb1x parameters.
    * @param database
    *   to use
    * @param port
    *   of the database
    * @param timePattern
    *   used for the data (default: [[ConfigParams.defaultTimePattern]])
    * @param url
    *   of the database
    */
  final case class TimeStampedInfluxDb1xParams(
      override val database: String,
      override val port: Int,
      timePattern: String = defaultTimePattern,
      override val url: String,
  ) extends InfluxDb1xParams

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
    *   to the database
    * @param password
    *   for login
    * @param schemaName
    *   name of the schema (default: public)
    * @param tableName
    *   name of the database table to use
    * @param userName
    *   for login
    */
  final case class BaseSqlParams(
      override val jdbcUrl: String,
      override val password: String,
      override val schemaName: String = "public",
      override val tableName: String,
      override val userName: String,
  ) extends SqlParams

  /** Time stamped sql parameters.
    * @param jdbcUrl
    *   to the database
    * @param password
    *   for login
    * @param schemaName
    *   name of the schema (default: public)
    * @param timePattern
    *   used for the data (default: [[ConfigParams.defaultTimePattern]])
    * @param userName
    *   for login
    */
  final case class TimeStampedSqlParams(
      override val jdbcUrl: String,
      override val password: String,
      override val schemaName: String = "public",
      timePattern: String = defaultTimePattern,
      override val userName: String,
  ) extends SqlParams

  /** Time stamped couchbase parameters.
    * @param bucketName
    *   name of the specific bucket
    * @param coordinateColumnName
    *   name of the column containing coordinates
    * @param keyPrefix
    *   prefix for the key
    * @param password
    *   for login
    * @param timePattern
    *   used for the data (default: [[ConfigParams.defaultTimePattern]])
    * @param url
    *   to the database
    * @param userName
    *   for login
    */
  final case class CouchbaseParams(
      bucketName: String,
      coordinateColumnName: String,
      keyPrefix: String,
      password: String,
      timePattern: String = defaultTimePattern,
      url: String,
      userName: String,
  )

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

  /** Kafka runtime parameters.
    */
  final case class RuntimeKafkaParams(
      override val bootstrapServers: String,
      override val linger: Int,
      override val runId: String,
      override val schemaRegistryUrl: String,
      topic: String,
  ) extends KafkaParams

}
