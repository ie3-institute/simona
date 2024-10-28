/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import edu.ie3.simona.config.IoConfigUtils.{InfluxDb1xParams, ResultKafkaParams}
import edu.ie3.simona.config.OutputConfig.{OutputCsvParams, OutputSinkConfig}

import java.util.UUID

/** Enumeration to describe all eligible types of
  * [[edu.ie3.datamodel.models.result.ResultEntity]] sink
  */
sealed trait ResultSinkType

// TODO: Check afterwards if we can get rid of those and replace them directly with config items
object ResultSinkType {

  final case class Csv(
      fileFormat: String = ".csv",
      filePrefix: String = "",
      fileSuffix: String = "",
      zipFiles: Boolean = false,
  ) extends ResultSinkType

  final case class InfluxDb1x(url: String, database: String, scenario: String)
      extends ResultSinkType

  final case class Kafka(
      topicNodeRes: String,
      runId: UUID,
      bootstrapServers: String,
      schemaRegistryUrl: String,
      linger: Int,
  ) extends ResultSinkType

  def apply(
      sinkConfig: OutputSinkConfig,
      runName: String,
  ): ResultSinkType = {
    val sink: Seq[Any] =
      Seq(sinkConfig.csv, sinkConfig.influxDb1x, sinkConfig.kafka).flatten

    if (sink.size > 1)
      throw new IllegalArgumentException(
        s"Multiple sinks are not supported! Provided sinks: '$sinkConfig'"
      )

    sink.headOption match {
      case Some(params: OutputCsvParams) =>
        Csv(
          params.fileFormat,
          params.filePrefix,
          params.fileSuffix,
          params.zipFiles,
        )
      case Some(params: InfluxDb1xParams) =>
        InfluxDb1x(buildInfluxDb1xUrl(params), params.database, runName)
      case Some(params: ResultKafkaParams) =>
        Kafka(
          params.topicNodeRes,
          UUID.fromString(params.runId),
          params.bootstrapServers,
          params.schemaRegistryUrl,
          params.linger,
        )
      case None =>
        throw new IllegalArgumentException(
          s"No sinks defined! Cannot determine the sink type!"
        )
      case unknown =>
        throw new IllegalArgumentException(
          s"Unknown sink type config parameter provided $unknown!"
        )
    }
  }

  def buildInfluxDb1xUrl(
      sinkConfig: InfluxDb1xParams
  ): String = {
    if (sinkConfig.url.endsWith("/")) sinkConfig.url.replaceAll("/", "")
    else sinkConfig.url
  }.trim.concat(s":${sinkConfig.port}")

}
