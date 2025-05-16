/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import edu.ie3.simona.config.ConfigParams.*
import edu.ie3.simona.config.OutputConfig

import java.util.UUID

/** Enumeration to describe all eligible types of
  * [[edu.ie3.datamodel.models.result.ResultEntity]] sink
  */
sealed trait ResultSinkType

object ResultSinkType {

  final case class Csv(
      fileFormat: String = ".csv",
      filePrefix: String = "",
      fileSuffix: String = "",
      compressOutputs: Boolean = false,
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
      sinkConfig: OutputConfig.Sink,
      runName: String,
  ): ResultSinkType = {
    val sink: Seq[Any] =
      Seq(sinkConfig.csv, sinkConfig.influxDb1x, sinkConfig.kafka).flatten

    if sink.size > 1 then
      throw new IllegalArgumentException(
        s"Multiple sinks are not supported! Provided sinks: '$sinkConfig'"
      )

    sink.headOption match {
      case Some(params: PsdmSinkCsvParams) =>
        Csv(
          params.fileFormat,
          params.filePrefix,
          params.fileSuffix,
          params.compressOutputs,
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
    if sinkConfig.url.endsWith("/") then sinkConfig.url.replaceAll("/", "")
    else sinkConfig.url
  }.trim.concat(s":${sinkConfig.port}")

}
