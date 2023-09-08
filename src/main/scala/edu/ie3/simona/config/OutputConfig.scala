package edu.ie3.simona.config

import edu.ie3.simona.config.IoConfigUtils.{InfluxDb1xParams, ResultKafkaParams}
import edu.ie3.simona.config.OutputConfig._

final case class OutputConfig(
    base: OutputBaseConfig,
    sink: OutputSinkConfig,
    grid: GridOutputConfig,
    participant: ParticipantOutputConfig,
)

object OutputConfig {

  final case class OutputBaseConfig(
      dir: String,
      addTimestampToOutputDir: Boolean
  )

  final case class OutputSinkConfig(
      csv: Option[OutputCsvParams],
      influxDb1x: Option[InfluxDb1xParams],
      kafka: Option[ResultKafkaParams],
  )

  final case class OutputCsvParams(
      fileFormat: String = ".csv",
      isHierarchic: Boolean = false,
      filePrefix: String = "",
      fileSuffix: String = "",
  )

  final case class GridOutputConfig(
      notifier: String,
      nodes: Boolean = true,
      lines: Boolean = true,
      switches: Boolean = true,
      transformers2w: Boolean = true,
      transformers3w: Boolean = true,
  )

  final case class ParticipantOutputConfig(
      defaultConfig: BaseOutputConfig,
      individualConfigs: Seq[BaseOutputConfig],
  )

  final case class BaseOutputConfig(
      notifier: String,
      powerRequestReply: Boolean,
      simulationResult: Boolean
  )

}
