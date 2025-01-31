/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.IoConfigUtils.{InfluxDb1xParams, ResultKafkaParams}
import edu.ie3.simona.config.OutputConfig._

final case class OutputConfig(
    base: OutputBaseConfig,
    sink: OutputSinkConfig = OutputSinkConfig.csv,
    grid: GridOutputConfig,
    participant: ParticipantOutputConfig,
    thermal: ThermalOutputConfig,
)

object OutputConfig {

  final case class OutputBaseConfig(
      dir: String,
      addTimestampToOutputDir: Boolean = true,
  )

  final case class OutputSinkConfig(
      csv: Option[OutputCsvParams],
      influxDb1x: Option[InfluxDb1xParams],
      kafka: Option[ResultKafkaParams],
  )

  object OutputSinkConfig {
    def csv: OutputSinkConfig =
      OutputSinkConfig(Some(OutputCsvParams.default), None, None)
  }

  final case class OutputCsvParams(
      fileFormat: String = ".csv",
      isHierarchic: Boolean = false,
      filePrefix: String = "",
      fileSuffix: String = "",
      zipFiles: Boolean = false,
  )

  object OutputCsvParams {
    def default: OutputCsvParams = OutputCsvParams()
  }

  final case class GridOutputConfig(
      notifier: String,
      nodes: Boolean = true,
      lines: Boolean = true,
      switches: Boolean = true,
      transformers2w: Boolean = true,
      transformers3w: Boolean = true,
  )

  final case class ParticipantOutputConfig(
      defaultConfig: ParticipantBaseOutputConfig,
      individualConfigs: Seq[ParticipantBaseOutputConfig],
  )

  final case class SimpleOutputConfig(
                                       override val notifier: java.lang.String,
                                       override val simulationResult: Boolean,

                                     ) extends BaseOutputConfig(notifier, simulationResult)

  sealed abstract class BaseOutputConfig(
      val notifier: String,
      val simulationResult: Boolean,
  )

  final case class ThermalOutputConfig(
                                        defaultConfig: SimpleOutputConfig,
                                        individualConfigs: Seq[SimpleOutputConfig],
                                      )

  case class ParticipantBaseOutputConfig(
                                          override val notifier: java.lang.String,
                                          override val simulationResult: Boolean,
                                          flexResult: Boolean,
                                          powerRequestReply: Boolean,
                                        )extends BaseOutputConfig(notifier, simulationResult)
}
