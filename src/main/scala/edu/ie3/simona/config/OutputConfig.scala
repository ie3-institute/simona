/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.OutputConfig._
import edu.ie3.simona.config.ConfigParams.{
  BaseInfluxDb1xParams,
  PsdmSinkCsvParams,
  ResultKafkaParams,
}

case class OutputConfig(
    base: Base,
    flex: Boolean = false,
    grid: GridOutputConfig,
    log: Log = Log(),
    participant: Participant,
    sink: Sink = Sink(),
    thermal: Thermal,
)

object OutputConfig {

  final case class GridOutputConfig(
      lines: Boolean = false,
      nodes: Boolean = false,
      notifier: String,
      switches: Boolean = false,
      transformers2w: Boolean = false,
      transformers3w: Boolean = false,
  )

  sealed trait BaseOutputConfig {
    val notifier: String
    val simulationResult: Boolean
  }

  final case class ParticipantBaseOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean,
      flexResult: Boolean = false,
      powerRequestReply: Boolean,
  ) extends BaseOutputConfig

  final case class SimpleOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean,
  ) extends BaseOutputConfig

  final case class Base(
      addTimestampToOutputDir: Boolean = true,
      dir: String,
  )

  final case class Log(
      level: String = "INFO"
  )

  final case class Participant(
      defaultConfig: ParticipantBaseOutputConfig,
      individualConfigs: List[ParticipantBaseOutputConfig] = List(),
  )

  final case class Sink(
      csv: Option[PsdmSinkCsvParams] = None,
      influxDb1x: Option[BaseInfluxDb1xParams] = None,
      kafka: Option[ResultKafkaParams] = None,
  )

  final case class Thermal(
      defaultConfig: SimpleOutputConfig,
      individualConfigs: List[SimpleOutputConfig] = List(),
  )
}
