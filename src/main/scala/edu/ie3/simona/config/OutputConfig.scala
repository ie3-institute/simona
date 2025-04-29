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
import edu.ie3.simona.config.SimonaConfig.AssetConfigs

/** Output configuration for simona.
  * @param base
  *   Output directory.
  * @param flex
  *   If flexibility options should be written (default: false).
  * @param grid
  *   Output configuration.
  * @param log
  *   Configuration for logging.
  * @param participant
  *   Output configuration.
  * @param sink
  *   Output sink.
  * @param thermal
  *   Output configuration.
  */
final case class OutputConfig(
    base: Base,
    flex: Boolean = false,
    grid: GridOutputConfig,
    log: Log = Log(),
    participant: AssetConfigs[ParticipantOutputConfig],
    sink: Sink = Sink(),
    thermal: AssetConfigs[SimpleOutputConfig],
)

object OutputConfig {

  /** Configuration for specific grid asset results.
    * @param congestions
    *   If congestion results should be written (default: false).
    * @param lines
    *   If line results should be written (default: false).
    * @param nodes
    *   If node results should be written (default: false).
    * @param switches
    *   If switch results should be written (default: false).
    * @param transformers2w
    *   If two-winding transformer results should be written (default: false).
    * @param transformers3w
    *   If three-winding transformer results should be written (default: false).
    */
  final case class GridOutputConfig(
      congestions: Boolean = false,
      lines: Boolean = false,
      nodes: Boolean = false,
      switches: Boolean = false,
      transformers2w: Boolean = false,
      transformers3w: Boolean = false,
  )

  /** Basic trait for all sub-output configurations.
    */
  sealed trait BaseOutputConfig {
    val notifier: String
    val simulationResult: Boolean
  }

  /** Output configuration for participants.
    * @param notifier
    *   That specifies the participant type.
    * @param simulationResult
    *   If simulation results should be written (default: false).
    * @param flexResult
    *   If flexibility option results should be written (default: false).
    * @param powerRequestReply
    *   If the power request reply should be written (default: false).
    */
  final case class ParticipantOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean = false,
      flexResult: Boolean = false,
      powerRequestReply: Boolean = false,
  ) extends BaseOutputConfig

  /** Simple output configuration (e.g. used for thermal outputs).
    * @param notifier
    *   That specifies the output asset type.
    * @param simulationResult
    *   If simulation results should be written (default: false).
    */
  final case class SimpleOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean,
  ) extends BaseOutputConfig

  /** Base output configuration
    * @param addTimestampToOutputDir
    *   If the time stamp should be added to the output directory (default:
    *   true).
    * @param dir
    *   The base output directory.
    */
  final case class Base(
      addTimestampToOutputDir: Boolean = true,
      dir: String,
  )

  /** The configuration for the logger.
    * @param level
    *   Of the logger.
    */
  final case class Log(
      level: String = "INFO"
  )

  /** Configuration for output sink.
    * @param csv
    *   Used for [[edu.ie3.datamodel.io.sink.CsvFileSink]] (default: None).
    * @param influxDb1x
    *   Used for [[edu.ie3.datamodel.io.sink.InfluxDbSink]] (default: None).
    * @param kafka
    *   Used for [[edu.ie3.simona.io.result.ResultEntityKafkaSink]] (default:
    *   None).
    */
  final case class Sink(
      csv: Option[PsdmSinkCsvParams] = None,
      influxDb1x: Option[BaseInfluxDb1xParams] = None,
      kafka: Option[ResultKafkaParams] = None,
  )
}
