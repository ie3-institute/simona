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
  *   output directory
  * @param flex
  *   if flexibility options should be written (default: false)
  * @param grid
  *   output configuration
  * @param log
  *   configuration for logging
  * @param participant
  *   output configuration
  * @param sink
  *   output sink
  * @param thermal
  *   output configuration
  */
final case class OutputConfig(
    base: Base,
    flex: Boolean = false,
    grid: GridOutputConfig,
    log: Log = Log.empty,
    participant: AssetConfigs[ParticipantOutputConfig],
    sink: Sink = Sink.empty,
    thermal: AssetConfigs[SimpleOutputConfig],
)

object OutputConfig {

  /** Configuration for specific grid asset results.
    * @param lines
    *   if line results should be written (default: false)
    * @param nodes
    *   if node results should be written (default: false)
    * @param switches
    *   if switch results should be written (default: false)
    * @param transformers2w
    *   if two-winding transformer results should be written (default: false)
    * @param transformers3w
    *   if three-winding transformer results should be written (default: false)
    */
  final case class GridOutputConfig(
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
    *   that specifies the participant type
    * @param simulationResult
    *   if simulation results should be written (default: false)
    * @param flexResult
    *   if flexibility option results should be written (default: false)
    * @param powerRequestReply
    *   if the power request reply should be written (default: false)
    */
  final case class ParticipantOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean = false,
      flexResult: Boolean = false,
      powerRequestReply: Boolean = false,
  ) extends BaseOutputConfig

  /** Simple output configuration (e.g. used for thermal outputs).
    * @param notifier
    *   that specifies the output asset type
    * @param simulationResult
    *   if simulation results should be written (default: false)
    */
  final case class SimpleOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean,
  ) extends BaseOutputConfig

  /** Base output configuration
    * @param addTimestampToOutputDir
    *   if the time stamp should be added to the output directory (default:
    *   true)
    * @param dir
    *   the base output directory
    */
  final case class Base(
      addTimestampToOutputDir: Boolean = true,
      dir: String,
  )

  /** The configuration for the logger.
    * @param level
    *   of the logger
    */
  final case class Log(
      level: String = "INFO"
  )

  object Log {

    /** Returns an empty [[Log]] with default params.
      */
    def empty: Log = Log()
  }

  /** Configuration for output sink.
    * @param csv
    *   used for [[edu.ie3.datamodel.io.sink.CsvFileSink]] (default: None)
    * @param influxDb1x
    *   used for [[edu.ie3.datamodel.io.sink.InfluxDbSink]] (default: None)
    * @param kafka
    *   used for [[edu.ie3.simona.io.result.ResultEntityKafkaSink]] (default:
    *   None)
    */
  final case class Sink(
      csv: Option[PsdmSinkCsvParams] = None,
      influxDb1x: Option[BaseInfluxDb1xParams] = None,
      kafka: Option[ResultKafkaParams] = None,
  )

  object Sink {

    /** Returns an empty [[Sink]] with default params.
      */
    def empty: Sink = Sink()
  }
}
