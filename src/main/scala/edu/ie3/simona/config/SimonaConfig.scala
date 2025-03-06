/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.Config
import edu.ie3.simona.exceptions.CriticalFailureException
import pureconfig.*
import pureconfig.error.*
import pureconfig.generic.*
import pureconfig.generic.semiauto.deriveConvert

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.deriving.Mirror

final case class SimonaConfig(
    simona: SimonaConfig.Simona
) derives ConfigConvert

object SimonaConfig {
  // pure config start
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  extension (c: ConfigConvert.type)
    inline def derived[A](using m: Mirror.Of[A]): ConfigConvert[A] =
      deriveConvert[A]

  /** Method to extract a config from a [[pureconfig.ConfigReader.Result]]
    * @param either
    *   that may contain a config
    * @tparam T
    *   type of config
    * @return
    *   the config, or throws an exception
    */
  protected implicit def extract[T](
      either: Either[ConfigReaderFailures, T]
  ): T =
    either match {
      case Left(readerFailures) =>
        val detailedErrors = readerFailures.toList
          .map {
            case CannotParse(msg, origin) =>
              f"CannotParse => $msg, Origin: $origin \n"
            case _: CannotRead =>
              f"CannotRead => Can not read config source} \n"
            case ConvertFailure(reason, _, path) =>
              f"ConvertFailure => Path: $path, Description: ${reason.description} \n"
            case ThrowableFailure(throwable, origin) =>
              f"ThrowableFailure => ${throwable.getMessage}, Origin: $origin \n"
            case failure =>
              f"Unknown failure type => ${failure.toString} \n"
          }
          .mkString("\n")
        throw new CriticalFailureException(
          s"Unable to load config due to following failures:\n$detailedErrors"
        )
      case Right(conf) => conf
    }

  def apply(typeSafeConfig: Config): SimonaConfig =
    apply(ConfigSource.fromConfig(typeSafeConfig))

  def apply(confSrc: ConfigObjectSource): SimonaConfig =
    confSrc.load[SimonaConfig]

  // pure config end

  /** Case class contains default and individual configs for assets.
    * @param defaultConfig
    *   to use
    * @param individualConfigs
    *   specific configs, that are used instead of the [[defaultConfig]]
    * @tparam T
    *   type of asset config
    */
  final case class AssetConfigs[T](
      defaultConfig: T,
      individualConfigs: List[T] = List.empty,
  )

  final case class BaseCsvParams(
      override val csvSep: String,
      override val directoryPath: String,
      override val isHierarchic: Boolean,
  ) extends CsvParams(csvSep, directoryPath, isHierarchic)
      derives ConfigConvert

  sealed abstract class BaseOutputConfig(
      val notifier: String,
      val simulationResult: Boolean,
  ) derives ConfigConvert
  sealed abstract class CsvParams(
      val csvSep: String,
      val directoryPath: String,
      val isHierarchic: Boolean,
  ) derives ConfigConvert

  final case class GridOutputConfig(
      lines: Boolean = false,
      nodes: Boolean = false,
      notifier: String,
      switches: Boolean = false,
      transformers2w: Boolean = false,
      transformers3w: Boolean = false,
  ) derives ConfigConvert

  sealed abstract class KafkaParams(
      val bootstrapServers: String,
      val linger: Int,
      val runId: String,
      val schemaRegistryUrl: String,
  ) derives ConfigConvert

  final case class ParticipantBaseOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean,
      flexResult: Boolean = false,
      powerRequestReply: Boolean,
  ) extends BaseOutputConfig(notifier, simulationResult)
      derives ConfigConvert

  final case class PrimaryDataCsvParams(
      override val csvSep: String,
      override val directoryPath: String,
      override val isHierarchic: Boolean,
      timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
  ) extends CsvParams(csvSep, directoryPath, isHierarchic)
      derives ConfigConvert

  sealed trait GridConfigParams {
    val gridIds: Option[List[String]]
    val voltLvls: Option[List[VoltLvlConfig]]
  }

  final case class RefSystemConfig(
      override val gridIds: Option[List[String]] = None,
      sNom: String,
      vNom: String,
      override val voltLvls: Option[List[VoltLvlConfig]] = None,
  ) extends GridConfigParams
      derives ConfigConvert

  final case class ResultKafkaParams(
      override val bootstrapServers: String,
      override val linger: Int,
      override val runId: String,
      override val schemaRegistryUrl: String,
      topicNodeRes: String,
  ) extends KafkaParams(bootstrapServers, linger, runId, schemaRegistryUrl)
      derives ConfigConvert

  final case class RuntimeKafkaParams(
      override val bootstrapServers: String,
      override val linger: Int,
      override val runId: String,
      override val schemaRegistryUrl: String,
      topic: String,
  ) extends KafkaParams(bootstrapServers, linger, runId, schemaRegistryUrl)
      derives ConfigConvert

  final case class SimpleOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean,
  ) extends BaseOutputConfig(notifier, simulationResult)
      derives ConfigConvert

  final case class TransformerControlGroup(
      measurements: List[String] = List.empty,
      transformers: List[String] = List.empty,
      vMax: Double,
      vMin: Double,
  ) derives ConfigConvert

  final case class VoltLvlConfig(
      id: String,
      vNom: String,
  ) derives ConfigConvert

  final case class VoltageLimitsConfig(
      override val gridIds: Option[List[String]] = None,
      vMax: Double,
      vMin: Double,
      override val voltLvls: Option[List[VoltLvlConfig]] = None,
  ) extends GridConfigParams
      derives ConfigConvert

  final case class Simona(
      control: Option[Simona.Control] = None,
      gridConfig: Simona.GridConfig = Simona.GridConfig(),
      input: Simona.Input,
      output: Simona.Output,
      powerflow: Simona.Powerflow,
      runtime: RuntimeConfig,
      simulationName: String,
      time: Simona.Time = Simona.Time(),
  ) derives ConfigConvert
  object Simona {
    final case class Control(
        transformer: List[TransformerControlGroup] = List.empty
    ) derives ConfigConvert

    final case class GridConfig(
        refSystems: Option[List[RefSystemConfig]] = None,
        voltageLimits: Option[List[VoltageLimitsConfig]] = None,
    ) derives ConfigConvert

    final case class Input(
        extSimDir: Option[String],
        grid: Input.Grid,
        primary: Input.Primary = Input.Primary(),
        weather: Input.Weather = Input.Weather(),
    ) derives ConfigConvert
    object Input {
      final case class Grid(
          datasource: Grid.Datasource
      ) derives ConfigConvert
      object Grid {
        final case class Datasource(
            csvParams: Option[BaseCsvParams] = None,
            id: String,
        ) derives ConfigConvert
      }

      final case class Primary(
          couchbaseParams: scala.Option[Primary.CouchbaseParams] = None,
          csvParams: Option[PrimaryDataCsvParams] = None,
          influxDb1xParams: Option[Primary.InfluxDb1xParams] = None,
          sqlParams: Option[Primary.SqlParams] = None,
      ) derives ConfigConvert
      object Primary {
        final case class CouchbaseParams(
            bucketName: String,
            coordinateColumnName: String,
            keyPrefix: String,
            password: String,
            timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
            url: String,
            userName: String,
        ) derives ConfigConvert

        final case class InfluxDb1xParams(
            database: String,
            port: Int,
            timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
            url: String,
        ) derives ConfigConvert

        final case class SqlParams(
            jdbcUrl: String,
            password: String,
            schemaName: String = "public",
            timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
            userName: String,
        ) derives ConfigConvert
      }

      final case class Weather(
          datasource: Weather.Datasource = Weather.Datasource()
      ) derives ConfigConvert
      object Weather {
        final case class Datasource(
            coordinateSource: Datasource.CoordinateSource =
              Datasource.CoordinateSource(),
            couchbaseParams: Option[Datasource.CouchbaseParams] = None,
            csvParams: Option[BaseCsvParams] = None,
            influxDb1xParams: Option[Datasource.InfluxDb1xParams] = None,
            maxCoordinateDistance: Double = 50000,
            resolution: Option[Long] = None,
            sampleParams: Option[Datasource.SampleParams] = None,
            scheme: String = "icon",
            sqlParams: Option[Datasource.SqlParams] = None,
            timestampPattern: Option[String] = None,
        ) derives ConfigConvert
        object Datasource {
          final case class CoordinateSource(
              csvParams: Option[BaseCsvParams] = None,
              gridModel: String = "icon",
              sampleParams: Option[CoordinateSource.SampleParams] = None,
              sqlParams: Option[CoordinateSource.SqlParams] = None,
          ) derives ConfigConvert
          object CoordinateSource {
            final case class SampleParams(
                use: Boolean = true
            ) derives ConfigConvert

            final case class SqlParams(
                jdbcUrl: String,
                password: String,
                schemaName: String = "public",
                tableName: String,
                userName: String,
            ) derives ConfigConvert
          }

          final case class CouchbaseParams(
              bucketName: String,
              coordinateColumnName: String,
              keyPrefix: String,
              password: String,
              url: String,
              userName: String,
          ) derives ConfigConvert

          final case class InfluxDb1xParams(
              database: String,
              port: Int,
              url: String,
          ) derives ConfigConvert

          final case class SampleParams(
              use: Boolean = true
          ) derives ConfigConvert

          final case class SqlParams(
              jdbcUrl: String,
              password: String,
              schemaName: String = "public",
              tableName: String,
              userName: String,
          ) derives ConfigConvert
        }
      }
    }

    final case class Output(
        base: Output.Base,
        flex: Boolean = false,
        grid: GridOutputConfig,
        log: Output.Log = Output.Log(),
        participant: Output.Participant,
        sink: Output.Sink = Output.Sink(),
        thermal: Output.Thermal,
    ) derives ConfigConvert
    object Output {
      final case class Base(
          addTimestampToOutputDir: Boolean = true,
          dir: String,
      ) derives ConfigConvert

      final case class Log(
          level: String = "INFO"
      ) derives ConfigConvert

      final case class Participant(
          defaultConfig: ParticipantBaseOutputConfig,
          individualConfigs: List[ParticipantBaseOutputConfig] = List.empty,
      ) derives ConfigConvert

      final case class Sink(
          csv: Option[Sink.Csv] = None,
          influxDb1x: Option[Sink.InfluxDb1x] = None,
          kafka: Option[ResultKafkaParams] = None,
      ) derives ConfigConvert
      object Sink {
        final case class Csv(
            compressOutputs: Boolean = false,
            fileFormat: String = ".csv",
            filePrefix: String = "",
            fileSuffix: String = "",
            isHierarchic: Boolean = false,
        ) derives ConfigConvert

        final case class InfluxDb1x(
            database: String,
            port: Int,
            url: String,
        ) derives ConfigConvert
      }

      final case class Thermal(
          defaultConfig: SimpleOutputConfig,
          individualConfigs: List[SimpleOutputConfig] = List.empty,
      ) derives ConfigConvert
    }

    final case class Powerflow(
        maxSweepPowerDeviation: Double,
        newtonraphson: Powerflow.Newtonraphson,
        resolution: FiniteDuration = 1.hours,
        stopOnFailure: Boolean = false,
        sweepTimeout: FiniteDuration = 30.seconds,
    ) derives ConfigConvert
    object Powerflow {
      final case class Newtonraphson(
          epsilon: List[Double] = List.empty,
          iterations: Int,
      ) derives ConfigConvert
    }

    final case class Time(
        endDateTime: String = "2011-05-01T01:00:00Z",
        schedulerReadyCheckWindow: Option[Int] = None,
        startDateTime: String = "2011-05-01T00:00:00Z",
    ) derives ConfigConvert
  }
}
