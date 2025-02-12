/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.{Config, ConfigRenderOptions}

import pureconfig._
import pureconfig.error._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._
import pureconfig.generic.semiauto.{deriveReader, deriveWriter}

import java.time.Duration
import scala.language.implicitConversions
import scala.util.Try

final case class SimonaConfig(
    simona: SimonaConfig.Simona
) {
  def render(options: ConfigRenderOptions): String =
    SimonaConfig.render(this, options)
}

object SimonaConfig {
  // pure config start
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  // TODO: replace with finite duration
  implicit def durationConvert: ConfigConvert[Duration] =
    ConfigConvert.viaStringTry(
      str => Try(Duration.parse(("PT" + str).toUpperCase)),
      x => x.toString,
    )

  // necessary to prevent StackOverFlowErrors during compilation
  implicit val baseRuntimeReader: ConfigReader[BaseRuntimeConfig] =
    deriveReader[BaseRuntimeConfig]
  implicit val baseRuntimeWriter: ConfigWriter[BaseRuntimeConfig] =
    deriveWriter[BaseRuntimeConfig]
  implicit val loadRuntimeReader: ConfigReader[LoadRuntimeConfig] =
    deriveReader[LoadRuntimeConfig]
  implicit val loadRuntimeWriter: ConfigWriter[LoadRuntimeConfig] =
    deriveWriter[LoadRuntimeConfig]
  implicit val ffiRuntimeReader: ConfigReader[FixedFeedInRuntimeConfig] =
    deriveReader[FixedFeedInRuntimeConfig]
  implicit val ffiRuntimeWriter: ConfigWriter[FixedFeedInRuntimeConfig] =
    deriveWriter[FixedFeedInRuntimeConfig]
  implicit val pvRuntimeReader: ConfigReader[PvRuntimeConfig] =
    deriveReader[PvRuntimeConfig]
  implicit val pvRuntimeWriter: ConfigWriter[PvRuntimeConfig] =
    deriveWriter[PvRuntimeConfig]
  implicit val wecRuntimeReader: ConfigReader[WecRuntimeConfig] =
    deriveReader[WecRuntimeConfig]
  implicit val wecRuntimeWriter: ConfigWriter[WecRuntimeConfig] =
    deriveWriter[WecRuntimeConfig]
  implicit val evcsRuntimeReader: ConfigReader[EvcsRuntimeConfig] =
    deriveReader[EvcsRuntimeConfig]
  implicit val evcsRuntimeWriter: ConfigWriter[EvcsRuntimeConfig] =
    deriveWriter[EvcsRuntimeConfig]
  implicit val emRuntimeReader: ConfigReader[EmRuntimeConfig] =
    deriveReader[EmRuntimeConfig]
  implicit val emRuntimeWriter: ConfigWriter[EmRuntimeConfig] =
    deriveWriter[EmRuntimeConfig]
  implicit val storageRuntimeReader: ConfigReader[StorageRuntimeConfig] =
    deriveReader[StorageRuntimeConfig]
  implicit val storageRuntimeWriter: ConfigWriter[StorageRuntimeConfig] =
    deriveWriter[StorageRuntimeConfig]
  implicit val hpRuntimeReader: ConfigReader[HpRuntimeConfig] =
    deriveReader[HpRuntimeConfig]
  implicit val hpRuntimeWriter: ConfigWriter[HpRuntimeConfig] =
    deriveWriter[HpRuntimeConfig]
  implicit val baseCsvReader: ConfigReader[BaseCsvParams] =
    deriveReader[BaseCsvParams]
  implicit val baseCsvWriter: ConfigWriter[BaseCsvParams] =
    deriveWriter[BaseCsvParams]
  implicit val partBaseOutputReader: ConfigReader[ParticipantBaseOutputConfig] =
    deriveReader[ParticipantBaseOutputConfig]
  implicit val partBaseOutputWriter: ConfigWriter[ParticipantBaseOutputConfig] =
    deriveWriter[ParticipantBaseOutputConfig]
  implicit val primaryDataCsvReader: ConfigReader[PrimaryDataCsvParams] =
    deriveReader[PrimaryDataCsvParams]
  implicit val primaryDataCsvWriter: ConfigWriter[PrimaryDataCsvParams] =
    deriveWriter[PrimaryDataCsvParams]
  implicit val resultKafkaReader: ConfigReader[ResultKafkaParams] =
    deriveReader[ResultKafkaParams]
  implicit val resultKafkaWriter: ConfigWriter[ResultKafkaParams] =
    deriveWriter[ResultKafkaParams]
  implicit val runtimeKafkaReader: ConfigReader[RuntimeKafkaParams] =
    deriveReader[RuntimeKafkaParams]
  implicit val runtimeKafkaWriter: ConfigWriter[RuntimeKafkaParams] =
    deriveWriter[RuntimeKafkaParams]
  implicit val simpleOutputReader: ConfigReader[SimpleOutputConfig] =
    deriveReader[SimpleOutputConfig]
  implicit val simpleOutputWriter: ConfigWriter[SimpleOutputConfig] =
    deriveWriter[SimpleOutputConfig]

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
        throw new RuntimeException(
          s"Unable to load config due to following failures:\n$detailedErrors"
        )
      case Right(conf) => conf
    }

  def apply(typeSafeConfig: Config): SimonaConfig =
    apply(ConfigSource.fromConfig(typeSafeConfig))

  def apply(confSrc: ConfigObjectSource): SimonaConfig =
    confSrc.load[SimonaConfig]

  def render(
      simonaConfig: SimonaConfig,
      options: ConfigRenderOptions,
  ): String = ConfigWriter[SimonaConfig].to(simonaConfig).render(options)

  // pure config end

  final case class BaseCsvParams(
      override val csvSep: String,
      override val directoryPath: String,
      override val isHierarchic: Boolean,
  ) extends CsvParams(csvSep, directoryPath, isHierarchic)

  sealed abstract class BaseOutputConfig(
      val notifier: String,
      val simulationResult: Boolean,
  )

  sealed abstract class BaseRuntimeConfig(
      val calculateMissingReactivePowerWithModel: Boolean = false,
      val scaling: Double = 1.0,
      val uuids: List[String] = List(),
  ) extends Serializable

  sealed abstract class CsvParams(
      val csvSep: String,
      val directoryPath: String,
      val isHierarchic: Boolean,
  )

  final case class EmRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
      aggregateFlex: String = "SELF_OPT_EXCL_REG",
      curtailRegenerative: Boolean = false,
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )

  final case class EvcsRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
      chargingStrategy: String = "maxPower",
      lowestEvSoc: Double = 0.2,
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )

  final case class FixedFeedInRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )

  final case class GridOutputConfig(
      lines: Boolean = false,
      nodes: Boolean = false,
      notifier: String,
      switches: Boolean = false,
      transformers2w: Boolean = false,
      transformers3w: Boolean = false,
  )

  final case class HpRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )

  sealed abstract class KafkaParams(
      val bootstrapServers: String,
      val linger: Int,
      val runId: String,
      val schemaRegistryUrl: String,
  )

  final case class LoadRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
      modelBehaviour: String,
      reference: String,
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )

  final case class ParticipantBaseOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean,
      flexResult: Boolean = false,
      powerRequestReply: Boolean,
  ) extends BaseOutputConfig(notifier, simulationResult)

  final case class PrimaryDataCsvParams(
      override val csvSep: String,
      override val directoryPath: String,
      override val isHierarchic: Boolean,
      timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
  ) extends CsvParams(csvSep, directoryPath, isHierarchic)

  final case class PvRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )

  final case class RefSystemConfig(
      gridIds: Option[List[String]] = None,
      sNom: String,
      vNom: String,
      voltLvls: Option[List[VoltLvlConfig]] = None,
  )

  final case class ResultKafkaParams(
      override val bootstrapServers: String,
      override val linger: Int,
      override val runId: String,
      override val schemaRegistryUrl: String,
      topicNodeRes: String,
  ) extends KafkaParams(bootstrapServers, linger, runId, schemaRegistryUrl)

  final case class RuntimeKafkaParams(
      override val bootstrapServers: String,
      override val linger: Int,
      override val runId: String,
      override val schemaRegistryUrl: String,
      topic: String,
  ) extends KafkaParams(bootstrapServers, linger, runId, schemaRegistryUrl)

  final case class SimpleOutputConfig(
      override val notifier: String,
      override val simulationResult: Boolean,
  ) extends BaseOutputConfig(notifier, simulationResult)

  final case class StorageRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
      initialSoc: Double = 0d,
      targetSoc: Option[Double],
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )

  final case class TransformerControlGroup(
      measurements: List[String] = List(),
      transformers: List[String] = List(),
      vMax: Double,
      vMin: Double,
  )

  final case class VoltLvlConfig(
      id: String,
      vNom: String,
  )

  final case class WecRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig(
        calculateMissingReactivePowerWithModel,
        scaling,
        uuids,
      )

  final case class Simona(
      control: Option[Simona.Control] = None,
      event: Simona.Event = Simona.Event(),
      gridConfig: Simona.GridConfig = Simona.GridConfig(),
      input: Simona.Input,
      output: Simona.Output,
      powerflow: Simona.Powerflow,
      runtime: Simona.Runtime,
      simulationName: String,
      time: Simona.Time = Simona.Time(),
  )
  object Simona {
    final case class Control(
        transformer: List[TransformerControlGroup] = List()
    )

    final case class Event(
        listener: Option[List[Event.Listener$Elm]] = None
    )
    object Event {
      final case class Listener$Elm(
          eventsToProcess: Option[List[String]] = None,
          fullClassPath: String,
      )
    }

    final case class GridConfig(
        refSystems: Option[List[RefSystemConfig]] = None
    )

    final case class Input(
        grid: Input.Grid,
        primary: Input.Primary = Input.Primary(),
        weather: Input.Weather = Input.Weather(),
    )
    object Input {
      final case class Grid(
          datasource: Grid.Datasource
      )
      object Grid {
        final case class Datasource(
            csvParams: Option[BaseCsvParams] = None,
            id: String,
        )
      }

      final case class Primary(
          couchbaseParams: scala.Option[Primary.CouchbaseParams] = None,
          csvParams: Option[PrimaryDataCsvParams] = None,
          influxDb1xParams: Option[Primary.InfluxDb1xParams] = None,
          sqlParams: Option[Primary.SqlParams] = None,
      )
      object Primary {
        final case class CouchbaseParams(
            bucketName: String,
            coordinateColumnName: String,
            keyPrefix: String,
            password: String,
            timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
            url: String,
            userName: String,
        )

        final case class InfluxDb1xParams(
            database: String,
            port: Int,
            timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
            url: String,
        )

        final case class SqlParams(
            jdbcUrl: String,
            password: String,
            schemaName: String = "public",
            timePattern: String = "yyyy-MM-dd'T'HH:mm:ss[.S[S][S]]X",
            userName: String,
        )
      }

      final case class Weather(
          datasource: Weather.Datasource = Weather.Datasource()
      )
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
        )
        object Datasource {
          final case class CoordinateSource(
              csvParams: Option[BaseCsvParams] = None,
              gridModel: String = "icon",
              sampleParams: Option[CoordinateSource.SampleParams] = None,
              sqlParams: Option[CoordinateSource.SqlParams] = None,
          )
          object CoordinateSource {
            final case class SampleParams(
                use: Boolean = true
            )

            final case class SqlParams(
                jdbcUrl: String,
                password: String,
                schemaName: String = "public",
                tableName: String,
                userName: String,
            )
          }

          final case class CouchbaseParams(
              bucketName: String,
              coordinateColumnName: String,
              keyPrefix: String,
              password: String,
              url: String,
              userName: String,
          )

          final case class InfluxDb1xParams(
              database: String,
              port: Int,
              url: String,
          )

          final case class SampleParams(
              use: Boolean = true
          )

          final case class SqlParams(
              jdbcUrl: String,
              password: String,
              schemaName: String = "public",
              tableName: String,
              userName: String,
          )
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
    )
    object Output {
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
          csv: Option[Sink.Csv] = None,
          influxDb1x: Option[Sink.InfluxDb1x] = None,
          kafka: Option[ResultKafkaParams] = None,
      )
      object Sink {
        final case class Csv(
            compressOutputs: Boolean = false,
            fileFormat: String = ".csv",
            filePrefix: String = "",
            fileSuffix: String = "",
            isHierarchic: Boolean = false,
        )

        final case class InfluxDb1x(
            database: String,
            port: Int,
            url: String,
        )
      }

      final case class Thermal(
          defaultConfig: SimpleOutputConfig,
          individualConfigs: List[SimpleOutputConfig] = List(),
      )
    }

    final case class Powerflow(
        maxSweepPowerDeviation: Double,
        newtonraphson: Powerflow.Newtonraphson,
        resolution: Duration = Duration.ofHours(1),
        stopOnFailure: Boolean = false,
        sweepTimeout: Duration = Duration.ofSeconds(30),
    )
    object Powerflow {
      final case class Newtonraphson(
          epsilon: List[Double] = List(),
          iterations: Int,
      )
    }

    final case class Runtime(
        listener: Runtime.Listener = Runtime.Listener(),
        participant: Runtime.Participant,
        selected_subgrids: Option[List[Int]] = None,
        selected_volt_lvls: Option[List[VoltLvlConfig]] = None,
    )
    object Runtime {
      final case class Listener(
          eventsToProcess: Option[List[String]] = None,
          kafka: Option[RuntimeKafkaParams] = None,
      )

      final case class Participant(
          em: Participant.Em,
          evcs: Participant.Evcs,
          fixedFeedIn: Participant.FixedFeedIn,
          hp: Participant.Hp,
          load: Participant.Load,
          pv: Participant.Pv,
          requestVoltageDeviationThreshold: Double = 1e-14,
          storage: Participant.Storage,
          wec: Participant.Wec,
      )
      object Participant {
        final case class Em(
            defaultConfig: EmRuntimeConfig,
            individualConfigs: List[EmRuntimeConfig] = List(),
        )

        final case class Evcs(
            defaultConfig: EvcsRuntimeConfig,
            individualConfigs: List[EvcsRuntimeConfig] = List(),
        )

        final case class FixedFeedIn(
            defaultConfig: FixedFeedInRuntimeConfig,
            individualConfigs: List[FixedFeedInRuntimeConfig] = List(),
        )

        final case class Hp(
            defaultConfig: HpRuntimeConfig,
            individualConfigs: List[HpRuntimeConfig] = List(),
        )

        final case class Load(
            defaultConfig: LoadRuntimeConfig,
            individualConfigs: List[LoadRuntimeConfig] = List(),
        )

        final case class Pv(
            defaultConfig: PvRuntimeConfig,
            individualConfigs: List[PvRuntimeConfig] = List(),
        )

        final case class Storage(
            defaultConfig: StorageRuntimeConfig,
            individualConfigs: List[StorageRuntimeConfig] = List(),
        )

        final case class Wec(
            defaultConfig: WecRuntimeConfig,
            individualConfigs: List[WecRuntimeConfig] = List(),
        )
      }
    }

    final case class Time(
        endDateTime: String = "2011-05-01T01:00:00Z",
        schedulerReadyCheckWindow: Option[Int] = None,
        startDateTime: String = "2011-05-01T00:00:00Z",
    )
  }
}
