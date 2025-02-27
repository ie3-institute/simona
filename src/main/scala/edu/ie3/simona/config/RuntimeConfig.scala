/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.RuntimeConfig._
import edu.ie3.simona.config.SimonaConfig.{
  AssetConfigs,
  RuntimeKafkaParams,
  VoltLvlConfig,
}
import edu.ie3.simona.config.RuntimeConfig.*
import edu.ie3.simona.config.SimonaConfig.{RuntimeKafkaParams, VoltLvlConfig}
import pureconfig.generic.ProductHint
import pureconfig.generic.semiauto.deriveConvert
import pureconfig.{CamelCase, ConfigConvert, ConfigFieldMapping}

import scala.deriving.Mirror
import scala.language.implicitConversions

/** Runtime configurations for simona.
  * @param em
  *   runtime configs for energy management systems
  * @param listener
  *   runtime listener configuration
  * @param participant
  *   runtime configuration
  * @param selectedSubgrids
  *   option for selected sub grids (default: None)
  * @param selectedVoltLvls
  *   option for selected voltage levels (default: None)
  */
final case class RuntimeConfig(
    em: AssetConfigs[EmRuntimeConfig] = AssetConfigs(EmRuntimeConfig()),
    listener: Listener = Listener(),
    participant: Participant = Participant.empty(),
    selectedSubgrids: Option[List[Int]] = None,
    selectedVoltLvls: Option[List[VoltLvlConfig]] = None,
) derives ConfigConvert

object RuntimeConfig {
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  extension (c: ConfigConvert.type)
    private inline def derived[A](using m: Mirror.Of[A]): ConfigConvert[A] =
      deriveConvert[A]

  /** Wraps an [[BaseRuntimeConfig]] with a [[ParticipantRuntimeConfigs]].
    *
    * @param config
    *   to wrap
    * @tparam T
    *   type of config
    * @return
    *   a [[ParticipantRuntimeConfigs]]
    */
  implicit def wrap[T <: BaseRuntimeConfig](
      config: T
  ): ParticipantRuntimeConfigs[T] =
    ParticipantRuntimeConfigs(config)

  final case class Listener(
      eventsToProcess: Option[List[String]] = None,
      kafka: Option[RuntimeKafkaParams] = None,
  ) derives ConfigConvert

  /** Runtime configurations for participants.
    * @param evcs
    *   runtime configs for electrical vehicle charging stations
    * @param fixedFeedIn
    *   runtime configs for fixed feed ins
    * @param hp
    *   runtime configs for heat pumps
    * @param load
    *   runtime configs for loads
    * @param pv
    *   runtime configs for photovoltaic plants
    * @param requestVoltageDeviationThreshold
    *   threshold for the voltage deviation
    * @param storage
    *   runtime configs for electrical storages
    * @param wec
    *   runtime configs for wind energy converters
    */
  final case class Participant(
      evcs: ParticipantRuntimeConfigs[EvcsRuntimeConfig],
      fixedFeedIn: ParticipantRuntimeConfigs[FixedFeedInRuntimeConfig],
      hp: ParticipantRuntimeConfigs[HpRuntimeConfig],
      load: ParticipantRuntimeConfigs[LoadRuntimeConfig],
      pv: ParticipantRuntimeConfigs[PvRuntimeConfig],
      requestVoltageDeviationThreshold: Double = 1e-14,
      storage: ParticipantRuntimeConfigs[StorageRuntimeConfig],
      wec: ParticipantRuntimeConfigs[WecRuntimeConfig],
  ) derives ConfigConvert

  object Participant {

    /** Returns a [[Participant]] object with default values.
      */
    def empty(): Participant = Participant(
      evcs = EvcsRuntimeConfig(),
      fixedFeedIn = FixedFeedInRuntimeConfig(),
      hp = HpRuntimeConfig(),
      load = LoadRuntimeConfig(),
      pv = PvRuntimeConfig(),
      storage = StorageRuntimeConfig(),
      wec = WecRuntimeConfig(),
    )
  }

  /** Case class contains default and individual configs for simulation runtime.
    * @param defaultConfig
    *   to use
    * @param individualConfigs
    *   specific configs, that are used instead of the [[defaultConfig]]
    * @tparam T
    *   type of runtime config
    */
  final case class ParticipantRuntimeConfigs[T <: BaseRuntimeConfig](
      defaultConfig: T,
      individualConfigs: List[T] = List.empty[T],
  ) derives ConfigConvert

  /** Basic trait for all runtime configs.
    */
  sealed trait BaseRuntimeConfig {
    val calculateMissingReactivePowerWithModel: Boolean
    val scaling: Double
    val uuids: List[String]
  }

  /** Runtime configuration for electric vehicle charging stations.
    * @param calculateMissingReactivePowerWithModel
    *   if missing reactive power may be filled up with model function (default:
    *   false)
    * @param scaling
    *   the scaling factor of the power output (default: 1.0)
    * @param uuids
    *   of the models that should use this config, for the default config this
    *   value is ignored
    * @param chargingStrategy
    *   the charging strategy to use
    * @param lowestEvSoc
    *   the lowest SOC possible for EV batteries (inverse of max dod)
    */
  final case class EvcsRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
      chargingStrategy: String = "maxPower",
      lowestEvSoc: Double = 0.2,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Runtime configuration for energy management systems.
    * @param calculateMissingReactivePowerWithModel
    *   if missing reactive power may be filled up with model function (default:
    *   false)
    * @param scaling
    *   the scaling factor of the power output (default: 1.0)
    * @param uuids
    *   of the models that should use this config, for the default config this
    *   value is ignored
    * @param aggregateFlex
    *   strategy for aggregating flexibilities (default: SELF_OPT_EXCL_REG)
    * @param curtailRegenerative
    *   if regenerative generation can be curtailed (default: false)
    */
  final case class EmRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
      aggregateFlex: String = "SELF_OPT_EXCL_REG",
      curtailRegenerative: Boolean = false,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Runtime configuration for fixed feed ins.
    * @param calculateMissingReactivePowerWithModel
    *   if missing reactive power may be filled up with model function (default:
    *   false)
    * @param scaling
    *   the scaling factor of the power output (default: 1.0)
    * @param uuids
    *   of the models that should use this config, for the default config this
    *   value is ignored
    */
  final case class FixedFeedInRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Runtime configuration for heat pumps.
    * @param calculateMissingReactivePowerWithModel
    *   if missing reactive power may be filled up with model function (default:
    *   false)
    * @param scaling
    *   the scaling factor of the power output (default: 1.0)
    * @param uuids
    *   of the models that should use this config, for the default config this
    *   value is ignored
    */
  final case class HpRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Runtime configuration for loads.
    * @param calculateMissingReactivePowerWithModel
    *   if missing reactive power may be filled up with model function (default:
    *   false)
    * @param scaling
    *   the scaling factor of the power output (default: 1.0)
    * @param uuids
    *   of the models that should use this config, for the default config this
    *   value is ignored
    * @param modelBehaviour
    *   the behaviour of the loads (default: fix)
    * @param reference
    *   defined to which reference a load model behaviour might be scaled
    *   (default: power)
    */
  final case class LoadRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
      modelBehaviour: String = "fix",
      reference: String = "power",
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Runtime configuration for photovoltaic plants.
    * @param calculateMissingReactivePowerWithModel
    *   if missing reactive power may be filled up with model function (default:
    *   false)
    * @param scaling
    *   the scaling factor of the power output (default: 1.0)
    * @param uuids
    *   of the models that should use this config, for the default config this
    *   value is ignored
    */
  final case class PvRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Runtime configuration for electrical storages.
    * @param calculateMissingReactivePowerWithModel
    *   if missing reactive power may be filled up with model function (default:
    *   false)
    * @param scaling
    *   the scaling factor of the power output (default: 1.0)
    * @param uuids
    *   of the models that should use this config, for the default config this
    *   value is ignored
    * @param initialSoc
    *   the initial state of charge in percent of the storage (default: 0.0)
    * @param targetSoc
    *   option for a targeted state of charge (default: None)
    */
  final case class StorageRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
      initialSoc: Double = 0d,
      targetSoc: Option[Double] = None,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Runtime configuration for wind energy converters.
    * @param calculateMissingReactivePowerWithModel
    *   if missing reactive power may be filled up with model function (default:
    *   false)
    * @param scaling
    *   the scaling factor of the power output (default: 1.0)
    * @param uuids
    *   of the models that should use this config, for the default config this
    *   value is ignored
    */
  final case class WecRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
  ) extends BaseRuntimeConfig
      derives ConfigConvert
}
