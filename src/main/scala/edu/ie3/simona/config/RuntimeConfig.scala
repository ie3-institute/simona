/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.ConfigParams.RuntimeKafkaParams
import edu.ie3.simona.config.RuntimeConfig.*
import edu.ie3.simona.config.SimonaConfig.VoltLvlConfig
import pureconfig.generic.ProductHint
import pureconfig.generic.semiauto.deriveConvert
import pureconfig.{CamelCase, ConfigConvert, ConfigFieldMapping}

import scala.deriving.Mirror

/** Runtime configurations for simona.
  * @param em
  *   Runtime configs for energy management systems.
  * @param listener
  *   Runtime listener configuration.
  * @param participant
  *   Runtime configuration.
  * @param selectedSubgrids
  *   Option for selected sub grids (default: None).
  * @param selectedVoltLvls
  *   Option for selected voltage levels (default: None).
  */
final case class RuntimeConfig(
    em: EmRuntimeConfigs = EmRuntimeConfigs(),
    listener: Listener = Listener(),
    participant: Participant = Participant(),
    selectedSubgrids: Option[List[Int]] = None,
    selectedVoltLvls: Option[List[VoltLvlConfig]] = None,
) derives ConfigConvert

object RuntimeConfig {
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  extension (c: ConfigConvert.type)
    private inline def derived[A](using m: Mirror.Of[A]): ConfigConvert[A] =
      deriveConvert[A]

  final case class Listener(
      eventsToProcess: Option[List[String]] = None,
      kafka: Option[RuntimeKafkaParams] = None,
  ) derives ConfigConvert

  /** Runtime configurations for participants.
    * @param bm
    *   Runtime configs for biomass power plants.
    * @param evcs
    *   Runtime configs for electrical vehicle charging stations.
    * @param fixedFeedIn
    *   Runtime configs for fixed feed ins.
    * @param hp
    *   Runtime configs for heat pumps.
    * @param load
    *   Runtime configs for loads.
    * @param pv
    *   Runtime configs for photovoltaic plants.
    * @param requestVoltageDeviationThreshold
    *   Threshold for the voltage deviation.
    * @param storage
    *   Runtime configs for electrical storages.
    * @param wec
    *   Runtime configs for wind energy converters.
    */
  final case class Participant(
      bm: BmRuntimeConfigs = BmRuntimeConfigs(),
      evcs: EvcsRuntimeConfigs = EvcsRuntimeConfigs(),
      fixedFeedIn: FixedFeedInRuntimeConfigs = FixedFeedInRuntimeConfigs(),
      hp: HpRuntimeConfigs = HpRuntimeConfigs(),
      load: LoadRuntimeConfigs = LoadRuntimeConfigs(),
      pv: PvRuntimeConfigs = PvRuntimeConfigs(),
      requestVoltageDeviationThreshold: Double = 1e-14,
      storage: StorageRuntimeConfigs = StorageRuntimeConfigs(),
      wec: WecRuntimeConfigs = WecRuntimeConfigs(),
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
    *   If missing reactive power may be filled up with model function (default:
    *   false).
    * @param scaling
    *   The scaling factor of the power output (default: 1.0).
    * @param uuids
    *   Of the models that should use this config, for the default config this
    *   value is ignored.
    * @param chargingStrategy
    *   The charging strategy to use.
    * @param departureTargetSoc
    *   The minimum SOC that an EV should have at departure.
    */
  final case class EvcsRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
      chargingStrategy: String = "maxPower",
      departureTargetSoc: Double = 0.75,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Case class contains default and individual configs for electric vehicle
    * charging stations.
    *
    * @param defaultConfig
    *   The default config used for all asset, that have no individual config.
    * @param individualConfigs
    *   Specific configs, that are used instead of the [[defaultConfig]].
    */
  final case class EvcsRuntimeConfigs(
      defaultConfig: EvcsRuntimeConfig = EvcsRuntimeConfig(),
      individualConfigs: List[EvcsRuntimeConfig] = List.empty,
  ) derives ConfigConvert

  /** Runtime configuration for energy management systems.
    * @param calculateMissingReactivePowerWithModel
    *   If missing reactive power may be filled up with model function (default:
    *   false).
    * @param scaling
    *   The scaling factor of the power output (default: 1.0).
    * @param uuids
    *   Of the models that should use this config, for the default config this
    *   value is ignored.
    * @param aggregateFlex
    *   Strategy for aggregating flexibilities (default: SELF_OPT_EXCL_REG).
    * @param curtailRegenerative
    *   If regenerative generation can be curtailed (default: false).
    */
  final case class EmRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
      aggregateFlex: String = "SELF_OPT_EXCL_REG",
      curtailRegenerative: Boolean = false,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Case class contains default and individual configs for energy management
    * systems.
    *
    * @param defaultConfig
    *   The default config used for all asset, that have no individual config.
    * @param individualConfigs
    *   Specific configs, that are used instead of the [[defaultConfig]].
    */
  final case class EmRuntimeConfigs(
      defaultConfig: EmRuntimeConfig = EmRuntimeConfig(),
      individualConfigs: List[EmRuntimeConfig] = List.empty,
  ) derives ConfigConvert

  /** Runtime configuration for fixed feed ins.
    * @param calculateMissingReactivePowerWithModel
    *   If missing reactive power may be filled up with model function (default:
    *   false).
    * @param scaling
    *   The scaling factor of the power output (default: 1.0).
    * @param uuids
    *   Of the models that should use this config, for the default config this
    *   value is ignored.
    */
  final case class FixedFeedInRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Case class contains default and individual configs for fixed feed ins.
    *
    * @param defaultConfig
    *   The default config used for all asset, that have no individual config.
    * @param individualConfigs
    *   Specific configs, that are used instead of the [[defaultConfig]].
    */
  final case class FixedFeedInRuntimeConfigs(
      defaultConfig: FixedFeedInRuntimeConfig = FixedFeedInRuntimeConfig(),
      individualConfigs: List[FixedFeedInRuntimeConfig] = List.empty,
  ) derives ConfigConvert

  /** Runtime configuration for heat pumps.
    * @param calculateMissingReactivePowerWithModel
    *   If missing reactive power may be filled up with model function (default:
    *   false).
    * @param scaling
    *   The scaling factor of the power output (default: 1.0).
    * @param uuids
    *   Of the models that should use this config, for the default config this
    *   value is ignored.
    */
  final case class HpRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Case class contains default and individual configs for heat pumps.
    *
    * @param defaultConfig
    *   The default config used for all asset, that have no individual config.
    * @param individualConfigs
    *   Specific configs, that are used instead of the [[defaultConfig]].
    */
  final case class HpRuntimeConfigs(
      defaultConfig: HpRuntimeConfig = HpRuntimeConfig(),
      individualConfigs: List[HpRuntimeConfig] = List.empty,
  ) derives ConfigConvert

  /** Runtime configuration for loads.
    * @param calculateMissingReactivePowerWithModel
    *   If missing reactive power may be filled up with model function (default:
    *   false).
    * @param scaling
    *   The scaling factor of the power output (default: 1.0).
    * @param uuids
    *   Of the models that should use this config, for the default config this
    *   value is ignored.
    * @param modelBehaviour
    *   The behaviour of the loads (default: fix).
    * @param reference
    *   Defined to which reference a load model behaviour might be scaled
    *   (default: power).
    */
  final case class LoadRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
      modelBehaviour: String = "fix",
      reference: String = "power",
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Case class contains default and individual configs for loads.
    *
    * @param defaultConfig
    *   The default config used for all asset, that have no individual config.
    * @param individualConfigs
    *   Specific configs, that are used instead of the [[defaultConfig]].
    */
  final case class LoadRuntimeConfigs(
      defaultConfig: LoadRuntimeConfig = LoadRuntimeConfig(),
      individualConfigs: List[LoadRuntimeConfig] = List.empty,
  ) derives ConfigConvert

  /** Runtime configuration for photovoltaic plants.
    * @param calculateMissingReactivePowerWithModel
    *   If missing reactive power may be filled up with model function (default:
    *   false).
    * @param scaling
    *   The scaling factor of the power output (default: 1.0).
    * @param uuids
    *   Of the models that should use this config, for the default config this
    *   value is ignored.
    */
  final case class PvRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Case class contains default and individual configs for photovoltaic
    * plants.
    *
    * @param defaultConfig
    *   The default config used for all asset, that have no individual config.
    * @param individualConfigs
    *   Specific configs, that are used instead of the [[defaultConfig]].
    */
  final case class PvRuntimeConfigs(
      defaultConfig: PvRuntimeConfig = PvRuntimeConfig(),
      individualConfigs: List[PvRuntimeConfig] = List.empty,
  ) derives ConfigConvert

  /** Runtime configuration for electrical storages.
    * @param calculateMissingReactivePowerWithModel
    *   If missing reactive power may be filled up with model function (default:
    *   false).
    * @param scaling
    *   The scaling factor of the power output (default: 1.0).
    * @param uuids
    *   Of the models that should use this config, for the default config this
    *   value is ignored.
    * @param initialSoc
    *   The initial state of charge in percent of the storage (default: 0.0).
    * @param targetSoc
    *   Option for a targeted state of charge (default: None).
    */
  final case class StorageRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
      initialSoc: Double = 0d,
      targetSoc: Option[Double] = None,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Case class contains default and individual configs for electrical
    * storages.
    *
    * @param defaultConfig
    *   The default config used for all asset, that have no individual config.
    * @param individualConfigs
    *   Specific configs, that are used instead of the [[defaultConfig]].
    */
  final case class StorageRuntimeConfigs(
      defaultConfig: StorageRuntimeConfig = StorageRuntimeConfig(),
      individualConfigs: List[StorageRuntimeConfig] = List.empty,
  ) derives ConfigConvert

  /** Runtime configuration for wind energy converters.
    * @param calculateMissingReactivePowerWithModel
    *   If missing reactive power may be filled up with model function (default:
    *   false).
    * @param scaling
    *   The scaling factor of the power output (default: 1.0).
    * @param uuids
    *   Of the models that should use this config, for the default config this
    *   value is ignored.
    */
  final case class WecRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Case class contains default and individual configs for wind energy
    * converters.
    *
    * @param defaultConfig
    *   The default config used for all asset, that have no individual config.
    * @param individualConfigs
    *   Specific configs, that are used instead of the [[defaultConfig]].
    */
  final case class WecRuntimeConfigs(
      defaultConfig: WecRuntimeConfig = WecRuntimeConfig(),
      individualConfigs: List[WecRuntimeConfig] = List.empty,
  ) derives ConfigConvert

  /** Runtime configuration for biomass plants.
    * @param calculateMissingReactivePowerWithModel
    *   If missing reactive power may be filled up with model function (default:
    *   false).
    * @param scaling
    *   The scaling factor of the power output (default: 1.0).
    * @param uuids
    *   Of the models that should use this config, for the default config this
    *   value is ignored.
    */
  final case class BmRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List.empty,
  ) extends BaseRuntimeConfig
      derives ConfigConvert

  /** Case class contains default and individual configs for biomass plants.
    *
    * @param defaultConfig
    *   The default config used for all asset, that have no individual config.
    * @param individualConfigs
    *   Specific configs, that are used instead of the [[defaultConfig]].
    */
  final case class BmRuntimeConfigs(
      defaultConfig: BmRuntimeConfig = BmRuntimeConfig(),
      individualConfigs: List[BmRuntimeConfig] = List.empty,
  ) derives ConfigConvert
}
