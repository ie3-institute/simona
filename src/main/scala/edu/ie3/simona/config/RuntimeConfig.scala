/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.ConfigParams.RuntimeKafkaParams
import edu.ie3.simona.config.RuntimeConfig._
import edu.ie3.simona.config.SimonaConfig.{AssetConfigs, VoltLvlConfig}
import pureconfig.generic.ProductHint
import pureconfig.{CamelCase, ConfigFieldMapping}

import scala.language.implicitConversions

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
    em: AssetConfigs[EmRuntimeConfig] = AssetConfigs(EmRuntimeConfig()),
    listener: Listener = Listener(),
    participant: Participant = Participant(),
    selectedSubgrids: Option[List[Int]] = None,
    selectedVoltLvls: Option[List[VoltLvlConfig]] = None,
)

object RuntimeConfig {
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  private val defaultUuids = List("default")

  /** Returns the default runtime configuration.
    */
  def default: RuntimeConfig = RuntimeConfig()

  /** Wraps an [[BaseRuntimeConfig]] with a [[AssetConfigs]].
    *
    * @param config
    *   To wrap.
    * @tparam T
    *   Type of config.
    * @return
    *   A [[AssetConfigs]].
    */
  implicit def wrap[T <: BaseRuntimeConfig](config: T): AssetConfigs[T] =
    AssetConfigs(config)

  final case class Listener(
      eventsToProcess: Option[List[String]] = None,
      kafka: Option[RuntimeKafkaParams] = None,
  )

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
      bm: AssetConfigs[BmRuntimeConfig] = BmRuntimeConfig(),
      evcs: AssetConfigs[EvcsRuntimeConfig] = EvcsRuntimeConfig(),
      fixedFeedIn: AssetConfigs[FixedFeedInRuntimeConfig] =
        FixedFeedInRuntimeConfig(),
      hp: AssetConfigs[HpRuntimeConfig] = HpRuntimeConfig(),
      load: AssetConfigs[LoadRuntimeConfig] = LoadRuntimeConfig(),
      pv: AssetConfigs[PvRuntimeConfig] = PvRuntimeConfig(),
      requestVoltageDeviationThreshold: Double = 1e-14,
      storage: AssetConfigs[StorageRuntimeConfig] = StorageRuntimeConfig(),
      wec: AssetConfigs[WecRuntimeConfig] = WecRuntimeConfig(),
  )

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
    * @param lowestEvSoc
    *   The lowest SOC possible for EV batteries (inverse of max dod).
    */
  final case class EvcsRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = defaultUuids,
      chargingStrategy: String = "maxPower",
      lowestEvSoc: Double = 0.2,
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = defaultUuids,
      aggregateFlex: String = "SELF_OPT_EXCL_REG",
      curtailRegenerative: Boolean = false,
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = defaultUuids,
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = defaultUuids,
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = defaultUuids,
      modelBehaviour: String = "fix",
      reference: String = "power",
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = defaultUuids,
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = defaultUuids,
      initialSoc: Double = 0d,
      targetSoc: Option[Double] = None,
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = defaultUuids,
  ) extends BaseRuntimeConfig

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
}
