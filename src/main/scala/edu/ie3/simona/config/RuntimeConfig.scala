/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.ConfigParams.RuntimeKafkaParams
import edu.ie3.simona.config.RuntimeConfig._
import edu.ie3.simona.config.SimonaConfig.VoltLvlConfig
import pureconfig.generic.ProductHint
import pureconfig.{CamelCase, ConfigFieldMapping}

import scala.language.implicitConversions

/** Runtime configurations for simona.
  * @param listener
  *   runtime listener configuration
  * @param participant
  *   runtime configuration
  * @param selected_subgrids
  *   option for selected sub grids (default: None)
  * @param selected_volt_lvls
  *   option for selected voltage levels (default: None)
  */
case class RuntimeConfig(
    listener: Listener = Listener(),
    participant: Participant = Participant.empty(),
    selected_subgrids: Option[List[Int]] = None,
    selected_volt_lvls: Option[List[VoltLvlConfig]] = None,
)

object RuntimeConfig {
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

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
  )

  /** Runtime configurations for participants.
    * @param em
    *   runtime configs for energy management systems
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
      // TODO: move em, since ems aren't participants
      em: ParticipantRuntimeConfigs[EmRuntimeConfig],
      evcs: ParticipantRuntimeConfigs[EvcsRuntimeConfig],
      fixedFeedIn: ParticipantRuntimeConfigs[FixedFeedInRuntimeConfig],
      hp: ParticipantRuntimeConfigs[HpRuntimeConfig],
      load: ParticipantRuntimeConfigs[LoadRuntimeConfig],
      pv: ParticipantRuntimeConfigs[PvRuntimeConfig],
      requestVoltageDeviationThreshold: Double = 1e-14,
      storage: ParticipantRuntimeConfigs[StorageRuntimeConfig],
      wec: ParticipantRuntimeConfigs[WecRuntimeConfig],
  )

  object Participant {

    /** Returns a [[Participant]] object with default values.
      */
    def empty(): Participant = Participant(
      em = EmRuntimeConfig(),
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
  final case class ParticipantRuntimeConfigs[+T <: BaseRuntimeConfig](
      defaultConfig: T,
      individualConfigs: List[T] = List(),
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
      override val uuids: List[String] = List(),
      chargingStrategy: String = "maxPower",
      lowestEvSoc: Double = 0.2,
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = List(),
      aggregateFlex: String = "SELF_OPT_EXCL_REG",
      curtailRegenerative: Boolean = false,
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = List(),
      modelBehaviour: String = "fix",
      reference: String = "power",
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = List(),
      initialSoc: Double = 0d,
      targetSoc: Option[Double] = None,
  ) extends BaseRuntimeConfig

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
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig
}
