/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.RuntimeConfig.{Listener, Participant}
import edu.ie3.simona.config.SimonaConfig.{RuntimeKafkaParams, VoltLvlConfig}
import pureconfig.generic.ProductHint
import pureconfig.generic.semiauto.{deriveReader, deriveWriter}
import pureconfig.{CamelCase, ConfigFieldMapping, ConfigReader, ConfigWriter}

import scala.language.implicitConversions

case class RuntimeConfig(
    listener: Listener = Listener(),
    participant: Participant = Participant.empty(),
    selected_subgrids: Option[List[Int]] = None,
    selected_volt_lvls: Option[List[VoltLvlConfig]] = None,
)

object RuntimeConfig {
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  // necessary to prevent StackOverFlowErrors during compilation
  implicit val loadRuntimeReader: ConfigReader[LoadRuntimeConfig] =
    deriveReader[LoadRuntimeConfig]
  implicit val loadRuntimeWriter: ConfigWriter[LoadRuntimeConfig] =
    deriveWriter[LoadRuntimeConfig]

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

  final case class Listener(
      eventsToProcess: Option[List[String]] = None,
      kafka: Option[RuntimeKafkaParams] = None,
  )

  final case class Participant(
      em: ParticipantRuntimeConfig[EmRuntimeConfig],
      evcs: ParticipantRuntimeConfig[EvcsRuntimeConfig],
      fixedFeedIn: ParticipantRuntimeConfig[FixedFeedInRuntimeConfig],
      hp: ParticipantRuntimeConfig[HpRuntimeConfig],
      load: ParticipantRuntimeConfig[LoadRuntimeConfig],
      pv: ParticipantRuntimeConfig[PvRuntimeConfig],
      requestVoltageDeviationThreshold: Double = 1e-14,
      storage: ParticipantRuntimeConfig[StorageRuntimeConfig],
      wec: ParticipantRuntimeConfig[WecRuntimeConfig],
  )

  // helper method
  implicit def wrap[T <: BaseRuntimeConfig](t: T): ParticipantRuntimeConfig[T] =
    ParticipantRuntimeConfig(t)

  object Participant {
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

  final case class ParticipantRuntimeConfig[+T <: BaseRuntimeConfig](
      defaultConfig: T,
      individualConfigs: List[T] = List(),
  )

  sealed trait BaseRuntimeConfig {
    val calculateMissingReactivePowerWithModel: Boolean
    val scaling: Double
    val uuids: List[String]
  }

  final case class EvcsRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
      chargingStrategy: String = "maxPower",
      lowestEvSoc: Double = 0.2,
  ) extends BaseRuntimeConfig

  final case class EmRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
      aggregateFlex: String = "SELF_OPT_EXCL_REG",
      curtailRegenerative: Boolean = false,
  ) extends BaseRuntimeConfig

  final case class FixedFeedInRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig

  final case class HpRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig

  final case class LoadRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
      modelBehaviour: String = "fix",
      reference: String = "power",
  ) extends BaseRuntimeConfig

  final case class PvRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig

  final case class StorageRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
      initialSoc: Double = 0d,
      targetSoc: Option[Double] = None,
  ) extends BaseRuntimeConfig

  final case class WecRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean = false,
      override val scaling: Double = 1.0,
      override val uuids: List[String] = List(),
  ) extends BaseRuntimeConfig
}
