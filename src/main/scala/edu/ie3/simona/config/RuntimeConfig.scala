/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.IoConfigUtils.RuntimeKafkaParams
import edu.ie3.simona.config.RuntimeConfig._
import edu.ie3.simona.config.SimonaConfig._
import pureconfig.generic.semiauto.{deriveReader, deriveWriter}
import pureconfig.{ConfigReader, ConfigWriter}

final case class RuntimeConfig(
    selectedSubgrids: Option[Seq[Int]],
    selectedVoltLvls: Option[Seq[VoltLvlConfig]],
    listener: RuntimeListenerConfig = RuntimeListenerConfig(None, None),
    participant: RuntimeParticipantsConfig,
)

object RuntimeConfig {
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

  final case class RuntimeListenerConfig(
      eventsToProcess: Option[Seq[String]],
      kafka: Option[RuntimeKafkaParams],
  )

  final case class RuntimeParticipantsConfig(
      em: RuntimeParticipantConfig[EmRuntimeConfig],
      evcs: RuntimeParticipantConfig[SimpleRuntimeConfig],
      fixedFeedIn: RuntimeParticipantConfig[SimpleRuntimeConfig],
      hp: RuntimeParticipantConfig[SimpleRuntimeConfig],
      load: RuntimeParticipantConfig[LoadRuntimeConfig],
      pv: RuntimeParticipantConfig[SimpleRuntimeConfig],
      requestVoltageDeviationThreshold: Double = 1e-14,
      storage: RuntimeParticipantConfig[StorageRuntimeConfig],
      wec: RuntimeParticipantConfig[SimpleRuntimeConfig],
  ) {
    def asSeq: Seq[RuntimeParticipantConfig[_ <: BaseRuntimeConfig]] = {
      Seq(
        em,
        evcs,
        fixedFeedIn,
        hp,
        load,
        pv,
        wec,
        storage,
      )
    }
  }

  final case class RuntimeParticipantConfig[+T <: BaseRuntimeConfig](
      defaultConfig: T,
      individualConfigs: Seq[T],
  )

  trait BaseRuntimeConfig {
    val uuids: Seq[String]
    val scaling: Double
    val calculateMissingReactivePowerWithModel: Boolean
  }

  final case class SimpleRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
  ) extends BaseRuntimeConfig

  final case class LoadRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
      modelBehaviour: String,
      reference: String,
  ) extends BaseRuntimeConfig

  final case class EvcsRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
      chargingStrategy: String = "maxPower",
      lowestEvSoc: Double = 0.2,
  ) extends BaseRuntimeConfig

  final case class EmRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
      curtailRegenerative: Boolean = false,
      aggregateFlex: String = "SELF_OPT_EXCL_REG",
  ) extends BaseRuntimeConfig

  final case class StorageRuntimeConfig(
      override val calculateMissingReactivePowerWithModel: Boolean,
      override val scaling: Double,
      override val uuids: List[String],
      initialSoc: Double = 0.0,
      targetSoc: Option[Double] = None,
  ) extends BaseRuntimeConfig
}
