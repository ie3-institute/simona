package edu.ie3.simona.config

import edu.ie3.simona.config.IoConfigUtils.RuntimeKafkaParams
import edu.ie3.simona.config.RuntimeConfig._
import edu.ie3.simona.config.SimonaConfig._

import scala.collection.immutable.Seq

final case class RuntimeConfig(
    selectedSubgrids: Seq[Int],
    selectedVoltLvls: Seq[VoltLvlConfig],
    listener: Option[RuntimeListenerConfig],
    participant: RuntimeParticipantsConfig,
)

object RuntimeConfig {
  final case class RuntimeListenerConfig(
      eventsToProcess: Option[Seq[String]],
      kafka: Option[RuntimeKafkaParams]
  )

  final case class RuntimeParticipantsConfig(
      requestVoltageDeviationThreshold: Double = 1e-14,
      load: RuntimeParticipantConfig[LoadRuntimeConfig],
      pv: RuntimeParticipantConfig[SimpleRuntimeConfig],
      fixedFeedIn: RuntimeParticipantConfig[SimpleRuntimeConfig],
      wec: RuntimeParticipantConfig[SimpleRuntimeConfig],
      evcs: RuntimeParticipantConfig[SimpleRuntimeConfig],
  )

  final case class RuntimeParticipantConfig[T](
      defaultConfig: T,
      individualConfigs: Seq[T]
  )

  trait BaseRuntimeConfig {
    val uuids: Seq[String]
    val scaling: Double
    val calculateMissingReactivePowerWithModel: Boolean
  }

  final case class SimpleRuntimeConfig(
      uuids: Seq[String],
      scaling: Double,
      calculateMissingReactivePowerWithModel: Boolean
  ) extends BaseRuntimeConfig

  final case class LoadRuntimeConfig(
      uuids: Seq[String],
      scaling: Double,
      calculateMissingReactivePowerWithModel: Boolean,
      modelBehaviour: String,
      reference: String
  ) extends BaseRuntimeConfig

}

