/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.IoConfigUtils.RuntimeKafkaParams
import edu.ie3.simona.config.RuntimeConfig._
import edu.ie3.simona.config.SimonaConfig._

final case class RuntimeConfig(
    selectedSubgrids: Option[Seq[Int]],
    selectedVoltLvls: Option[Seq[VoltLvlConfig]],
    listener: RuntimeListenerConfig = RuntimeListenerConfig(None, None),
    participant: RuntimeParticipantsConfig,
)

object RuntimeConfig {
  final case class RuntimeListenerConfig(
      eventsToProcess: Option[Seq[String]],
      kafka: Option[RuntimeKafkaParams],
  )

  final case class RuntimeParticipantsConfig(
      requestVoltageDeviationThreshold: Double = 1e-14,
      load: RuntimeParticipantConfig[LoadRuntimeConfig],
      pv: RuntimeParticipantConfig[SimpleRuntimeConfig],
      fixedFeedIn: RuntimeParticipantConfig[SimpleRuntimeConfig],
      wec: RuntimeParticipantConfig[SimpleRuntimeConfig],
      evcs: RuntimeParticipantConfig[SimpleRuntimeConfig],
  ) {
    def asSeq: Seq[RuntimeParticipantConfig[_ <: BaseRuntimeConfig]] = {
      Seq(
        load,
        pv,
        fixedFeedIn,
        wec,
        evcs,
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
      uuids: Seq[String],
      scaling: Double,
      calculateMissingReactivePowerWithModel: Boolean,
  ) extends BaseRuntimeConfig

  final case class LoadRuntimeConfig(
      uuids: Seq[String],
      scaling: Double,
      calculateMissingReactivePowerWithModel: Boolean,
      modelBehaviour: String,
      reference: String,
  ) extends BaseRuntimeConfig

  final case class EvcsRuntimeConfig(
      uuids: Seq[String],
      scaling: Double,
      calculateMissingReactivePowerWithModel: Boolean,
      chargingStrategy: String,
      lowestEvSoc: Double,
  ) extends BaseRuntimeConfig

  final case class EmRuntimeConfig(
      uuids: Seq[String],
      scaling: Double,
      calculateMissingReactivePowerWithModel: Boolean,
      curtailRegenerative: Boolean,
      aggregateFlex: String,
  ) extends BaseRuntimeConfig

  
}
