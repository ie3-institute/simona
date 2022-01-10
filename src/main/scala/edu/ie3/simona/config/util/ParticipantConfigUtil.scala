/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config.util

import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig._

import java.util.UUID

final case class ParticipantConfigUtil private (
    private val configs: Map[UUID, SimonaConfig.BaseRuntimeConfig],
    private val defaultLoadConfig: LoadRuntimeConfig,
    private val defaultFixedFeedInConfig: FixedFeedInRuntimeConfig,
    private val defaultPvConfig: PvRuntimeConfig,
    private val defaultWecConfig: WecRuntimeConfig,
    private val defaultEvcsConfig: EvcsRuntimeConfig
) {

  /** Queries for a [[LoadRuntimeConfig]], that applies for the given uuid and
    * either returns the config for the requested uuid or the default config. If
    * the requested uuid is valid, but the return type is not of type
    * [[LoadRuntimeConfig]] the default config for this type is returned.
    *
    * @param uuid
    *   Identifier of the requested load model
    * @return
    *   the requested [[LoadRuntimeConfig]] or a default value
    */
  def getLoadConfigOrDefault(uuid: UUID): LoadRuntimeConfig =
    configs.get(uuid) match {
      case Some(loadConfig: LoadRuntimeConfig) => loadConfig
      case _                                   => defaultLoadConfig
    }

  /** Queries for a [[PvRuntimeConfig]], that applies for the given uuid and
    * either returns the config for the requested uuid or the default config. If
    * the requested uuid is valid, but the return type is not of type
    * [[PvRuntimeConfig]] the default config for this type is returned.
    *
    * @param uuid
    *   Identifier of the requested load model
    * @return
    *   the requested [[PvRuntimeConfig]] or a default value
    */
  def getPvConfigOrDefault(uuid: UUID): PvRuntimeConfig =
    configs.get(uuid) match {
      case Some(pvRuntimeConfig: PvRuntimeConfig) => pvRuntimeConfig
      case _                                      => defaultPvConfig
    }

  def getWecConfigOrDefault(uuid: UUID): WecRuntimeConfig =
    configs.get(uuid) match {
      case Some(wecRuntimeConfig: WecRuntimeConfig) => wecRuntimeConfig
      case _                                        => defaultWecConfig
    }

  /** Queries for a [[FixedFeedInRuntimeConfig]], that applies for the given
    * uuid and either returns the config for the requested uuid or the default
    * config. If the requested uuid is valid, but the return type is not of type
    * [[FixedFeedInRuntimeConfig]] the default config for this type is returned.
    *
    * @param uuid
    *   Identifier of the requested fixed feed in model
    * @return
    *   the requested [[FixedFeedInRuntimeConfig]] or a default value
    */
  def getFixedFeedConfigOrDefault(uuid: UUID): FixedFeedInRuntimeConfig =
    configs.get(uuid) match {
      case Some(ffinConfig: FixedFeedInRuntimeConfig) => ffinConfig
      case _ => defaultFixedFeedInConfig
    }

  /** Queries for a [[EvcsRuntimeConfig]], that applies for the given uuid and
    * either returns the config for the requested uuid or the default config. If
    * the requested uuid is valid, but the return type is not of type
    * [[EvcsRuntimeConfig]] the default config for this type is returned.
    *
    * @param uuid
    *   Identifier of the requested Evcs model
    * @return
    *   the requested [[EvcsRuntimeConfig]] or a default value
    */
  def getEvcsConfigOrDefault(uuid: UUID): EvcsRuntimeConfig =
    configs.get(uuid) match {
      case Some(evcsConfig: EvcsRuntimeConfig) => evcsConfig
      case _                                   => defaultEvcsConfig
    }
}

case object ParticipantConfigUtil {

  /** Creates a system participant config utility from the given participant
    * configuration. It builds a map from uuid to individual system participants
    * config for faster access.
    *
    * @param subConfig
    *   Configuration sub tree for the behaviour of system participants
    * @return
    *   a matching config utility
    */
  def apply(
      subConfig: SimonaConfig.Simona.Runtime.Participant
  ): ParticipantConfigUtil = {
    new ParticipantConfigUtil(
      buildUuidMapping(
        subConfig.load.individualConfigs ++
          subConfig.fixedFeedIn.individualConfigs ++
          subConfig.pv.individualConfigs ++
          subConfig.evcs.individualConfigs ++
          subConfig.wec.individualConfigs
      ),
      subConfig.load.defaultConfig,
      subConfig.fixedFeedIn.defaultConfig,
      subConfig.pv.defaultConfig,
      subConfig.wec.defaultConfig,
      subConfig.evcs.defaultConfig
    )
  }

  private def buildUuidMapping(
      configs: List[BaseRuntimeConfig]
  ): Map[UUID, BaseRuntimeConfig] =
    configs
      .flatMap(modelConfig =>
        modelConfig.uuids
          .map(UUID.fromString(_) -> modelConfig)
      )
      .toMap

}
