/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config.util

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.BaseOutputConfig
import edu.ie3.simona.event.notifier.{Notifier, ParticipantNotifierConfig}
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.util.EntityMapperUtil

/** A config utility to handle the output configuration for participant models.
  * It holds a map from participant model type to actual config for speeding up
  * processing.
  *
  * @param defaultConfig
  *   Default config to use, when there is no specific one
  * @param configs
  *   Mapping from notifier identifier to it's notifier configuration
  */
final case class BaseOutputConfigUtil(
    private val defaultConfig: ParticipantNotifierConfig,
    private val configs: Map[
      NotifierIdentifier.Value,
      ParticipantNotifierConfig
    ]
) {
  def getOrDefault(
      notifierId: NotifierIdentifier.Value
  ): ParticipantNotifierConfig =
    configs.getOrElse(notifierId, defaultConfig)

  /** Get all identifiers of [[Notifier]] implementations, that will announce
    * new simulation results
    *
    * @return
    *   A set of applicable notifiers
    */
  def simulationResultIdentifiersToConsider: Set[NotifierIdentifier.Value] =
    if (defaultConfig.simulationResultInfo) {
      /* Generally inform about all simulation results, but not on those, that are explicitly marked */
      NotifierIdentifier.values -- configs.flatMap {
        case (
              notifierId,
              ParticipantNotifierConfig(resultInfo, _)
            ) if !resultInfo =>
          Some(notifierId)
        case _ => None
      }
    } else {
      /* Only register those events, that are explicitly marked to be considered */
      configs.flatMap {
        case (
              notifierId,
              ParticipantNotifierConfig(resultInfo, _)
            ) if resultInfo =>
          Some(notifierId)
        case _ => None
      }.toSet
    }

  def simulationResultEntitiesToConsider: Set[Class[_ <: ResultEntity]] =
    simulationResultIdentifiersToConsider.map(notifierId =>
      EntityMapperUtil.getResultEntityClass(notifierId)
    )
}

case object BaseOutputConfigUtil {
  def apply(
      subConfig: SimonaConfig.Simona.Output.Participant
  ): BaseOutputConfigUtil = {
    val defaultConfig = subConfig.defaultConfig match {
      case BaseOutputConfig(_, powerRequestReply, simulationResult) =>
        ParticipantNotifierConfig(simulationResult, powerRequestReply)
    }
    val configMap = subConfig.individualConfigs.map {
      case BaseOutputConfig(notifier, powerRequestReply, simulationResult) =>
        try {
          val id = NotifierIdentifier(notifier)
          id -> ParticipantNotifierConfig(simulationResult, powerRequestReply)
        } catch {
          case e: NoSuchElementException =>
            throw new InvalidConfigParameterException(
              s"Cannot parse $notifier to known result event notifier.",
              e
            )
        }
    }.toMap
    new BaseOutputConfigUtil(defaultConfig, configMap)
  }
}
