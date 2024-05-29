/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerData
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ProvisionMessage
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import org.apache.pekko.actor.ActorRef

sealed trait PrimaryDataMessage

object PrimaryDataMessage {

  /** Provides primary data in the form of [[ApparentPower]]
    *
    * @param tick
    *   Tick, the data belongs to
    * @param data
    *   The actual payload
    * @param nextDataTick
    *   Option to the next tick, when data is available
    */
  @deprecated
  final case class ApparentPowerProvisionMessage(
      override val tick: Long,
      override val serviceRef: ActorRef,
      override val data: ApparentPowerData,
      override val nextDataTick: Option[Long],
      override val unlockKey: Option[ScheduleKey] = None,
  ) extends ProvisionMessage[ApparentPowerData]
      with PrimaryDataMessage
}
