/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ProvisionMessage,
  ServiceInternal,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.ServiceRegistrationMessage
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.{ActorRef => ClassicRef}

import java.util.UUID

sealed trait PrimaryDataMessage extends ServiceInternal

object PrimaryDataMessage {

  private[services] trait PrimaryInternal extends PrimaryDataMessage

  /** Provide primary data to subscribes
    *
    * @param tick
    *   Current tick
    * @param data
    *   The payload
    * @param nextDataTick
    *   The next tick, when data is available
    */
  final case class ProvidePrimaryDataMessage(
      override val tick: Long,
      override val serviceRef: ActorRef[PrimaryDataMessage],
      override val data: PrimaryData,
      override val nextDataTick: Option[Long],
  ) extends ServiceMessage.ProvisionMessage[PrimaryData]
      with PrimaryDataMessage

  /** Message to register with a primary data service.
    *
    * @param actorRef
    *   actor ref for the agent to be registered
    * @param inputModelUuid
    *   Identifier of the input model
    */
  final case class PrimaryServiceRegistrationMessage(
      actorRef: ClassicRef,
      inputModelUuid: UUID,
  ) extends ServiceRegistrationMessage
      with PrimaryDataMessage

  /** This message can be sent from a proxy to a subordinate worker in order to
    * forward the original registration request. This message may only be used,
    * if no further information are needed.
    *
    * @param requestingActor
    *   Reference to the requesting actor
    */
  final case class WorkerRegistrationMessage(requestingActor: ClassicRef)
      extends ServiceRegistrationMessage
      with PrimaryDataMessage

  /** Provides primary data in the form of [[ComplexPower]]
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
      override val serviceRef: ActorRef[PrimaryDataMessage],
      override val data: ComplexPower,
      override val nextDataTick: Option[Long],
  ) extends ProvisionMessage[ComplexPower]
      with PrimaryDataMessage
}
