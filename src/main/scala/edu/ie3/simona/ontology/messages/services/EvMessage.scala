/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology.EvMovementsMessage.EvcsMovements
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ProvisionMessage,
  ServiceRegistrationMessage
}

import java.util.UUID

sealed trait EvMessage

object EvMessage {

  /** Indicate the [[edu.ie3.simona.service.ev.ExtEvDataService]] that the
    * requesting agent wants to receive EV movements
    *
    * @param evcs
    *   the charging station
    */
  final case class RegisterForEvDataMessage(
      evcs: UUID
  ) extends EvMessage
      with ServiceRegistrationMessage

  trait EvData extends SecondaryData

  /** Provide EV movements for the requested tick
    *
    * @param tick
    *   The tick, for which the data is requested for
    * @param data
    *   Actual information
    * @param nextDataTick
    *   Foreseen next tick, where data is available. Usually, no ticks can be
    *   foreseen within evs
    */
  final case class ProvideEvDataMessage(
      override val tick: Long,
      override val data: EvData,
      override val nextDataTick: Option[Long] = None
  ) extends EvMessage
      with ProvisionMessage[EvData]

  /** Requests number of free lots from evcs
    * @param tick
    *   The latest tick that the data is requested for
    */
  final case class EvFreeLotsRequest(
      tick: Long
  )

  final case object CurrentPriceRequest extends EvData

  /** Hold EV movements for one Evcs
    *
    * @param movements
    *   arrivals/departures of EVs
    */
  final case class EvMovementData(
      movements: EvcsMovements
  ) extends EvData {}

  trait EvResponseMessage extends EvMessage

  final case class FreeLotsResponse(
      evcs: UUID,
      freeLots: Int
  ) extends EvResponseMessage

  final case class DepartedEvsResponse(
      evcs: UUID,
      evModels: Set[EvModel]
  ) extends EvResponseMessage

  final case class CurrentPriceResponse(
      evcs: UUID,
      currentPrice: Double
  ) extends EvResponseMessage
}
