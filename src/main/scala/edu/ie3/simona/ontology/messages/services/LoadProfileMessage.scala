/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.datamodel.models.profile.LoadProfile
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ProvisionMessage,
  ServiceRegistrationMessage,
}
import org.apache.pekko.actor.ActorRef
import squants.Power

sealed trait LoadProfileMessage

/** Declares all messages sent and received by the load profile service and load
  * profile data provided through these messages
  */
object LoadProfileMessage {

  /** Indicate the [[edu.ie3.simona.service.load.LoadProfileService]] that the
    * requested agent wants to receive
    * [[edu.ie3.datamodel.models.value.PValue]]s for the provided load profile
    * @param loadProfile
    *   of the
    *   [[edu.ie3.datamodel.models.timeseries.repetitive.LoadProfileTimeSeries]]
    */
  final case class RegisterForLoadProfileService(
      loadProfile: LoadProfile
  ) extends LoadProfileMessage
      with ServiceRegistrationMessage

  /** Provide power value for the requested tick
    * @param tick
    *   The tick, for which the data is requested for
    * @param data
    *   Actual information
    * @param nextDataTick
    *   Foreseen next tick, where data is available
    */
  final case class ProvideLoadProfileValue(
      override val tick: Long,
      override val serviceRef: ActorRef,
      override val data: LoadProfileData,
      override val nextDataTick: Option[Long],
  ) extends LoadProfileMessage
      with ProvisionMessage[LoadProfileData]

  /** Container class for the load profile information at a certain point in
    * time
    *
    * @param power
    *   value
    */
  final case class LoadProfileData(
      power: Power
  ) extends SecondaryData

}
