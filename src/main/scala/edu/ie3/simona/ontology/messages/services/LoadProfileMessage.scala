/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.datamodel.models.profile.LoadProfile
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ServiceRegistrationMessage
import org.apache.pekko.actor.typed.ActorRef
import squants.Power

sealed trait LoadProfileMessage

/** Declares all messages sent and received by the load profile service and load
  * profile data provided through these messages.
  */
object LoadProfileMessage {

  private[services] trait LoadProfileMessageInternal extends LoadProfileMessage

  /** Indicate the [[edu.ie3.simona.service.load.LoadProfileService]] that the
    * requested agent wants to receive
    * [[edu.ie3.datamodel.models.value.PValue]]s for the provided load profile.
    * @param requestingActor
    *   The actor requesting registration for load profile data.
    * @param loadProfile
    *   Of the
    *   [[edu.ie3.datamodel.models.timeseries.repetitive.LoadProfileTimeSeries]].
    */
  final case class RegisterForLoadProfileService(
      requestingActor: ActorRef[ParticipantAgent.Request],
      loadProfile: LoadProfile,
  ) extends LoadProfileMessage
      with ServiceRegistrationMessage

  /** Container class for the load profile information at a certain point in
    * time.
    *
    * @param averagePower
    *   The average power for the current interval.
    */
  final case class LoadData(
      averagePower: Power
  ) extends SecondaryData

}
