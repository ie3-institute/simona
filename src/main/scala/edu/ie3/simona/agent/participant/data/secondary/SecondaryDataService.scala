/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.data.secondary

import org.apache.pekko.actor.ActorRef
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.Data.SecondaryData.WholesalePrice
import edu.ie3.simona.agent.participant.data.DataService
import edu.ie3.simona.ontology.messages.services.EvMessage.EvData
import edu.ie3.simona.ontology.messages.services.LoadProfileMessage.LoadProfileData
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData

/** Common properties to all secondary data services
  */
sealed trait SecondaryDataService[+D <: SecondaryData] extends DataService[D]

object SecondaryDataService {
  final case class ActorPriceService(override val actorRef: ActorRef)
      extends SecondaryDataService[WholesalePrice]

  final case class ActorWeatherService(override val actorRef: ActorRef)
      extends SecondaryDataService[WeatherData]

  final case class ActorLoadProfileService(override val actorRef: ActorRef)
      extends SecondaryDataService[LoadProfileData]

  final case class ActorExtEvDataService(override val actorRef: ActorRef)
      extends SecondaryDataService[EvData]
}
