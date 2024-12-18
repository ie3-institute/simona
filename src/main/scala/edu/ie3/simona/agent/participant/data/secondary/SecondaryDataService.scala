/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.data.secondary

import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.Data.SecondaryData.WholesalePrice
import edu.ie3.simona.agent.participant.data.DataService
import edu.ie3.simona.ontology.messages.services.EvMessage.EvData
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.ontology.messages.services.{
  EvMessage,
  ServiceMessage,
  WeatherMessage,
  WholeSalePriceMessage,
}
import org.apache.pekko.actor.typed.ActorRef

/** Common properties to all secondary data services
  */
sealed trait SecondaryDataService[+D <: SecondaryData, M <: ServiceMessage]
    extends DataService[D, M]

object SecondaryDataService {

  type SecondaryServiceType =
    SecondaryDataService[_ <: SecondaryData, _ <: ServiceMessage]

  final case class ActorPriceService(
      override val actorRef: ActorRef[WholeSalePriceMessage]
  ) extends SecondaryDataService[WholesalePrice, WholeSalePriceMessage]

  final case class ActorWeatherService(
      override val actorRef: ActorRef[WeatherMessage]
  ) extends SecondaryDataService[WeatherData, WeatherMessage]

  final case class ActorExtEvDataService(
      override val actorRef: ActorRef[EvMessage]
  ) extends SecondaryDataService[EvData, EvMessage]
}
