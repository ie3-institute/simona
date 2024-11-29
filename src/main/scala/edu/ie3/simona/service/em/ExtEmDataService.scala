/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.api.data.em.ExtEmDataConnection
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import org.apache.pekko.actor.{Props, ActorRef => ClassicRef}

object ExtEmDataService {

  def props(scheduler: ClassicRef): Props = Props()

  case class InitExtEmData(
      extEmDataConnection: ExtEmDataConnection
  ) extends InitializeServiceStateData

}
