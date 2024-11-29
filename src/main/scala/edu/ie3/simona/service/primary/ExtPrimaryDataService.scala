/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import org.apache.pekko.actor.{ActorRef, Props}

object ExtPrimaryDataService {

  def props(scheduler: ActorRef): Props = Props()

  case class InitExtPrimaryData(
      extPrimaryDataConnection: ExtPrimaryDataConnection
  ) extends InitializeServiceStateData
}
