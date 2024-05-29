/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.data.primary

import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.PrimaryDataWithApparentPower
import edu.ie3.simona.agent.participant.data.DataService
import edu.ie3.util.scala.quantities.ApparentPower
import org.apache.pekko.actor.ActorRef

/** Enum-like trait to denote possible external data sources for systems
  */
sealed trait PrimaryDataService[P, +D <: PrimaryData[P]] extends DataService[D]

object PrimaryDataService {

  /** Dummy implementation of a primary data source
    *
    * @param actorRef
    *   actor reference of the actual source
    */
  final case class DummyPrimaryService(override val actorRef: ActorRef)
      extends PrimaryDataService[ApparentPower, PrimaryDataWithApparentPower]
}
