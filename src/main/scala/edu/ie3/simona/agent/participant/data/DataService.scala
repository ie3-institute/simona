/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.data

import edu.ie3.simona.akka.SimonaActorRef

/** Common properties to all data sources providing data from the outside of a
  * SystemParticipant model but not necessarily from the outside of the
  * simulation (but could be).
  */
trait DataService[+D <: Data] {

  /** A reference to the actor
    */
  val actorRef: SimonaActorRef
}
