/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import org.apache.pekko.actor.typed.ActorRef

/** Collections of all messages, that are send to and from the different
  * services
  */
sealed trait ServiceMessage

object ServiceMessage {

  private[services] trait ServiceInternal extends ServiceMessage

  /** Actual provision of data
    *
    * @tparam D
    *   type of data that is delivered
    */
  trait ProvisionMessage[D <: Data] extends ServiceMessage {
    val tick: Long
    val serviceRef: ActorRef[_]
    val data: D

    /** Next tick at which data could arrive. If None, no data is expected for
      * the rest of the simulation
      */
    val nextDataTick: Option[Long]
  }
}
