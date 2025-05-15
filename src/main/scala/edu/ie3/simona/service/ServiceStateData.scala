/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import org.apache.pekko.actor.typed.ActorRef

trait ServiceStateData

object ServiceStateData {

  /** Data that is required to initialize a SimonaService
    */
  trait InitializeServiceStateData extends ServiceStateData

  trait ServiceBaseStateData extends ServiceStateData

  final case class ServiceConstantStateData(
      scheduler: ActorRef[SchedulerMessage],
      activationAdapter: ActorRef[Activation],
  ) extends ServiceStateData

}
