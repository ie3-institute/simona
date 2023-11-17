/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.trigger

import edu.ie3.simona.service.ServiceStateData
import edu.ie3.simona.util.SimonaConstants

trait Trigger {
  def tick: Long
}

/** General trigger for [[edu.ie3.simona.agent.SimonaAgent]] s
  */
object Trigger {

  /** Initialization Triggers
    */
  sealed trait InitializeTrigger extends Trigger {
    override val tick: Long = SimonaConstants.INIT_SIM_TICK
  }

  /** Trigger to initialize the service agents (normally during simulation
    * initialization)
    */
  @Deprecated
  final case class InitializeServiceTrigger[+I <: ServiceStateData](
      initializeStateData: I
  ) extends InitializeTrigger

  /** Trigger to start a general activity e.g. reactivate the actor. May only be
    * sent by [[edu.ie3.simona.scheduler.Scheduler]]
    */
  @Deprecated
  final case class ActivityStartTrigger(tick: Long) extends Trigger

}
