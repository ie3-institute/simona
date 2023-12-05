/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test

import org.apache.pekko.actor.{ActorRef, ActorSystem}
import org.apache.pekko.testkit.TestProbe
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleTriggerMessage
import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.simona.test.common.AgentSpec

/** Class to help building tests for
  * [[edu.ie3.simona.agent.participant.ParticipantAgent]]s
  *
  * @param actorSystem
  *   The actor system to use for building actors
  */
class ParticipantAgentSpec(actorSystem: ActorSystem)
    extends AgentSpec(actorSystem) {
  protected val scheduler: TestProbe = TestProbe("schedulerProbe")
  protected val primaryServiceProxy: TestProbe = TestProbe(
    "primaryServiceProxyProbe"
  )
  protected val weatherService: TestProbe = TestProbe("weatherServiceProbe")
}
