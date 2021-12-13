/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import akka.actor.ActorSystem
import akka.testkit.ImplicitSender
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.akka.SimonaActorRef
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

/** Class to help building tests for agents
  *
  * @param actorSystem
  *   The actor system to use for building actors
  */
class AgentSpec(actorSystem: ActorSystem)
    extends TestKitWithShutdown(actorSystem)
    with ImplicitSender
    with SchedulerMessageFunctions
    with AnyWordSpecLike
    with should.Matchers
    with PrivateMethodTester
    with LazyLogging {

  val systemListener: Iterable[SimonaActorRef] = Iterable.empty
}
