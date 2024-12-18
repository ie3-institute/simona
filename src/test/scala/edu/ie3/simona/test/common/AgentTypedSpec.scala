/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import com.typesafe.scalalogging.LazyLogging
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

/** Class to help building tests for agents
  */
class AgentTypedSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers
    with PrivateMethodTester
    with LazyLogging {

  val systemListener: Iterable[ActorRef] = Iterable.empty
}
