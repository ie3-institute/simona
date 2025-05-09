/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import edu.ie3.simona.event.listener.DeadLetterListener
import org.apache.pekko.actor.{ActorSystem, DeadLetter}
import org.apache.pekko.testkit.TestKit
import org.scalatest.BeforeAndAfterAll

class TestKitWithShutdown(actorSystem: ActorSystem)
    extends TestKit(actorSystem)
    with UnitSpec
    with BeforeAndAfterAll {

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    // Listen to dead letter office
    val deadLetterListener = system.actorOf(DeadLetterListener.props())
    system.eventStream.subscribe(deadLetterListener, classOf[DeadLetter])
  }

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

}
