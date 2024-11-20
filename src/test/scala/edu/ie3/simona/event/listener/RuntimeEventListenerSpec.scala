/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener
import edu.ie3.simona.config.RuntimeConfig.RuntimeListenerConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent.{
  Done,
  Error,
  Initializing,
  Ready,
  Simulating,
}
import edu.ie3.simona.test.common.UnitSpec
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

class RuntimeEventListenerSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with RuntimeTestData {

  "A runtime event listener" must {
    "add a valid runtime event to the blocking queue for further processing" in {
      val eventQueue = new LinkedBlockingQueue[RuntimeEvent]()

      val listenerRef = spawn(
        RuntimeEventListener(
          RuntimeListenerConfig(
            None,
            None,
          ),
          Some(eventQueue),
          startDateTimeString,
        )
      )

      //  valid runtime events
      val eventsToQueue: Seq[RuntimeEvent] = List(
        Initializing,
        Ready(currentTick, duration),
        Simulating(currentTick, 0),
        Done(endTick, duration, errorInSim = false),
        Error(errMsg),
      )

      for (event <- eventsToQueue) {
        listenerRef ! event
        val actualEvent = eventQueue.poll(10, TimeUnit.SECONDS)
        actualEvent match {
          case Initializing  =>
          case _: Ready      =>
          case _: Simulating =>
          case _: Done       =>
          case _: Error      =>
          case _             => fail()
        }
      }
    }

  }
}
