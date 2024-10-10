/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import akka.actor.testkit.typed.scaladsl.{
  ActorTestKit,
  LoggingTestKit,
  ScalaTestWithActorTestKit
}
import com.typesafe.config.ConfigValueFactory
import edu.ie3.simona.config.RuntimeConfig.RuntimeListenerConfig
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent.{
  CheckWindowPassed,
  Done,
  Error,
  InitComplete,
  Initializing,
  Ready,
  Simulating
}
import edu.ie3.simona.util.TickUtil._
import edu.ie3.util.TimeUtil
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.event.Level

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

class RuntimeEventListenerSpec
    extends ScalaTestWithActorTestKit(
      ActorTestKit.ApplicationTestConfig.withValue(
        // Timeout for LoggingTestKit via TestKitSettings
        // Log message sometimes seem to take a while until caught by the test kit
        "akka.actor.testkit.typed.filter-leeway",
        ConfigValueFactory.fromAnyRef("30s")
      )
    )
    with AnyWordSpecLike
    with should.Matchers
    with PrivateMethodTester {

  // global variables
  val eventQueue = new LinkedBlockingQueue[RuntimeEvent]()
  val startDateTimeString = "2011-01-01 00:00:00"
  val endTick = 3600
  val duration = 10805000
  val errMsg =
    "Und wenn du lange in einen Abgrund blickst, blickt der Abgrund auch in dich hinein"

  // to change for Ready event test cases
  val currentTick = 0

  // build the listener
  private val listenerRef = spawn(
    RuntimeEventListener(
      RuntimeListenerConfig(
        None,
        None
      ),
      Some(eventQueue),
      startDateTimeString
    )
  )

  "A runtime event listener" must {
    "add a valid runtime event to the blocking queue for further processing" in {

      //  valid runtime events
      val eventsToQueue: Seq[RuntimeEvent] = List(
        Initializing,
        Ready(currentTick, duration),
        Simulating(currentTick, 0),
        Done(endTick, duration, 0, errorInSim = false),
        Error(errMsg)
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

    "return valid log messages for each status event" in {

      def calcTime(curTick: Long): String = {
        TimeUtil.withDefaults.toString(
          curTick.toDateTime(
            TimeUtil.withDefaults.toZonedDateTime(startDateTimeString)
          )
        )
      }

      val events = Seq(
        (
          Initializing,
          Level.INFO,
          "Initializing Agents and Services for Simulation ... "
        ),
        (
          InitComplete(0L),
          Level.INFO,
          s"Initialization complete. (duration: 0h : 0m : 0s )"
        ),
        (
          Ready(currentTick, 0L),
          Level.INFO,
          s"Switched from 'Simulating' to 'Ready'. Last simulated time: ${calcTime(currentTick)}."
        ),
        (
          Simulating(currentTick, endTick),
          Level.INFO,
          s"Simulating from ${calcTime(currentTick)} until ${calcTime(endTick)}."
        ),
        (
          CheckWindowPassed(currentTick, 0L),
          Level.INFO,
          s"Simulation until ${calcTime(currentTick)} completed."
        ),
        (
          Done(endTick, duration, 0, errorInSim = false),
          Level.INFO,
          s"Simulation completed with \u001b[0;32mSUCCESS (Failed PF: 0)\u001b[0;30m in time step ${calcTime(endTick)}. Total runtime: 3h : 0m : 5s "
        ),
        (
          Error(errMsg),
          Level.ERROR,
          errMsg
        )
      )

      events.foreach { case (event, level, msg) =>
        LoggingTestKit.empty
          .withLogLevel(level)
          .withMessageContains(msg)
          .expect {
            listenerRef ! event
          }
      }

    }
  }
}
