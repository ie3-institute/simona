/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ActorTestKit,
  LoggingTestKit,
  ScalaTestWithActorTestKit,
}
import com.typesafe.config.ConfigValueFactory
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent.{
  CheckWindowPassed,
  Done,
  Error,
  InitComplete,
  Initializing,
  PowerFlowFailed,
  Ready,
  Simulating,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.util.TickUtil._
import edu.ie3.util.TimeUtil
import org.slf4j.event.Level

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

class RuntimeEventListenerSpec
    extends ScalaTestWithActorTestKit(
      ActorTestKit.ApplicationTestConfig.withValue(
        // Timeout for LoggingTestKit via TestKitSettings
        // Log message sometimes seem to take a while until caught by the test kit
        "org.apache.pekko.actor.testkit.typed.filter-leeway",
        ConfigValueFactory.fromAnyRef("30s"),
      )
    )
    with UnitSpec {

  // global variables
  val startDateTimeString = "2011-01-01T00:00:00Z"
  val endTick = 3600
  val duration = 10805000
  val errMsg = "testing error msg"

  val currentTick = 0

  "A runtime event listener" must {
    "add a valid runtime event to the blocking queue for further processing" in {
      val eventQueue = new LinkedBlockingQueue[RuntimeEvent]()

      val listenerRef = spawn(
        RuntimeEventListener(
          SimonaConfig.Simona.Runtime.Listener(
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

    "return valid log messages for each status event" in {

      val listenerRef = spawn(
        RuntimeEventListener(
          SimonaConfig.Simona.Runtime.Listener(
            None,
            None,
          ),
          None,
          startDateTimeString,
        )
      )

      def calcTime(curTick: Long): String = {
        TimeUtil.withDefaults.toString(
          curTick.toDateTime(
            TimeUtil.withDefaults.toZonedDateTime(startDateTimeString)
          )
        )
      }

      // Fail two power flows, should get counted
      listenerRef ! PowerFlowFailed
      listenerRef ! PowerFlowFailed

      val events = Seq(
        (
          Initializing,
          Level.INFO,
          "Initializing Agents and Services for Simulation ... ",
        ),
        (
          InitComplete(0L),
          Level.INFO,
          s"Initialization complete. (duration: 0h : 0m : 0s )",
        ),
        (
          Ready(currentTick, 0L),
          Level.INFO,
          s"Switched from 'Simulating' to 'Ready'. Last simulated time: ${calcTime(currentTick)}.",
        ),
        (
          Simulating(currentTick, endTick),
          Level.INFO,
          s"Simulating from ${calcTime(currentTick)} until ${calcTime(endTick)}.",
        ),
        (
          CheckWindowPassed(currentTick, 0L),
          Level.INFO,
          s"Simulation until ${calcTime(currentTick)} completed.",
        ),
        (
          Done(endTick, duration, errorInSim = false),
          Level.INFO,
          s"Simulation completed with \u001b[0;32mSUCCESS (Failed PF: 2)\u001b[0;30m in time step ${calcTime(endTick)}. Total runtime: 3h : 0m : 5s ",
        ),
        (
          Error(errMsg),
          Level.ERROR,
          errMsg,
        ),
      )

      val loggingTestKit = LoggingTestKit.empty
        // logger name for ActorContext loggers (ctx.log) is the class name
        .withLoggerName(RuntimeEventListener.getClass.getName)

      events.foreach { case (event, level, msg) =>
        loggingTestKit
          .withLogLevel(level)
          .withMessageContains(msg)
          .expect {
            listenerRef ! event
          }
      }

    }
  }
}
