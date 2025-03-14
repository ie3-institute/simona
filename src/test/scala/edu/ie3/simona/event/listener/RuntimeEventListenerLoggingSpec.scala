/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import com.typesafe.config.ConfigValueFactory
import edu.ie3.simona.config.RuntimeConfig
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
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ActorTestKit,
  LoggingTestKit,
  ScalaTestWithActorTestKit,
}
import org.slf4j.event.Level

/** Logging must be tested in a separate test, since LoggingTestKit can still
  * receive logs from test that it was not enabled for
  */
class RuntimeEventListenerLoggingSpec
    extends ScalaTestWithActorTestKit(
      ActorTestKit.ApplicationTestConfig.withValue(
        // Timeout for LoggingTestKit via TestKitSettings
        // Log message sometimes seem to take a while until caught by the test kit
        "org.apache.pekko.actor.testkit.typed.filter-leeway",
        ConfigValueFactory.fromAnyRef("30s"),
      )
    )
    with UnitSpec
    with RuntimeTestData {

  "A runtime event listener" must {
    "return valid log messages for each status event" in {

      val listenerRef = spawn(
        RuntimeEventListener(
          RuntimeConfig.Listener(
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
          s"Initialization complete. (duration: 0h : 0m : 0s : 0ms )",
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
          s"Simulation completed with \u001b[0;32mSUCCESS (Failed PF: 2)\u001b[0;0m in time step ${calcTime(endTick)}. Total runtime: 3h : 0m : 5s ",
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
