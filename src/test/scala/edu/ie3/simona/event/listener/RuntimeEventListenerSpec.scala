/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import akka.actor.ActorSystem
import akka.testkit.{EventFilter, ImplicitSender, TestActorRef}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.event.RuntimeEvent.{
  CheckWindowPassed,
  Done,
  Error,
  InitComplete,
  Initializing,
  Ready,
  Simulating
}
import edu.ie3.simona.event.{Event, RuntimeEvent}
import edu.ie3.simona.logging.SimonaLogging.SimonaBusLogging
import edu.ie3.simona.test.common.TestKitWithShutdown
import edu.ie3.simona.util.TickUtil._
import edu.ie3.util.TimeUtil
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.concurrent.duration.Duration

class RuntimeEventListenerSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "RuntimeEventListenerSpec",
        ConfigFactory
          .parseString(
            """
                       akka.loggers = ["edu.ie3.simona.test.common.SilentTestEventListener"]
            |akka.loglevel="info"
          """.stripMargin
          )
      )
    )
    with ImplicitSender
    with AnyWordSpecLike
    with should.Matchers
    with PrivateMethodTester {

  implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)

  // global variables
  val eventQueue = new LinkedBlockingQueue[RuntimeEvent]()
  val startDateTimeString = "2011-01-01 00:00:00"
  val endTick = 3600
  val duration = 0.1
  val errMsg =
    "Und wenn du lange in einen Abgrund blickst, blickt der Abgrund auch in dich hinein"

  // to change for Ready event test cases
  val currentTick = 0

  // build the listener
  private val listenerRef = TestActorRef(
    new RuntimeEventListener(
      None,
      Some(eventQueue),
      startDateTimeString
    )
  )

  private val logPrefix = listenerRef.underlyingActor.log match {
    case simonaLogging: SimonaBusLogging =>
      simonaLogging.prefix()
    case x =>
      throw new IllegalArgumentException(
        s"Invalid logger in RuntimeEventListener: $x"
      )
  }

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

      def roundDuration(dur: Double): Double =
        roundAt(5)(dur / 1000)

      def roundAt(precision: Int)(number: Double): Double = {
        val s = math pow (10, precision)
        (math round number * s) / s
      }

      def durationAndMemoryString(dur: Double) = {
        val memory = math.round(
          10 * (Runtime.getRuntime.totalMemory() - Runtime.getRuntime
            .freeMemory()) / Math
            .pow(1000, 3)
        ) / 10.0

        s"(duration: ${roundDuration(dur)} s, memory: $memory GB)"
      }

      // runtime events without schedulerReadyCheckWindow dependency
      val eventsToProcess = List(
        Initializing,
        InitComplete(0),
        Ready(currentTick, 0),
        Simulating(currentTick, 0),
        CheckWindowPassed(currentTick, 0),
        Done(endTick, duration, 0, errorInSim = false),
        Error(errMsg)
      )

      val assertDoneFilter
          : PartialFunction[RuntimeEvent, (EventFilter, Event)] = {
        case event @ Initializing =>
          (
            EventFilter.info(
              message =
                s"$logPrefix Initializing Agents and Services for Simulation ... ",
              occurrences = 1
            ),
            event
          )
        case event @ InitComplete(duration) =>
          (
            EventFilter.info(
              message =
                s"$logPrefix Initialization complete. (duration: ${roundDuration(duration)} s)",
              occurrences = 1
            ),
            event
          )

        case event @ CheckWindowPassed(tick, duration) =>
          (
            EventFilter.info(
              message =
                s"$logPrefix ******* Simulation until ${calcTime(tick)} completed. ${durationAndMemoryString(duration)} ******",
              occurrences = 1
            ),
            event
          )
        case event @ Ready(tick, duration) =>
          (
            EventFilter.info(
              message =
                s"$logPrefix ******* Switched from 'Simulating' to 'Ready'. Last simulated time: ${calcTime(tick)}. ${durationAndMemoryString(duration)}  ******",
              occurrences = 1
            ),
            event
          )

        case event @ Simulating(startTick, endTick) =>
          (
            EventFilter.info(
              message =
                s"$logPrefix ******* Simulating from ${calcTime(startTick)} until ${calcTime(endTick)}. *******",
              occurrences = 1
            ),
            event
          )

        case event @ Done(tick, duration, _, _) =>
          (
            EventFilter.info(
              message =
                s"$logPrefix ******* Simulation completed with \u001b[0;32mSUCCESS (Failed PF: 0)\u001b[0;30m in time step ${calcTime(tick)}. Total runtime: ${roundDuration(duration)} s *******",
              occurrences = 1
            ),
            event
          )

        case event @ Error(errMsg) =>
          (
            EventFilter.error(
              message = s"$logPrefix $errMsg",
              occurrences = 1
            ),
            event
          )

        case _ => fail()

      }

      val intercept: PartialFunction[(EventFilter, Event), Unit] = {
        case (filter: EventFilter, event: RuntimeEvent) =>
          filter.intercept {
            listenerRef ! event
          }
          filter assertDone Duration("1 s")
      }

      eventsToProcess.foreach(assertDoneFilter andThen intercept)
    }
  }
}
