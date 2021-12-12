/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, TimeUnit}
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.event.listener.RuntimeEventListener
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeServiceTrigger
}
import edu.ie3.simona.service.ServiceStateData
import edu.ie3.simona.test.common.{ConfigTestData, TestKitWithShutdown}
import org.scalatest._
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

import scala.collection.mutable

class ExtendedSimSchedulerSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "ExtendedSimSchedulerSpec",
        ConfigFactory
          .parseString("""
            |akka.loggers = ["akka.testkit.TestEventListener"]
            |akka.loglevel="OFF"
          """.stripMargin)
      )
    )
    with ImplicitSender
    with AnyWordSpecLike
    with should.Matchers
    with PrivateMethodTester
    with ConfigTestData {

  val eventQueue: BlockingQueue[RuntimeEvent] =
    new LinkedBlockingQueue[RuntimeEvent]

  private final case class DummyInitServiceStateData(
  ) extends ServiceStateData

  "The extended version of the SimScheduler" should {

    def expectReturnComplete(
        scheduler: ActorRef,
        triggerToBeScheduled: Option[Vector[ScheduleTriggerMessage]] = None
    ): Unit = {
      expectMsgPF() {
        case msg: TriggerWithIdMessage =>
          msg.trigger match {
            case InitializeServiceTrigger(_) =>
              scheduler ! CompletionMessage(msg.triggerId, triggerToBeScheduled)
            case ActivityStartTrigger(_) =>
              scheduler ! CompletionMessage(msg.triggerId, triggerToBeScheduled)
            case _ =>
              fail(s"Unexpected trigger received: $msg")
          }
        case unexpected =>
          fail(s"Unexpected message received: $unexpected")
      }
    }

    def setupExtendedTest(simonaConfig: SimonaConfig): ActorRef = {
      // clean event queue
      eventQueue.clear()

      // build the run time listener
      val runtimeListeners = Vector(
        TestActorRef(
          new RuntimeEventListener(
            None,
            Some(eventQueue),
            simonaConfig.simona.time.startDateTime
          )
        )
      )

      // build scheduler
      system.actorOf(
        SimScheduler
          .props(
            simonaConfig.simona.time,
            runtimeListeners,
            stopOnFailedPowerFlow = false,
            autoStart = false
          )
      )
    }

    def expectSimulatingEvent(
        receivedEvent: Simulating,
        expectedElements: mutable.ArrayBuffer[RuntimeEvent]
    ): Assertion = {
      val expectedHead = expectedElements.headOption.getOrElse(
        fail("ExpectedElements is empty!")
      ) match {
        case expectedHead: Simulating =>
          expectedHead
        case unexpectedEvent =>
          fail(
            s"Unexpected event when Simulating was expected: $unexpectedEvent"
          )
      }
      expectedElements.remove(0)
      receivedEvent.startTick shouldBe expectedHead.startTick
      receivedEvent.endTick shouldBe expectedHead.endTick
    }

    def expectDoneEvent(
        receivedEvent: Done,
        expectedElements: mutable.ArrayBuffer[RuntimeEvent]
    ): Boolean = {
      val expectedHead = expectedElements.headOption.getOrElse(
        fail("ExpectedElements is empty!")
      ) match {
        case expectedHead: Done =>
          expectedHead
        case unexpectedEvent =>
          fail(s"Unexpected event when Done was expected: $unexpectedEvent")
      }
      expectedElements.remove(0)
      receivedEvent.tick shouldBe expectedHead.tick
      expectedElements.isEmpty
    }

    "finish simulation correctly when endTick == pauseScheduleAtTick" in {

      val simonaConfig = SimonaConfig(
        ConfigFactory
          .parseString(
            s"""simona.time.startDateTime = "2011-01-01 00:00:00"
            simona.time.endDateTime = "2011-01-01 01:00:00"
            simona.time.schedulerReadyCheckWindow = 900""".stripMargin
          )
          .withFallback(typesafeConfig)
          .resolve()
      )

      val scheduler = setupExtendedTest(simonaConfig)

      // send to init triggers to scheduler
      scheduler ! ScheduleTriggerMessage(
        InitializeServiceTrigger(DummyInitServiceStateData()),
        testActor
      )
      scheduler ! ScheduleTriggerMessage(
        InitializeServiceTrigger(DummyInitServiceStateData()),
        testActor
      )

      // tell scheduler to init
      scheduler ! InitSimMessage

      // we expect two init triggers we need to answer with completion, otherwise scheduler won't go on
      expectReturnComplete(
        scheduler,
        Some(
          Vector(ScheduleTriggerMessage(ActivityStartTrigger(3600), testActor))
        )
      )
      expectReturnComplete(
        scheduler,
        Some(
          Vector(ScheduleTriggerMessage(ActivityStartTrigger(3600), testActor))
        )
      )

      // wait until init is done
      var initDone = false
      while (!initDone) {
        eventQueue.poll(2, TimeUnit.SECONDS) match {
          case InitComplete(_) =>
            initDone = true
          case _ => false
        }
      }

      // tell scheduler we want to run until tick 3600
      scheduler ! StartScheduleMessage(Some(3600L))

      // the expected events in their expected order
      val expectedElements: mutable.ArrayBuffer[RuntimeEvent] =
        mutable.ArrayBuffer(
          Simulating(0, 3600),
          CheckWindowPassed(900, 0),
          CheckWindowPassed(1800, 0),
          CheckWindowPassed(2700, 0),
          CheckWindowPassed(3600, 0),
          Ready(3600, 0),
          Done(3600, 0, 0, errorInSim = false)
        )
      var runDone = false
      while (!runDone) {
        eventQueue.poll(2, TimeUnit.SECONDS) match {
          case event: CheckWindowPassed =>
            val expectedHead = expectedElements.headOption.getOrElse(
              fail("ExpectedElements is empty!")
            ) match {
              case expectedHead: CheckWindowPassed =>
                expectedHead
              case unexpectedEvent =>
                fail(
                  s"Unexpected event when CheckWindowPassed was expected: $unexpectedEvent"
                )
            }
            expectedElements.remove(0)
            event.tick shouldBe expectedHead.tick

            if (event.tick == 2700) {
              // when we passed tick 2700 we also expect that two message arrive at tick 3600 we need to answer with complete
              expectReturnComplete(scheduler)
              expectReturnComplete(scheduler)

              // we also expect that the scheduler answers with a SimulationSuccessfullMessage to the sender of the run request (this test)
              expectMsgPF() {
                case SimulationSuccessfulMessage =>
                case msg =>
                  fail(
                    s"Unexpected message received when SimulationSuccessfulMessage was expected: $msg"
                  )
              }
            }
          case event: Simulating =>
            expectSimulatingEvent(event, expectedElements)

          case event: Ready =>
            val expectedHead = expectedElements.headOption.getOrElse(
              fail("ExpectedElements is empty!")
            ) match {
              case expectedHead: Ready =>
                expectedHead
              case unexpectedEvent =>
                fail(
                  s"Unexpected event when Ready was expected: $unexpectedEvent"
                )
            }
            expectedElements.remove(0)
            event.tick shouldBe expectedHead.tick
          case event: Done =>
            runDone = expectDoneEvent(event, expectedElements)
          case unexpectedEvent =>
            fail(s"Unexpected event received: $unexpectedEvent")
        }
      }
    }

    "work as expected when resolution == schedulerReadyCheckWindow" in {

      val simonaConfig = SimonaConfig(
        ConfigFactory
          .parseString(
            s"""simona.time.startDateTime = "2011-01-01 00:00:00"
            simona.time.endDateTime = "2011-01-01 01:00:00"
            simona.time.schedulerReadyCheckWindow = 900""".stripMargin
          )
          .withFallback(typesafeConfig)
          .resolve()
      )

      val scheduler = setupExtendedTest(simonaConfig)
      val resolution = 900

      // send to init triggers to scheduler
      scheduler ! ScheduleTriggerMessage(
        InitializeServiceTrigger(DummyInitServiceStateData()),
        testActor
      )
      scheduler ! ScheduleTriggerMessage(
        InitializeServiceTrigger(DummyInitServiceStateData()),
        testActor
      )

      // tell scheduler to init
      scheduler ! InitSimMessage

      // we expect two init triggers we need to answer with completion, otherwise scheduler won't go on
      expectReturnComplete(
        scheduler,
        Some(
          Vector(
            ScheduleTriggerMessage(ActivityStartTrigger(resolution), testActor)
          )
        )
      )
      expectReturnComplete(
        scheduler,
        Some(
          Vector(
            ScheduleTriggerMessage(ActivityStartTrigger(resolution), testActor)
          )
        )
      )

      // wait until init is done
      var initDone = false
      while (!initDone) {
        eventQueue.poll(2, TimeUnit.SECONDS) match {
          case InitComplete(_) =>
            initDone = true
          case _ => false
        }
      }

      // tell scheduler we want to run until tick 3600
      scheduler ! StartScheduleMessage()

      // the expected events in their expected order
      val expectedElements: mutable.ArrayBuffer[RuntimeEvent] =
        mutable.ArrayBuffer(
          Simulating(0, 3600),
          CheckWindowPassed(900, 0),
          CheckWindowPassed(1800, 0),
          CheckWindowPassed(2700, 0),
          CheckWindowPassed(3600, 0),
          Done(3600, 0, 0, errorInSim = false)
        )

      // expect a simulating event
      eventQueue.poll(2, TimeUnit.SECONDS) match {
        case simulating: Simulating =>
          expectSimulatingEvent(simulating, expectedElements)
        case event => fail(s"Expected Simulating event but received $event")
      }

      // return with completion incl. new trigger for resolution*2
      expectReturnComplete(
        scheduler,
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(resolution * 2),
              testActor
            )
          )
        )
      )
      expectReturnComplete(
        scheduler,
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(resolution * 2),
              testActor
            )
          )
        )
      )

      // expect a checkWindowPassed event
      eventQueue.poll(2, TimeUnit.SECONDS) match {
        case event: CheckWindowPassed =>
          val expectedHead = expectedElements.headOption.getOrElse(
            fail("ExpectedElements is empty!")
          ) match {
            case expectedHead: CheckWindowPassed =>
              expectedHead
            case unexpectedEvent =>
              fail(
                s"Unexpected event when CheckWindowPassed was expected: $unexpectedEvent"
              )
          }
          expectedElements.remove(0)
          event.tick shouldBe expectedHead.tick

        case event => fail(s"Expected Simulating event but received $event")
      }

      // return with completion incl. new trigger for resolution*3
      expectReturnComplete(
        scheduler,
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(resolution * 3),
              testActor
            )
          )
        )
      )
      expectReturnComplete(
        scheduler,
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(resolution * 3),
              testActor
            )
          )
        )
      )

      // expect a checkWindowPassed event
      eventQueue.poll(2, TimeUnit.SECONDS) match {
        case event: CheckWindowPassed =>
          val expectedHead = expectedElements.headOption.getOrElse(
            fail("ExpectedElements is empty!")
          ) match {
            case expectedHead: CheckWindowPassed =>
              expectedHead
            case unexpectedEvent =>
              fail(
                s"Unexpected event when CheckWindowPassed was expected: $unexpectedEvent"
              )
          }
          expectedElements.remove(0)
          event.tick shouldBe expectedHead.tick

        case event => fail(s"Expected Simulating event but received $event")
      }

      // return with completion incl. new trigger for resolution*4
      expectReturnComplete(
        scheduler,
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(resolution * 4),
              testActor
            )
          )
        )
      )
      expectReturnComplete(
        scheduler,
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(resolution * 4),
              testActor
            )
          )
        )
      )

      // expect a checkWindowPassed for tick 2700, 3600 and a Done event
      var runDone = false
      while (!runDone) {
        eventQueue.poll(2, TimeUnit.SECONDS) match {
          case event: CheckWindowPassed =>
            val expectedHead = expectedElements.headOption.getOrElse(
              fail("ExpectedElements is empty!")
            ) match {
              case expectedHead: CheckWindowPassed =>
                expectedHead
              case unexpectedEvent =>
                fail(
                  s"Unexpected event when CheckWindowPassed was expected: $unexpectedEvent"
                )
            }
            expectedElements.remove(0)
            event.tick shouldBe expectedHead.tick

            if (event.tick == 2700) {
              // when we passed tick 2700 we also expect that two message arrive at tick 3600 we need to answer with complete
              expectReturnComplete(scheduler)
              expectReturnComplete(scheduler)

              // we also expect that the scheduler answers with a SimulationSuccessfullMessage to the sender of the run request (this test)
              expectMsgPF() {
                case SimulationSuccessfulMessage =>
                case msg =>
                  fail(
                    s"Unexpected message received when SimulationSuccessfulMessage was expected: $msg"
                  )
              }
            }

          case event: Done =>
            runDone = expectDoneEvent(event, expectedElements)
          case event => fail(s"Expected Simulating event but received $event")
        }
      }

      expectedElements.size shouldBe 0

    }

    "finish simulation correctly when endTick < pauseScheduleAtTick" in {

      val simonaConfig = SimonaConfig(
        ConfigFactory
          .parseString(
            s"""simona.time.startDateTime = "2011-01-01 00:00:00"
            simona.time.endDateTime = "2011-01-01 01:00:00"
            simona.time.schedulerReadyCheckWindow = 900""".stripMargin
          )
          .withFallback(typesafeConfig)
          .resolve()
      )

      val scheduler = setupExtendedTest(simonaConfig)

      // send to init triggers to scheduler
      scheduler ! ScheduleTriggerMessage(
        InitializeServiceTrigger(DummyInitServiceStateData()),
        testActor
      )
      scheduler ! ScheduleTriggerMessage(
        InitializeServiceTrigger(DummyInitServiceStateData()),
        testActor
      )

      // tell scheduler to init
      scheduler ! InitSimMessage

      // we expect two init triggers we need to answer with completion, otherwise scheduler won't go on
      expectReturnComplete(
        scheduler,
        Some(
          Vector(ScheduleTriggerMessage(ActivityStartTrigger(3600), testActor))
        )
      )
      expectReturnComplete(
        scheduler,
        Some(
          Vector(ScheduleTriggerMessage(ActivityStartTrigger(3600), testActor))
        )
      )

      // wait until init is done
      var initDone = false
      while (!initDone) {
        eventQueue.poll(2, TimeUnit.SECONDS) match {
          case InitComplete(_) =>
            initDone = true
          case _ => false
        }
      }

      // tell scheduler we want to run until tick 7200 (endTick == 3600)
      scheduler ! StartScheduleMessage(Some(7200L))

      val expectedElements: mutable.ArrayBuffer[RuntimeEvent] =
        mutable.ArrayBuffer(
          Simulating(0, 3600),
          CheckWindowPassed(900, 0),
          CheckWindowPassed(1800, 0),
          CheckWindowPassed(2700, 0),
          CheckWindowPassed(3600, 0),
          Done(3600, 0, 0, errorInSim = false)
        )

      var runDone = false
      while (!runDone) {
        eventQueue.poll(2, TimeUnit.SECONDS) match {
          case event: CheckWindowPassed =>
            val expectedHead = expectedElements.headOption.getOrElse(
              fail("ExpectedElements is empty!")
            ) match {
              case expectedHead: CheckWindowPassed =>
                expectedHead
              case unexpectedEvent =>
                fail(
                  s"Unexpected event when CheckWindowPassed was expected: $unexpectedEvent"
                )
            }
            expectedElements.remove(0)
            event.tick shouldBe expectedHead.tick

            if (event.tick == 2700) {
              // when we passed tick 2700 we also expect that two message arrive at tick 3600 we need to answer with complete
              expectReturnComplete(scheduler)
              expectReturnComplete(scheduler)

              // we also expect that the scheduler answers with a SimulationSuccessfullMessage to the sender of the run request (this test)
              expectMsgPF() {
                case SimulationSuccessfulMessage =>
                case msg =>
                  fail(
                    s"Unexpected message received when SimulationSuccessfulMessage was expected: $msg"
                  )
              }
            }
          case event: Simulating =>
            expectSimulatingEvent(event, expectedElements)
          case event: Done =>
            runDone = expectDoneEvent(event, expectedElements)
          case unexpectedEvent =>
            fail(s"Unexpected event received: $unexpectedEvent")
        }
      }
    }

    "finish simulation correctly when endTick > first pauseScheduleAtTick" in {

      val simonaConfig = SimonaConfig(
        ConfigFactory
          .parseString(
            s"""simona.time.startDateTime = "2011-01-01 00:00:00"
            simona.time.endDateTime = "2011-01-01 02:00:00"
            simona.time.schedulerReadyCheckWindow = 900""".stripMargin
          )
          .withFallback(typesafeConfig)
          .resolve()
      )

      val scheduler = setupExtendedTest(simonaConfig)

      // send to init triggers to scheduler
      scheduler ! ScheduleTriggerMessage(
        InitializeServiceTrigger(DummyInitServiceStateData()),
        testActor
      )
      scheduler ! ScheduleTriggerMessage(
        InitializeServiceTrigger(DummyInitServiceStateData()),
        testActor
      )

      // tell scheduler to init
      scheduler ! InitSimMessage

      // we expect two init triggers we need to answer with completion, otherwise scheduler won't go on
      expectReturnComplete(
        scheduler,
        Some(
          Vector(ScheduleTriggerMessage(ActivityStartTrigger(3600), testActor))
        )
      )
      expectReturnComplete(
        scheduler,
        Some(
          Vector(ScheduleTriggerMessage(ActivityStartTrigger(3600), testActor))
        )
      )

      // wait until init is done
      var initDone = false
      while (!initDone) {
        eventQueue.poll(2, TimeUnit.SECONDS) match {
          case InitComplete(_) =>
            initDone = true
          case _ => false
        }
      }

      // tell scheduler we want to run until tick 7200 (endTick == 3600)
      scheduler ! StartScheduleMessage(Some(3600L))

      val expectedElements: mutable.ArrayBuffer[RuntimeEvent] =
        mutable.ArrayBuffer(
          Simulating(0, 3600),
          CheckWindowPassed(900, 0),
          CheckWindowPassed(1800, 0),
          CheckWindowPassed(2700, 0),
          CheckWindowPassed(3600, 0),
          Ready(3600, 0),
          Simulating(3601, 7200),
          CheckWindowPassed(4500, 0),
          CheckWindowPassed(5400, 0),
          CheckWindowPassed(6300, 0),
          CheckWindowPassed(7200, 0),
          Ready(7200, 0),
          Done(7200, 0, 0, errorInSim = false)
        )

      var runDone = false
      while (!runDone) {
        eventQueue.poll(2, TimeUnit.SECONDS) match {
          case event: CheckWindowPassed =>
            val expectedHead = expectedElements.headOption.getOrElse(
              fail("ExpectedElements is empty!")
            ) match {
              case expectedHead: CheckWindowPassed =>
                expectedHead
              case unexpectedEvent =>
                fail(
                  s"Unexpected event when CheckWindowPassed was expected: $unexpectedEvent"
                )
            }
            expectedElements.remove(0)
            event.tick shouldBe expectedHead.tick

            if (event.tick == 2700 || event.tick == 6300) {
              // when we passed tick 2700 or 6300 we also expect that two message arrive at tick 3600 we need to answer with complete
              expectReturnComplete(
                scheduler,
                Some(
                  Vector(
                    ScheduleTriggerMessage(
                      ActivityStartTrigger(7200),
                      testActor
                    )
                  )
                )
              )
              expectReturnComplete(
                scheduler,
                Some(
                  Vector(
                    ScheduleTriggerMessage(
                      ActivityStartTrigger(7200),
                      testActor
                    )
                  )
                )
              )
            }

            if (event.tick == 6300) {
              expectReturnComplete(scheduler)
              expectReturnComplete(scheduler)
              // we also expect that the scheduler answers with a SimulationSuccessfulMessage @ tick 6300 to the sender of the run request (this test)
              expectMsgPF() {
                case SimulationSuccessfulMessage =>
                case msg =>
                  fail(
                    s"Unexpected message received when SimulationSuccessfulMessage was expected: $msg"
                  )

              }
            }

          case event: Simulating =>
            expectSimulatingEvent(event, expectedElements)
          case event: Ready =>
            val expectedHead = expectedElements.headOption.getOrElse(
              fail("ExpectedElements is empty!")
            ) match {
              case expectedHead: Ready =>
                expectedHead
              case unexpectedEvent =>
                fail(
                  s"Unexpected event when Ready was expected: $unexpectedEvent"
                )
            }
            expectedElements.remove(0)
            event.tick shouldBe expectedHead.tick

            // when we received the first ready, we need to tell the scheduler to go on
            if (event.tick == 3600)
              scheduler ! StartScheduleMessage(Some(7200L))

          case event: Done =>
            runDone = expectDoneEvent(event, expectedElements)
          case unexpectedEvent =>
            fail(s"Unexpected event received: $unexpectedEvent")
        }
      }
    }

    "stop correctly for a pauseScheduleAtTick if endTick - pausescheduleAtTick = 1" in {

      val simonaConfig = SimonaConfig(
        ConfigFactory
          .parseString(
            s"""simona.time.startDateTime = "2011-01-01 00:00:00"
            simona.time.endDateTime = "2011-01-01 01:00:00"
            simona.time.schedulerReadyCheckWindow = 900""".stripMargin
          )
          .withFallback(typesafeConfig)
          .resolve()
      )

      val scheduler = setupExtendedTest(simonaConfig)

      // send to init triggers to scheduler
      scheduler ! ScheduleTriggerMessage(
        InitializeServiceTrigger(DummyInitServiceStateData()),
        testActor
      )
      scheduler ! ScheduleTriggerMessage(
        InitializeServiceTrigger(DummyInitServiceStateData()),
        testActor
      )

      // tell scheduler to init
      scheduler ! InitSimMessage

      // we expect two init triggers we need to answer with completion, otherwise scheduler won't go on
      expectReturnComplete(
        scheduler,
        Some(
          Vector(ScheduleTriggerMessage(ActivityStartTrigger(3600), testActor))
        )
      )
      expectReturnComplete(
        scheduler,
        Some(
          Vector(ScheduleTriggerMessage(ActivityStartTrigger(3600), testActor))
        )
      )

      // wait until init is done
      var initDone = false
      while (!initDone) {
        eventQueue.poll(2, TimeUnit.SECONDS) match {
          case InitComplete(_) =>
            initDone = true
          case _ => false
        }
      }

      // tell scheduler we want to run until tick 7200 (endTick == 3600)
      scheduler ! StartScheduleMessage(Some(3599L))

      val expectedElements: mutable.ArrayBuffer[RuntimeEvent] =
        mutable.ArrayBuffer(
          Simulating(0, 3599),
          CheckWindowPassed(900, 0),
          CheckWindowPassed(1800, 0),
          CheckWindowPassed(2700, 0),
          Ready(3599, 0),
          Simulating(3600, 3600),
          CheckWindowPassed(3600, 0),
          Ready(3600, 0),
          Done(3600, 0, 0, errorInSim = false)
        )

      var runDone = false
      while (!runDone) {
        eventQueue.poll(2, TimeUnit.SECONDS) match {
          case event: CheckWindowPassed =>
            val expectedHead = expectedElements.headOption.getOrElse(
              fail("ExpectedElements is empty!")
            ) match {
              case expectedHead: CheckWindowPassed =>
                expectedHead
              case unexpectedEvent =>
                fail(
                  s"Unexpected event when CheckWindowPassed was expected: $unexpectedEvent"
                )
            }
            expectedElements.remove(0)
            event.tick shouldBe expectedHead.tick

          case event: Simulating =>
            expectSimulatingEvent(event, expectedElements)
          case event: Ready =>
            val expectedHead = expectedElements.headOption.getOrElse(
              fail("ExpectedElements is empty!")
            ) match {
              case expectedHead: Ready =>
                expectedHead
              case unexpectedEvent =>
                fail(
                  s"Unexpected event when Ready was expected: $unexpectedEvent"
                )
            }
            expectedElements.remove(0)
            event.tick shouldBe expectedHead.tick

            // when we received the first ready after 3599 ticks, we need to tell the scheduler to go on
            if (event.tick == 3599) {
              scheduler ! StartScheduleMessage(Some(3600L))

              // when continue the sim we also expect that two message arrive at tick 3600 we need to answer with complete
              expectReturnComplete(
                scheduler,
                Some(
                  Vector(
                    ScheduleTriggerMessage(
                      ActivityStartTrigger(7200),
                      testActor
                    )
                  )
                )
              )
              expectReturnComplete(
                scheduler,
                Some(
                  Vector(
                    ScheduleTriggerMessage(
                      ActivityStartTrigger(7200),
                      testActor
                    )
                  )
                )
              )
            }

          case event: Done =>
            runDone = expectDoneEvent(event, expectedElements)
          case unexpectedEvent =>
            fail(s"Unexpected event received: $unexpectedEvent")
        }
      }

      expectMsgPF() {
        case SimulationSuccessfulMessage =>
        case msg =>
          fail(
            s"Unexpected message received when SimulationSuccessfulMessage was expected: $msg"
          )
      }
    }

    "should work correctly if the parallel window is disabled (== 0), powerflow resolution == 1 and readyCheckWindow == 1" in {

      def checkWindowPassed(
          expectedElements: mutable.ArrayBuffer[RuntimeEvent],
          timeoutInSec: Int = 2
      ) = {
        // expect check window passed
        eventQueue.poll(timeoutInSec, TimeUnit.SECONDS) match {
          case event: CheckWindowPassed =>
            val expectedHead = expectedElements.headOption.getOrElse(
              fail("ExpectedElements is empty!")
            ) match {
              case expectedHead: CheckWindowPassed =>
                expectedHead
              case unexpectedEvent =>
                fail(
                  s"Unexpected event when CheckWindowPassed was expected: $unexpectedEvent"
                )
            }
            expectedElements.remove(0)
            event.tick shouldBe expectedHead.tick

          case event => fail(s"Expected Simulating event but received $event")
        }

      }

      // test configuration part
      val noOfTestActors =
        Range(1, 2) // todo does not work with more than one actor

      // the expected events in their expected order
      val expectedElements: mutable.ArrayBuffer[RuntimeEvent] =
        mutable.ArrayBuffer(
          Simulating(0, 10),
          CheckWindowPassed(1, 0),
          CheckWindowPassed(2, 0),
          CheckWindowPassed(3, 0),
          CheckWindowPassed(4, 0),
          CheckWindowPassed(5, 0),
          CheckWindowPassed(6, 0),
          CheckWindowPassed(7, 0),
          CheckWindowPassed(8, 0),
          CheckWindowPassed(9, 0),
          CheckWindowPassed(10, 0),
          Ready(10, 0),
          Done(10, 0, 0, errorInSim = false)
        )

      val simonaConfig = SimonaConfig(
        ConfigFactory
          .parseString(
            s"""simona.time.startDateTime = "2011-01-01 00:00:00"
              simona.time.endDateTime = "2011-01-01 00:00:10"
              simona.time.schedulerReadyCheckWindow = 1""".stripMargin
          )
          .withFallback(typesafeConfig)
          .resolve()
      )

      val scheduler = setupExtendedTest(simonaConfig)

      noOfTestActors.foreach(_ =>
        // send to init trigger to scheduler
        scheduler ! ScheduleTriggerMessage(
          InitializeServiceTrigger(DummyInitServiceStateData()),
          testActor
        )
      )

      // tell scheduler to init
      scheduler ! InitSimMessage

      // we expect one init trigger we need to answer with completion, otherwise scheduler won't go on
      expectReturnComplete(
        scheduler,
        Some(
          Vector(ScheduleTriggerMessage(ActivityStartTrigger(1), testActor))
        )
      )

      // wait until init is done
      var initDone = false
      while (!initDone) {
        eventQueue.poll(2, TimeUnit.SECONDS) match {
          case InitComplete(_) =>
            initDone = true
          case _ =>
            initDone = false
        }
      }

      // tell scheduler we want to run until tick 10
      scheduler ! StartScheduleMessage(Some(10L))

      // expect a simulating event
      eventQueue.poll(2, TimeUnit.SECONDS) match {
        case simulating: Simulating =>
          expectSimulatingEvent(simulating, expectedElements)
        case event => fail(s"Expected Simulating event but received $event")
      }

      // interact with the scheduler for each tick
      for (tick <- 2 to 10) {
        noOfTestActors.foreach(_ =>
          expectReturnComplete(
            scheduler,
            Some(
              Vector(
                ScheduleTriggerMessage(
                  ActivityStartTrigger(tick),
                  testActor
                )
              )
            )
          )
        )
        // we expect exactly one check window passed event as
        // all activity start triggers for the last tick should be answered
        checkWindowPassed(expectedElements)
        eventQueue.size() shouldBe 0
      }

      // answer with completion for the last tick, schedule another tick to test if the simulation event terminates as
      // expected
      noOfTestActors.foreach(_ =>
        expectReturnComplete(
          scheduler,
          Some(
            Vector(
              ScheduleTriggerMessage(
                ActivityStartTrigger(11),
                testActor
              )
            )
          )
        )
      )

      // finally expect a simulation successful message
      expectMsgPF() {
        case SimulationSuccessfulMessage =>
        case msg =>
          fail(
            s"Unexpected message received when SimulationSuccessfulMessage was expected: $msg"
          )

      }

    }
  }
}
