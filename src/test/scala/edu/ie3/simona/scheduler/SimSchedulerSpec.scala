/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor._
import akka.testkit.{ImplicitSender, TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.TimeConfig
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeTrigger,
}
import edu.ie3.simona.scheduler.SimSchedulerSpec.{
  DummySupervisor,
  RichTriggeredAgent,
}
import edu.ie3.simona.test.common.{TestKitWithShutdown, UnitSpec}
import edu.ie3.simona.util.SimonaConstants
import org.mockito.Mockito.doReturn
import org.scalatestplus.mockito.MockitoSugar.mock

import scala.reflect.ClassTag

class SimSchedulerSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "SimSchedulerSpec",
        ConfigFactory
          .parseString(
            """
            |akka.loggers=["edu.ie3.simona.test.common.SilentTestEventListener"]
            |akka.loglevel="debug"
            """.stripMargin
          ),
      )
    )
    with ImplicitSender {

  private val defaultTimeConfig = TimeConfig(
    startDateTime = "2011-01-01 00:00:00",
    endDateTime = "2011-01-01 01:00:00",
    schedulerReadyCheckWindow = Some(900),
  )

  def setupScheduler(
      autostart: Boolean = false,
      stopOnFailedPowerFlow: Boolean = false,
      timeConfig: TimeConfig = defaultTimeConfig,
  ): (TestActorRef[SimScheduler], TestProbe) = {
    val resultEventListener = TestProbe("ResultEventListener")

    // make sure that the scheduler stops on exceptions
    val supervisorRef = TestActorRef(new DummySupervisor())

    val simScheduler = TestActorRef[SimScheduler](
      SimScheduler.props(
        timeConfig,
        Iterable(resultEventListener.ref),
        stopOnFailedPowerFlow = stopOnFailedPowerFlow,
        autoStart = autostart,
      ),
      supervisor = supervisorRef,
    )

    (simScheduler, resultEventListener)
  }

  "The SimScheduler" should {

    "initialize as expected when receiving triggers before InitSimMessage" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      val triggeredAgent1 = TestProbe()
      val initTrigger1 = createMockInitTrigger()
      simScheduler ! ScheduleTriggerMessage(
        initTrigger1,
        triggeredAgent1.ref,
      )

      val triggeredAgent2 = TestProbe()
      val initTrigger2 = createMockInitTrigger()
      simScheduler !
        ScheduleTriggerMessage(
          initTrigger2,
          triggeredAgent2.ref,
        )

      triggeredAgent1.expectNoMessage()
      triggeredAgent2.expectNoMessage()

      simScheduler ! InitSimMessage

      resultEventListener.expectMsg(Initializing)

      val receivedTrigger1 =
        triggeredAgent1.expectMsgType[TriggerWithIdMessage]
      receivedTrigger1.trigger shouldBe initTrigger1
      receivedTrigger1.receiverActor shouldBe triggeredAgent1.ref

      val receivedTrigger2 =
        triggeredAgent2.expectMsgType[TriggerWithIdMessage]
      receivedTrigger2.trigger shouldBe initTrigger2
      receivedTrigger2.receiverActor shouldBe triggeredAgent2.ref

      triggeredAgent1.send(
        simScheduler,
        CompletionMessage(
          receivedTrigger1.triggerId,
          None,
        ),
      )

      resultEventListener.expectNoMessage()

      triggeredAgent2.send(
        simScheduler,
        CompletionMessage(
          receivedTrigger2.triggerId,
          None,
        ),
      )

      resultEventListener.expectMsgType[InitComplete]
    }

    "initialize as expected when receiving triggers after init trigger" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      simScheduler ! InitSimMessage

      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()
      val initTrigger1 = createMockInitTrigger()
      simScheduler ! ScheduleTriggerMessage(
        initTrigger1,
        triggeredAgent1.ref,
      )

      val triggeredAgent2 = TestProbe()
      val initTrigger2 = createMockInitTrigger()
      simScheduler ! ScheduleTriggerMessage(
        initTrigger2,
        triggeredAgent2.ref,
      )

      // trigger are sent right away
      val receivedTrigger1 =
        triggeredAgent1.expectMsgType[TriggerWithIdMessage]
      receivedTrigger1.trigger shouldBe initTrigger1
      receivedTrigger1.receiverActor shouldBe triggeredAgent1.ref

      val receivedTrigger2 =
        triggeredAgent2.expectMsgType[TriggerWithIdMessage]
      receivedTrigger2.trigger shouldBe initTrigger2
      receivedTrigger2.receiverActor shouldBe triggeredAgent2.ref

      triggeredAgent1.send(
        simScheduler,
        CompletionMessage(
          receivedTrigger1.triggerId,
          None,
        ),
      )

      resultEventListener.expectNoMessage()

      triggeredAgent2.send(
        simScheduler,
        CompletionMessage(
          receivedTrigger2.triggerId,
          None,
        ),
      )

      resultEventListener.expectMsgType[InitComplete]
    }

    "start simulation when autostart is enabled" in {
      val (simScheduler, resultEventListener) = setupScheduler(autostart = true)

      val triggeredAgent1 = TestProbe()
      val initTrigger1 = createMockInitTrigger()
      simScheduler ! ScheduleTriggerMessage(
        initTrigger1,
        triggeredAgent1.ref,
      )

      triggeredAgent1.expectNoMessage()

      simScheduler ! InitSimMessage

      resultEventListener.expectMsg(Initializing)

      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(0L, 900L),
      )

      resultEventListener.expectMsgType[InitComplete]
      resultEventListener.expectMsg(Simulating(0L, 3600L))

      val receivedTrigger1Start =
        triggeredAgent1.expectMsgType[TriggerWithIdMessage]
      receivedTrigger1Start.trigger shouldBe ActivityStartTrigger(0L)
      receivedTrigger1Start.receiverActor shouldBe triggeredAgent1.ref
    }

    "send triggers as expected when triggered at x*schedulerReadyCheckWindow" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()

      val initTrigger1 = createMockInitTrigger()
      simScheduler ! ScheduleTriggerMessage(
        initTrigger1,
        triggeredAgent1.ref,
      )

      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(0L),
      )

      resultEventListener.expectMsgType[InitComplete]

      simScheduler ! StartScheduleMessage(Some(3600L))

      resultEventListener.expectMsg(Simulating(0L, 3600L))

      triggeredAgent1.expectAstAndComplete(
        simScheduler,
        resultEventListener,
        0L,
        Seq(900L),
      )

      // no CheckWindowPassed at tick 0
      resultEventListener.expectNoMessage()

      triggeredAgent1.expectAstAndComplete(
        simScheduler,
        resultEventListener,
        900L,
        Seq(1800L),
      )

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 900L

      triggeredAgent1.expectAstAndComplete(
        simScheduler,
        resultEventListener,
        1800L,
        Seq(2700L),
      )

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 1800L

      triggeredAgent1.expectAstAndComplete(
        simScheduler,
        resultEventListener,
        2700L,
        Seq(3600L),
      )

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 2700L

      triggeredAgent1.expectAstAndComplete(
        simScheduler,
        resultEventListener,
        3600L,
      )

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 3600L
      resultEventListener.expectMsgType[Ready].tick shouldBe 3600L
      val doneMsg = resultEventListener.expectMsgType[Done]
      doneMsg.tick shouldBe 3600L
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      // we expect that the scheduler answers with a SimulationSuccessfulMessage
      // to the sender of the run request (this test)
      expectMsg(SimulationSuccessfulMessage)
    }

    "finish simulation correctly when endTick == pauseScheduleAtTick" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()

      val initTrigger1 = createMockInitTrigger()
      simScheduler ! ScheduleTriggerMessage(
        initTrigger1,
        triggeredAgent1.ref,
      )
      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(3600L),
      )

      resultEventListener.expectMsgType[InitComplete]

      triggeredAgent1.expectNoMessage()

      simScheduler ! StartScheduleMessage(Some(3600L))

      resultEventListener.expectMsg(Simulating(0L, 3600L))
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 900L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 1800L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 2700L

      triggeredAgent1.expectAstAndComplete(
        simScheduler,
        resultEventListener,
        3600L,
      )

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 3600L
      resultEventListener.expectMsgType[Ready].tick shouldBe 3600L
      val doneMsg = resultEventListener.expectMsgType[Done]
      doneMsg.tick shouldBe 3600L
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      // we expect that the scheduler answers with a SimulationSuccessfulMessage
      // to the sender of the run request (this test)
      expectMsg(SimulationSuccessfulMessage)
    }

    "finish simulation correctly when endTick < pauseScheduleAtTick" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()

      val initTrigger1 = createMockInitTrigger()
      simScheduler ! ScheduleTriggerMessage(
        initTrigger1,
        triggeredAgent1.ref,
      )
      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(3600L),
      )

      resultEventListener.expectMsgType[InitComplete]

      triggeredAgent1.expectNoMessage()

      simScheduler ! StartScheduleMessage(Some(7200L))

      resultEventListener.expectMsg(Simulating(0L, 3600L))
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 900L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 1800L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 2700L

      triggeredAgent1.expectAstAndComplete(
        simScheduler,
        resultEventListener,
        3600L,
      )

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 3600L

      // no ready msg here

      val doneMsg = resultEventListener.expectMsgType[Done]
      doneMsg.tick shouldBe 3600L
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      // we expect that the scheduler answers with a SimulationSuccessfulMessage
      // to the sender of the run request (this test)
      expectMsg(SimulationSuccessfulMessage)
    }

    "finish simulation correctly when endTick > first pauseScheduleAtTick" in {
      val (simScheduler, resultEventListener) = setupScheduler(
        timeConfig = defaultTimeConfig.copy(endDateTime = "2011-01-01 02:00:00")
      )

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()

      simScheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        triggeredAgent1.ref,
      )
      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(3600L),
      )

      resultEventListener.expectMsgType[InitComplete]

      triggeredAgent1.expectNoMessage()

      simScheduler ! StartScheduleMessage(Some(3600L))

      resultEventListener.expectMsg(Simulating(0L, 3600L))
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 900L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 1800L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 2700L

      triggeredAgent1.expectAstAndComplete(
        simScheduler,
        resultEventListener,
        3600L,
        Seq(7200L),
      )

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 3600L
      resultEventListener.expectMsgType[Ready].tick shouldBe 3600L

      simScheduler ! StartScheduleMessage(Some(7200L))

      resultEventListener.expectMsg(Simulating(3601, 7200))

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 4500L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 5400L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 6300L

      triggeredAgent1.expectAstAndComplete(
        simScheduler,
        resultEventListener,
        7200L,
      )

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 7200L
      resultEventListener.expectMsgType[Ready].tick shouldBe 7200L

      val doneMsg = resultEventListener.expectMsgType[Done]
      doneMsg.tick shouldBe 7200L
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      // we expect that the scheduler answers with a SimulationSuccessfulMessage
      // to the sender of the run request (this test)
      expectMsg(SimulationSuccessfulMessage)
    }

    "pause and finish simulation correctly for a pauseScheduleAtTick if endTick - pauseScheduleAtTick = 1" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()

      simScheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        triggeredAgent1.ref,
      )

      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(3600L),
      )

      resultEventListener.expectMsgType[InitComplete]

      triggeredAgent1.expectNoMessage()

      simScheduler ! StartScheduleMessage(Some(3599L))

      resultEventListener.expectMsg(Simulating(0L, 3599L))
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 900L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 1800L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 2700L
      resultEventListener.expectMsgType[Ready].tick shouldBe 3599L

      simScheduler ! StartScheduleMessage(Some(3600L))

      resultEventListener.expectMsg(Simulating(3600L, 3600L))

      triggeredAgent1.expectAstAndComplete(
        simScheduler,
        resultEventListener,
        3600L,
      )

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 3600L
      resultEventListener.expectMsgType[Ready].tick shouldBe 3600L
      val doneMsg = resultEventListener.expectMsgType[Done]
      doneMsg.tick shouldBe 3600L
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      // we expect that the scheduler answers with a SimulationSuccessfulMessage
      // to the sender of the run request (this test)
      expectMsg(SimulationSuccessfulMessage)
    }

    "work correctly if timeBinSize == 1 and readyCheckWindow == 1" in {
      val triggeredAgents = Range.inclusive(1, 5).map(i => TestProbe(s"SPA_$i"))

      val (simScheduler, resultEventListener) = setupScheduler(
        timeConfig = defaultTimeConfig.copy(
          startDateTime = "2011-01-01 00:00:00",
          endDateTime = "2011-01-01 00:00:10",
          schedulerReadyCheckWindow = Some(1),
        )
      )

      triggeredAgents.foreach(actor =>
        // send to init trigger to scheduler
        simScheduler ! ScheduleTriggerMessage(
          createMockInitTrigger(),
          actor.ref,
        )
      )

      // tell scheduler to init
      simScheduler ! InitSimMessage

      resultEventListener.expectMsg(Initializing)

      triggeredAgents.foreach {
        _.expectInitAndComplete(
          simScheduler,
          resultEventListener,
          Seq(1L),
        )
      }

      // wait until init is done
      resultEventListener.expectMsgType[InitComplete]

      // tell scheduler we want to run until tick 10
      simScheduler ! StartScheduleMessage(Some(10L))

      // expect a simulating event
      resultEventListener.expectMsg(Simulating(0L, 10L))

      // interact with the scheduler for each tick
      for (tick <- 1L to 9L) {
        triggeredAgents.foreach {
          _.expectAstAndComplete(
            simScheduler,
            resultEventListener,
            tick,
            Seq(tick + 1),
          )
        }
        // we expect exactly one check window passed event as
        // all activity start triggers for the last tick should be answered
        resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe tick
      }

      // answer with completion for the last tick, schedule another tick to test if the simulation event terminates as
      // expected
      triggeredAgents.foreach(
        _.expectAstAndComplete(
          simScheduler,
          resultEventListener,
          10L,
          Seq(11L),
        )
      )

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 10L
      resultEventListener.expectMsgType[Ready].tick shouldBe 10L
      val doneMsg = resultEventListener.expectMsgType[Done]
      doneMsg.tick shouldBe 10L
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      // finally expect a simulation successful message
      expectMsg(SimulationSuccessfulMessage)
    }

    /* exceptional cases */

    "fail validation of StartScheduleMessage if pauseScheduleAtTick < nowInTicks" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      // observe if scheduler dies
      val deathWatch = TestProbe()
      deathWatch.watch(simScheduler)

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()

      simScheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        triggeredAgent1.ref,
      )
      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(900L),
      )

      resultEventListener.expectMsgType[InitComplete]

      simScheduler ! StartScheduleMessage(Some(1800L))
      resultEventListener.expectMsg(Simulating(0L, 1800L))

      // should still live
      deathWatch.expectNoMessage()

      // send faulty StartScheduleMessage
      simScheduler ! StartScheduleMessage(Some(0L))

      // exception should be thrown,
      deathWatch.expectTerminated(simScheduler)

      resultEventListener.expectNoMessage()
    }

    "do nothing when receiving StartScheduleTrigger during initialization" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      // observe if scheduler dies
      val deathWatch = TestProbe()
      deathWatch.watch(simScheduler)

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      simScheduler ! StartScheduleMessage(None)

      resultEventListener.expectNoMessage()
      // scheduler should not die, but just continue
      deathWatch.expectNoMessage()
    }

    "do nothing when receiving StartScheduleTrigger and already started" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      // observe if scheduler dies
      val deathWatch = TestProbe()
      deathWatch.watch(simScheduler)

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()

      simScheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        triggeredAgent1.ref,
      )
      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(900L),
      )

      resultEventListener.expectMsgType[InitComplete]

      simScheduler ! StartScheduleMessage(Some(1800L))
      resultEventListener.expectMsg(Simulating(0L, 1800L))

      simScheduler ! StartScheduleMessage(None)

      resultEventListener.expectNoMessage()
      // scheduler should not die, but just continue
      deathWatch.expectNoMessage()
    }

    "handle PowerFlow failures when stopOnFailedPowerFlow = false" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()

      simScheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        triggeredAgent1.ref,
      )
      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(0L),
      )

      resultEventListener.expectMsgType[InitComplete]

      simScheduler ! StartScheduleMessage(None)
      resultEventListener.expectMsg(Simulating(0L, 3600L))

      val receivedTrigger1 = triggeredAgent1.expectMsgType[TriggerWithIdMessage]

      triggeredAgent1.send(simScheduler, PowerFlowFailedMessage)
      triggeredAgent1.send(simScheduler, PowerFlowFailedMessage)
      triggeredAgent1.send(simScheduler, PowerFlowFailedMessage)

      resultEventListener.expectNoMessage()

      simScheduler ! CompletionMessage(
        receivedTrigger1.triggerId,
        None,
      )

      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 900L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 1800L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 2700L
      resultEventListener.expectMsgType[CheckWindowPassed].tick shouldBe 3600L

      val doneMsg = resultEventListener.expectMsgType[Done]
      doneMsg.tick shouldBe 3600L
      doneMsg.noOfFailedPF shouldBe 3
      doneMsg.errorInSim shouldBe false

      expectMsg(SimulationSuccessfulMessage)
    }

    "handle PowerFlow failures when stopOnFailedPowerFlow = true" in {
      val (simScheduler, resultEventListener) =
        setupScheduler(stopOnFailedPowerFlow = true)

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()

      simScheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        triggeredAgent1.ref,
      )
      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(0L),
      )

      resultEventListener.expectMsgType[InitComplete]

      simScheduler ! StartScheduleMessage(None)
      resultEventListener.expectMsg(Simulating(0L, 3600L))

      val receivedTrigger1 = triggeredAgent1.expectMsgType[TriggerWithIdMessage]

      triggeredAgent1.send(simScheduler, PowerFlowFailedMessage)
      triggeredAgent1.send(simScheduler, PowerFlowFailedMessage)

      resultEventListener.expectNoMessage()

      simScheduler ! CompletionMessage(
        receivedTrigger1.triggerId,
        None,
      )

      val doneMsg = resultEventListener.expectMsgType[Done]
      doneMsg.tick shouldBe 0L
      doneMsg.noOfFailedPF shouldBe 2
      doneMsg.errorInSim shouldBe true

      expectMsg(SimulationFailureMessage)
    }

    "terminate on child actor termination when initializing" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      checkTerminationHandling(simScheduler, resultEventListener)
    }

    "terminate on child actor termination when paused" in {
      val (simScheduler, resultEventListener) = setupScheduler()

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()

      simScheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        triggeredAgent1.ref,
      )
      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(0L),
      )

      resultEventListener.expectMsgType[InitComplete]

      checkTerminationHandling(simScheduler, resultEventListener)
    }

    "terminate on child actor termination when running" in {
      val (simScheduler, resultEventListener) = setupScheduler(autostart = true)

      simScheduler ! InitSimMessage
      resultEventListener.expectMsg(Initializing)

      val triggeredAgent1 = TestProbe()

      simScheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        triggeredAgent1.ref,
      )
      triggeredAgent1.expectInitAndComplete(
        simScheduler,
        resultEventListener,
        Seq(0L),
      )

      resultEventListener.expectMsgType[InitComplete]
      resultEventListener.expectMsg(Simulating(0L, 3600L))

      checkTerminationHandling(simScheduler, resultEventListener)
    }
  }

  def createMockInitTrigger(): InitializeTrigger = {
    val mockTrigger = mock[InitializeTrigger]
    doReturn(SimonaConstants.INIT_SIM_TICK).when(mockTrigger).tick
    mockTrigger
  }

  private def checkTerminationHandling(
      simScheduler: TestActorRef[SimScheduler],
      resultEventListener: TestProbe,
  ): Unit = {
    // observe scheduler for stopping
    val deathWatch = TestProbe()
    deathWatch.watch(simScheduler)

    // give scheduler a child that dies
    val dyingActor = TestProbe()
    simScheduler.watch(dyingActor.ref)

    dyingActor.ref ! PoisonPill

    resultEventListener.expectMsgType[Error]
    expectMsg(SimulationFailureMessage)

    // scheduler should have died
    deathWatch.expectTerminated(simScheduler)
  }
}

object SimSchedulerSpec extends UnitSpec {

  class DummySupervisor extends Actor {
    override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
      case _: RuntimeException => SupervisorStrategy.stop
    }

    override def receive: Receive = { case _ => }
  }

  implicit class RichTriggeredAgent(private val triggeredAgent: TestProbe) {

    def expectInitAndComplete(
        simScheduler: TestActorRef[SimScheduler],
        resultEventListener: TestProbe,
        newTicks: Seq[Long] = Seq.empty,
    ): Unit =
      expectTriggerAndComplete[InitializeTrigger](
        simScheduler,
        resultEventListener,
        SimonaConstants.INIT_SIM_TICK,
        newTicks,
      )

    def expectAstAndComplete(
        simScheduler: TestActorRef[SimScheduler],
        resultEventListener: TestProbe,
        expectedTick: Long,
        newTicks: Seq[Long] = Seq.empty,
    ): Unit =
      expectTriggerAndComplete[ActivityStartTrigger](
        simScheduler,
        resultEventListener,
        expectedTick,
        newTicks,
      )

    private def expectTriggerAndComplete[T <: Trigger](
        simScheduler: TestActorRef[SimScheduler],
        resultEventListener: TestProbe,
        expectedTick: Long,
        newTicks: Seq[Long] = Seq.empty,
    )(implicit tag: ClassTag[T]): Unit = {
      val receivedTrigger =
        triggeredAgent.expectMsgType[TriggerWithIdMessage]

      receivedTrigger.trigger match {
        case trigger: T =>
          trigger.tick shouldBe expectedTick
        case unexpected =>
          fail(s"Received unexpected trigger $unexpected")
      }
      receivedTrigger.receiverActor shouldBe triggeredAgent.ref

      // when an agent is triggered, we can be sure that no result event
      // is fired at least until the corresponding completion has been sent out
      resultEventListener.expectNoMessage()

      val newTriggers =
        Option.when(newTicks.nonEmpty)(
          newTicks.map(tick =>
            ScheduleTriggerMessage(
              ActivityStartTrigger(tick),
              triggeredAgent.ref,
            )
          )
        )

      triggeredAgent.send(
        simScheduler,
        CompletionMessage(
          receivedTrigger.triggerId,
          newTriggers,
        ),
      )
    }

  }

}
