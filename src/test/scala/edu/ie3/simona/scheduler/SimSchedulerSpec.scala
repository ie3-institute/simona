/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor._
import akka.testkit.{ImplicitSender, TestActorRef}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.akka.SimonaActorRef.RichActorRef
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.event.listener.RuntimeEventListener
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger.InitializeServiceTrigger
import edu.ie3.simona.scheduler.main.SimScheduler
import edu.ie3.simona.service.ServiceStateData
import edu.ie3.simona.test.common.{ConfigTestData, TestKitWithShutdown}
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, TimeUnit}

class SimSchedulerSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "SimSchedulerSpec",
        ConfigFactory
          .parseString(
            """
                         |akka.loggers =["edu.ie3.simona.test.common.SilentTestEventListener"]
                         |akka.loglevel="debug"
          """.stripMargin
          )
      )
    )
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with PrivateMethodTester
    with ConfigTestData {

  implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)

  private final case class DummyInitServiceStateData(
  ) extends ServiceStateData

  // setup values
  val resolutionInSec = 3600
  val pauseScheduleAtTick: Long = Long.MaxValue
  val triggerId = 0
  val eventQueue: BlockingQueue[RuntimeEvent] =
    new LinkedBlockingQueue[RuntimeEvent]

  // setup config for scheduler
  private val config = ConfigFactory
    .parseString(s"""
            simona.execution.computationMode = Local

            simona.time.startDateTime = "2011-01-01 00:00:00"
            simona.time.endDateTime = "2011-01-01 01:00:00"
            simona.input.grid.datasource.id = "csv"
            simona.time.schedulerReadyCheckWindow = 900
            simona.output.base.dir = "testOutput/"
            simona.output.grid = {
              notifier = "grid"
              nodes = false
              lines = false
              switches = false
              transformers2w = false
              transformers3w = false
            }
            simona.output.participant.defaultConfig = {
              notifier = "default"
              powerRequestReply = false
              simulationResult = false
            }
            simona.output.participant.individualConfigs = []
            simona.input.grid.datasource.id = "csv"
            simona.input.grid.datasource.csvParams.folderPath = "netdata"
            simona.input.grid.datasource.csvParams.csvSep =","
            simona.runtime.participant.load = {
              defaultConfig = {
                calculateMissingReactivePowerWithModel = false
                uuids = ["default"]
                scaling = 1.0
                modelBehaviour = "fix"
                reference = "power"
              }
              individualConfigs = []
            }
            simona.runtime.participant.fixedFeedIn = {
              defaultConfig = {
                calculateMissingReactivePowerWithModel = false
                uuids = ["default"]
                scaling = 1.0
              }
              individualConfigs = [
                {
                  calculateMissingReactivePowerWithModel = false
                  uuids = ["4eeaf76a-ec17-4fc3-872d-34b7d6004b03"]
                  scaling = 1.0
                }
              ]
            }

            simona.runtime.participant.pv = {
              defaultConfig = {
                calculateMissingReactivePowerWithModel = false
                uuids = ["default"]
                scaling = 1.0
              }
              individualConfigs = [
                {
                  calculateMissingReactivePowerWithModel = false
                  uuids = ["4eeaf76a-ec17-4fc3-872d-34b7d6004b03"]
                  scaling = 1.0
                }
              ]
            }

            simona.runtime.participant.wec = {
              defaultConfig = {
                calculateMissingReactivePowerWithModel = false
                uuids = ["default"]
                scaling = 1.0
              }
              individualConfigs = [
                {
                  calculateMissingReactivePowerWithModel = false
                  uuids = ["4eeaf76a-ec17-4fc3-872d-34b7d6004b03"]
                  scaling = 1.0
                }
              ]
            }

            simona.runtime.participant.evcs = {
              defaultConfig = {
                calculateMissingReactivePowerWithModel = false
                uuids = ["default"]
                scaling = 1.0
              }
              individualConfigs = [
                {
                  calculateMissingReactivePowerWithModel = false
                  uuids = ["4eeaf76a-ec17-4fc3-872d-34b7d6004b03"]
                  scaling = 1.0
                }
              ]
            }

            simona.powerflow.maxSweepPowerDeviation = 1E-5 // the maximum allowed deviation in power between two sweeps, before overall convergence is assumed
            simona.powerflow.skipOnFailure = true
            simona.powerflow.resolution = "${resolutionInSec}s"
            simona.powerflow.newtonraphson.epsilon = [1E-12]
            simona.powerflow.newtonraphson.iterations = 50
            simona.simulationName = "ConfigTestDataSimulation"
            simona.gridConfig.refSystems = []
          """)
    .resolve()
  override protected val simonaConfig: SimonaConfig = SimonaConfig(config)

  // build the run time listener
  val runtimeListeners = Vector(
    TestActorRef(
      new RuntimeEventListener(
        None,
        Some(eventQueue),
        simonaConfig.simona.time.startDateTime
      )
    ).asLocal
  )

  // build the scheduler
  private val scheduler = TestActorRef(
    new SimScheduler(
      simonaConfig.simona.time,
      runtimeListeners,
      simonaConfig.simona.time.stopOnFailedPowerFlow
    )
  )

  "The SimScheduler" must {

    "notify the runtime event listener when agents and services are busy initializing " in {

      // send the start schedule message to the scheduler
      scheduler ! InitSimMessage

      // expect the first runtime event to be initializing
      eventQueue.take() match {
        case Initializing =>
          // to receive the next Ready event
          // necessary to receive a valid trigger that can be completed
          scheduler ! ScheduleTriggerMessage(
            InitializeServiceTrigger(DummyInitServiceStateData()),
            self.asLocal
          )

          // expect a trigger message from the scheduler which when completed will provide with a
          // valid completion message to be sent to the scheduler
          val triggerWithIdMessage = expectMsgPF() {
            case triggerWithIdMessage @ TriggerWithIdMessage(
                  InitializeServiceTrigger(_),
                  _,
                  _
                ) =>
              triggerWithIdMessage
            case _ =>
              fail()
          }

          // send completion message with a valid trigger ID
          scheduler ! CompletionMessage(
            triggerWithIdMessage.triggerId,
            self.asLocal
          )

        case _ => fail()
      }
    }

    "notify Ready and Simulating events depending on schedulerReadyCheckWindow" +
      "notify Done on successful simulation termination" in {

        simonaConfig.simona.time.schedulerReadyCheckWindow match {
          case Some(pauseScheduleCheck) =>
            var occurrences = (resolutionInSec / pauseScheduleCheck) + 2

            assert(eventQueue.size == occurrences + 1)

            // check if Ready and Simulating events are notified according to
            // the config value provided in simona.time.schedulerReadyCheckWindow
            while (occurrences > 0) {
              eventQueue.take() match {
                case _: InitComplete      =>
                case _: CheckWindowPassed =>
                case _: Simulating        =>
                case _                    => fail()
              }
              occurrences -= 1
            }

            // the final event thrown should be Done
            eventQueue.take() match {
              case _: Done =>
              case _       => fail()
            }

          case None =>
            // The only event thrown in the absence of a value in
            // schedulerReadyCheckWindow should be Done
            assert(eventQueue.size == 1)
            eventQueue.take() match {
              case _: Done =>
              case _       => fail()
            }
        }
      }

    // FIXME currently, this is broken
    "notify with Error event on receipt of actor termination message" ignore {

      // create a test actor and kill the same to receive Terminated
      // in simScheduler and Error event in the runtimeEventListener
      val deathActor: ActorRef = system.actorOf(Props.empty)
      scheduler.watch(deathActor)
      deathActor ! PoisonPill

      assert(eventQueue.size == 1)
      eventQueue.take() match {
        case _: Error =>
        case _        => fail()
      }
    }
  }

}
