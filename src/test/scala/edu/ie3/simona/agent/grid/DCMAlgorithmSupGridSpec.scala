/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.CongestionManagementSupport.Congestions
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.grid.GridAgentMessages._
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.model.grid.{RefSystem, VoltageLimits}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped, UnitSpec}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps

import scala.concurrent.duration.DurationInt

/** Test to ensure the functions that a [[GridAgent]] in superior position
  * should be able to do if the DCMSAlgorithm is used. The scheduler, the
  * weather service as well as the [[GridAgent]] inferior to the superior
  * [[GridAgent]] are simulated by the TestKit.
  */
class DCMAlgorithmSupGridSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with ConfigTestData
    with DbfsTestGrid
    with TestSpawnerTyped {

  private val cmConfig = ConfigFactory.parseString("""
      |simona.congestionManagement.enableTransformerTapping = true
      |""".stripMargin)

  private val config = SimonaConfig(cmConfig.withFallback(typesafeConfig))

  private val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  private val runtimeEvents: TestProbe[RuntimeEvent] =
    TestProbe("runtimeEvents")
  private val primaryService: TestProbe[ServiceMessage] =
    TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")
  private val hvGrid: TestProbe[GridAgent.Request] = TestProbe("hvGrid")

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    runtimeEventListener = runtimeEvents.ref,
    primaryServiceProxy = primaryService.ref.toClassic,
    weather = weatherService.ref.toClassic,
    evDataService = None,
  )

  val resultListener: TestProbe[ResultEvent] = TestProbe("resultListener")

  "A GridAgent actor in superior position with async test" should {
    val superiorGridAgent: ActorRef[GridAgent.Request] = testKit.spawn(
      GridAgent(
        environmentRefs,
        config,
        listener = Iterable(resultListener.ref),
      )
    )

    s"initialize itself when it receives an init activation" in {
      val subnetGatesToActorRef: Map[SubGridGate, ActorRef[GridAgent.Request]] =
        ehvSubGridGates.map(gate => gate -> hvGrid.ref).toMap

      val gridAgentInitData =
        GridAgentInitData(
          ehvGridContainer,
          Seq.empty[ThermalGrid],
          subnetGatesToActorRef,
          RefSystem("5000 MVA", "380 kV"),
          VoltageLimits(0.9, 1.1),
        )

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      superiorGridAgent ! CreateGridAgent(gridAgentInitData, key)

      val scheduleActivationMsg =
        scheduler.expectMessageType[ScheduleActivation]
      scheduleActivationMsg.tick shouldBe INIT_SIM_TICK
      scheduleActivationMsg.unlockKey shouldBe Some(key)
      val gridAgentActivation = scheduleActivationMsg.actor

      superiorGridAgent ! WrappedActivation(Activation(INIT_SIM_TICK))
      scheduler.expectMessage(Completion(gridAgentActivation, Some(3600)))
    }

    s"skip simulate grid and check for congestions correctly if no congestions occurred" in {
      gotoSimulateGrid()

      skipSimulationAndGetNextStep(
        Congestions(
          voltageCongestions = false,
          lineCongestions = false,
          transformerCongestions = false,
        )
      )

      // inferior should receive a next state message to go to the idle state
      hvGrid.expectMessageType[GotoIdle.type]

      // expect a completion message from the superior grid
      scheduler.expectMessageType[Completion] match {
        case Completion(_, Some(7200)) =>
        case x =>
          fail(
            s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
          )
      }
    }

    s"handle unresolvable congestions correctly" in {
      gotoSimulateGrid()

      // transformer congestion cannot be resolved, because using flex options is not
      // enable by the provided config
      skipSimulationAndGetNextStep(
        Congestions(
          voltageCongestions = false,
          lineCongestions = false,
          transformerCongestions = true,
        )
      )

      // inferior should receive a next state message to go to the idle state
      hvGrid.expectMessageType[GotoIdle.type]

      // expect a completion message from the superior grid
      scheduler.expectMessageType[Completion] match {
        case Completion(_, Some(7200)) =>
        case x =>
          fail(
            s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
          )
      }
    }

    s"update transformer tapping correctly" in {
      gotoSimulateGrid()

      skipSimulationAndGetNextStep(
        Congestions(
          voltageCongestions = true,
          lineCongestions = false,
          transformerCongestions = false,
        )
      )

      // inferior should receive a next state message to go to a congestion management step
      hvGrid.expectMessageType[NextStepRequest]

      // skipping the step for now
      // TODO: Update test after implementing transformer tapping
      superiorGridAgent ! FinishStep
      hvGrid.expectMessageType[FinishStep.type]

      // skipping the simulation
      hvGrid.expectMessageType[RequestGridPower]

      skipSimulationAndGetNextStep(
        Congestions(
          voltageCongestions = false,
          lineCongestions = false,
          transformerCongestions = false,
        )
      )

      // inferior should receive a next state message to go to the idle state
      hvGrid.expectMessageType[GotoIdle.type]

      // expect a completion message from the superior grid
      // after all steps are finished
      scheduler.expectMessageType[Completion](10.seconds) match {
        case Completion(_, Some(7200)) =>
        case x =>
          fail(
            s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
          )
      }
    }

    def skipSimulationAndGetNextStep(congestions: Congestions): Unit = {
      // skip simulation and go to congestion check
      superiorGridAgent ! FinishGridSimulationTrigger(3600)

      // inferior grid receives a FinishGridSimulationTrigger and goes into the congestion check state
      hvGrid.expectMessage(FinishGridSimulationTrigger(3600))

      // we expect a request for grid congestion values here
      val lastSender =
        hvGrid.expectMessageType[CongestionCheckRequest](10.seconds) match {
          case CongestionCheckRequest(sender) => sender
          case x =>
            fail(
              s"Invalid message received when expecting a request for grid congestion values! Message was $x"
            )
        }

      // send congestions
      lastSender ! CongestionResponse(congestions, hvGrid.ref)
    }

    def gotoSimulateGrid(): Unit = {
      superiorGridAgent ! WrappedActivation(Activation(3600))

      // we expect a completion message
      scheduler.expectMessageType[Completion].newTick shouldBe Some(3600)
    }
  }
}
