/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.detection

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  CreateGridAgent,
  FinishGridSimulationTrigger,
  WrappedActivation,
}
import edu.ie3.simona.agent.grid.congestion.CMMessages.{FinishStep, StartStep}
import edu.ie3.simona.agent.grid.congestion.Congestions
import edu.ie3.simona.agent.grid.congestion.detection.DetectionMessages.{
  CongestionCheckRequest,
  CongestionResponse,
  ReceivedCongestions,
}
import edu.ie3.simona.agent.grid.{DBFSMockGridAgents, GridAgent}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.model.grid.{RefSystem, VoltageLimits}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps

import scala.language.implicitConversions

class CongestionDetectionSpec
    extends ScalaTestWithActorTestKit
    with DBFSMockGridAgents
    with ConfigTestData
    with DbfsTestGrid
    with TestSpawnerTyped {

  private val config = SimonaConfig(
    ConfigFactory
      .parseString("""
        |simona.congestionManagement.enableDetection = true
        |""".stripMargin)
      .withFallback(typesafeConfig)
      .resolve()
  )

  private val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  private val runtimeEvents: TestProbe[RuntimeEvent] =
    TestProbe("runtimeEvents")
  private val primaryService = TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    runtimeEventListener = runtimeEvents.ref,
    primaryServiceProxy = primaryService.ref.toClassic,
    weather = weatherService.ref.toClassic,
    evDataService = None,
  )

  val resultListener: TestProbe[ResultEvent] = TestProbe("resultListener")

  "The congestion detection" should {

    "work as expected in center position" in {
      val superiorGridAgent = SuperiorGA(
        TestProbe("superiorGridAgent_1000"),
        Seq(supNodeA.getUuid, supNodeB.getUuid),
      )

      val inferiorGrid11 =
        InferiorGA(TestProbe("inferiorGridAgent_11"), Seq(node1.getUuid))

      val inferiorGrid12 =
        InferiorGA(TestProbe("inferiorGridAgent_12"), Seq(node2.getUuid))

      val inferiorGrid13 = InferiorGA(
        TestProbe("inferiorGridAgent_13"),
        Seq(node3.getUuid, node4.getUuid),
      )

      // this subnet has 1 superior grid (ehv) and 3 inferior grids (mv). Map the gates to test probes accordingly
      val subGridGateToActorRef = hvSubGridGates.map {
        case gate if gate.getInferiorSubGrid == hvGridContainer.getSubnet =>
          gate -> superiorGridAgent.ref
        case gate =>
          val actor = gate.getInferiorSubGrid match {
            case 11 => inferiorGrid11
            case 12 => inferiorGrid12
            case 13 => inferiorGrid13
          }
          gate -> actor.ref
      }.toMap

      val centerGridAgent =
        testKit.spawn(
          GridAgent(
            environmentRefs,
            config,
            listener = Iterable(resultListener.ref),
          )
        )

      // initialize the agent and go to the detection step
      initAgent(centerGridAgent, subGridGateToActorRef)

      inferiorGrid11.gaProbe.expectMessageType[FinishGridSimulationTrigger]
      inferiorGrid12.gaProbe.expectMessageType[FinishGridSimulationTrigger]
      inferiorGrid13.gaProbe.expectMessageType[FinishGridSimulationTrigger]

      centerGridAgent ! CongestionCheckRequest(superiorGridAgent.ref)

      val sender11 =
        inferiorGrid11.gaProbe.expectMessageType[CongestionCheckRequest].sender
      val sender12 =
        inferiorGrid12.gaProbe.expectMessageType[CongestionCheckRequest].sender
      val sender13 =
        inferiorGrid13.gaProbe.expectMessageType[CongestionCheckRequest].sender

      sender11 ! CongestionResponse(
        inferiorGrid11.ref,
        Congestions(true, false, false),
      )
      sender12 ! CongestionResponse(
        inferiorGrid12.ref,
        Congestions(false, true, false),
      )
      sender13 ! CongestionResponse(
        inferiorGrid13.ref,
        Congestions(false, false, false),
      )

      val allCongestions =
        superiorGridAgent.gaProbe.expectMessageType[CongestionResponse]
      allCongestions.value shouldBe Congestions(true, true, false)

      centerGridAgent ! FinishStep

      inferiorGrid11.gaProbe.expectMessageType[FinishStep.type]
      inferiorGrid12.gaProbe.expectMessageType[FinishStep.type]
      inferiorGrid13.gaProbe.expectMessageType[FinishStep.type]

      scheduler.expectMessageType[Completion]
    }
  }

  def initAgent(
      gridAgent: ActorRef[GridAgent.Request],
      subGridGateToActorRef: Map[SubGridGate, ActorRef[GridAgent.Request]],
  ): Unit = {

    val gridAgentInitData =
      GridAgentInitData(
        hvGridContainer,
        Seq.empty[ThermalGrid],
        subGridGateToActorRef,
        RefSystem("2000 MVA", "110 kV"),
        VoltageLimits(0.9, 1.1),
      )

    val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
    // lock activation scheduled
    scheduler.expectMessageType[ScheduleActivation]

    gridAgent ! CreateGridAgent(
      gridAgentInitData,
      key,
    )

    val scheduleActivationMsg =
      scheduler.expectMessageType[ScheduleActivation]
    scheduleActivationMsg.tick shouldBe INIT_SIM_TICK
    scheduleActivationMsg.unlockKey shouldBe Some(key)
    val gridAgentActivation = scheduleActivationMsg.actor

    gridAgent ! WrappedActivation(Activation(INIT_SIM_TICK))
    scheduler.expectMessage(Completion(gridAgentActivation, Some(3600)))

    gridAgent ! WrappedActivation(Activation(3600))

    scheduler.expectMessageType[Completion].newTick shouldBe Some(3600)

    gridAgent ! FinishGridSimulationTrigger(3600)
  }

}
