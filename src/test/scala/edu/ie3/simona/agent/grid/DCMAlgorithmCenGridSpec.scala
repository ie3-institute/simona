/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.CongestionManagementSupport.CongestionManagementSteps.TransformerTapping
import edu.ie3.simona.agent.grid.CongestionManagementSupport.Congestions
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  CongestionCheckRequest,
  CongestionResponse,
  CreateGridAgent,
  FinishGridSimulationTrigger,
  FinishStep,
  GotoIdle,
  NextStepRequest,
  RequestGridPower,
  SlackVoltageRequest,
  WrappedActivation,
}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.model.grid.{RefSystem, VoltageLimits}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped}
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps

import scala.concurrent.duration.DurationInt

class DCMAlgorithmCenGridSpec
    extends ScalaTestWithActorTestKit
    with DBFSMockGridAgents
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
  private val primaryService = TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val superiorGridAgent: TestProbe[GridAgent.Request] = TestProbe(
    "superiorGridAgent_1000"
  )

  private val inferiorGrid11: TestProbe[GridAgent.Request] = TestProbe(
    "inferiorGridAgent_11"
  )

  private val inferiorGrid12: TestProbe[GridAgent.Request] = TestProbe(
    "inferiorGridAgent_12"
  )

  private val inferiorGrid13: TestProbe[GridAgent.Request] = TestProbe(
    "inferiorGridAgent_13"
  )

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    runtimeEventListener = runtimeEvents.ref,
    primaryServiceProxy = primaryService.ref.toClassic,
    weather = weatherService.ref.toClassic,
    evDataService = None,
  )

  val resultListener: TestProbe[ResultEvent] = TestProbe("resultListener")

  "A GridAgent actor in center position with async test" should {

    val centerGridAgent =
      testKit.spawn(
        GridAgent(
          environmentRefs,
          config,
          listener = Iterable(resultListener.ref),
        )
      )
    s"initialize itself when it receives an init activation" in {

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

      centerGridAgent ! CreateGridAgent(
        gridAgentInitData,
        key,
      )

      val scheduleActivationMsg =
        scheduler.expectMessageType[ScheduleActivation]
      scheduleActivationMsg.tick shouldBe INIT_SIM_TICK
      scheduleActivationMsg.unlockKey shouldBe Some(key)
      val gridAgentActivation = scheduleActivationMsg.actor

      centerGridAgent ! WrappedActivation(Activation(INIT_SIM_TICK))
      scheduler.expectMessage(Completion(gridAgentActivation, Some(3600)))
    }

    s"skip simulate grid and check for congestions correctly if no congestions occurred" in {
      skipSimulation()

      cmStart()
      cmFinish()
    }

    s"update transformer tapping correctly" in {
      skipSimulation()

      val congestions = Congestions(
        voltageCongestions = true,
        lineCongestions = false,
        transformerCongestions = false,
      )

      cmStart(congestions)

      // initiate transformer tapping
      centerGridAgent ! NextStepRequest(
        TransformerTapping
      )

      // inferior grids should receive a next state message to go to a congestion management step
      inferiorGrid11.expectMessageType[NextStepRequest]
      inferiorGrid12.expectMessageType[NextStepRequest]
      inferiorGrid13.expectMessageType[NextStepRequest]

      // skipping the step for now
      // TODO: Update test after implementing transformer tapping
      centerGridAgent ! FinishStep
      inferiorGrid11.expectMessageType[FinishStep.type]
      inferiorGrid12.expectMessageType[FinishStep.type]
      inferiorGrid13.expectMessageType[FinishStep.type]

      // skipping the simulation
      skipSimulation(true)
      cmStart()
      cmFinish()
    }

    /** Method to reduce duplicate code
      * @param midTest
      *   to check if this is in the middle of a test or at the beginning
      */
    def skipSimulation(midTest: Boolean = false): Unit = {
      if (!midTest) {
        centerGridAgent ! WrappedActivation(Activation(3600))

        // we expect a completion message
        scheduler.expectMessageType[Completion].newTick shouldBe Some(3600)
      } else {
        inferiorGrid11.expectMessageType[RequestGridPower]
        inferiorGrid12.expectMessageType[RequestGridPower]
        inferiorGrid13.expectMessageType[RequestGridPower]
        superiorGridAgent.expectMessageType[SlackVoltageRequest]
      }

      // skip simulation and go to congestion check
      centerGridAgent ! FinishGridSimulationTrigger(3600)

      // inferior grid receives a FinishGridSimulationTrigger and goes into the congestion check state
      inferiorGrid11.expectMessage(FinishGridSimulationTrigger(3600))
      inferiorGrid12.expectMessage(FinishGridSimulationTrigger(3600))
      inferiorGrid13.expectMessage(FinishGridSimulationTrigger(3600))
    }

    /** Method to reduce duplicate code
      * @param congestions
      *   to be send to the [[centerGridAgent]] (default: no congestions)
      */
    def cmStart(
        congestions: Congestions = Congestions(
          voltageCongestions = false,
          lineCongestions = false,
          transformerCongestions = false,
        )
    ): Unit = {
      centerGridAgent ! CongestionCheckRequest(superiorGridAgent.ref)

      // we expect a request for grid congestion values here
      val congestionCheckRequestSender11 =
        inferiorGrid11.expectMessageType[CongestionCheckRequest]
      val congestionCheckRequestSender12 =
        inferiorGrid12.expectMessageType[CongestionCheckRequest]
      val congestionCheckRequestSender13 =
        inferiorGrid13.expectMessageType[CongestionCheckRequest]

      // send congestions
      congestionCheckRequestSender11.sender ! CongestionResponse(
        inferiorGrid11.ref,
        congestions,
      )
      congestionCheckRequestSender12.sender ! CongestionResponse(
        inferiorGrid12.ref,
        congestions,
      )
      congestionCheckRequestSender13.sender ! CongestionResponse(
        inferiorGrid13.ref,
        congestions,
      )

      // we expect transformer congestions in the whole grid
      val allCongestions = superiorGridAgent
        .expectMessageType[CongestionResponse](30.seconds)
        .value
      allCongestions shouldBe congestions
    }

    /** Method to reduce duplicate code
      */
    def cmFinish(): Unit = {
      // telling all inferior grids to go back to idle
      centerGridAgent ! GotoIdle

      // inferior should receive a next state message to go to the idle state
      inferiorGrid11.expectMessageType[GotoIdle.type]
      inferiorGrid12.expectMessageType[GotoIdle.type]
      inferiorGrid13.expectMessageType[GotoIdle.type]

      // expect a completion message from the superior grid
      scheduler.expectMessageType[Completion] match {
        case Completion(_, Some(7200)) =>
        case x =>
          fail(
            s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
          )
      }
    }
  }
}
