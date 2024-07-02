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
import edu.ie3.simona.agent.grid.CongestionManagementSupport.{
  Congestions,
  VoltageRange,
}
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.grid.GridAgentMessages._
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.model.grid.{RefSystem, TransformerModel, VoltageLimits}
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
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import squants.electro.Kilovolts
import squants.energy.Kilowatts

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

  // config with congestion management and transformer tapping enabled
  private val tappingEnabledConfig = ConfigFactory.parseString("""
      |simona.congestionManagement.enable = true
      |simona.congestionManagement.enableTransformerTapping = true
      |""".stripMargin)

  private val configWithTransformerTapping = SimonaConfig(
    tappingEnabledConfig.withFallback(typesafeConfig)
  )

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
    val refSystem = RefSystem(Kilowatts(600), Kilovolts(110))

    val tappingModel = TransformerModel(
      transformer1,
      refSystem,
      start,
      end,
    )

    val tappingModel2 = TransformerModel(
      transformer2,
      refSystem,
      start,
      end,
    )

    s"skip simulate grid and check for congestions correctly if no congestions occurred" in {
      val superiorGridAgent =
        initAgentAndGotoSimulateGrid(configWithTransformerTapping)

      val lastSender = skipSimulationAndGetNextStep(superiorGridAgent)

      // send the requesting grid agent a CongestionResponse with no congestions
      lastSender ! CongestionResponse(
        hvGrid.ref,
        Congestions(
          voltageCongestions = false,
          lineCongestions = false,
          transformerCongestions = false,
        ),
      )

      // since their are no congestions the inferior should receive
      // a next state message to go to the idle state
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

    s"skip simulate grid and handle unresolvable congestions correctly" in {
      val superiorGridAgent =
        initAgentAndGotoSimulateGrid(configWithTransformerTapping)

      val lastSender = skipSimulationAndGetNextStep(superiorGridAgent)

      // voltage congestion cannot be resolved, because using flex options is not
      // enable by the provided config
      lastSender ! CongestionResponse(
        hvGrid.ref,
        Congestions(
          voltageCongestions = false,
          lineCongestions = false,
          transformerCongestions = true,
        ),
      )

      // since the congestion management can't resolve the congestions
      // the inferior should receive a next state message to go to the idle state
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

    s"skip simulate grid and update transformer tapping correctly" in {
      val superiorGridAgent =
        initAgentAndGotoSimulateGrid(configWithTransformerTapping)

      val lastSender1 = skipSimulationAndGetNextStep(superiorGridAgent)

      // sending the superior grid a solvable congestion
      lastSender1 ! CongestionResponse(
        hvGrid.ref,
        Congestions(
          voltageCongestions = true,
          lineCongestions = false,
          transformerCongestions = false,
        ),
      )

      // since the received congestion can be resolved the inferior should
      // receive a next state message to go to a congestion management step
      hvGrid.expectMessageType[NextStepRequest]

      // both transformer models should have their default tap pos
      tappingModel.currentTapPos shouldBe 0
      tappingModel2.currentTapPos shouldBe 0

      // the inferior will receive a request to send the possible voltage range
      // and send a VoltageRangeResponse to the superior grid
      hvGrid.expectMessageType[RequestVoltageOptions] match {
        case RequestVoltageOptions(sender, subnet) =>
          subnet shouldBe 1000

          sender ! VoltageRangeResponse(
            hvGrid.ref,
            (
              VoltageRange(0.025.asPu, 0.01.asPu),
              Set(tappingModel, tappingModel2),
            ),
          )
      }

      // the inferior will receive a voltage delta from the superior grid
      // after the superior grid change the transformer tapping
      hvGrid.expectMessageType[VoltageDeltaResponse](120.seconds) match {
        case VoltageDeltaResponse(delta) =>
          delta should equalWithTolerance(0.015.asPu)
      }

      // both transformer models tap pos should have changed by the same amount
      tappingModel.currentTapPos shouldBe -1
      tappingModel2.currentTapPos shouldBe -1

      // skipping the simulation
      hvGrid.expectMessageType[RequestGridPower]

      val lastSender2 = skipSimulationAndGetNextStep(superiorGridAgent)

      // sending the superior grid that no more congestions are present
      lastSender2 ! CongestionResponse(
        hvGrid.ref,
        Congestions(
          voltageCongestions = false,
          lineCongestions = false,
          transformerCongestions = false,
        ),
      )

      // since their are no congestions the inferior should receive
      // a next state message to go to the idle state
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

    // helper methods

    /** There is no need to perform an actual simulation of the grid, therefor
      * we can use this method to skip the
      * @param superiorGridAgent
      *   the superior grid agent
      * @return
      *   the [[ActorRef]] of the last sender
      */
    def skipSimulationAndGetNextStep(
        superiorGridAgent: ActorRef[GridAgent.Request]
    ): ActorRef[GridAgent.Request] = {
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

      // return the last sender
      lastSender
    }

    /** Method to initialize a superior grid agent with the given config. The
      * grid agent is already in the simulateGrid state.
      * @param simonaConfig
      *   that enables or disables certain congestion management steps
      * @return
      *   the [[ActorRef]] of the created superior grid agent
      */
    def initAgentAndGotoSimulateGrid(
        simonaConfig: SimonaConfig
    ): ActorRef[GridAgent.Request] = {
      // init a superior grid agent with the given config
      // that enabled certain congestion management options
      val superiorGridAgent: ActorRef[GridAgent.Request] = testKit.spawn(
        GridAgent(
          environmentRefs,
          simonaConfig,
          listener = Iterable(resultListener.ref),
        )
      )

      val subnetGatesToActorRef: Map[SubGridGate, ActorRef[GridAgent.Request]] =
        ehvSubGridGates.map(gate => gate -> hvGrid.ref).toMap

      val gridAgentInitData =
        GridAgentInitData(
          ehvGridContainer,
          Seq.empty[ThermalGrid],
          subnetGatesToActorRef,
          RefSystem("5000 MVA", "110 kV"),
          VoltageLimits(0.9, 1.05),
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

      // goto simulate grid
      superiorGridAgent ! WrappedActivation(Activation(3600))

      // we expect a completion message
      scheduler.expectMessageType[Completion].newTick shouldBe Some(3600)

      superiorGridAgent
    }
  }
}
