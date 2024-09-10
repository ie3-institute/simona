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
import edu.ie3.simona.agent.grid.CongestionManagementSupport.{
  Congestions,
  VoltageRange,
}
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.{
  ExchangePower,
  ExchangeVoltage,
}
import edu.ie3.simona.agent.grid.GridAgentMessages._
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
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.Megavars
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import squants.electro.Kilovolts
import squants.energy.Megawatts

class DCMAlgorithmCenGridSpec
    extends ScalaTestWithActorTestKit
    with DBFSMockGridAgents
    with ConfigTestData
    with DbfsTestGrid
    with TestSpawnerTyped {

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
  private val primaryService = TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid, supNodeB.getUuid),
  )

  private val inferiorGrid11 =
    InferiorGA(TestProbe("inferiorGridAgent_11"), Seq(node1.getUuid))

  private val inferiorGrid12 =
    InferiorGA(TestProbe("inferiorGridAgent_12"), Seq(node2.getUuid))

  private val inferiorGrid13 = InferiorGA(
    TestProbe("inferiorGridAgent_13"),
    Seq(node3.getUuid, node4.getUuid),
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
    val noCongestions = Congestions(
      voltageCongestions = false,
      lineCongestions = false,
      transformerCongestions = false,
    )

    val voltageCongestions = noCongestions.copy(voltageCongestions = true)

    s"simulate grid and check for congestions correctly if no congestions occurred" in {
      // init grid agent and simulate the grid
      val centerGridAgent =
        initAgentAndGotoSimulateGrid(configWithTransformerTapping)
      simulateGrid(centerGridAgent)

      // after the simulation ends the center grid should receive a CongestionCheckRequest
      // from the superior grid
      centerGridAgent ! CongestionCheckRequest(superiorGridAgent.ref)

      // we expect a request for grid congestion values here
      val congestionCheckRequestSender11 =
        inferiorGrid11.expectCongestionCheckRequest()
      val congestionCheckRequestSender12 =
        inferiorGrid12.expectCongestionCheckRequest()
      val congestionCheckRequestSender13 =
        inferiorGrid13.expectCongestionCheckRequest()

      // send the center grid messages that indicate that no congestion occurred
      // in the inferior grids
      congestionCheckRequestSender11 ! CongestionResponse(
        inferiorGrid11.ref,
        noCongestions,
      )
      congestionCheckRequestSender12 ! CongestionResponse(
        inferiorGrid12.ref,
        noCongestions,
      )
      congestionCheckRequestSender13 ! CongestionResponse(
        inferiorGrid13.ref,
        noCongestions,
      )

      // after the center grid agent has processed all received congestions
      // the superior grid will receive a congestion response
      superiorGridAgent.expectCongestionResponse(noCongestions)

      // since there are no congestions tell all inferior grids to go back to idle
      centerGridAgent ! GotoIdle

      // inferior should receive a next state message to go to the idle state
      inferiorGrid11.gaProbe.expectMessageType[GotoIdle.type]
      inferiorGrid12.gaProbe.expectMessageType[GotoIdle.type]
      inferiorGrid13.gaProbe.expectMessageType[GotoIdle.type]

      // expect a completion message from the superior grid
      scheduler.expectMessageType[Completion] match {
        case Completion(_, Some(7200)) =>
        case x =>
          fail(
            s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
          )
      }
    }

    s"simulate grid and update transformer tapping correctly" in {
      // init grid agent and simulate the grid
      val centerGridAgent =
        initAgentAndGotoSimulateGrid(configWithTransformerTapping)
      simulateGrid(centerGridAgent)

      // after the simulation ends the center grid should receive a CongestionCheckRequest
      // from the superior grid
      centerGridAgent ! CongestionCheckRequest(superiorGridAgent.ref)

      // we expect a request for grid congestion values here
      val congestionCheckRequestSender11 =
        inferiorGrid11.expectCongestionCheckRequest()
      val congestionCheckRequestSender12 =
        inferiorGrid12.expectCongestionCheckRequest()
      val congestionCheckRequestSender13 =
        inferiorGrid13.expectCongestionCheckRequest()

      // send the center grid messages that indicate that congestions occurred
      // in some inferior grids
      congestionCheckRequestSender11 ! CongestionResponse(
        inferiorGrid11.ref,
        voltageCongestions,
      )
      congestionCheckRequestSender12 ! CongestionResponse(
        inferiorGrid12.ref,
        noCongestions,
      )
      congestionCheckRequestSender13 ! CongestionResponse(
        inferiorGrid13.ref,
        voltageCongestions,
      )

      // after the center grid agent has processed all received congestions
      // the superior grid will receive a congestion response
      superiorGridAgent.expectCongestionResponse(voltageCongestions)

      // since the superior grid receives a message the shows that there is at
      // least one congestion in the grid, it starts the congestions management
      // because there are voltage congestions and the transformet tapping has
      // not run yet, the next step is the transformer tapping
      centerGridAgent ! NextStepRequest(
        TransformerTapping
      )

      // inferior grids should receive a next state message to go to the transformer tapping step
      inferiorGrid11.gaProbe
        .expectMessageType[NextStepRequest]
        .nextStep shouldBe TransformerTapping
      inferiorGrid12.gaProbe
        .expectMessageType[NextStepRequest]
        .nextStep shouldBe TransformerTapping
      inferiorGrid13.gaProbe
        .expectMessageType[NextStepRequest]
        .nextStep shouldBe TransformerTapping

      // since the transformer tapping was started, the superior grid
      // requests the possible voltage range from the center grid
      centerGridAgent ! RequestVoltageOptions(superiorGridAgent.ref, 1000)

      // the center grid will request the voltage ranges from its inferior grid
      // therefore the inferior grids should receive a VoltageRangeRequest
      val voltageRangeRequester11 = inferiorGrid11.expectVoltageRangeRequest()
      val voltageRangeRequester12 = inferiorGrid12.expectVoltageRangeRequest()
      val voltageRangeRequester13 = inferiorGrid13.expectVoltageRangeRequest()

      // each inferior grid will send its possible voltage range to the center grid
      voltageRangeRequester11 ! VoltageRangeResponse(
        inferiorGrid11.ref,
        (
          VoltageRange((-0.01).asPu, (-0.02).asPu),
          Set(mvTransformers(transformer11.getUuid)),
        ),
      )
      voltageRangeRequester12 ! VoltageRangeResponse(
        inferiorGrid12.ref,
        (
          VoltageRange(0.07.asPu, 0.01.asPu),
          Set(mvTransformers(transformer12.getUuid)),
        ),
      )
      voltageRangeRequester13 ! VoltageRangeResponse(
        inferiorGrid13.ref,
        (
          VoltageRange(0.06.asPu, 0.asPu),
          Set(
            mvTransformers(transformer13a.getUuid),
            mvTransformers(transformer13b.getUuid),
          ),
        ),
      )

      // after the center grid received all voltage ranges
      // the superior grid should receive a voltage range from the center grid
      val (voltageDeltaRequest, tappingModels) =
        superiorGridAgent.expectVoltageRangeResponse(
          VoltageRange(0.04.asPu, 0.01.asPu, 0.025.asPu)
        )

      // the superior grid will update the transformer tappings
      // and send the the resulting voltage delta to the center grid
      tappingModels.size shouldBe 2
      tappingModels.foreach(_.decrTapPos(2))
      voltageDeltaRequest ! VoltageDeltaResponse(0.03.asPu)

      // the inferior grids should receive a voltage delta from the center grid
      inferiorGrid11.gaProbe
        .expectMessageType[VoltageDeltaResponse]
        .delta should equalWithTolerance((-0.01).asPu)
      inferiorGrid12.gaProbe
        .expectMessageType[VoltageDeltaResponse]
        .delta should equalWithTolerance(0.03.asPu)
      inferiorGrid13.gaProbe
        .expectMessageType[VoltageDeltaResponse]
        .delta should equalWithTolerance(0.03.asPu)

      // this transformer can be tapped
      mvTransformers(transformer11.getUuid).currentTapPos shouldBe 4

      // these transformers can't be tapped and should keep their default tap pos
      mvTransformers(transformer12.getUuid).currentTapPos shouldBe 0
      mvTransformers(transformer13a.getUuid).currentTapPos shouldBe 0
      mvTransformers(transformer13a.getUuid).currentTapPos shouldBe 0

      // skipping this simulation step
      skipSimulation(centerGridAgent)

      // aks for congestion check
      centerGridAgent ! CongestionCheckRequest(superiorGridAgent.ref)

      // we expect a request for grid congestion values here
      val congestionCheckRequestSender21 =
        inferiorGrid11.expectCongestionCheckRequest()
      val congestionCheckRequestSender22 =
        inferiorGrid12.expectCongestionCheckRequest()
      val congestionCheckRequestSender23 =
        inferiorGrid13.expectCongestionCheckRequest()

      // send congestions
      congestionCheckRequestSender21 ! CongestionResponse(
        inferiorGrid11.ref,
        noCongestions,
      )
      congestionCheckRequestSender22 ! CongestionResponse(
        inferiorGrid12.ref,
        noCongestions,
      )
      congestionCheckRequestSender23 ! CongestionResponse(
        inferiorGrid13.ref,
        noCongestions,
      )

      // after the simulation ends the center grid should receive a CongestionCheckRequest
      // from the superior grid
      superiorGridAgent.expectCongestionResponse(noCongestions)

      // since there are no congestions tell all inferior grids to go back to idle
      centerGridAgent ! GotoIdle

      // inferior should receive a next state message to go to the idle state
      inferiorGrid11.gaProbe.expectMessageType[GotoIdle.type]
      inferiorGrid12.gaProbe.expectMessageType[GotoIdle.type]
      inferiorGrid13.gaProbe.expectMessageType[GotoIdle.type]

      // expect a completion message from the superior grid
      scheduler.expectMessageType[Completion] match {
        case Completion(_, Some(7200)) =>
        case x =>
          fail(
            s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
          )
      }
    }

    // helper methods

    /** Method to initialize a superior grid agent with the given config. The
      * grid agent is already in the simulateGrid state.
      *
      * @param simonaConfig
      *   that enables or disables certain congestion management steps
      * @return
      *   the [[ActorRef]] of the created superior grid agent
      */
    def initAgentAndGotoSimulateGrid(
        simonaConfig: SimonaConfig
    ): ActorRef[GridAgent.Request] = {
      val centerGridAgent =
        testKit.spawn(
          GridAgent(
            environmentRefs,
            simonaConfig,
            listener = Iterable(resultListener.ref),
          )
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

      // goto simulate grid
      centerGridAgent ! WrappedActivation(Activation(3600))

      // we expect a completion message
      scheduler.expectMessageType[Completion].newTick shouldBe Some(3600)

      centerGridAgent
    }

    /** Method to skip a simulation step.
      * @param centerGridAgent
      *   center grid agent
      */
    def skipSimulation(centerGridAgent: ActorRef[GridAgent.Request]): Unit = {
      inferiorGrid11.gaProbe.expectMessageType[RequestGridPower]
      inferiorGrid12.gaProbe.expectMessageType[RequestGridPower]
      inferiorGrid13.gaProbe.expectMessageType[RequestGridPower]
      superiorGridAgent.gaProbe.expectMessageType[SlackVoltageRequest]

      // skip simulation and go to congestion check
      centerGridAgent ! FinishGridSimulationTrigger(3600)

      // inferior grid receives a FinishGridSimulationTrigger and goes into the congestion check state
      inferiorGrid11.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))
      inferiorGrid12.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))
      inferiorGrid13.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))
    }

    /** Method to reduce duplicate code. This runs a simple simulation based on
      * the [[DBFSAlgorithmCenGridSpec]].
      */
    def simulateGrid(centerGridAgent: ActorRef[GridAgent.Request]): Unit = {
      // start the simulation
      val firstSweepNo = 0

      // send the start grid simulation trigger
      centerGridAgent ! WrappedActivation(Activation(3600))

      /* We expect one grid power request message per inferior grid */

      val firstPowerRequestSender11 = inferiorGrid11.expectGridPowerRequest()

      val firstPowerRequestSender12 = inferiorGrid12.expectGridPowerRequest()

      val firstPowerRequestSender13 = inferiorGrid13.expectGridPowerRequest()

      // we expect a request for voltage values of two nodes
      // (voltages are requested by our agent under test from the superior grid)
      val firstSlackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(firstSweepNo)

      // normally the inferior grid agents ask for the slack voltage as well to do their power flow calculations
      // we simulate this behaviour now by doing the same for our three inferior grid agents
      inferiorGrid11.requestSlackVoltage(centerGridAgent, firstSweepNo)

      inferiorGrid12.requestSlackVoltage(centerGridAgent, firstSweepNo)

      inferiorGrid13.requestSlackVoltage(centerGridAgent, firstSweepNo)

      // as we are in the first sweep, all provided slack voltages should be equal
      // to 1 p.u. (in physical values, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)
      inferiorGrid11.expectSlackVoltageProvision(
        firstSweepNo,
        Seq(
          ExchangeVoltage(
            node1.getUuid,
            Kilovolts(110d),
            Kilovolts(0d),
          )
        ),
      )

      inferiorGrid12.expectSlackVoltageProvision(
        firstSweepNo,
        Seq(
          ExchangeVoltage(
            node2.getUuid,
            Kilovolts(110d),
            Kilovolts(0d),
          )
        ),
      )

      inferiorGrid13.expectSlackVoltageProvision(
        firstSweepNo,
        Seq(
          ExchangeVoltage(
            node3.getUuid,
            Kilovolts(110d),
            Kilovolts(0d),
          ),
          ExchangeVoltage(
            node4.getUuid,
            Kilovolts(110d),
            Kilovolts(0d),
          ),
        ),
      )

      // we now answer the request of our centerGridAgent
      // with three fake grid power messages and one fake slack voltage message

      firstPowerRequestSender11 ! GridPowerResponse(
        inferiorGrid11.nodeUuids.map(nodeUuid =>
          ExchangePower(
            nodeUuid,
            Megawatts(0.0),
            Megavars(0.0),
          )
        )
      )

      firstPowerRequestSender12 ! GridPowerResponse(
        inferiorGrid12.nodeUuids.map(nodeUuid =>
          ExchangePower(
            nodeUuid,
            Megawatts(0.0),
            Megavars(0.0),
          )
        )
      )

      firstPowerRequestSender13 ! GridPowerResponse(
        inferiorGrid13.nodeUuids.map(nodeUuid =>
          ExchangePower(
            nodeUuid,
            Megawatts(0.0),
            Megavars(0.0),
          )
        )
      )

      firstSlackVoltageRequestSender ! SlackVoltageResponse(
        firstSweepNo,
        Seq(
          ExchangeVoltage(
            supNodeA.getUuid,
            Kilovolts(380d),
            Kilovolts(0d),
          ),
          ExchangeVoltage(
            supNodeB.getUuid,
            Kilovolts(380d),
            Kilovolts(0d),
          ),
        ),
      )

      // power flow calculation should run now. After it's done,
      // our test agent should now be ready to provide the grid power values,
      // hence we ask for them and expect a corresponding response
      superiorGridAgent.requestGridPower(centerGridAgent, firstSweepNo)

      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            Megawatts(0.0),
            Megavars(0.0),
          ),
          ExchangePower(
            supNodeB.getUuid,
            Megawatts(0.160905770717798),
            Megavars(-1.4535602349123878),
          ),
        )
      )

      // we start a second sweep by asking for next sweep values which should trigger the whole procedure again
      val secondSweepNo = 1

      superiorGridAgent.requestGridPower(centerGridAgent, secondSweepNo)

      // the agent now should ask for updated slack voltages from the superior grid
      val secondSlackAskSender =
        superiorGridAgent.expectSlackVoltageRequest(secondSweepNo)

      // the superior grid would answer with updated slack voltage values
      secondSlackAskSender ! SlackVoltageResponse(
        secondSweepNo,
        Seq(
          ExchangeVoltage(
            supNodeB.getUuid,
            Kilovolts(374.22694614463d), // 380 kV @ 10°
            Kilovolts(65.9863075134335d), // 380 kV @ 10°
          ),
          ExchangeVoltage( // this one should currently be ignored anyways
            supNodeA.getUuid,
            Kilovolts(380d),
            Kilovolts(0d),
          ),
        ),
      )

      // After the intermediate power flow calculation, we expect one grid power
      // request message per inferior subgrid

      val secondPowerRequestSender11 =
        inferiorGrid11.expectGridPowerRequest()

      val secondPowerRequestSender12 =
        inferiorGrid12.expectGridPowerRequest()

      val secondPowerRequestSender13 =
        inferiorGrid13.expectGridPowerRequest()

      // normally the inferior grid agents ask for the slack voltage as well to do their power flow calculations
      // we simulate this behaviour now by doing the same for our three inferior grid agents

      inferiorGrid11.requestSlackVoltage(centerGridAgent, secondSweepNo)

      inferiorGrid12.requestSlackVoltage(centerGridAgent, secondSweepNo)

      inferiorGrid13.requestSlackVoltage(centerGridAgent, secondSweepNo)

      // as we are in the second sweep, all provided slack voltages should be unequal
      // to 1 p.u. (in physical values, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)

      inferiorGrid11.expectSlackVoltageProvision(
        secondSweepNo,
        Seq(
          ExchangeVoltage(
            node1.getUuid,
            Kilovolts(108.487669651919932d),
            Kilovolts(19.101878551141232d),
          )
        ),
      )

      inferiorGrid12.expectSlackVoltageProvision(
        secondSweepNo,
        Seq(
          ExchangeVoltage(
            node2.getUuid,
            Kilovolts(108.449088870497683d),
            Kilovolts(19.10630456834157630d),
          )
        ),
      )

      inferiorGrid13.expectSlackVoltageProvision(
        secondSweepNo,
        Seq(
          ExchangeVoltage(
            node3.getUuid,
            Kilovolts(108.470028019077087d),
            Kilovolts(19.104403047662570d),
          ),
          ExchangeVoltage(
            node4.getUuid,
            Kilovolts(108.482524607256866d),
            Kilovolts(19.1025584700935336d),
          ),
        ),
      )

      // we now answer the requests of our centerGridAgent
      // with three fake grid power message

      secondPowerRequestSender11 ! GridPowerResponse(
        inferiorGrid11.nodeUuids.map(nodeUuid =>
          ExchangePower(
            nodeUuid,
            Megawatts(0.0),
            Megavars(0.0),
          )
        )
      )

      secondPowerRequestSender12 ! GridPowerResponse(
        inferiorGrid12.nodeUuids.map(nodeUuid =>
          ExchangePower(
            nodeUuid,
            Megawatts(0.0),
            Megavars(0.0),
          )
        )
      )

      secondPowerRequestSender13 ! GridPowerResponse(
        inferiorGrid13.nodeUuids.map(nodeUuid =>
          ExchangePower(
            nodeUuid,
            Megawatts(0.0),
            Megavars(0.0),
          )
        )
      )

      // we expect that the GridAgent unstashes the messages and return a value for our power request
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            Megawatts(0.0),
            Megavars(0.0),
          ),
          ExchangePower(
            supNodeB.getUuid,
            Megawatts(0.16090577067051856),
            Megavars(-1.4535602358772026),
          ),
        )
      )

      // normally the slack node would send a FinishGridSimulationTrigger to all
      // connected inferior grids, because the slack node is just a mock, we imitate this behavior
      centerGridAgent ! FinishGridSimulationTrigger(3600)

      // after a FinishGridSimulationTrigger is send the inferior grids, they themselves will send the
      // Trigger forward the trigger to their connected inferior grids. Therefore the inferior grid
      // agent should receive a FinishGridSimulationTrigger
      inferiorGrid11.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))

      inferiorGrid12.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))

      inferiorGrid13.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))
    }

  }
}
