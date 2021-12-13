/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestFSMRef}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.GridAgentState.{
  HandlePowerFlowCalculations,
  SimulateGrid
}
import edu.ie3.simona.akka.SimonaActorRef.RichActorRef
import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.ontology.messages.PowerMessage.{
  ProvideGridPowerMessage,
  RequestGridPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.VoltageMessage.{
  ProvideSlackVoltageMessage,
  RequestSlackVoltageMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeGridAgentTrigger,
  StartGridSimulationTrigger
}
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.test.common.{
  ConfigTestData,
  StateTransitionTester,
  TestKitWithShutdown,
  UnitSpec
}
import edu.ie3.util.quantities.PowerSystemUnits._
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._

/** Test to ensure the functions that a [[GridAgent]] in center position should
  * be able to do if the DBFSAlgorithm is used. The scheduler, the weather
  * service as well as the inferior and superior [[GridAgent]] s are simulated
  * by the TestKit. By now this test does NOT cover interactions with generation
  * or load asset agents due to unavailability during test development. Hence it
  * would make sense to extend this test in the future to include asset agent
  * interaction or cover this behaviour by another (integration) test!
  */
class DBFSAlgorithmCenGridSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "DBFSAlgorithmSpec",
        ConfigFactory
          .parseString("""
            |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
            |akka.loglevel="OFF"
        """.stripMargin)
      )
    )
    with UnitSpec
    with ConfigTestData
    with ImplicitSender
    with DbfsTestGrid {

  private val floatPrecision: Double = 0.00000000001

  private val environmentRefs = EnvironmentRefs(
    scheduler = self.asLocal,
    primaryServiceProxy = self.asLocal,
    weather = self.asLocal,
    evDataService = None
  )

  "A GridAgent actor in center position with FSM test" should {

    val centerGridAgent = TestFSMRef(
      new GridAgent(
        environmentRefs,
        simonaConfig,
        listener = Iterable.empty[SimonaActorRef]
      )
    )

    s"be in state $Uninitialized after startup" in {
      centerGridAgent.stateName shouldBe Uninitialized
    }

    s"initialize itself when it receives a $InitializeGridAgentTrigger with corresponding data" in {
      val triggerId = 0

      // this subnet has 1 superior grid (HöS) and 4 inferior grids (MS)
      val subGridGateToActorRef =
        hvSubGridGates.map(gate => gate -> self.asLocal).toMap

      val gridAgentInitData =
        GridAgentInitData(
          hvGridContainer,
          subGridGateToActorRef,
          RefSystem("2000 MVA", "110 kV")
        )

      // send init data to agent and expect a CompletionMessage
      implicit val timeout: Timeout = Timeout(1, TimeUnit.SECONDS)
      val expectedCompletionMessage =
        Await.result(
          centerGridAgent ? TriggerWithIdMessage(
            InitializeGridAgentTrigger(gridAgentInitData),
            triggerId,
            centerGridAgent.asLocal
          ),
          timeout.duration
        )

      expectedCompletionMessage shouldBe CompletionMessage(
        0,
        centerGridAgent.asLocal,
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(3600),
              centerGridAgent.asLocal
            )
          )
        )
      )

      // grid agent state should be idle afterwards
      centerGridAgent.stateName shouldBe Idle

    }

    s"go to $SimulateGrid when it receives an activity start trigger" in {

      val activityStartTriggerId = 1

      centerGridAgent ! TriggerWithIdMessage(
        ActivityStartTrigger(3600),
        activityStartTriggerId,
        centerGridAgent.asLocal
      )

      expectMsgPF() {
        case CompletionMessage(
              triggerId,
              actor,
              Some(Vector(ScheduleTriggerMessage(triggerToBeScheduled, _)))
            ) =>
          triggerId shouldBe 1
          actor shouldBe centerGridAgent.asLocal
          triggerToBeScheduled shouldBe StartGridSimulationTrigger(3600)
        case x =>
          fail(
            s"Invalid message received when expecting a completion message after activity start trigger. Message was $x"
          )
      }

      // grid agent stat should be simulate grid afterwards
      centerGridAgent.stateName shouldBe SimulateGrid

    }

    s"start the simulation when a $StartGridSimulationTrigger is send" in {

      val startGridSimulationTriggerId = 2
      val firstSweepNo = 0
      val slackNodeUuid =
        UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e")
      val inferiorGridNodeUuids = Vector(
        UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75"),
        UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0"),
        UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c"),
        UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae")
      )

      // send the start grid simulation trigger
      centerGridAgent ! TriggerWithIdMessage(
        StartGridSimulationTrigger(3600),
        startGridSimulationTriggerId,
        centerGridAgent.asLocal
      )
      centerGridAgent.stateName shouldBe SimulateGrid

      // we expect 4 requests for grid power values as we have 4 inferior grids
      val firstGridPowerRequests = receiveWhile() {
        case msg @ RequestGridPowerMessage(_, _) => msg -> lastSender
      }.toMap

      firstGridPowerRequests.size shouldBe 4
      firstGridPowerRequests.keys should contain allOf (
        RequestGridPowerMessage(
          firstSweepNo,
          UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75")
        ),
        RequestGridPowerMessage(
          firstSweepNo,
          UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0")
        ),
        RequestGridPowerMessage(
          firstSweepNo,
          UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c")
        ),
        RequestGridPowerMessage(
          firstSweepNo,
          UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae")
        )
      )

      // we expect 1 request for slack voltage values
      // (slack values are requested by our agent under test from the superior grid)
      val firstSlackVoltageRequest = expectMsgPF() {
        case request @ RequestSlackVoltageMessage(sweepNo, nodeId) =>
          sweepNo shouldBe firstSweepNo
          nodeId shouldBe UUID.fromString(
            "9fe5fa33-6d3b-4153-a829-a16f4347bc4e"
          )
          (request, lastSender)
        case x =>
          fail(
            s"Invalid message received when expecting slack voltage request message. Message was $x"
          )
      }

      // normally the inferior grid agents ask for the slack voltage as well to do their power flow calculations
      // we simulate this behaviour now by doing the same for our 4 inferior grid agents
      inferiorGridNodeUuids.foreach { nodeUuid =>
        centerGridAgent ! RequestSlackVoltageMessage(firstSweepNo, nodeUuid)
      }

      // as we are in the first sweep, all provided slack voltages should be equal
      // to 1 p.u. (in physical values, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)
      val providedSlackVoltages = receiveWhile() {
        case provideSlackVoltageMessage: ProvideSlackVoltageMessage =>
          provideSlackVoltageMessage
        case x =>
          fail(
            s"Invalid message received when expecting slack voltage provision message. Message was $x"
          )
      }

      providedSlackVoltages.size shouldBe 4
      providedSlackVoltages should contain allOf (
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75"),
          Quantities.getQuantity(110, KILOVOLT),
          Quantities.getQuantity(0, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0"),
          Quantities.getQuantity(110, KILOVOLT),
          Quantities.getQuantity(0, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c"),
          Quantities.getQuantity(110, KILOVOLT),
          Quantities.getQuantity(0, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae"),
          Quantities.getQuantity(110, KILOVOLT),
          Quantities.getQuantity(0, KILOVOLT)
        )
      )

      centerGridAgent.stateName shouldBe SimulateGrid

      // we now answer the request of our centerGridAgent
      // with 4 fake grid power messages and 1 fake slack voltage message
      firstGridPowerRequests.foreach { requestSender =>
        val askSender = requestSender._2
        val requestNodeUuid = requestSender._1.nodeUuid
        askSender ! ProvideGridPowerMessage(
          requestNodeUuid,
          Quantities.getQuantity(0, KILOWATT),
          Quantities.getQuantity(0, KILOVAR)
        )
      }

      val transitionTester =
        StateTransitionTester(centerGridAgent, ignoreSameState = true)
      transitionTester.startListening(SimulateGrid)

      val slackAskSender = firstSlackVoltageRequest._2
      val slackRequestNodeUuid = firstSlackVoltageRequest._1.nodeUuid
      val slackRequestSweepNo = firstSlackVoltageRequest._1.currentSweepNo
      slackAskSender ! ProvideSlackVoltageMessage(
        slackRequestSweepNo,
        slackRequestNodeUuid,
        Quantities.getQuantity(380, KILOVOLT),
        Quantities.getQuantity(0, KILOVOLT)
      )

      // we expect to end up in SimulateGrid but we have to pass HandlePowerFlowCalculations beforehand
      transitionTester.expectStateChange(
        HandlePowerFlowCalculations,
        max = 1.minute
      )
      transitionTester.expectStateChange(SimulateGrid, max = 1.minute)

      transitionTester.stopListening()

      // our test agent should now be ready to provide the grid power values, hence we ask for them and expect a
      // corresponding response
      centerGridAgent ! RequestGridPowerMessage(firstSweepNo, slackNodeUuid)

      expectMsgPF(Duration(15, TimeUnit.SECONDS)) {
        case ProvideGridPowerMessage(nodeUuid, p, q) =>
          nodeUuid shouldBe slackNodeUuid
          p should equalWithTolerance(
            Quantities.getQuantity(0.080423711881702500000, MEGAVOLTAMPERE),
            floatPrecision
          )
          q should equalWithTolerance(
            Quantities.getQuantity(-1.45357503915666260000, MEGAVOLTAMPERE),
            floatPrecision
          )
        case x =>
          fail(
            s"Invalid message received when expecting grid power values message. Message was $x"
          )
      }

      // we start a second sweep by asking for next sweep values which should trigger the whole procedure again
      val secondSweepNo = firstSweepNo + 1
      centerGridAgent ! RequestGridPowerMessage(secondSweepNo, slackNodeUuid)

      // the agent now should ask for updated slack voltages from the superior grid
      val secondSlackVoltageRequest = expectMsgPF() {
        case request @ RequestSlackVoltageMessage(sweepNo, nodeId) =>
          sweepNo shouldBe secondSweepNo
          nodeId shouldBe UUID.fromString(
            "9fe5fa33-6d3b-4153-a829-a16f4347bc4e"
          )
          (request, lastSender)
        case x =>
          fail(
            s"Invalid message received when expecting slack voltage request message. Message was $x"
          )
      }

      // the agent should then go to HandlePowerFlowCalculations and wait for the response on updated slack value
      centerGridAgent.stateName shouldBe HandlePowerFlowCalculations

      // the superior grid would answer with updated slack voltage values
      val secondSlackAskSender = secondSlackVoltageRequest._2
      secondSlackAskSender ! ProvideSlackVoltageMessage(
        secondSweepNo,
        slackRequestNodeUuid,
        Quantities.getQuantity(380, KILOVOLT),
        Quantities.getQuantity(0, KILOVOLT)
      )

      // after the intermediate power flow calculation
      // we expect 4 requests for grid power values as we have 4 inferior grids
      val secondGridPowerRequests = receiveWhile() {
        case msg @ RequestGridPowerMessage(_, _) => msg -> lastSender
      }.toMap

      secondGridPowerRequests.size shouldBe 4
      secondGridPowerRequests.keys should contain allOf (
        RequestGridPowerMessage(
          secondSweepNo,
          UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75")
        ),
        RequestGridPowerMessage(
          secondSweepNo,
          UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0")
        ),
        RequestGridPowerMessage(
          secondSweepNo,
          UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c")
        ),
        RequestGridPowerMessage(
          secondSweepNo,
          UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae")
        )
      )

      // the agent should then go back to SimulateGrid and wait for the powers of the inferior grid
      awaitAssert(centerGridAgent.stateName shouldBe SimulateGrid)

      // normally the inferior grid agents ask for the slack voltage as well to do their power flow calculations
      // we simulate this behaviour now by doing the same for our 4 inferior grid agents
      inferiorGridNodeUuids.foreach { nodeUuid =>
        centerGridAgent ! RequestSlackVoltageMessage(firstSweepNo, nodeUuid)
      }

      // as we are in the second sweep, all provided slack voltages should be unequal
      // to 1 p.u. (in physical values, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)
      val secondProvidedSlackVoltages = receiveWhile() {
        case provideSlackVoltageMessage: ProvideSlackVoltageMessage =>
          provideSlackVoltageMessage
        case x =>
          fail(
            s"Invalid message received when expecting slack voltage provision message. Message was $x"
          )
      }

      secondProvidedSlackVoltages.size shouldBe 4
      val secondExpectedResults = List(
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75"),
          Quantities.getQuantity(110.1277081582144170, KILOVOLT),
          Quantities.getQuantity(-0.011124597905979507, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0"),
          Quantities.getQuantity(110.1422124824355620, KILOVOLT),
          Quantities.getQuantity(-0.014094294956794604, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c"),
          Quantities.getQuantity(110.1196117051188620, KILOVOLT),
          Quantities.getQuantity(-0.009318349620959118, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae"),
          Quantities.getQuantity(110.147346134387320, KILOVOLT),
          Quantities.getQuantity(-0.015819259689252657, KILOVOLT)
        )
      )

      secondProvidedSlackVoltages.foreach(providedSlackVoltage => {
        val expectedResult = secondExpectedResults
          .find(expectedResult =>
            expectedResult.nodeUuid == providedSlackVoltage.nodeUuid
          )
          .getOrElse(
            fail(
              s"Unable to find expected nodeUuid ${providedSlackVoltage.nodeUuid}"
            )
          )
        expectedResult.currentSweepNo shouldBe providedSlackVoltage.currentSweepNo

        if (
          !(expectedResult.e.getValue
            .doubleValue() - providedSlackVoltage.e.getValue
            .doubleValue() < 1e-12)
        )
          fail(
            s"Real part of node ${expectedResult.nodeUuid} is ${providedSlackVoltage.e} but the expected result is ${expectedResult.e}"
          )

        if (
          !(expectedResult.f.getValue
            .doubleValue() - providedSlackVoltage.e.getValue
            .doubleValue() < 1e-12)
        )
          fail(
            s"Real part of node ${expectedResult.nodeUuid} is ${providedSlackVoltage.f} but the expected result is ${expectedResult.f}"
          )

      })

      transitionTester.startListening(SimulateGrid)

      // we now answer the request of our centerGridAgent
      // with 4 fake grid power messages
      secondGridPowerRequests.foreach { requestSender =>
        val askSender = requestSender._2
        val requestNodeUuid = requestSender._1.nodeUuid
        askSender ! ProvideGridPowerMessage(
          requestNodeUuid,
          Quantities.getQuantity(0, KILOWATT),
          Quantities.getQuantity(0, KILOVAR)
        )
      }

      // we expect to end up in SimulateGrid but we have to pass HandlePowerFlowCalculations beforehand
      transitionTester.expectStateChange(
        HandlePowerFlowCalculations,
        max = 1.minute
      )
      transitionTester.expectStateChange(SimulateGrid, max = 1.minute)

      transitionTester.stopListening()

      // as the akka testkit does the unstash handling incorrectly, we need to send a second request for grid power messages
      centerGridAgent ! RequestGridPowerMessage(secondSweepNo, slackNodeUuid)

      expectMsgPF() {
        case ProvideGridPowerMessage(nodeUuid, p, q) =>
          nodeUuid shouldBe slackNodeUuid
          p should equalWithTolerance(
            Quantities.getQuantity(0.080423711881452700000, MEGAVOLTAMPERE),
            floatPrecision
          )
          q should equalWithTolerance(
            Quantities.getQuantity(-1.45357503915621860000, MEGAVOLTAMPERE),
            floatPrecision
          )
        case x =>
          fail(
            s"Invalid message received when expecting grid power message. Message was $x"
          )
      }

    }
  }

  "A GridAgent actor in center position with async test" should {

    val centerGridAgent =
      system.actorOf(
        GridAgent.props(
          environmentRefs,
          simonaConfig,
          listener = Iterable.empty[SimonaActorRef]
        )
      )

    s"initialize itself when it receives a $InitializeGridAgentTrigger with corresponding data" in {
      val triggerId = 0

      // this subnet has 1 superior grid (HöS) and 4 inferior grids (MS)
      val subGridGateToActorRef =
        hvSubGridGates.map(gate => gate -> self.asLocal).toMap

      val gridAgentInitData =
        GridAgentInitData(
          hvGridContainer,
          subGridGateToActorRef,
          RefSystem("2000 MVA", "110 kV")
        )

      // send init data to agent and expect a CompletionMessage
      implicit val timeout: Timeout = Timeout(1, TimeUnit.SECONDS)
      val expectedCompletionMessage =
        Await.result(
          centerGridAgent ? TriggerWithIdMessage(
            InitializeGridAgentTrigger(gridAgentInitData),
            triggerId,
            centerGridAgent.asLocal
          ),
          timeout.duration
        )

      expectedCompletionMessage shouldBe CompletionMessage(
        0,
        centerGridAgent.asLocal,
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(3600),
              centerGridAgent.asLocal
            )
          )
        )
      )

    }

    s"go to $SimulateGrid when it receives an activity start trigger" in {

      val activityStartTriggerId = 1

      centerGridAgent ! TriggerWithIdMessage(
        ActivityStartTrigger(3600),
        activityStartTriggerId,
        centerGridAgent.asLocal
      )

      expectMsgPF() {
        case CompletionMessage(
              triggerId,
              actor,
              Some(Vector(ScheduleTriggerMessage(triggerToBeScheduled, _)))
            ) =>
          triggerId shouldBe 1
          actor shouldBe centerGridAgent.asLocal
          triggerToBeScheduled shouldBe StartGridSimulationTrigger(3600)
        case x =>
          fail(
            s"Invalid message received when expecting a completion message after activity start trigger. Message was $x"
          )
      }
    }

    s"start the simulation when a $StartGridSimulationTrigger is send" in {

      val startGridSimulationTriggerId = 2
      val firstSweepNo = 0
      val slackNodeUuid =
        UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e")
      val inferiorGridNodeUuids = Vector(
        UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75"),
        UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0"),
        UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c"),
        UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae")
      )

      // send the start grid simulation trigger
      centerGridAgent ! TriggerWithIdMessage(
        StartGridSimulationTrigger(3600),
        startGridSimulationTriggerId,
        centerGridAgent.asLocal
      )

      // we expect 4 requests for grid power values as we have 4 inferior grids
      val firstGridPowerRequests = receiveWhile() {
        case msg @ RequestGridPowerMessage(_, _) => msg -> lastSender
      }.toMap

      firstGridPowerRequests.size shouldBe 4
      firstGridPowerRequests.keys should contain allOf (
        RequestGridPowerMessage(
          firstSweepNo,
          UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75")
        ),
        RequestGridPowerMessage(
          firstSweepNo,
          UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0")
        ),
        RequestGridPowerMessage(
          firstSweepNo,
          UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c")
        ),
        RequestGridPowerMessage(
          firstSweepNo,
          UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae")
        )
      )

      // we expect 1 request for slack voltage values
      // (slack values are requested by our agent under test from the superior grid)
      val firstSlackVoltageRequest = expectMsgPF() {
        case request @ RequestSlackVoltageMessage(sweepNo, nodeId) =>
          sweepNo shouldBe firstSweepNo
          nodeId shouldBe UUID.fromString(
            "9fe5fa33-6d3b-4153-a829-a16f4347bc4e"
          )
          (request, lastSender)
        case x =>
          fail(
            s"Invalid message received when expecting slack voltage request message. Message was $x"
          )
      }

      // normally the inferior grid agents ask for the slack voltage as well to do their power flow calculations
      // we simulate this behaviour now by doing the same for our 4 inferior grid agents
      inferiorGridNodeUuids.foreach { nodeUuid =>
        centerGridAgent ! RequestSlackVoltageMessage(firstSweepNo, nodeUuid)
      }

      // as we are in the first sweep, all provided slack voltages should be equal
      // to 1 p.u. (in physical values, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)
      val providedSlackVoltages = receiveWhile() {
        case provideSlackVoltageMessage: ProvideSlackVoltageMessage =>
          provideSlackVoltageMessage
        case x =>
          fail(
            s"Invalid message received when expecting slack voltage provision message. Message was $x"
          )
      }

      providedSlackVoltages.size shouldBe 4
      providedSlackVoltages should contain allOf (
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75"),
          Quantities.getQuantity(110, KILOVOLT),
          Quantities.getQuantity(0, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0"),
          Quantities.getQuantity(110, KILOVOLT),
          Quantities.getQuantity(0, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c"),
          Quantities.getQuantity(110, KILOVOLT),
          Quantities.getQuantity(0, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae"),
          Quantities.getQuantity(110, KILOVOLT),
          Quantities.getQuantity(0, KILOVOLT)
        )
      )
      // we now answer the request of our centerGridAgent
      // with 4 fake grid power messages and 1 fake slack voltage message
      firstGridPowerRequests.foreach { requestSender =>
        val askSender = requestSender._2
        val requestNodeUuid = requestSender._1.nodeUuid
        askSender ! ProvideGridPowerMessage(
          requestNodeUuid,
          Quantities.getQuantity(0, KILOWATT),
          Quantities.getQuantity(0, KILOVAR)
        )
      }

      val slackAskSender = firstSlackVoltageRequest._2
      val slackRequestNodeUuid = firstSlackVoltageRequest._1.nodeUuid
      val slackRequestSweepNo = firstSlackVoltageRequest._1.currentSweepNo
      slackAskSender ! ProvideSlackVoltageMessage(
        slackRequestSweepNo,
        slackRequestNodeUuid,
        Quantities.getQuantity(380, KILOVOLT),
        Quantities.getQuantity(0, KILOVOLT)
      )

      // our test agent should now be ready to provide the grid power values, hence we ask for them and expect a
      // corresponding response
      centerGridAgent ! RequestGridPowerMessage(firstSweepNo, slackNodeUuid)

      expectMsgPF() {
        case ProvideGridPowerMessage(nodeUuid, p, q) =>
          nodeUuid shouldBe slackNodeUuid
          p should equalWithTolerance(
            Quantities.getQuantity(0.080423711881452700000, MEGAVOLTAMPERE),
            floatPrecision
          )
          q should equalWithTolerance(
            Quantities.getQuantity(-1.45357503915666260000, MEGAVOLTAMPERE),
            floatPrecision
          )
        case x =>
          fail(
            s"Invalid message received when expecting grid power values message. Message was $x"
          )
      }

      // we start a second sweep by asking for next sweep values which should trigger the whole procedure again
      val secondSweepNo = firstSweepNo + 1
      centerGridAgent ! RequestGridPowerMessage(secondSweepNo, slackNodeUuid)

      // the agent now should ask for updated slack voltages from the superior grid
      val secondSlackVoltageRequest = expectMsgPF() {
        case request @ RequestSlackVoltageMessage(sweepNo, nodeId) =>
          sweepNo shouldBe secondSweepNo
          nodeId shouldBe UUID.fromString(
            "9fe5fa33-6d3b-4153-a829-a16f4347bc4e"
          )
          (request, lastSender)
        case x =>
          fail(
            s"Invalid message received when expecting slack voltage request message. Message was $x"
          )
      }

      // the superior grid would answer with updated slack voltage values
      val secondSlackAskSender = secondSlackVoltageRequest._2
      secondSlackAskSender ! ProvideSlackVoltageMessage(
        secondSweepNo,
        slackRequestNodeUuid,
        Quantities.getQuantity(380, KILOVOLT),
        Quantities.getQuantity(0, KILOVOLT)
      )

      // after the intermediate power flow calculation
      // we expect 4 requests for grid power values as we have 4 inferior grids
      val secondGridPowerRequests = receiveWhile() {
        case msg @ RequestGridPowerMessage(_, _) => msg -> lastSender
      }.toMap

      secondGridPowerRequests.size shouldBe 4
      secondGridPowerRequests.keys should contain allOf (
        RequestGridPowerMessage(
          secondSweepNo,
          UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75")
        ),
        RequestGridPowerMessage(
          secondSweepNo,
          UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0")
        ),
        RequestGridPowerMessage(
          secondSweepNo,
          UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c")
        ),
        RequestGridPowerMessage(
          secondSweepNo,
          UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae")
        )
      )

      // the agent should then go back to SimulateGrid and wait for the powers of the inferior grid
      // awaitAssert(centerGridAgent.stateName shouldBe SimulateGrid)

      // normally the inferior grid agents ask for the slack voltage as well to do their power flow calculations
      // we simulate this behaviour now by doing the same for our 4 inferior grid agents
      inferiorGridNodeUuids.foreach { nodeUuid =>
        centerGridAgent ! RequestSlackVoltageMessage(firstSweepNo, nodeUuid)
      }

      // as we are in the second sweep, all provided slack voltages should be unequal
      // to 1 p.u. (in physical values, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)
      val secondProvidedSlackVoltages = receiveWhile() {
        case provideSlackVoltageMessage: ProvideSlackVoltageMessage =>
          provideSlackVoltageMessage
        case x =>
          fail(
            s"Invalid message received when expecting slack voltage provision message. Message was $x"
          )
      }

      secondProvidedSlackVoltages.size shouldBe 4
      val secondExpectedResults = List(
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75"),
          Quantities.getQuantity(110.1277081582144170, KILOVOLT),
          Quantities.getQuantity(-0.011124597905979507, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0"),
          Quantities.getQuantity(110.1422124824355620, KILOVOLT),
          Quantities.getQuantity(-0.014094294956794604, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c"),
          Quantities.getQuantity(110.1196117051188620, KILOVOLT),
          Quantities.getQuantity(-0.009318349620959118, KILOVOLT)
        ),
        ProvideSlackVoltageMessage(
          firstSweepNo,
          UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae"),
          Quantities.getQuantity(110.147346134387320, KILOVOLT),
          Quantities.getQuantity(-0.015819259689252657, KILOVOLT)
        )
      )

      secondProvidedSlackVoltages.foreach(providedSlackVoltage => {
        val expectedResult = secondExpectedResults
          .find(expectedResult =>
            expectedResult.nodeUuid == providedSlackVoltage.nodeUuid
          )
          .getOrElse(
            fail(
              s"Unable to find expected nodeUuid ${providedSlackVoltage.nodeUuid}"
            )
          )
        expectedResult.currentSweepNo shouldBe providedSlackVoltage.currentSweepNo
        if (
          !(expectedResult.e.getValue
            .doubleValue() - providedSlackVoltage.e.getValue
            .doubleValue() < 1e-12)
        )
          fail(
            s"Real part of node ${expectedResult.nodeUuid} is ${providedSlackVoltage.e} but the expected result is ${expectedResult.e}"
          )

        if (
          !(expectedResult.f.getValue
            .doubleValue() - providedSlackVoltage.e.getValue
            .doubleValue() < 1e-12)
        )
          fail(
            s"Real part of node ${expectedResult.nodeUuid} is ${providedSlackVoltage.f} but the expected result is ${expectedResult.f}"
          )
      })

      // we now answer the request of our centerGridAgent
      // with 4 fake grid power messages
      secondGridPowerRequests.foreach { requestSender =>
        val askSender = requestSender._2
        val requestNodeUuid = requestSender._1.nodeUuid
        askSender ! ProvideGridPowerMessage(
          requestNodeUuid,
          Quantities.getQuantity(0, KILOWATT),
          Quantities.getQuantity(0, KILOVAR)
        )
      }

      // we expect that the GridAgent unstashes the messages and return a value for our power request
      expectMsgPF() {
        case ProvideGridPowerMessage(nodeUuid, p, q) =>
          nodeUuid shouldBe slackNodeUuid
          p should equalWithTolerance(
            Quantities.getQuantity(0.080423711881702500000, MEGAVOLTAMPERE),
            floatPrecision
          )
          q should equalWithTolerance(
            Quantities.getQuantity(-1.45357503915621860000, MEGAVOLTAMPERE),
            floatPrecision
          )
        case x =>
          fail(
            s"Invalid message received when expecting grid power message. Message was $x"
          )
      }

    }
  }
}
