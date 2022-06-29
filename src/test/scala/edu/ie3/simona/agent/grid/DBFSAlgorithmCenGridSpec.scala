/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestProbe}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.state.GridAgentState.SimulateGrid
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.ontology.messages.PowerMessage.ProvideGridPowerMessage.ExchangePower
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
  TestKitWithShutdown,
  UnitSpec
}
import edu.ie3.util.quantities.PowerSystemUnits._
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.concurrent.Await

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

  private val scheduler = TestProbe("scheduler")
  private val primaryService = TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val superiorGridAgent = TestProbe("superiorGridAgent_1000")
  private val inferiorAndNodes11 = (
    TestProbe("inferiorGridAgent_11"),
    Vector(UUID.fromString("78c5d473-e01b-44c4-afd2-e4ff3c4a5d7c"))
  )
  private val inferiorAndNodes12 = (
    TestProbe("inferiorGridAgent_12"),
    Vector(UUID.fromString("e364ef00-e6ca-46b1-ba2b-bb73c0c6fee0"))
  )
  private val inferiorAndNodes13 = (
    TestProbe("inferiorGridAgent_13"),
    Vector(
      UUID.fromString("d44ba8ed-81db-4a22-a40d-f7c0d0808a75"),
      UUID.fromString("47ef9983-8fcf-4713-be90-093fc27864ae")
    )
  )

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    primaryServiceProxy = primaryService.ref,
    weather = weatherService.ref,
    evDataService = None
  )

  "A GridAgent actor in center position with async test" should {

    val centerGridAgent =
      system.actorOf(
        GridAgent.props(
          environmentRefs,
          simonaConfig,
          listener = Iterable.empty[ActorRef]
        )
      )

    s"initialize itself when it receives a $InitializeGridAgentTrigger with corresponding data" in {
      val triggerId = 0

      // this subnet has 1 superior grid (HöS) and 3 inferior grids (MS). Map the gates to test probes accordingly
      val subGridGateToActorRef = hvSubGridGates.map {
        case gate if gate.getInferiorSubGrid == hvGridContainer.getSubnet =>
          gate -> superiorGridAgent.ref
        case gate =>
          val actor = gate.getInferiorSubGrid match {
            case 11 => inferiorAndNodes11._1
            case 12 => inferiorAndNodes12._1
            case 13 => inferiorAndNodes13._1
          }
          gate -> actor.ref
      }.toMap

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
            centerGridAgent
          ),
          timeout.duration
        )

      expectedCompletionMessage shouldBe CompletionMessage(
        0,
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(3600),
              centerGridAgent
            )
          )
        )
      )

    }

    s"go to $SimulateGrid when it receives an activity start trigger" in {

      val activityStartTriggerId = 1

      scheduler.send(
        centerGridAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600),
          activityStartTriggerId,
          centerGridAgent
        )
      )

      scheduler.expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(Vector(ScheduleTriggerMessage(triggerToBeScheduled, _)))
            ) =>
          triggerId shouldBe 1
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

      // send the start grid simulation trigger
      scheduler.send(
        centerGridAgent,
        TriggerWithIdMessage(
          StartGridSimulationTrigger(3600),
          startGridSimulationTriggerId,
          centerGridAgent
        )
      )

      /* We expect one grid power request message per inferior grid */

      inferiorAndNodes11._1
        .expectMsgType[RequestGridPowerMessage]
        .nodeUuids shouldBe inferiorAndNodes11._2
      val firstPowerRequestSender11 = inferiorAndNodes11._1.lastSender

      inferiorAndNodes12._1
        .expectMsgType[RequestGridPowerMessage]
        .nodeUuids shouldBe inferiorAndNodes12._2
      val firstPowerRequestSender12 = inferiorAndNodes12._1.lastSender

      inferiorAndNodes13._1
        .expectMsgType[RequestGridPowerMessage]
        .nodeUuids should contain allElementsOf inferiorAndNodes13._2
      val firstPowerRequestSender13 = inferiorAndNodes13._1.lastSender

      // we expect 1 request for slack voltage values
      // (slack values are requested by our agent under test from the superior grid)
      val firstSlackVoltageRequest = superiorGridAgent.expectMsgPF() {
        case request @ RequestSlackVoltageMessage(sweepNo, nodeId) =>
          sweepNo shouldBe firstSweepNo
          nodeId shouldBe slackNodeUuid
          (request, superiorGridAgent.lastSender)
        case x =>
          fail(
            s"Invalid message received when expecting slack voltage request message. Message was $x"
          )
      }

      // normally the inferior grid agents ask for the slack voltage as well to do their power flow calculations
      // we simulate this behaviour now by doing the same for our three inferior grid agents

      inferiorAndNodes11._2.foreach { nodeUuid =>
        inferiorAndNodes11._1.send(
          centerGridAgent,
          RequestSlackVoltageMessage(firstSweepNo, nodeUuid)
        )
      }

      inferiorAndNodes12._2.foreach { nodeUuid =>
        inferiorAndNodes12._1.send(
          centerGridAgent,
          RequestSlackVoltageMessage(firstSweepNo, nodeUuid)
        )
      }

      inferiorAndNodes13._2.foreach { nodeUuid =>
        inferiorAndNodes13._1.send(
          centerGridAgent,
          RequestSlackVoltageMessage(firstSweepNo, nodeUuid)
        )
      }

      // as we are in the first sweep, all provided slack voltages should be equal
      // to 1 p.u. (in physical values, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)
      inferiorAndNodes11._2.foreach { nodeUuid =>
        inferiorAndNodes11._1.expectMsg(
          ProvideSlackVoltageMessage(
            firstSweepNo,
            nodeUuid,
            Quantities.getQuantity(110, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          )
        )
      }

      inferiorAndNodes12._2.foreach { nodeUuid =>
        inferiorAndNodes12._1.expectMsg(
          ProvideSlackVoltageMessage(
            firstSweepNo,
            nodeUuid,
            Quantities.getQuantity(110, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          )
        )
      }

      inferiorAndNodes13._2.foreach { nodeUuid =>
        inferiorAndNodes13._1.expectMsg(
          ProvideSlackVoltageMessage(
            firstSweepNo,
            nodeUuid,
            Quantities.getQuantity(110, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          )
        )
      }

      // we now answer the request of our centerGridAgent
      // with 3 fake grid power messages and 1 fake slack voltage message

      inferiorAndNodes11._1.send(
        firstPowerRequestSender11,
        ProvideGridPowerMessage(
          inferiorAndNodes11._2.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      inferiorAndNodes12._1.send(
        firstPowerRequestSender12,
        ProvideGridPowerMessage(
          inferiorAndNodes12._2.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      inferiorAndNodes13._1.send(
        firstPowerRequestSender13,
        ProvideGridPowerMessage(
          inferiorAndNodes13._2.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      val slackRequestNodeUuid = firstSlackVoltageRequest match {
        case (voltageRequest, slackAskSender) =>
          val slackRequestNodeUuid = voltageRequest.nodeUuid
          val slackRequestSweepNo = voltageRequest.currentSweepNo
          superiorGridAgent.send(
            slackAskSender,
            ProvideSlackVoltageMessage(
              slackRequestSweepNo,
              slackRequestNodeUuid,
              Quantities.getQuantity(380, KILOVOLT),
              Quantities.getQuantity(0, KILOVOLT)
            )
          )
          slackRequestNodeUuid
      }

      // our test agent should now be ready to provide the grid power values, hence we ask for them and expect a
      // corresponding response

      superiorGridAgent.send(
        centerGridAgent,
        RequestGridPowerMessage(
          firstSweepNo,
          Vector(slackNodeUuid)
        )
      )

      superiorGridAgent.expectMsgPF() {
        case ProvideGridPowerMessage(exchangedPower) =>
          exchangedPower.size shouldBe 1
          exchangedPower.headOption match {
            case Some(ExchangePower(nodeUuid, p, q)) =>
              nodeUuid shouldBe slackNodeUuid
              p should equalWithTolerance(
                Quantities.getQuantity(0.080423711881452700000, MEGAVOLTAMPERE),
                floatPrecision
              )
              q should equalWithTolerance(
                Quantities.getQuantity(-1.45357503915666260000, MEGAVOLTAMPERE),
                floatPrecision
              )
            case None =>
              fail("Did not expect to get nothing.")
          }
        case x =>
          fail(
            s"Invalid message received when expecting grid power values message. Message was $x"
          )
      }

      // we start a second sweep by asking for next sweep values which should trigger the whole procedure again
      val secondSweepNo = firstSweepNo + 1
      superiorGridAgent.send(
        centerGridAgent,
        RequestGridPowerMessage(
          secondSweepNo,
          Vector(slackNodeUuid)
        )
      )

      // the agent now should ask for updated slack voltages from the superior grid
      val secondSlackVoltageRequest = superiorGridAgent.expectMsgPF() {
        case request @ RequestSlackVoltageMessage(sweepNo, nodeId) =>
          sweepNo shouldBe secondSweepNo
          nodeId shouldBe slackNodeUuid
          (request, superiorGridAgent.lastSender)
        case x =>
          fail(
            s"Invalid message received when expecting slack voltage request message. Message was $x"
          )
      }

      // the superior grid would answer with updated slack voltage values
      val secondSlackAskSender = secondSlackVoltageRequest._2
      superiorGridAgent.send(
        secondSlackAskSender,
        ProvideSlackVoltageMessage(
          secondSweepNo,
          slackRequestNodeUuid,
          Quantities.getQuantity(380, KILOVOLT),
          Quantities.getQuantity(0, KILOVOLT)
        )
      )

      // after the intermediate power flow calculation
      // We expect one grid power request message, as all four sub grids are mapped onto one actor reference
      inferiorAndNodes11._1
        .expectMsgType[RequestGridPowerMessage]
        .nodeUuids shouldBe inferiorAndNodes11._2
      val secondPowerRequestSender11 = inferiorAndNodes11._1.lastSender

      inferiorAndNodes12._1
        .expectMsgType[RequestGridPowerMessage]
        .nodeUuids shouldBe inferiorAndNodes12._2
      val secondPowerRequestSender12 = inferiorAndNodes12._1.lastSender

      inferiorAndNodes13._1
        .expectMsgType[RequestGridPowerMessage]
        .nodeUuids should contain allElementsOf inferiorAndNodes13._2
      val secondPowerRequestSender13 = inferiorAndNodes13._1.lastSender

      // normally the inferior grid agents ask for the slack voltage as well to do their power flow calculations
      // we simulate this behaviour now by doing the same for our 4 inferior grid agents

      inferiorAndNodes11._2.foreach { nodeUuid =>
        inferiorAndNodes11._1.send(
          centerGridAgent,
          RequestSlackVoltageMessage(firstSweepNo, nodeUuid)
        )
      }

      inferiorAndNodes12._2.foreach { nodeUuid =>
        inferiorAndNodes12._1.send(
          centerGridAgent,
          RequestSlackVoltageMessage(firstSweepNo, nodeUuid)
        )
      }

      inferiorAndNodes13._2.foreach { nodeUuid =>
        inferiorAndNodes13._1.send(
          centerGridAgent,
          RequestSlackVoltageMessage(firstSweepNo, nodeUuid)
        )
      }

      // as we are in the second sweep, all provided slack voltages should be unequal
      // to 1 p.u. (in physical values, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)

      inferiorAndNodes11._2.foreach { expectedNodeUuid =>
        val received =
          inferiorAndNodes11._1.expectMsgType[ProvideSlackVoltageMessage]

        received.currentSweepNo shouldBe firstSweepNo
        received.nodeUuid shouldBe expectedNodeUuid
        received.e should equalWithTolerance(
          Quantities.getQuantity(110.1196117051188620, KILOVOLT)
        )
        received.f should equalWithTolerance(
          Quantities.getQuantity(-0.009318349620959118, KILOVOLT)
        )
      }

      inferiorAndNodes12._2.foreach { expectedNodeUuid =>
        val received =
          inferiorAndNodes12._1.expectMsgType[ProvideSlackVoltageMessage]

        received.currentSweepNo shouldBe firstSweepNo
        received.nodeUuid shouldBe expectedNodeUuid
        received.e should equalWithTolerance(
          Quantities.getQuantity(110.1422124824355620, KILOVOLT)
        )
        received.f should equalWithTolerance(
          Quantities.getQuantity(-0.014094294956794604, KILOVOLT)
        )
      }

      inferiorAndNodes13._1.expectMsgType[ProvideSlackVoltageMessage] match {
        case received if received.nodeUuid.equals(node3a.getUuid) =>
          received.currentSweepNo shouldBe firstSweepNo
          received.nodeUuid shouldBe node3a.getUuid
          received.e should equalWithTolerance(
            Quantities.getQuantity(110.147346134387320, KILOVOLT)
          )
          received.f should equalWithTolerance(
            Quantities.getQuantity(-0.015819259689252657, KILOVOLT)
          )
        case received if received.nodeUuid.equals(node3b.getUuid) =>
          received.currentSweepNo shouldBe firstSweepNo
          received.nodeUuid shouldBe node3b.getUuid
          received.e should equalWithTolerance(
            Quantities.getQuantity(110.1277081582144170, KILOVOLT)
          )
          received.f should equalWithTolerance(
            Quantities.getQuantity(-0.011124597905979507, KILOVOLT)
          )
        case received =>
          fail(s"Msg with unknown node UUID ${received.nodeUuid} was received")
      }

      // we now answer the request of our centerGridAgent
      // with 1 fake grid power message
      inferiorAndNodes11._1.send(
        secondPowerRequestSender11,
        ProvideGridPowerMessage(
          inferiorAndNodes11._2.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      inferiorAndNodes12._1.send(
        secondPowerRequestSender12,
        ProvideGridPowerMessage(
          inferiorAndNodes12._2.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      inferiorAndNodes13._1.send(
        secondPowerRequestSender13,
        ProvideGridPowerMessage(
          inferiorAndNodes13._2.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      // we expect that the GridAgent unstashes the messages and return a value for our power request
      superiorGridAgent.expectMsgPF() {
        case ProvideGridPowerMessage(exchangedPower) =>
          exchangedPower.size shouldBe 1
          exchangedPower.headOption match {
            case Some(ExchangePower(nodeUuid, p, q)) =>
              nodeUuid shouldBe slackNodeUuid
              p should equalWithTolerance(
                Quantities.getQuantity(0.080423711881702500000, MEGAVOLTAMPERE),
                floatPrecision
              )
              q should equalWithTolerance(
                Quantities.getQuantity(-1.45357503915621860000, MEGAVOLTAMPERE),
                floatPrecision
              )
            case None =>
              fail("I did not expect to get nothing.")
          }

        case x =>
          fail(
            s"Invalid message received when expecting grid power message. Message was $x"
          )
      }

    }
  }
}
