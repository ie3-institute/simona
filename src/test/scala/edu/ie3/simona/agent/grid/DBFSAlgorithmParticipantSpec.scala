/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.DBFSAlgorithmParticipantSpec.SuperiorGA
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
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage.ExchangeVoltage
import edu.ie3.simona.ontology.messages.VoltageMessage.{
  ProvideSlackVoltageMessage,
  RequestSlackVoltageMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.trigger.Trigger._
import edu.ie3.simona.test.common.model.grid.DbfsTestGridWithParticipants
import edu.ie3.simona.test.common.{
  ConfigTestData,
  TestKitWithShutdown,
  UnitSpec
}
import edu.ie3.util.quantities.PowerSystemUnits.{KILOVOLT, MEGAVAR, MEGAWATT}
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

class DBFSAlgorithmParticipantSpec
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
    with DbfsTestGridWithParticipants {

  private val scheduler = TestProbe("scheduler")
  private val primaryService = TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    primaryServiceProxy = primaryService.ref,
    weather = weatherService.ref,
    evDataService = None
  )

  protected val resultListener: TestProbe = TestProbe("resultListener")
  protected val loadListener: TestProbe = TestProbe("loadListener")

  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid)
  )

  protected var participantAgent: ActorRef = testActor

  "Test participant" should {

    val gridAgentWithParticipants = system.actorOf(
      GridAgent.props(
        environmentRefs,
        simonaConfig,
        Iterable(resultListener.ref)
      )
    )

    s"initialize itself when it receives a $InitializeGridAgentTrigger with corresponding data" in {
      val triggerId = 0
      val loadAgentTriggerId = 1

      // this subnet has 1 superior grid (HöS) and 3 inferior grids (MS). Map the gates to test probes accordingly
      val subGridGateToActorRef: Map[SubGridGate, ActorRef] =
        hvSubGridGates.map {
          case gate if gate.getInferiorSubGrid == hvGridContainer.getSubnet =>
            gate -> superiorGridAgent.ref
        }.toMap

      val gridAgentInitData =
        GridAgentInitData(
          hvGridContainer,
          subGridGateToActorRef,
          RefSystem("2000 MVA", "110 kV")
        )

      // send init data to agent and expect a CompletionMessage
      scheduler.send(
        gridAgentWithParticipants,
        TriggerWithIdMessage(
          InitializeGridAgentTrigger(gridAgentInitData),
          triggerId,
          gridAgentWithParticipants
        )
      )

      val initializeTrigger: InitializeParticipantAgentTrigger[_, _] =
        scheduler.expectMsgPF() {
          case ScheduleTriggerMessage(
                initializeTrigger: InitializeParticipantAgentTrigger[_, _],
                loadAgent
              ) =>
            participantAgent = loadAgent

            initializeTrigger
        }

      scheduler.expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(
                Vector(
                  ScheduleTriggerMessage(triggerToBeScheduled, gridAgentActor)
                )
              )
            ) =>
          triggerId shouldBe 0
          triggerToBeScheduled shouldBe ActivityStartTrigger(3600)
          gridAgentActor shouldBe gridAgentWithParticipants
        case x =>
          fail(
            s"Invalid message received when expecting a completion message after an init trigger. Message was $x"
          )
      }

      scheduler.send(
        participantAgent,
        TriggerWithIdMessage(
          initializeTrigger,
          loadAgentTriggerId,
          participantAgent
        )
      )

      primaryService.expectMsg(
        PrimaryServiceRegistrationMessage(load1.getUuid)
      )

      primaryService.send(participantAgent, RegistrationFailedMessage)

      scheduler.expectMsgPF(10.seconds) {
        case CompletionMessage(
              loadAgentTriggerId,
              Some(triggerMessage :: Nil)
            ) =>
          loadAgentTriggerId shouldBe 1
          scheduler.send(
            gridAgentWithParticipants,
            triggerMessage.trigger
          )

        case x =>
          fail(
            s"Invalid message received when expecting a completion message after an init trigger. Message was $x"
          )
      }

    }

    s"go to $SimulateGrid when it receives an activity start trigger" in {

      val activityStartTriggerId = 1

      // send init data to agent
      scheduler.send(
        gridAgentWithParticipants,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600),
          activityStartTriggerId,
          gridAgentWithParticipants
        )
      )

      // we expect a completion message
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

      scheduler.send(
        participantAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600),
          activityStartTriggerId,
          gridAgentWithParticipants
        )
      )

      scheduler.expectMsgPF() {
        case CompletionMessage(
              triggerId,
              None
            ) =>
          triggerId shouldBe 1
        case x =>
          fail(
            s"Invalid message received when expecting a completion message after activity start trigger. Message was $x"
          )
      }

    }

    "check the request asset power message indirectly" in {

      val startGridSimulationTriggerId = 3
      val firstSweepNo = 0

      val voltageEhv =
        Array(
          (
            Quantities.getQuantity(380, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          ),
          (
            Quantities.getQuantity(380, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          ),
          (
            Quantities.getQuantity(380, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          )
        )

      val powerEhv =
        Array(
          (
            Quantities.getQuantity(0.08021413413301926, MEGAWATT),
            Quantities.getQuantity(0.0001845926447252566, MEGAVAR)
          ),
          (
            Quantities.getQuantity(0.08021413413301926, MEGAWATT),
            Quantities.getQuantity(0.0001845926447252566, MEGAVAR)
          ),
          (
            Quantities.getQuantity(0.08021413413301926, MEGAWATT),
            Quantities.getQuantity(0.0001845926447252566, MEGAVAR)
          )
        )

      // send the start grid simulation trigger
      // the gird agent should send a RequestAssetPowerMessage to the load agent
      scheduler.send(
        gridAgentWithParticipants,
        TriggerWithIdMessage(
          StartGridSimulationTrigger(3600),
          startGridSimulationTriggerId,
          gridAgentWithParticipants
        )
      )

      // we expect a request for voltage values of our slack node
      // (voltages are requested by our agent under test from the superior grid)
      val firstSlackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(firstSweepNo)

      // we now answer the request of our gridAgentsWithParticipants
      // with a fake slack voltage message
      superiorGridAgent.gaProbe.send(
        firstSlackVoltageRequestSender,
        ProvideSlackVoltageMessage(
          firstSweepNo,
          Seq(
            ExchangeVoltage(
              supNodeA.getUuid,
              voltageEhv(firstSweepNo)._1,
              voltageEhv(firstSweepNo)._2
            )
          )
        )
      )

      // power flow calculation should run now. After it's done,
      // our test agent should now be ready to provide the grid power values,
      // hence we ask for them and expect a corresponding response
      superiorGridAgent.requestGridPower(
        gridAgentWithParticipants,
        firstSweepNo
      )

      // the gridAgentWithParticipants have received a AssetPowerChangedMessage
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            powerEhv(firstSweepNo)._1,
            powerEhv(firstSweepNo)._2
          )
        )
      )

      // we start a second sweep by asking for next sweep values which should trigger the whole procedure again
      val secondSweepNo = firstSweepNo + 1

      superiorGridAgent.requestGridPower(
        gridAgentWithParticipants,
        secondSweepNo
      )

      // the agent now should ask for updated slack voltages from the superior grid
      val secondSlackAskSender =
        superiorGridAgent.expectSlackVoltageRequest(secondSweepNo)

      // the superior grid would answer with updated slack voltage values
      superiorGridAgent.gaProbe.send(
        secondSlackAskSender,
        ProvideSlackVoltageMessage(
          secondSweepNo,
          Seq(
            ExchangeVoltage( // this one should currently be ignored anyways
              supNodeA.getUuid,
              voltageEhv(secondSweepNo)._1,
              voltageEhv(secondSweepNo)._2
            )
          )
        )
      )

      // here the gridAgentWithParticipants have received a AssetPowerUnchangedMessage
      // we expect that the GridAgent unstashes the messages and return a value for our power request
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            powerEhv(secondSweepNo)._1,
            powerEhv(secondSweepNo)._2
          )
        )
      )

      // we start a third sweep by asking for next sweep values which should trigger the whole procedure again
      val thirdSweepNo = secondSweepNo + 1

      superiorGridAgent.requestGridPower(
        gridAgentWithParticipants,
        thirdSweepNo
      )

      // the agent now should ask for updated slack voltages from the superior grid
      val thirdSlackAskSender =
        superiorGridAgent.expectSlackVoltageRequest(thirdSweepNo)

      // the superior grid would answer with updated slack voltage values
      superiorGridAgent.gaProbe.send(
        thirdSlackAskSender,
        ProvideSlackVoltageMessage(
          thirdSweepNo,
          Seq(
            ExchangeVoltage( // this one should currently be ignored anyways
              supNodeA.getUuid,
              voltageEhv(thirdSweepNo)._1,
              voltageEhv(thirdSweepNo)._2
            )
          )
        )
      )

      // here the gridAgentWithParticipants have received a AssetPowerUnchangedMessage
      // we expect that the GridAgent unstashes the messages and return a value for our power request
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            powerEhv(thirdSweepNo)._1,
            powerEhv(thirdSweepNo)._2
          )
        )
      )

      // normally the superior grid agent would check weather the power flow calculation converges and would
      // send a CompletionMessage to the scheduler and a FinishGridSimulationTrigger to the inferior grid agent
      // after the convergence
      // (here we do it by hand)
      superiorGridAgent.gaProbe.send(
        scheduler.ref,
        CompletionMessage(
          startGridSimulationTriggerId + thirdSweepNo,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(7200),
                superiorGridAgent.gaProbe.ref
              )
            )
          )
        )
      )
      superiorGridAgent.gaProbe.send(
        gridAgentWithParticipants,
        FinishGridSimulationTrigger(3600)
      )

      scheduler.expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(Seq(message: ScheduleTriggerMessage))
            ) =>
          triggerId shouldBe 5
          message shouldBe ScheduleTriggerMessage(
            ActivityStartTrigger(7200),
            superiorGridAgent.gaProbe.ref
          )

        case x =>
          fail(
            s"Invalid message received when expecting a completion message for simulate grid after cleanup! Message was $x"
          )
      }

    }
  }
}

object DBFSAlgorithmParticipantSpec extends UnitSpec {
  private val floatPrecision: Double = 0.00000000001

  sealed trait GAActorAndModel {
    val gaProbe: TestProbe
    val nodeUuids: Seq[UUID]
    def ref: ActorRef = gaProbe.ref
  }

  final case class SuperiorGA(
      override val gaProbe: TestProbe,
      override val nodeUuids: Seq[UUID]
  ) extends GAActorAndModel {

    def expectSlackVoltageRequest(expectedSweepNo: Int): ActorRef = {
      inside(
        gaProbe
          .expectMsgType[RequestSlackVoltageMessage](180.seconds)
      ) {
        case RequestSlackVoltageMessage(msgSweepNo: Int, msgUuids: Seq[UUID]) =>
          msgSweepNo shouldBe expectedSweepNo
          msgUuids should have size nodeUuids.size
          msgUuids should contain allElementsOf nodeUuids
      }

      gaProbe.lastSender
    }

    def expectGridPowerProvision(
        expectedExchangedPowers: Seq[ExchangePower]
    ): Unit = {
      inside(gaProbe.expectMsgType[ProvideGridPowerMessage](180.seconds)) {
        case ProvideGridPowerMessage(exchangedPower) =>
          exchangedPower should have size expectedExchangedPowers.size

          expectedExchangedPowers.foreach { expectedPower =>
            exchangedPower.find(_.nodeUuid == expectedPower.nodeUuid) match {
              case Some(ExchangePower(_, actualP, actualQ)) =>
                actualP should equalWithTolerance(
                  expectedPower.p,
                  floatPrecision
                )
                actualQ should equalWithTolerance(
                  expectedPower.q,
                  floatPrecision
                )
              case None =>
                fail(
                  s"Expected ExchangePower with node UUID ${expectedPower.nodeUuid} " +
                    s"was not included in ProvideGridPowerMessage."
                )
            }
          }

      }
    }

    def requestGridPower(receiver: ActorRef, sweepNo: Int): Unit = {
      gaProbe.send(
        receiver,
        RequestGridPowerMessage(
          sweepNo,
          nodeUuids
        )
      )
    }
  }
}
