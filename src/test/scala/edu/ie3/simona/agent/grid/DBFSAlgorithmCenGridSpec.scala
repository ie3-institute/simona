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
import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.DBFSAlgorithmCenGridSpec.{
  InferiorGA,
  SuperiorGA
}
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
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

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

  private val scheduler = TestProbe("scheduler")
  private val primaryService = TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid, supNodeB.getUuid)
  )

  private val inferiorGrid11 =
    InferiorGA(TestProbe("inferiorGridAgent_11"), Seq(node1.getUuid))

  private val inferiorGrid12 =
    InferiorGA(TestProbe("inferiorGridAgent_12"), Seq(node2.getUuid))

  private val inferiorGrid13 = InferiorGA(
    TestProbe("inferiorGridAgent_13"),
    Seq(node3a.getUuid, node3b.getUuid)
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
          RefSystem("2000 MVA", "110 kV")
        )

      // send init data to agent and expect a CompletionMessage
      implicit val timeout: Timeout = 3 seconds
      val actualInitReply =
        Await.result(
          centerGridAgent ? TriggerWithIdMessage(
            InitializeGridAgentTrigger(gridAgentInitData),
            triggerId,
            centerGridAgent
          ),
          timeout.duration
        )

      actualInitReply shouldBe CompletionMessage(
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
            Quantities.getQuantity(110, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          )
        )
      )

      inferiorGrid12.expectSlackVoltageProvision(
        firstSweepNo,
        Seq(
          ExchangeVoltage(
            node2.getUuid,
            Quantities.getQuantity(110, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          )
        )
      )

      inferiorGrid13.expectSlackVoltageProvision(
        firstSweepNo,
        Seq(
          ExchangeVoltage(
            node3a.getUuid,
            Quantities.getQuantity(110, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          ),
          ExchangeVoltage(
            node3b.getUuid,
            Quantities.getQuantity(110, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          )
        )
      )

      // we now answer the request of our centerGridAgent
      // with three fake grid power messages and one fake slack voltage message

      inferiorGrid11.gaProbe.send(
        firstPowerRequestSender11,
        ProvideGridPowerMessage(
          inferiorGrid11.nodeUuids.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      inferiorGrid12.gaProbe.send(
        firstPowerRequestSender12,
        ProvideGridPowerMessage(
          inferiorGrid12.nodeUuids.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      inferiorGrid13.gaProbe.send(
        firstPowerRequestSender13,
        ProvideGridPowerMessage(
          inferiorGrid13.nodeUuids.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      superiorGridAgent.gaProbe.send(
        firstSlackVoltageRequestSender,
        ProvideSlackVoltageMessage(
          firstSweepNo,
          Seq(
            ExchangeVoltage(
              supNodeA.getUuid,
              Quantities.getQuantity(380, KILOVOLT),
              Quantities.getQuantity(0, KILOVOLT)
            ),
            ExchangeVoltage(
              supNodeB.getUuid,
              Quantities.getQuantity(380, KILOVOLT),
              Quantities.getQuantity(0, KILOVOLT)
            )
          )
        )
      )

      // power flow calculation should run now. After it's done,
      // our test agent should now be ready to provide the grid power values,
      // hence we ask for them and expect a corresponding response
      superiorGridAgent.requestGridPower(centerGridAgent, firstSweepNo)

      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            Quantities.getQuantity(0, MEGAWATT),
            Quantities.getQuantity(0, MEGAVAR)
          ),
          ExchangePower(
            supNodeB.getUuid,
            Quantities.getQuantity(0.160905770717798, MEGAWATT),
            Quantities.getQuantity(-1.4535602349123878, MEGAVAR)
          )
        )
      )

      // we start a second sweep by asking for next sweep values which should trigger the whole procedure again
      val secondSweepNo = firstSweepNo + 1

      superiorGridAgent.requestGridPower(centerGridAgent, secondSweepNo)

      // the agent now should ask for updated slack voltages from the superior grid
      val secondSlackAskSender =
        superiorGridAgent.expectSlackVoltageRequest(secondSweepNo)

      // the superior grid would answer with updated slack voltage values
      superiorGridAgent.gaProbe.send(
        secondSlackAskSender,
        ProvideSlackVoltageMessage(
          secondSweepNo,
          Seq(
            ExchangeVoltage(
              supNodeB.getUuid,
              Quantities.getQuantity(374.22694614463, KILOVOLT), // 380 kV @ 10°
              Quantities.getQuantity(65.9863075134335, KILOVOLT) // 380 kV @ 10°
            ),
            ExchangeVoltage( // this one should currently be ignored anyways
              supNodeA.getUuid,
              Quantities.getQuantity(380, KILOVOLT),
              Quantities.getQuantity(0, KILOVOLT)
            )
          )
        )
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
            Quantities.getQuantity(108.487669651919932, KILOVOLT),
            Quantities.getQuantity(19.101878551141232, KILOVOLT)
          )
        )
      )

      inferiorGrid12.expectSlackVoltageProvision(
        secondSweepNo,
        Seq(
          ExchangeVoltage(
            node2.getUuid,
            Quantities.getQuantity(108.449088870497683, KILOVOLT),
            Quantities.getQuantity(19.10630456834157630, KILOVOLT)
          )
        )
      )

      inferiorGrid13.expectSlackVoltageProvision(
        secondSweepNo,
        Seq(
          ExchangeVoltage(
            node3a.getUuid,
            Quantities.getQuantity(108.470028019077087, KILOVOLT),
            Quantities.getQuantity(19.104403047662570, KILOVOLT)
          ),
          ExchangeVoltage(
            node3b.getUuid,
            Quantities.getQuantity(108.482524607256866, KILOVOLT),
            Quantities.getQuantity(19.1025584700935336, KILOVOLT)
          )
        )
      )

      // we now answer the requests of our centerGridAgent
      // with three fake grid power message
      inferiorGrid11.gaProbe.send(
        secondPowerRequestSender11,
        ProvideGridPowerMessage(
          inferiorGrid11.nodeUuids.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      inferiorGrid12.gaProbe.send(
        secondPowerRequestSender12,
        ProvideGridPowerMessage(
          inferiorGrid12.nodeUuids.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      inferiorGrid13.gaProbe.send(
        secondPowerRequestSender13,
        ProvideGridPowerMessage(
          inferiorGrid13.nodeUuids.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )
        )
      )

      // we expect that the GridAgent unstashes the messages and return a value for our power request
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            Quantities.getQuantity(0, MEGAWATT),
            Quantities.getQuantity(0, MEGAVAR)
          ),
          ExchangePower(
            supNodeB.getUuid,
            Quantities.getQuantity(0.16090577067051856, MEGAWATT),
            Quantities.getQuantity(-1.4535602358772026, MEGAVAR)
          )
        )
      )

    }
  }
}

object DBFSAlgorithmCenGridSpec extends UnitSpec {
  private val floatPrecision: Double = 0.00000000001

  sealed trait GAActorAndModel {
    val gaProbe: TestProbe
    val nodeUuids: Seq[UUID]
    def ref: ActorRef = gaProbe.ref
  }

  final case class InferiorGA(
      override val gaProbe: TestProbe,
      override val nodeUuids: Seq[UUID]
  ) extends GAActorAndModel {

    def expectGridPowerRequest(): ActorRef = {
      gaProbe
        .expectMsgType[RequestGridPowerMessage]
        .nodeUuids should contain allElementsOf nodeUuids

      gaProbe.lastSender
    }

    def expectSlackVoltageProvision(
        expectedSweepNo: Int,
        expectedExchangedVoltages: Seq[ExchangeVoltage]
    ): Unit = {
      inside(gaProbe.expectMsgType[ProvideSlackVoltageMessage]) {
        case ProvideSlackVoltageMessage(sweepNo, exchangedVoltages) =>
          sweepNo shouldBe expectedSweepNo

          exchangedVoltages.size shouldBe expectedExchangedVoltages.size
          expectedExchangedVoltages.foreach { expectedVoltage =>
            exchangedVoltages.find(
              _.nodeUuid == expectedVoltage.nodeUuid
            ) match {
              case Some(ExchangeVoltage(_, actualE, actualF)) =>
                actualE should equalWithTolerance(
                  expectedVoltage.e,
                  floatPrecision
                )
                actualF should equalWithTolerance(
                  expectedVoltage.f,
                  floatPrecision
                )
              case None =>
                fail(
                  s"Expected ExchangeVoltage with node UUID ${expectedVoltage.nodeUuid} " +
                    s"was not included in ProvideSlackVoltageMessage."
                )
            }
          }
      }
    }

    def requestSlackVoltage(receiver: ActorRef, sweepNo: Int): Unit =
      gaProbe.send(
        receiver,
        RequestSlackVoltageMessage(sweepNo, nodeUuids)
      )
  }

  final case class SuperiorGA(
      override val gaProbe: TestProbe,
      override val nodeUuids: Seq[UUID]
  ) extends GAActorAndModel {

    def expectSlackVoltageRequest(expectedSweepNo: Int): ActorRef = {
      inside(
        gaProbe
          .expectMsgType[RequestSlackVoltageMessage]
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
      inside(gaProbe.expectMsgType[ProvideGridPowerMessage](10.seconds)) {
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
