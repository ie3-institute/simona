/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.{ImplicitSender, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgent.FinishGridSimulationTrigger
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.state.GridAgentState.SimulateGrid
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.PowerMessage.ProvideGridPowerMessage.ExchangePower
import edu.ie3.simona.ontology.messages.PowerMessage.{
  FailedPowerFlow,
  ProvideGridPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage.ExchangeVoltage
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.test.common.{
  ConfigTestData,
  TestKitWithShutdown,
  TestSpawnerClassic
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.scala.quantities.Megavars
import squants.electro.Kilovolts
import squants.energy.Megawatts

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

class DBFSAlgorithmFailedPowerFlowSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "DBFSAlgorithmSpec",
        ConfigFactory
          .parseString("""
          |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
          |pekko.loglevel="OFF"
        """.stripMargin)
      )
    )
    with DBFSMockGridAgents
    with ConfigTestData
    with ImplicitSender
    with DbfsTestGrid
    with TestSpawnerClassic {

  private val scheduler = TestProbe("scheduler")
  private val runtimeEvents = TestProbe("runtimeEvents")
  private val primaryService = TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid)
  )

  private val inferiorGridAgent =
    InferiorGA(TestProbe("inferiorGridAgent"), Seq(node1.getUuid))

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    runtimeEventListener = runtimeEvents.ref,
    primaryServiceProxy = primaryService.ref,
    weather = weatherService.ref,
    evDataService = None
  )

  val resultListener: TestProbe = TestProbe("resultListener")

  "A GridAgent actor in center position with async test" should {

    val centerGridAgent =
      system.actorOf(
        GridAgent.props(
          environmentRefs,
          simonaConfig,
          listener = Iterable(resultListener.ref)
        )
      )

    s"initialize itself when it receives an init activation" in {
      // this subnet has 1 superior grid (ehv) and 3 inferior grids (mv). Map the gates to test probes accordingly
      val subGridGateToActorRef = hvSubGridGatesPF.map {
        case gate if gate.getInferiorSubGrid == hvGridContainerPF.getSubnet =>
          gate -> superiorGridAgent.ref
        case gate =>
          val actor = gate.getInferiorSubGrid match {
            case 11 => inferiorGridAgent
          }
          gate -> actor.ref
      }.toMap

      val gridAgentInitData =
        GridAgentInitData(
          hvGridContainerPF,
          Seq.empty[ThermalGrid],
          subGridGateToActorRef,
          RefSystem("2000 MVA", "110 kV")
        )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      centerGridAgent ! GridAgent.Create(
        gridAgentInitData,
        key
      )
      scheduler.expectMsg(
        ScheduleActivation(centerGridAgent.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(centerGridAgent, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(
        Completion(
          centerGridAgent.toTyped,
          Some(3600)
        )
      )

    }

    s"go to $SimulateGrid when it receives an activation" in {

      // send init data to agent
      scheduler.send(
        centerGridAgent,
        Activation(3600)
      )

      // we expect a completion message
      scheduler.expectMsg(
        Completion(
          centerGridAgent.toTyped,
          Some(3600)
        )
      )
    }

    s"start the simulation when an activation is sent is sent, handle failed power flow if it occurs" in {
      val sweepNo = 0

      // send the start grid simulation trigger
      scheduler.send(centerGridAgent, Activation(3600))

      // we expect a request for grid power values here for sweepNo $sweepNo
      val powerRequestSender = inferiorGridAgent.expectGridPowerRequest()

      // we expect a request for voltage values of slack node
      val slackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(sweepNo)

      // normally the inferior grid agents ask for the slack voltage as well to do their power flow calculation
      // we simulate this behaviour now by doing the same for our inferior grid agent
      inferiorGridAgent.requestSlackVoltage(centerGridAgent, sweepNo)

      // as we are in the first sweep, provided slack voltage should be equal
      // to 1 p.u. (in physical value, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)
      inferiorGridAgent.expectSlackVoltageProvision(
        sweepNo,
        Seq(
          ExchangeVoltage(
            node1.getUuid,
            Kilovolts(110d),
            Kilovolts(0d)
          )
        )
      )

      // we now answer the request of our centerGridAgent
      // with a fake grid power message and one fake slack voltage message
      inferiorGridAgent.gaProbe.send(
        powerRequestSender,
        ProvideGridPowerMessage(
          inferiorGridAgent.nodeUuids.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Megawatts(1000.0),
              Megavars(0.0)
            )
          )
        )
      )

      superiorGridAgent.gaProbe.send(
        slackVoltageRequestSender,
        ProvideSlackVoltageMessage(
          sweepNo,
          Seq(
            ExchangeVoltage(
              supNodeA.getUuid,
              Kilovolts(380d),
              Kilovolts(0d)
            )
          )
        )
      )

      // power flow calculation should run now. After it's done,
      // our test agent should now be ready to provide the grid power values,
      // hence we ask for them and expect a corresponding response
      superiorGridAgent.requestGridPower(centerGridAgent, sweepNo)

      // the requested power is to high for the grid to handle, therefore the superior grid agent
      // receives a FailedPowerFlow message
      // wait 30 seconds max for power flow to finish
      superiorGridAgent.gaProbe.expectMsg(30 seconds, FailedPowerFlow)

      // normally the slack node would send a FinishGridSimulationTrigger to all
      // connected inferior grids, because the slack node is just a mock, we imitate this behavior
      superiorGridAgent.gaProbe.send(
        centerGridAgent,
        FinishGridSimulationTrigger(3600)
      )

      // after a FinishGridSimulationTrigger is send to the inferior grids, they themselves will
      // forward the trigger to their connected inferior grids. Therefore the inferior grid agent
      // should receive a FinishGridSimulationTrigger
      inferiorGridAgent.gaProbe.expectMsg(FinishGridSimulationTrigger(3600))

      // after all grids have received a FinishGridSimulationTrigger, the scheduler should receive a CompletionMessage
      scheduler.expectMsg(
        Completion(
          centerGridAgent.toTyped,
          Some(7200)
        )
      )

      resultListener.expectNoMessage()

      // PowerFlowFailed events are only sent by the slack subgrid
      runtimeEvents.expectNoMessage()
    }

  }
}
