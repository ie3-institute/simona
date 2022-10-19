/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.state.GridAgentState.SimulateGrid
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
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
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  FinishGridSimulationTrigger,
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
import scala.concurrent.duration.DurationInt

/** Test to ensure the functions that a [[GridAgent]] in superior position
  * should be able to do if the DBFSAlgorithm is used. The scheduler, the
  * weather service as well as the [[GridAgent]] inferior to the superior
  * [[GridAgent]] are simulated by the TestKit.
  */
class DBFSAlgorithmSupGridSpec
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

  private val scheduler: TestProbe = TestProbe("scheduler")
  private val primaryService: TestProbe = TestProbe("primaryService")
  private val weatherService: TestProbe = TestProbe("weatherService")
  private val hvGrid: TestProbe = TestProbe("hvGrid")

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    primaryServiceProxy = primaryService.ref,
    weather = weatherService.ref,
    evDataService = None
  )

  val resultListener: TestProbe = TestProbe("resultListener")

  "A GridAgent actor in superior position with async test" should {

    val superiorGridAgentFSM = system.actorOf(
      GridAgent.props(
        environmentRefs,
        simonaConfig,
        listener = Iterable(resultListener.ref)
      )
    )

    s"initialize itself when it receives a $InitializeGridAgentTrigger with corresponding data" in {
      val triggerId = 0

      val subnetGatesToActorRef: Map[SubGridGate, ActorRef] =
        ehvSubGridGates.map(gate => gate -> hvGrid.ref).toMap

      val gridAgentInitData =
        GridAgentInitData(
          ehvGridContainer,
          subnetGatesToActorRef,
          RefSystem("5000 MVA", "380 kV")
        )

      // send init data to agent
      scheduler.send(
        superiorGridAgentFSM,
        TriggerWithIdMessage(
          InitializeGridAgentTrigger(gridAgentInitData),
          triggerId,
          superiorGridAgentFSM
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          0,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(3600),
                superiorGridAgentFSM
              )
            )
          )
        )
      )

    }

    s"go to $SimulateGrid when it receives an activity start trigger" in {
      val activityStartTriggerId = 1

      // send init data to agent
      scheduler.send(
        superiorGridAgentFSM,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600),
          activityStartTriggerId,
          superiorGridAgentFSM
        )
      )

      // we expect a completion message
      scheduler.expectMsg(
        CompletionMessage(
          1,
          Some(
            Seq(
              ScheduleTriggerMessage(
                StartGridSimulationTrigger(3600),
                superiorGridAgentFSM
              )
            )
          )
        )
      )

    }

    s"start the simulation, do 2 sweeps and should end afterwards when no deviation on nodal " +
      s"power is recognized in the superior when a $StartGridSimulationTrigger is send" in {

        for (sweepNo <- 0 to 1) {

          val startGridSimulationTriggerId = sweepNo + 2
          val requestedConnectionNodeUuids =
            Vector(UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e"))

          // send the start grid simulation trigger
          scheduler.send(
            superiorGridAgentFSM,
            TriggerWithIdMessage(
              StartGridSimulationTrigger(3600),
              startGridSimulationTriggerId,
              superiorGridAgentFSM
            )
          )

          // we expect a request for grid power values here for sweepNo $sweepNo
          hvGrid.expectMsgPF() {
            case requestGridPowerMessage: RequestGridPowerMessage =>
              requestGridPowerMessage.currentSweepNo shouldBe sweepNo
              requestGridPowerMessage.nodeUuids should contain allElementsOf requestedConnectionNodeUuids
            case x =>
              fail(
                s"Invalid message received when expecting a request for grid power values! Message was $x"
              )

          }

          // we return with a fake grid power message
          // / as we are using the ask pattern, we cannot send it to the grid agent directly but have to send it to the
          // / ask sender
          hvGrid.send(
            hvGrid.lastSender,
            ProvideGridPowerMessage(
              requestedConnectionNodeUuids.map { uuid =>
                ExchangePower(
                  uuid,
                  Quantities.getQuantity(0, KILOWATT),
                  Quantities.getQuantity(0, KILOVAR)
                )
              }
            )
          )

          // we expect a completion message here and that the agent goes back to simulate grid
          // and waits until the newly scheduled StartGridSimulationTrigger is send
          scheduler.expectMsgPF() {
            case CompletionMessage(
                  2,
                  Some(
                    Seq(
                      ScheduleTriggerMessage(
                        StartGridSimulationTrigger(3600),
                        _
                      )
                    )
                  )
                ) =>
            // we expect another completion message when the agent is in SimulateGrid again
            case CompletionMessage(
                  3,
                  Some(
                    Seq(
                      ScheduleTriggerMessage(ActivityStartTrigger(7200), _)
                    )
                  )
                ) =>
              // agent should be in Idle again and listener should contain power flow result data
              resultListener.expectMsgPF() {
                case powerFlowResultEvent: PowerFlowResultEvent =>
                  powerFlowResultEvent.nodeResults.headOption match {
                    case Some(value) =>
                      value.getvMag().getValue shouldBe 1
                      value.getvAng().getValue shouldBe 0
                  }

                  // due to the fact that the used grid does not contain anything besides the one ehv node
                  // we do not expect any results for the following elements
                  powerFlowResultEvent.lineResults shouldBe empty
                  powerFlowResultEvent.switchResults shouldBe empty
                  powerFlowResultEvent.transformer2wResults shouldBe empty
                  powerFlowResultEvent.transformer3wResults shouldBe empty
              }

              hvGrid.expectMsg(FinishGridSimulationTrigger(3600))

            case x =>
              fail(
                s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
              )
          }
        }

      }

    s"start the simulation when a $StartGridSimulationTrigger is sent, do 5 sweeps and should end afterwards, if the " +
      s"nodal power exchange converges not before the fifth sweep." in {

        // configuration of the test
        val maxNumberOfTestSweeps = 4
        // / array that holds the deviations that should be recognized
        // // size must be maxNumberOfTestSweeps + 1 and the last two elements MUST be equal, while all other has to be
        // // bigger in difference of p OR q than the epsilon provided in simonaConfig (see above @ head of the test)
        val deviations =
          Array(
            (
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            ),
            (
              Quantities.getQuantity(100, KILOWATT),
              Quantities.getQuantity(100, KILOVAR)
            ),
            (
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(100, KILOVAR)
            ),
            (
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            ),
            (
              Quantities.getQuantity(0, KILOWATT),
              Quantities.getQuantity(0, KILOVAR)
            )
          )

        // bring agent in simulate grid state
        val activityStartTriggerId = 1

        scheduler.send(
          superiorGridAgentFSM,
          TriggerWithIdMessage(
            ActivityStartTrigger(3600),
            activityStartTriggerId,
            superiorGridAgentFSM
          )
        )

        // we expect a completion message
        scheduler.expectMsg(
          CompletionMessage(
            1,
            Some(
              Seq(
                ScheduleTriggerMessage(
                  StartGridSimulationTrigger(3600),
                  superiorGridAgentFSM
                )
              )
            )
          )
        )

        // go on with testing the sweep behaviour
        for (sweepNo <- 0 to maxNumberOfTestSweeps) {

          val startGridSimulationTriggerId = sweepNo + 4
          val requestedConnectionNodeUuids =
            Vector(UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e"))

          // send the start grid simulation trigger
          scheduler.send(
            superiorGridAgentFSM,
            TriggerWithIdMessage(
              StartGridSimulationTrigger(3600),
              startGridSimulationTriggerId,
              superiorGridAgentFSM
            )
          )

          // we expect a request for grid power values here for sweepNo $sweepNo
          hvGrid.expectMsgPF() {
            case requestGridPowerMessage: RequestGridPowerMessage =>
              requestGridPowerMessage.currentSweepNo shouldBe sweepNo
              requestGridPowerMessage.nodeUuids should contain allElementsOf requestedConnectionNodeUuids
            case x =>
              fail(
                s"Invalid message received when expecting a request for grid power values! Message was $x"
              )
          }

          // we return with a fake grid power message
          // / as we are using the ask pattern, we cannot send it to the grid agent directly but have to send it to the
          // / ask sender
          hvGrid.send(
            hvGrid.lastSender,
            ProvideGridPowerMessage(
              requestedConnectionNodeUuids.map { uuid =>
                ExchangePower(
                  uuid,
                  deviations(sweepNo)._1,
                  deviations(sweepNo)._2
                )
              }
            )
          )

          // we expect a completion message here and that the agent goes back to simulate grid
          // and waits until the newly scheduled StartGridSimulationTrigger is send

          // Simulate Grid
          scheduler.expectMsgPF(30.seconds) {
            case CompletionMessage(
                  _,
                  Some(
                    Seq(
                      ScheduleTriggerMessage(
                        StartGridSimulationTrigger(3600),
                        _
                      )
                    )
                  )
                ) =>
            // when we received a FinishGridSimulationTrigger (as inferior grid agent)
            // we expect another completion message then as well (scheduler view)
            case CompletionMessage(
                  _,
                  Some(
                    Seq(
                      ScheduleTriggerMessage(ActivityStartTrigger(7200), _)
                    )
                  )
                ) =>
              // after doing cleanup stuff, our agent should go back to idle again and listener should contain power flow result data
              resultListener.expectMsgPF() {
                case powerFlowResultEvent: PowerFlowResultEvent =>
                  powerFlowResultEvent.nodeResults.headOption match {
                    case Some(value) =>
                      value.getvMag().getValue shouldBe 1
                      value.getvAng().getValue shouldBe 0
                  }

                  // due to the fact that the used grid does not contain anything besides the one ehv node
                  // we do not expect any results for the following elements
                  powerFlowResultEvent.lineResults shouldBe empty
                  powerFlowResultEvent.switchResults shouldBe empty
                  powerFlowResultEvent.transformer2wResults shouldBe empty
                  powerFlowResultEvent.transformer3wResults shouldBe empty
              }

              hvGrid.expectMsg(FinishGridSimulationTrigger(3600))

            case x =>
              fail(
                s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
              )
          }
        }
      }
  }
}
