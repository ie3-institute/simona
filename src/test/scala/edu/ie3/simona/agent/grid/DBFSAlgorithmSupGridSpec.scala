/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import java.util.UUID
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestFSMRef}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.GridAgentState.SimulateGrid
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

  private val environmentRefs = EnvironmentRefs(
    scheduler = self,
    primaryServiceProxy = self,
    weather = self,
    evDataService = None
  )

  "A GridAgent actor in superior position with FSM test" should {

    val superiorGridAgentFSM = TestFSMRef(
      new GridAgent(
        environmentRefs,
        simonaConfig,
        listener = Iterable.empty[ActorRef]
      )
    )

    s"be in state $Uninitialized after startup" in {
      superiorGridAgentFSM.stateName shouldBe Uninitialized
    }

    s"initialize itself when it receives a $InitializeGridAgentTrigger with corresponding data" in {
      val triggerId = 0

      // hs test actor (below ehv/sup actor)
      val hsActorRef = this.testActor

      val subGridGateToActorRef: Map[SubGridGate, ActorRef] =
        ehvSubGridGates.map(gate => gate -> hsActorRef).toMap

      val gridAgentInitData =
        GridAgentInitData(
          ehvGridContainer,
          Seq.empty[ThermalGrid],
          subGridGateToActorRef,
          RefSystem("5000 MVA", "380 kV")
        )

      // send init data to agent
      superiorGridAgentFSM ! TriggerWithIdMessage(
        InitializeGridAgentTrigger(gridAgentInitData),
        triggerId,
        superiorGridAgentFSM
      )

      // grid agent state should be idle afterwards
      superiorGridAgentFSM.stateName shouldBe Idle

      expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(Vector(ScheduleTriggerMessage(triggerToBeScheduled, _)))
            ) =>
          triggerId shouldBe 0
          triggerToBeScheduled shouldBe ActivityStartTrigger(3600)
        case x =>
          fail(
            s"Invalid message received when expecting a completion message after an init trigger. Message was $x"
          )
      }

    }

    s"go to $SimulateGrid when it receives an activity start trigger" in {

      val activityStartTriggerId = 1

      superiorGridAgentFSM ! TriggerWithIdMessage(
        ActivityStartTrigger(3600),
        activityStartTriggerId,
        superiorGridAgentFSM
      )

      // grid agent stat should be simulate grid afterwards
      superiorGridAgentFSM.stateName shouldBe SimulateGrid

      // we expect a completion message
      expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(Vector(ScheduleTriggerMessage(triggerToBeScheduled, _)))
            ) =>
          triggerId shouldBe 1
          triggerToBeScheduled shouldBe StartGridSimulationTrigger(3600)
        case x =>
          fail(
            s"Invalid message received when expecting a completion message after a start activity trigger. Message was $x"
          )
      }

    }

    s"start the simulation, do 2 sweeps and should end afterwards when no deviation on nodal " +
      s"power is recognized in the superior grid when a $StartGridSimulationTrigger is send" in {

        for (sweepNo <- 0 to 1) {

          val startGridSimulationTriggerId = sweepNo + 2
          val requestedConnectionNodeUuid =
            UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e")

          // send the start grid simulation trigger
          superiorGridAgentFSM ! TriggerWithIdMessage(
            StartGridSimulationTrigger(3600),
            startGridSimulationTriggerId,
            superiorGridAgentFSM
          )

          // we expect a request for grid power values here for sweepNo $sweepNo
          expectMsgPF() {
            case requestGridPowerMessage: RequestGridPowerMessage =>
              requestGridPowerMessage.currentSweepNo shouldBe sweepNo
              requestGridPowerMessage.nodeUuid shouldBe requestedConnectionNodeUuid
            case x =>
              fail(
                s"Invalid message received when expecting a request for grid power values! Message was $x"
              )

          }

          // we return with a fake grid power message
          // / as we are using the ask pattern, we cannot send it to the grid agent directly but have to send it to the
          // / ask sender
          val askSender = lastSender
          askSender ! ProvideGridPowerMessage(
            requestedConnectionNodeUuid,
            Quantities.getQuantity(0, KILOWATT),
            Quantities.getQuantity(0, KILOVAR)
          )

          // we expect a completion message here (sweepNo == 0) and that the agent goes back to simulate grid
          // and waits until the newly scheduled StartGridSimulationTrigger is send
          expectMsgPF() {
            /* should happen for sweepNo == 0, receiver is scheduler */
            case CompletionMessage(
                  2,
                  Some(
                    Vector(
                      ScheduleTriggerMessage(
                        StartGridSimulationTrigger(3600),
                        _
                      )
                    )
                  )
                ) =>
              // might take some time until we are in SimulateGrid again
              awaitAssert(superiorGridAgentFSM.stateName shouldBe SimulateGrid)
            case FinishGridSimulationTrigger(3600) =>
              /* should happen for sweepNo == 1, receiver is hsActor */
              // if this message is send out by the sup agent, it is in state SimulateGrid -
              // depending on the host this test is running, the sup agent might already have changed state to Idle again
              // (follows after sending this FinishGridSimulationTrigger message to the hsActor)
              // IMHO it's not safe to check for this state here, because if the receive process is only a few nanoseconds
              // to slow, the test will fail
              // furthermore, as it is expected to be send a completion message after the FinishGridSimulationTrigger
              // in the state SimulateGrid too, it should be enough to check if we receive this completion message, because then
              // the agent implicitly has to have been in state SimulateGrid. (see DBFSAlgorithm:317-362 for understanding)
              // I comment this out for now, but leave the "dead code" with the explanation here as well (for now).
              // If this solves the issue, we can remove this in the long term.
              //            awaitAssert(superiorGridAgentFSM.stateName shouldBe SimulateGrid, interval = 1.nanos)
              /* we expect another completion message then to be send to the scheduler, receiver is scheduler then */
              expectMsgPF() {
                case CompletionMessage(
                      3,
                      Some(
                        Vector(
                          ScheduleTriggerMessage(ActivityStartTrigger(7200), _)
                        )
                      )
                    ) =>
                  // after doing cleanup stuff, our agent should go back to idle again
                  awaitAssert(superiorGridAgentFSM.stateName shouldBe Idle)
                case x =>
                  fail(
                    s"Invalid message received when expecting a completion message for simulate grid after cleanup! Message was $x"
                  )
              }
            case x =>
              fail(
                s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
              )
          }
        }

      }

    s"start the simulation, do 5 sweeps and should end afterwards when a deviation on nodal power is " +
      s"recognized after the first two sweeps in the superior when a $StartGridSimulationTrigger is send" in {

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

        superiorGridAgentFSM ! TriggerWithIdMessage(
          ActivityStartTrigger(3600),
          activityStartTriggerId,
          superiorGridAgentFSM
        )

        // we expect a completion message
        expectMsgPF() {
          case CompletionMessage(
                triggerId,
                Some(Vector(ScheduleTriggerMessage(triggerToBeScheduled, _)))
              ) =>
            triggerId shouldBe 1
            triggerToBeScheduled shouldBe StartGridSimulationTrigger(3600)
          case x =>
            fail(
              s"Invalid message received when expecting a completion message after a start activity trigger. Message was $x"
            )
        }

        // grid agent state should be simulate grid afterwards
        superiorGridAgentFSM.stateName shouldBe SimulateGrid

        // go on with testing the sweep behaviour
        for (sweepNo <- 0 to maxNumberOfTestSweeps) {

          val startGridSimulationTriggerId = sweepNo + 4
          val requestedConnectionNodeUuid =
            UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e")

          // send the start grid simulation trigger
          superiorGridAgentFSM ! TriggerWithIdMessage(
            StartGridSimulationTrigger(3600),
            startGridSimulationTriggerId,
            superiorGridAgentFSM
          )

          // we expect a request for grid power values here for sweepNo $sweepNo
          expectMsgPF() {
            case requestGridPowerMessage: RequestGridPowerMessage =>
              requestGridPowerMessage.currentSweepNo shouldBe sweepNo
              requestGridPowerMessage.nodeUuid shouldBe requestedConnectionNodeUuid
            case x =>
              fail(
                s"Invalid message received when expecting a request for grid power values! Message was $x"
              )
          }

          // we return with a fake grid power message
          // / as we are using the ask pattern, we cannot send it to the grid agent directly but have to send it to the
          // / ask sender
          val askSender = lastSender
          askSender ! ProvideGridPowerMessage(
            requestedConnectionNodeUuid,
            deviations(sweepNo)._1,
            deviations(sweepNo)._2
          )

          // we expect a completion message here and that the agent goes back to simulate grid
          // and waits until the newly scheduled StartGridSimulationTrigger is send
          expectMsgPF() {
            case CompletionMessage(
                  _,
                  Some(
                    Vector(
                      ScheduleTriggerMessage(
                        StartGridSimulationTrigger(3600),
                        _
                      )
                    )
                  )
                ) =>
            case FinishGridSimulationTrigger(3600) =>
              // when we received a FinishGridSimulationTrigger (as inferior grid agent)
              // we expect another completion message then as well (scheduler view)
              expectMsgPF() {
                case CompletionMessage(
                      _,
                      Some(
                        Vector(
                          ScheduleTriggerMessage(ActivityStartTrigger(7200), _)
                        )
                      )
                    ) =>
                  // after doing cleanup stuff, our agent should go back to idle again
                  awaitAssert(superiorGridAgentFSM.stateName shouldBe Idle)
                case x =>
                  fail(
                    s"Invalid message received when expecting a completion message for simulate grid after cleanup! Message was $x"
                  )
              }
            case x =>
              fail(
                s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
              )
          }
        }
      }
  }

  "A GridAgent actor in superior position with async test" should {

    val superiorGridAgentFSM = system.actorOf(
      GridAgent.props(
        environmentRefs,
        simonaConfig,
        listener = Iterable.empty[ActorRef]
      )
    )

    s"initialize itself when it receives a $InitializeGridAgentTrigger with corresponding data" in {
      val triggerId = 0

      val hsActorRef = this.testActor

      val subnetGatesToActorRef: Map[SubGridGate, ActorRef] =
        ehvSubGridGates.map(gate => gate -> hsActorRef).toMap

      val gridAgentInitData =
        GridAgentInitData(
          ehvGridContainer,
          Seq.empty[ThermalGrid],
          subnetGatesToActorRef,
          RefSystem("5000 MVA", "380 kV")
        )

      // send init data to agent
      superiorGridAgentFSM ! TriggerWithIdMessage(
        InitializeGridAgentTrigger(gridAgentInitData),
        triggerId,
        superiorGridAgentFSM
      )

      expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(Vector(ScheduleTriggerMessage(triggerToBeScheduled, _)))
            ) =>
          triggerId shouldBe 0
          triggerToBeScheduled shouldBe ActivityStartTrigger(3600)
        case x =>
          fail(
            s"Invalid message received when expecting a completion message after an init trigger. Message was $x"
          )
      }

    }

    s"go to $SimulateGrid when it receives an activity start trigger" in {

      val activityStartTriggerId = 1

      superiorGridAgentFSM ! TriggerWithIdMessage(
        ActivityStartTrigger(3600),
        activityStartTriggerId,
        superiorGridAgentFSM
      )

      // we expect a completion message
      expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(Vector(ScheduleTriggerMessage(triggerToBeScheduled, _)))
            ) =>
          triggerId shouldBe 1
          triggerToBeScheduled shouldBe StartGridSimulationTrigger(3600)
        case x =>
          fail(
            s"Invalid message received when expecting a completion message after a start activity trigger. Message was $x"
          )
      }

    }

    s"start the simulation, do 2 sweeps and should end afterwards when no deviation on nodal " +
      s"power is recognized in the superior when a $StartGridSimulationTrigger is send" in {

        for (sweepNo <- 0 to 1) {

          val startGridSimulationTriggerId = sweepNo + 2
          val requestedConnectionNodeUuid =
            UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e")

          // send the start grid simulation trigger
          superiorGridAgentFSM ! TriggerWithIdMessage(
            StartGridSimulationTrigger(3600),
            startGridSimulationTriggerId,
            superiorGridAgentFSM
          )

          // we expect a request for grid power values here for sweepNo $sweepNo
          expectMsgPF() {
            case requestGridPowerMessage: RequestGridPowerMessage =>
              requestGridPowerMessage.currentSweepNo shouldBe sweepNo
              requestGridPowerMessage.nodeUuid shouldBe requestedConnectionNodeUuid
            case x =>
              fail(
                s"Invalid message received when expecting a request for grid power values! Message was $x"
              )

          }

          // we return with a fake grid power message
          // / as we are using the ask pattern, we cannot send it to the grid agent directly but have to send it to the
          // / ask sender
          val askSender = lastSender
          askSender ! ProvideGridPowerMessage(
            requestedConnectionNodeUuid,
            Quantities.getQuantity(0, KILOWATT),
            Quantities.getQuantity(0, KILOVAR)
          )

          // we expect a completion message here and that the agent goes back to simulate grid
          // and waits until the newly scheduled StartGridSimulationTrigger is send
          expectMsgPF() {
            case CompletionMessage(
                  2,
                  Some(
                    Vector(
                      ScheduleTriggerMessage(
                        StartGridSimulationTrigger(3600),
                        _
                      )
                    )
                  )
                ) =>
            case FinishGridSimulationTrigger(3600) =>
              // we expect another completion message when the agent is in SimulateGrid again
              expectMsgPF() {
                case CompletionMessage(
                      3,
                      Some(
                        Vector(
                          ScheduleTriggerMessage(ActivityStartTrigger(7200), _)
                        )
                      )
                    ) =>
                // agent should be in Idle again
                case x =>
                  fail(
                    s"Invalid message received when expecting a completion message for simulate grid after cleanup! Message was $x"
                  )
              }
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

        superiorGridAgentFSM ! TriggerWithIdMessage(
          ActivityStartTrigger(3600),
          activityStartTriggerId,
          superiorGridAgentFSM
        )

        // we expect a completion message
        expectMsgPF() {
          case CompletionMessage(
                triggerId,
                Some(Vector(ScheduleTriggerMessage(triggerToBeScheduled, _)))
              ) =>
            triggerId shouldBe 1
            triggerToBeScheduled shouldBe StartGridSimulationTrigger(3600)
          case x =>
            fail(
              s"Invalid message received when expecting a completion message after a start activity trigger. Message was $x"
            )
        }

        // go on with testing the sweep behaviour
        for (sweepNo <- 0 to maxNumberOfTestSweeps) {

          val startGridSimulationTriggerId = sweepNo + 4
          val requestedConnectionNodeUuid =
            UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e")

          // send the start grid simulation trigger
          superiorGridAgentFSM ! TriggerWithIdMessage(
            StartGridSimulationTrigger(3600),
            startGridSimulationTriggerId,
            superiorGridAgentFSM
          )

          // we expect a request for grid power values here for sweepNo $sweepNo
          expectMsgPF() {
            case requestGridPowerMessage: RequestGridPowerMessage =>
              requestGridPowerMessage.currentSweepNo shouldBe sweepNo
              requestGridPowerMessage.nodeUuid shouldBe requestedConnectionNodeUuid
            case x =>
              fail(
                s"Invalid message received when expecting a request for grid power values! Message was $x"
              )
          }

          // we return with a fake grid power message
          // / as we are using the ask pattern, we cannot send it to the grid agent directly but have to send it to the
          // / ask sender
          val askSender = lastSender
          askSender ! ProvideGridPowerMessage(
            requestedConnectionNodeUuid,
            deviations(sweepNo)._1,
            deviations(sweepNo)._2
          )

          // we expect a completion message here and that the agent goes back to simulate grid
          // and waits until the newly scheduled StartGridSimulationTrigger is send

          // Simulate Grid
          expectMsgPF() {
            case CompletionMessage(
                  _,
                  Some(
                    Vector(
                      ScheduleTriggerMessage(
                        StartGridSimulationTrigger(3600),
                        _
                      )
                    )
                  )
                ) =>
            case FinishGridSimulationTrigger(3600) =>
              // when we received a FinishGridSimulationTrigger (as inferior grid agent)
              // we expect another completion message then as well (scheduler view)
              expectMsgPF() {
                case CompletionMessage(
                      _,
                      Some(
                        Vector(
                          ScheduleTriggerMessage(ActivityStartTrigger(7200), _)
                        )
                      )
                    ) =>
                // after doing cleanup stuff, our agent should go back to idle again
                case x =>
                  fail(
                    s"Invalid message received when expecting a completion message for simulate grid after cleanup! Message was $x"
                  )
              }
            case x =>
              fail(
                s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
              )
          }
        }
      }
  }
}
