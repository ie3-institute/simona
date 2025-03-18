/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.ExchangePower
import edu.ie3.simona.agent.grid.GridAgentMessages._
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.model.grid.{RefSystem, VoltageLimits}
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
import edu.ie3.util.scala.quantities.Megavars
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import squants.energy.Megawatts

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

/** Test to ensure the functions that a [[GridAgent]] in superior position
  * should be able to do if the DBFSAlgorithm is used. The scheduler, the
  * weather service as well as the [[GridAgent]] inferior to the superior
  * [[GridAgent]] are simulated by the TestKit.
  */
class DBFSAlgorithmSupGridSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with ConfigTestData
    with DbfsTestGrid
    with TestSpawnerTyped {

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
    emDataService = None,
    evDataService = None,
  )

  val resultListener: TestProbe[ResultEvent] = TestProbe("resultListener")

  "A GridAgent actor in superior position with async test" should {
    val superiorGridAgentFSM: ActorRef[GridAgent.Request] = testKit.spawn(
      GridAgent(
        environmentRefs,
        simonaConfig,
        listener = Iterable(resultListener.ref),
      )
    )

    s"initialize itself when it receives an init activation" in {
      val subnetGatesToActorRef: Map[SubGridGate, ActorRef[GridAgent.Request]] =
        ehvSubGridGates.map(gate => gate -> hvGrid.ref).toMap

      val gridAgentInitData =
        GridAgentInitData(
          ehvGridContainer,
          Seq.empty[ThermalGrid],
          subnetGatesToActorRef,
          RefSystem("5000 MVA", "380 kV"),
          VoltageLimits(0.9, 1.1),
        )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      superiorGridAgentFSM ! CreateGridAgent(gridAgentInitData, key)

      val scheduleActivationMsg =
        scheduler.expectMessageType[ScheduleActivation]
      scheduleActivationMsg.tick shouldBe 3600
      scheduleActivationMsg.unlockKey shouldBe Some(key)
    }

    s"go to SimulateGrid when it receives an activity start trigger" in {
      // send init data to agent
      superiorGridAgentFSM ! WrappedActivation(Activation(3600))

      // we expect a completion message
      scheduler.expectMessageType[Completion].newTick shouldBe Some(3600)
    }

    s"start the simulation, do 2 sweeps and should end afterwards when no deviation on nodal " +
      s"power is recognized in the superior when an activation is sent is send" in {

        for (sweepNo <- 0 to 1) {

          val requestedConnectionNodeUuids =
            Vector(UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e"))

          // send the start grid simulation trigger
          superiorGridAgentFSM ! WrappedActivation(Activation(3600))

          // we expect a request for grid power values here for sweepNo $sweepNo
          val message = hvGrid.expectMessageType[RequestGridPower]

          val lastSender = message match {
            case requestGridPowerMessage: RequestGridPower =>
              requestGridPowerMessage.currentSweepNo shouldBe sweepNo
              requestGridPowerMessage.nodeUuids should contain allElementsOf requestedConnectionNodeUuids

              requestGridPowerMessage.sender
            case x =>
              fail(
                s"Invalid message received when expecting a request for grid power values! Message was $x"
              )
          }

          // we return with a fake grid power message
          // / as we are using the ask pattern, we cannot send it to the grid agent directly but have to send it to the
          // / ask sender
          lastSender ! GridPowerResponse(
            requestedConnectionNodeUuids.map { uuid =>
              ExchangePower(
                uuid,
                Megawatts(0.0),
                Megavars(0.0),
              )
            }
          )

          // we expect a completion message here and that the agent goes back to simulate grid
          // and waits until the newly scheduled StartGridSimulationTrigger is sent
          // wait 30 seconds max for power flow to finish
          scheduler.expectMessageType[Completion](130 seconds) match {
            case Completion(_, Some(3600)) =>
            // we expect another completion message when the agent is in SimulateGrid again
            case Completion(_, Some(7200)) =>
              // agent should be in Idle again and listener should contain power flow result data
              val resultMessage = resultListener.expectMessageType[ResultEvent]

              resultMessage match {
                case powerFlowResultEvent: PowerFlowResultEvent =>
                  powerFlowResultEvent.nodeResults.headOption match {
                    case Some(value) =>
                      value.getvMag().getValue shouldBe 1
                      value.getvAng().getValue shouldBe 0
                    case None =>
                      fail(s"Expected a result but got none.")
                  }

                  // due to the fact that the used grid does not contain anything besides the one ehv node
                  // we do not expect any results for the following elements
                  powerFlowResultEvent.lineResults shouldBe empty
                  powerFlowResultEvent.switchResults shouldBe empty
                  powerFlowResultEvent.transformer2wResults shouldBe empty
                  powerFlowResultEvent.transformer3wResults shouldBe empty
              }

              // no failed power flow
              runtimeEvents.expectNoMessage()

              hvGrid.expectMessage(FinishGridSimulationTrigger(3600))

            case x =>
              fail(
                s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
              )
          }
        }

      }

    s"start the simulation when an activation is sent is sent, do 5 sweeps and should end afterwards, if the " +
      s"nodal power exchange converges not before the fifth sweep." in {

        // configuration of the test
        val maxNumberOfTestSweeps = 4
        // / array that holds the deviations that should be recognized
        // // size must be maxNumberOfTestSweeps + 1 and the last two elements MUST be equal, while all other has to be
        // // bigger in difference of p OR q than the epsilon provided in simonaConfig (see above @ head of the test)
        val deviations =
          Array(
            (
              Megawatts(0.0),
              Megavars(0.0),
            ),
            (
              Megawatts(0.1),
              Megavars(0.1),
            ),
            (
              Megawatts(0.0),
              Megavars(0.1),
            ),
            (
              Megawatts(0.0),
              Megavars(0.0),
            ),
            (
              Megawatts(0.0),
              Megavars(0.0),
            ),
          )

        // bring agent in simulate grid state
        superiorGridAgentFSM ! WrappedActivation(Activation(3600))

        // we expect a completion message
        scheduler.expectMessageType[Completion].newTick shouldBe Some(3600)

        // go on with testing the sweep behaviour
        for (sweepNo <- 0 to maxNumberOfTestSweeps) {

          val requestedConnectionNodeUuids =
            Vector(UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e"))

          // send the start grid simulation trigger
          superiorGridAgentFSM ! WrappedActivation(Activation(3600))

          // we expect a request for grid power values here for sweepNo $sweepNo
          val message = hvGrid.expectMessageType[GridAgent.Request]

          val lastSender = message match {
            case requestGridPowerMessage: RequestGridPower =>
              requestGridPowerMessage.currentSweepNo shouldBe sweepNo
              requestGridPowerMessage.nodeUuids should contain allElementsOf requestedConnectionNodeUuids

              requestGridPowerMessage.sender
            case x =>
              fail(
                s"Invalid message received when expecting a request for grid power values! Message was $x"
              )
          }

          // we return with a fake grid power message
          // / as we are using the ask pattern, we cannot send it to the grid agent directly but have to send it to the
          // / ask sender
          lastSender ! GridPowerResponse(
            requestedConnectionNodeUuids.map { uuid =>
              ExchangePower(
                uuid,
                deviations(sweepNo)._1,
                deviations(sweepNo)._2,
              )
            }
          )

          // we expect a completion message here and that the agent goes back to simulate grid
          // and waits until the newly scheduled StartGridSimulationTrigger is sent

          // Simulate Grid
          // wait 30 seconds max for power flow to finish
          scheduler.expectMessageType[Completion](30 seconds) match {
            case Completion(_, Some(3600)) =>
            // when we received a FinishGridSimulationTrigger (as inferior grid agent)
            // we expect another completion message then as well (scheduler view)
            case Completion(_, Some(7200)) =>
              // after doing cleanup stuff, our agent should go back to idle again and listener should contain power flow result data
              val resultMessage =
                resultListener.expectMessageType[ResultEvent]

              resultMessage match {
                case powerFlowResultEvent: PowerFlowResultEvent =>
                  powerFlowResultEvent.nodeResults.headOption match {
                    case Some(value) =>
                      value.getvMag().getValue shouldBe 1
                      value.getvAng().getValue shouldBe 0
                    case None =>
                      fail(s"Expected a result but got none.")
                  }

                  // due to the fact that the used grid does not contain anything besides the one ehv node
                  // we do not expect any results for the following elements
                  powerFlowResultEvent.lineResults shouldBe empty
                  powerFlowResultEvent.switchResults shouldBe empty
                  powerFlowResultEvent.transformer2wResults shouldBe empty
                  powerFlowResultEvent.transformer3wResults shouldBe empty
              }

              // no failed power flow
              runtimeEvents.expectNoMessage()

              hvGrid.expectMessage(FinishGridSimulationTrigger(3600))

            case x =>
              fail(
                s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
              )
          }
        }
      }
  }
}
