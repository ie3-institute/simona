/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.event.listener.ResultEventListener.ResultMessage
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.main.RunSimona.SimonaEnded
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.scheduler.TimeAdvancer
import edu.ie3.simona.scheduler.TimeAdvancer.StartSimMessage
import edu.ie3.simona.sim.SimMessage.StartSimulation
import edu.ie3.simona.sim.SimonaSimSpec.{
  MockSetup,
  stopOnMessage,
  throwOnMessage,
}
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}
import edu.ie3.simona.test.common.UnitSpec
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => ClassicRef}

class SimonaSimSpec extends ScalaTestWithActorTestKit with UnitSpec {

  "SimonaSim" should {
    "stop and indicate failure to starter" when {

      "child stops due to thrown exception" in {
        val starter = TestProbe[SimonaEnded]("starter")
        val timeAdvancer = TestProbe[TimeAdvancer.Incoming]("timeAdvancer")

        val receiveThrowingActor =
          TestProbe[ActorRef[RuntimeEvent]]("receiveThrowingActor")

        val simonaSim = spawn(
          SimonaSim(
            new MockSetup(timeAdvancer.ref) {
              override def runtimeEventListener(
                  context: ActorContext[_]
              ): ActorRef[RuntimeEvent] = {
                // has to be created by correct context
                val throwingActor =
                  context.spawn[RuntimeEvent](throwOnMessage, "throwingActor")
                // send ref to the outside
                receiveThrowingActor.ref ! throwingActor
                throwingActor
              }
            }
          ),
          "simonaSim",
        )

        simonaSim ! StartSimulation(starter.ref)

        // Initialization has started, mock actors are being created
        val failActor =
          receiveThrowingActor.expectMessageType[ActorRef[RuntimeEvent]]
        timeAdvancer.expectMessage(StartSimMessage())

        // Simulation should still "run" at this point
        starter.expectNoMessage()

        // We cause an actor to fail.
        // (The actor reacts to any message with an exception,
        // we just pick the first best fit)
        failActor ! RuntimeEvent.Initializing

        // SimonaSim should run its failure routine and tell us that a failure happened
        starter.expectMessage(SimonaEnded(successful = false))
      }

      "child stops by changing behavior" in {
        val starter = TestProbe[SimonaEnded]("starter")
        val timeAdvancer = TestProbe[TimeAdvancer.Incoming]("timeAdvancer")

        val receiveStoppingActor =
          TestProbe[ActorRef[RuntimeEvent]]("receiveStoppingActor")

        val simonaSim = spawn(
          SimonaSim(
            new MockSetup(timeAdvancer.ref) {
              override def runtimeEventListener(
                  context: ActorContext[_]
              ): ActorRef[RuntimeEvent] = {
                // has to be created by correct context
                val stoppingActor =
                  context.spawn[RuntimeEvent](stopOnMessage, "stoppingActor")
                // send ref to the outside
                receiveStoppingActor.ref ! stoppingActor
                stoppingActor
              }
            }
          ),
          "simonaSim",
        )

        simonaSim ! StartSimulation(starter.ref)

        // Initialization has started, mock actors are being created
        val stoppingActor =
          receiveStoppingActor.expectMessageType[ActorRef[RuntimeEvent]]
        timeAdvancer.expectMessage(StartSimMessage())

        // Simulation should still "run" at this point
        starter.expectNoMessage()

        // We cause an actor to fail.
        // (The actor reacts to any message by stopping itself,
        // we just pick the first best fit)
        stoppingActor ! RuntimeEvent.Initializing

        // SimonaSim should run its failure routine and tell us that a failure happened
        starter.expectMessage(SimonaEnded(successful = false))
      }
    }

    "exception is thrown while initializing" in {
      val starter = TestProbe[SimonaEnded]("starter")

      val simonaSim = spawn(
        SimonaSim(
          new MockSetup(TestProbe().ref) {
            override def timeAdvancer(
                context: ActorContext[_],
                simulation: ActorRef[SimMessage],
                runtimeEventListener: ActorRef[RuntimeEvent],
            ): ActorRef[TimeAdvancer.Incoming] = {
              throw new RuntimeException("Test exception")
            }
          }
        ),
        "simonaSim",
      )

      // Initialization should not have started yet
      starter.expectNoMessage()

      simonaSim ! StartSimulation(starter.ref)

      // SimonaSim should run its failure routine and tell us that a failure happened
      starter.expectMessage(SimonaEnded(successful = false))
    }
  }

  // TODO:
  // SimulationEnded(successful = true)
  // -> also test that all actors that were started also are stopped
  // SimulationEnded(successful = false)
  // REL fails in edge cases

  // generally: test that Error is sent to RuntimeEventListener
}

object SimonaSimSpec {

  def empty[T]: Behavior[T] = Behaviors.receiveMessage { _ =>
    Behaviors.same
  }

  def forwardMessage[T](recipient: ActorRef[T]): Behavior[T] =
    Behaviors.receiveMessage { msg =>
      recipient ! msg
      Behaviors.same
    }

  def throwOnMessage[T]: Behavior[T] = Behaviors.receiveMessage { _ =>
    throw new RuntimeException("Nah, I'm not gonna do that!")
  }

  def stopOnMessage[T]: Behavior[T] = Behaviors.receiveMessage { _ =>
    Behaviors.stopped
  }

  class MockSetup(timeAdvancer: ActorRef[TimeAdvancer.Incoming])
      extends SimonaSetup {

    override val args: Array[String] = Array.empty[String]

    override def runtimeEventListener(
        context: ActorContext[_]
    ): ActorRef[RuntimeEvent] = context.spawn(empty, "runtimeEvent")

    override def systemParticipantsListener(
        context: ActorContext[_]
    ): Seq[ActorRef[ResultMessage]] = Seq.empty

    override def primaryServiceProxy(
        context: ActorContext[_],
        scheduler: ActorRef[SchedulerMessage],
    ): ClassicRef =
      context.spawn(empty, "primaryService").toClassic

    override def weatherService(
        context: ActorContext[_],
        scheduler: ActorRef[SchedulerMessage],
    ): ClassicRef =
      context.spawn(empty, "weatherService").toClassic

    override def timeAdvancer(
        context: ActorContext[_],
        simulation: ActorRef[SimMessage],
        runtimeEventListener: ActorRef[RuntimeEvent],
    ): ActorRef[TimeAdvancer.Incoming] =
      context.spawn(forwardMessage(timeAdvancer), "timeAdvancerForwarder")

    override def scheduler(
        context: ActorContext[_],
        timeAdvancer: ActorRef[TimeAdvancer.Incoming],
    ): ActorRef[SchedulerMessage] = context.spawn(empty, "scheduler")

    override def gridAgents(
        context: ActorContext[_],
        environmentRefs: EnvironmentRefs,
        systemParticipantListener: Seq[ActorRef[ResultEvent]],
    ): Iterable[ClassicRef] = Iterable.empty

    override def extSimulations(
        context: ActorContext[_],
        scheduler: ActorRef[SchedulerMessage],
    ): ExtSimSetupData =
      ExtSimSetupData(Iterable.empty, Map.empty)
  }
}
