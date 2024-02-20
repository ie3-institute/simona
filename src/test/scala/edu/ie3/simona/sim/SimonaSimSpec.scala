/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.event.listener.{ResultEventListener, RuntimeEventListener}
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.main.RunSimona.SimonaEnded
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.scheduler.TimeAdvancer
import edu.ie3.simona.sim.SimonaSimSpec._
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}
import edu.ie3.simona.test.common.UnitSpec
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  LogCapturing,
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => ClassicRef}

import java.util.UUID

class SimonaSimSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with LogCapturing {

  "SimonaSim" should {

    "stop and indicate success to starter" when {

      "receiving SimulationEnded(successful = true) from TimeAdvancer" in {}

    }

    "stop and indicate failure to starter" when {

      "receiving SimulationEnded(successful = false) from TimeAdvancer" in {}

      "child stops due to thrown exception" in {
        val starter = TestProbe[SimonaEnded]("starter")
        val timeAdvancer = TestProbe[TimeAdvancer.Incoming]("timeAdvancer")
        val runtimeListener =
          TestProbe[RuntimeEventListener.Incoming]("runtimeEventListener")

        val receiveThrowingActor =
          TestProbe[ActorRef[SchedulerMessage]]("receiveThrowingActor")

        val simonaSim = spawn(
          SimonaSim(
            new MockSetup(Some(runtimeListener.ref), Some(timeAdvancer.ref)) {

              override def scheduler(
                  context: ActorContext[_],
                  timeAdvancer: ActorRef[TimeAdvancer.Incoming],
              ): ActorRef[SchedulerMessage] = {
                // we cannot return a testprobe ref here,
                // needs to be a proper actor created by context
                val throwingActor = context
                  .spawn[SchedulerMessage](
                    throwOnMessage,
                    uniqueName("throwingActor"),
                  )
                // send ref to the outside
                receiveThrowingActor.ref ! throwingActor
                throwingActor
              }

            }
          ),
          uniqueName("simonaSim"),
        )

        simonaSim ! SimonaSim.Start(starter.ref)

        // Initialization has started, mock actors are being created
        val throwingActor =
          receiveThrowingActor.expectMessageType[ActorRef[SchedulerMessage]]
        timeAdvancer.expectMessage(TimeAdvancer.Start())

        // Simulation should still "run" at this point
        starter.expectNoMessage()

        // We cause an actor to fail.
        // (The actor reacts to any message with an exception,
        // we just pick the first best fit)
        throwingActor ! Completion(TestProbe().ref)

        runtimeListener.expectMessage(
          RuntimeEvent.Error("Simulation stopped with error.")
        )
        runtimeListener.expectMessage(RuntimeEventListener.Stop)

        // SimonaSim should run its failure routine and tell us that a failure happened
        starter.expectMessage(SimonaEnded(successful = false))
        starter.expectTerminated(simonaSim)
      }

      "child stops by changing behavior" in {
        val starter = TestProbe[SimonaEnded]("starter")
        val timeAdvancer = TestProbe[TimeAdvancer.Incoming]("timeAdvancer")
        val runtimeListener =
          TestProbe[RuntimeEventListener.Incoming]("runtimeEventListener")

        val receiveStoppingActor =
          TestProbe[ActorRef[SchedulerMessage]]("receiveStoppingActor")

        val simonaSim = spawn(
          SimonaSim(
            new MockSetup(Some(runtimeListener.ref), Some(timeAdvancer.ref)) {

              override def scheduler(
                  context: ActorContext[_],
                  timeAdvancer: ActorRef[TimeAdvancer.Incoming],
              ): ActorRef[SchedulerMessage] = {
                // we cannot return a testprobe ref here,
                // needs to be a proper actor created by context
                val stoppingActor =
                  context.spawn[SchedulerMessage](
                    stopOnMessage,
                    uniqueName("stoppingActor"),
                  )
                // send ref to the outside
                receiveStoppingActor.ref ! stoppingActor
                stoppingActor
              }

            }
          ),
          uniqueName("simonaSim"),
        )

        simonaSim ! SimonaSim.Start(starter.ref)

        // Initialization has started, mock actors are being created
        val stoppingActor =
          receiveStoppingActor.expectMessageType[ActorRef[SchedulerMessage]]
        timeAdvancer.expectMessage(TimeAdvancer.Start())

        // Simulation should still "run" at this point
        starter.expectNoMessage()

        // We cause an actor to fail.
        // (The actor reacts to any message by stopping itself,
        // we just pick the first best fit)
        stoppingActor ! Completion(TestProbe().ref)

        runtimeListener.expectMessage(
          RuntimeEvent.Error("Simulation stopped with error.")
        )
        runtimeListener.expectMessage(RuntimeEventListener.Stop)

        // SimonaSim should run its failure routine and tell us that a failure happened
        starter.expectMessage(SimonaEnded(successful = false))
        starter.expectTerminated(simonaSim)
      }

      "exception is thrown while initializing" in {
        val starter = TestProbe[SimonaEnded]("starter")

        val simonaSim = spawn(
          SimonaSim(
            new MockSetup() {

              override def resultEventListener(
                  context: ActorContext[_]
              ): Seq[ActorRef[ResultEventListener.Incoming]] =
                throwTestException()
            }
          ),
          uniqueName("simonaSim"),
        )

        // Initialization should not have started yet
        starter.expectNoMessage()

        simonaSim ! SimonaSim.Start(starter.ref)

        // SimonaSim should run its failure routine and tell us that a failure happened
        starter.expectMessage(SimonaEnded(successful = false))
        starter.expectTerminated(simonaSim)
      }
    }

  }

  // TODO:
  // REL fails in edge cases

}

object SimonaSimSpec {

  def empty[T]: Behavior[T] = Behaviors.receiveMessage { _ =>
    Behaviors.same
  }

  def forwardMessage[T](recipient: Option[ActorRef[T]]): Behavior[T] =
    Behaviors.receiveMessage { msg =>
      recipient.foreach(_ ! msg)
      Behaviors.same
    }

  def throwOnMessage[T]: Behavior[T] = Behaviors.receiveMessage { _ =>
    throwTestException()
  }

  def stopOnMessage[T]: Behavior[T] = Behaviors.receiveMessage { _ =>
    Behaviors.stopped
  }

  def throwTestException[T](): T = throw new RuntimeException(
    "This is an exception for test purposes. It is expected to be thrown."
  )

  /** @param name
    * @return
    */
  def uniqueName(name: String): String =
    s"${name}_${UUID.randomUUID()}"

  class MockSetup(
      runtimeEventProbe: Option[ActorRef[RuntimeEventListener.Incoming]] = None,
      timeAdvancerProbe: Option[ActorRef[TimeAdvancer.Incoming]] = None,
  ) extends SimonaSetup {

    override val args: Array[String] = Array.empty[String]

    override def runtimeEventListener(
        context: ActorContext[_]
    ): ActorRef[RuntimeEventListener.Incoming] = context.spawn(
      forwardMessage(runtimeEventProbe),
      uniqueName("runtimeEventForwarder"),
    )

    override def resultEventListener(
        context: ActorContext[_]
    ): Seq[ActorRef[ResultEventListener.Incoming]] = Seq.empty

    override def primaryServiceProxy(
        context: ActorContext[_],
        scheduler: ActorRef[SchedulerMessage],
    ): ClassicRef =
      context.spawn(empty, uniqueName("primaryService")).toClassic

    override def weatherService(
        context: ActorContext[_],
        scheduler: ActorRef[SchedulerMessage],
    ): ClassicRef =
      context.spawn(empty, uniqueName("weatherService")).toClassic

    override def timeAdvancer(
        context: ActorContext[_],
        simulation: ActorRef[SimonaSim.SimulationEnded.type],
        runtimeEventListener: ActorRef[RuntimeEvent],
    ): ActorRef[TimeAdvancer.Incoming] =
      context.spawn(
        forwardMessage(timeAdvancerProbe),
        uniqueName("timeAdvancerForwarder"),
      )

    override def scheduler(
        context: ActorContext[_],
        timeAdvancer: ActorRef[TimeAdvancer.Incoming],
    ): ActorRef[SchedulerMessage] =
      context.spawn(empty, uniqueName("scheduler"))

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
