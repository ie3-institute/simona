/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.listener.{
  DelayedStopHelper,
  ResultEventListener,
  RuntimeEventListener,
}
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.main.RunSimona.SimonaEnded
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.services.{
  LoadProfileMessage,
  ServiceMessage,
  WeatherMessage,
}
import edu.ie3.simona.scheduler.TimeAdvancer
import edu.ie3.simona.scheduler.core.Core.CoreFactory
import edu.ie3.simona.scheduler.core.RegularSchedulerCore
import edu.ie3.simona.sim.SimonaSim.SimulationEnded
import edu.ie3.simona.sim.SimonaSimSpec._
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import java.nio.file.Path
import java.util.UUID

class SimonaSimSpec extends ScalaTestWithActorTestKit with UnitSpec {

  "SimonaSim" should {

    "indicate success to the starter and stop" when {

      "receiving SimulationEnded from TimeAdvancer" in {
        val starter = TestProbe[SimonaEnded]("starter")
        val runtimeListener =
          TestProbe[RuntimeEventListener.Request]("runtimeEventListener")
        val resultListener =
          TestProbe[ResultEventListener.Request]("resultEventListener")
        val timeAdvancer = TestProbe[TimeAdvancer.Request]("timeAdvancer")
        val extSimAdapter = TestProbe[ExtSimAdapter.Stop]("extSimAdapter")

        val simonaSim = spawn(
          SimonaSim(
            new MockSetup(
              Some(runtimeListener.ref),
              Some(resultListener.ref),
              Some(timeAdvancer.ref),
            ) {
              override def extSimulations(
                  context: ActorContext[_],
                  scheduler: ActorRef[SchedulerMessage],
                  extSimPath: Option[Path],
              ): ExtSimSetupData = {
                // We cannot return a TestProbe ref here,
                // needs to be a proper actor created by context
                val extSim = context.spawn(
                  forwardMessage(Some(extSimAdapter.ref)),
                  uniqueName("extSimAdapterForwarder"),
                )
                ExtSimSetupData(
                  Iterable(extSim.toClassic),
                  Seq.empty,
                  Seq.empty,
                  Seq.empty,
                )
              }
            }
          ),
          uniqueName("simonaSim"),
        )

        simonaSim ! SimonaSim.Start(starter.ref)

        // Initialization has started, mock actors are being created
        timeAdvancer.expectMessage(TimeAdvancer.Start)

        // Simulation should still "run" at this point
        starter.expectNoMessage()

        // TimeAdvancer reached its last tick, ending
        simonaSim ! SimulationEnded

        // Actors receive stop message
        runtimeListener.expectMessage(DelayedStopHelper.FlushAndStop)
        resultListener.expectMessage(DelayedStopHelper.FlushAndStop)
        extSimAdapter.expectMessage(
          ExtSimAdapter.Stop(simulationSuccessful = true)
        )

        // Both runtime and result listener actors have stopped, thus we can end
        starter.expectMessage(SimonaEnded(successful = true))
        starter.expectTerminated(simonaSim)
      }

    }

    "indicate failure to starter and stop" when {

      "child stops due to thrown exception" in {
        val starter = TestProbe[SimonaEnded]("starter")
        val runtimeListener =
          TestProbe[RuntimeEventListener.Request]("runtimeEventListener")
        val resultListener =
          TestProbe[ResultEventListener.Request]("resultEventListener")
        val timeAdvancer = TestProbe[TimeAdvancer.Request]("timeAdvancer")

        val receiveThrowingActor =
          TestProbe[ActorRef[Any]]("receiveThrowingActor")

        val simonaSim = spawn(
          SimonaSim(
            new MockSetup(
              Some(runtimeListener.ref),
              Some(resultListener.ref),
              Some(timeAdvancer.ref),
            ) {

              override def primaryServiceProxy(
                  context: ActorContext[_],
                  scheduler: ActorRef[SchedulerMessage],
                  extSimSetupData: ExtSimSetupData,
              ): ActorRef[ServiceMessage] = {
                val throwingActor = context
                  .spawn[Any](
                    throwOnMessage,
                    uniqueName("throwingActor"),
                  )
                // Send ref to the outside to make it accessible
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
          receiveThrowingActor.expectMessageType[ActorRef[Any]]
        timeAdvancer.expectMessage(TimeAdvancer.Start)

        // Simulation should still "run" at this point
        starter.expectNoMessage()

        // We cause an actor to fail.
        // (The mock actor reacts to any message with an
        // exception, we just pick the first best fit)
        throwingActor ! "throw"

        // Runtime/result event listeners receive stop message
        runtimeListener.expectMessage(
          RuntimeEvent.Error("Simulation stopped with error.")
        )
        runtimeListener.expectMessage(DelayedStopHelper.FlushAndStop)
        resultListener.expectMessage(DelayedStopHelper.FlushAndStop)

        // Both runtime and result listener actors have stopped, thus we can end
        starter.expectMessage(SimonaEnded(successful = false))
        starter.expectTerminated(simonaSim)
      }

      "child stops by changing behavior" in {
        val starter = TestProbe[SimonaEnded]("starter")
        val runtimeListener =
          TestProbe[RuntimeEventListener.Request]("runtimeEventListener")
        val resultListener =
          TestProbe[ResultEventListener.Request]("resultEventListener")
        val timeAdvancer = TestProbe[TimeAdvancer.Request]("timeAdvancer")

        val receiveStoppingActor =
          TestProbe[ActorRef[Any]]("receiveStoppingActor")

        val simonaSim = spawn(
          SimonaSim(
            new MockSetup(
              Some(runtimeListener.ref),
              Some(resultListener.ref),
              Some(timeAdvancer.ref),
            ) {

              override def primaryServiceProxy(
                  context: ActorContext[_],
                  scheduler: ActorRef[SchedulerMessage],
                  extSimSetupData: ExtSimSetupData,
              ): ActorRef[ServiceMessage] = {
                val stoppingActor =
                  context.spawn[Any](
                    stopOnMessage,
                    uniqueName("stoppingActor"),
                  )
                // Send ref to the outside to make it accessible
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
          receiveStoppingActor.expectMessageType[ActorRef[Any]]
        timeAdvancer.expectMessage(TimeAdvancer.Start)

        // Simulation should still "run" at this point
        starter.expectNoMessage()

        // We cause an actor to fail.
        // (The mock actor reacts to any message by stopping
        // itself, we just pick the first best fit)
        stoppingActor ! "stop"

        // Runtime/result event listeners receive stop message
        runtimeListener.expectMessage(
          RuntimeEvent.Error("Simulation stopped with error.")
        )
        runtimeListener.expectMessage(DelayedStopHelper.FlushAndStop)
        resultListener.expectMessage(DelayedStopHelper.FlushAndStop)

        // Both runtime and result listener actors have stopped, thus we can end
        starter.expectMessage(SimonaEnded(successful = false))
        starter.expectTerminated(simonaSim)
      }

      "RuntimeEventListener stops unexpectedly" in {
        val starter = TestProbe[SimonaEnded]("starter")
        val resultListener =
          TestProbe[ResultEventListener.Request]("resultEventListener")
        val timeAdvancer = TestProbe[TimeAdvancer.Request]("timeAdvancer")

        val receiveThrowingActor =
          TestProbe[ActorRef[RuntimeEventListener.Request]](
            "receiveThrowingActor"
          )

        val simonaSim = spawn(
          SimonaSim(
            new MockSetup(
              None,
              Some(resultListener.ref),
              Some(timeAdvancer.ref),
            ) {

              override def runtimeEventListener(
                  context: ActorContext[_]
              ): ActorRef[RuntimeEventListener.Request] = {
                val throwingActor = context
                  .spawn[RuntimeEventListener.Request](
                    throwOnMessage,
                    uniqueName("throwingActor"),
                  )
                // Send ref to the outside
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
          receiveThrowingActor
            .expectMessageType[ActorRef[RuntimeEventListener.Request]]
        timeAdvancer.expectMessage(TimeAdvancer.Start)

        // Simulation should still "run" at this point
        starter.expectNoMessage()

        // We cause the RuntimeEventListener to fail.
        // (The mock actor reacts to any message with an
        // exception, we just pick the first best fit)
        throwingActor ! RuntimeEvent.Initializing

        // Result event listener receives stop message
        resultListener.expectMessage(DelayedStopHelper.FlushAndStop)

        // Both runtime and result listener actors have stopped, thus we can end
        starter.expectMessage(SimonaEnded(successful = false))
        starter.expectTerminated(simonaSim)
      }

      "exception is thrown while initializing" in {
        val starter = TestProbe[SimonaEnded]("starter")

        val simonaSim = spawn(
          SimonaSim(
            new MockSetup() {

              override def resultEventListener(
                  context: ActorContext[_],
                  extSimSetupData: ExtSimSetupData,
              ): Seq[ActorRef[ResultEventListener.Request]] =
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

}

object SimonaSimSpec {

  /** Behavior that does nothing on receiving message */
  def empty[T]: Behavior[T] = Behaviors.receiveMessage { _ =>
    Behaviors.same
  }

  /** Behavior that forwards all messages to given actor. This is required
    * because unless actors are created by the ActorContext inside SimonaSim,
    * they cannot be stopped by SimonaSim.
    */
  def forwardMessage[T](recipient: Option[ActorRef[T]]): Behavior[T] =
    Behaviors.receiveMessage { msg =>
      recipient.foreach(_ ! msg)
      Behaviors.same
    }

  /** Similarly to [[forwardMessage]], this behavior stops on receiving
    * [[DelayedStopHelper.FlushAndStop]] and forwards all other messages to
    * given actor.
    */
  def stoppableForwardMessage[T >: DelayedStopHelper.StoppingMsg](
      recipient: Option[ActorRef[T]]
  ): Behavior[T] =
    Behaviors.receiveMessage {
      case msg @ DelayedStopHelper.FlushAndStop =>
        recipient.foreach(_ ! msg)
        Behaviors.stopped
      case msg =>
        recipient.foreach(_ ! msg)
        Behaviors.same
    }

  /** Behavior that throws a RuntimeException on receiving any message, which
    * should cause the actor to stop
    */
  def throwOnMessage[T]: Behavior[T] = Behaviors.receiveMessage { _ =>
    throwTestException()
  }

  /** Behavior that stops itself on the first received message */
  def stopOnMessage[T]: Behavior[T] = Behaviors.receiveMessage { _ =>
    Behaviors.stopped
  }

  def throwTestException[T](): T = throw new RuntimeException(
    "This is an exception for test purposes. It is expected to be thrown."
  )

  /** Makes the given actor name unique by appending a random UUID */
  def uniqueName(name: String): String =
    s"${name}_${UUID.randomUUID()}"

  /** Mock implementation of [[SimonaSetup]]
    *
    * @param runtimeEventProbe
    *   Optional ActorRef that messages received by RuntimeEventListener are
    *   forwarded to
    * @param resultEventProbe
    *   Optional ActorRef that messages received by ResultEventListener are
    *   forwarded to
    * @param timeAdvancerProbe
    *   Optional ActorRef that messages received by TimeAdvancer are forwarded
    *   to
    */
  class MockSetup(
      runtimeEventProbe: Option[ActorRef[RuntimeEventListener.Request]] = None,
      resultEventProbe: Option[ActorRef[ResultEventListener.Request]] = None,
      timeAdvancerProbe: Option[ActorRef[TimeAdvancer.Request]] = None,
  ) extends SimonaSetup
      with ConfigTestData {

    override val args: Array[String] = Array.empty[String]
    override val simonaConfig: SimonaConfig = SimonaConfig(typesafeConfig)

    override def logOutputDir: Path = throw new NotImplementedError()

    override def runtimeEventListener(
        context: ActorContext[_]
    ): ActorRef[RuntimeEventListener.Request] = context.spawn(
      stoppableForwardMessage(runtimeEventProbe),
      uniqueName("runtimeEventForwarder"),
    )

    override def resultEventListener(
        context: ActorContext[_],
        extSimSetupData: ExtSimSetupData,
    ): Seq[ActorRef[ResultEventListener.Request]] = Seq(
      context.spawn(
        stoppableForwardMessage(resultEventProbe),
        uniqueName("resultEventForwarder"),
      )
    )

    override def primaryServiceProxy(
        context: ActorContext[_],
        scheduler: ActorRef[SchedulerMessage],
        extSimSetupData: ExtSimSetupData,
    ): ActorRef[ServiceMessage] =
      context.spawn(empty, uniqueName("primaryService"))

    override def weatherService(
        context: ActorContext[_],
        scheduler: ActorRef[SchedulerMessage],
    ): ActorRef[WeatherMessage] =
      context.spawn(empty, uniqueName("weatherService"))

    override def loadProfileService(
        context: ActorContext[_],
        scheduler: ActorRef[SchedulerMessage],
    ): ActorRef[LoadProfileMessage] =
      context.spawn(empty, uniqueName("loadProfileService"))

    override def timeAdvancer(
        context: ActorContext[_],
        simulation: ActorRef[SimonaSim.SimulationEnded.type],
        runtimeEventListener: ActorRef[RuntimeEvent],
    ): ActorRef[TimeAdvancer.Request] =
      context.spawn(
        forwardMessage(timeAdvancerProbe),
        uniqueName("timeAdvancerForwarder"),
      )

    override def scheduler(
        context: ActorContext[_],
        timeAdvancer: ActorRef[SchedulerMessage],
        coreFactory: CoreFactory = RegularSchedulerCore,
    ): ActorRef[SchedulerMessage] =
      context.spawn(empty, uniqueName("scheduler"))

    override def gridAgents(
        context: ActorContext[_],
        environmentRefs: EnvironmentRefs,
        resultEventListeners: Seq[ActorRef[ResultEvent]],
    ): Iterable[ActorRef[GridAgent.Request]] = Iterable.empty

    override def extSimulations(
        context: ActorContext[_],
        scheduler: ActorRef[SchedulerMessage],
        extSimPath: Option[Path],
    ): ExtSimSetupData =
      ExtSimSetupData.apply
  }
}
