/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import org.apache.pekko.actor.typed.scaladsl.adapter.{
  ClassicActorRefOps,
  ClassicActorSystemOps
}
import org.apache.pekko.actor.{
  Actor,
  ActorContext,
  ActorRef,
  ActorSystem,
  Props
}
import org.apache.pekko.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.scheduler.TimeAdvancer
import edu.ie3.simona.scheduler.TimeAdvancer.StartSimMessage
import edu.ie3.simona.sim.SimMessage.{InitSim, SimulationFailure}
import edu.ie3.simona.sim.SimonaSimFailSpec.FailSim
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}
import edu.ie3.simona.test.common.AgentSpec

class SimonaSimFailSpec
    extends AgentSpec(
      ActorSystem(
        "SimonaSimFailSpec",
        ConfigFactory
          .parseString("""
                     |pekko.loggers = ["org.apache.pekko.testkit.TestEventListener"]
                     |pekko.loglevel="OFF"
        """.stripMargin)
      )
    ) {
  "A SimonaSim" should {
    "properly fail on uncaught exception" in {
      val timeAdvancer = TestProbe("timeAdvancer")

      val failSim = TestActorRef.create[FailSim](
        system,
        Props(
          new FailSim(
            system,
            timeAdvancer.ref.toTyped
          )
        )
      )

      /* Init the simulation */
      failSim ! InitSim

      /* The sim asks the scheduler to start it's schedule */
      timeAdvancer.expectMsg(StartSimMessage())

      /* Trigger the child to fail */
      failSim.underlyingActor.getChild ! "fail"

      expectMsg(SimulationFailure)
    }
  }
}

object SimonaSimFailSpec {
  class FailSim(
      actorSystem: ActorSystem,
      timeAdvancer: org.apache.pekko.actor.typed.ActorRef[TimeAdvancer.Incoming]
  ) extends SimonaSim(
        new DummySetup(
          actorSystem,
          timeAdvancer
        )
      ) {
    val child: ActorRef = context.actorOf(Props(new Loser))
    context.watch(child)

    def getChild: ActorRef = child
  }

  class Loser extends Actor {
    override def receive: Receive = { case _ =>
      throw new RuntimeException("Nah, I'm not gonna do that!")
    }
  }

  class DummySetup(
      actorSystem: ActorSystem,
      timeAdvancer: org.apache.pekko.actor.typed.ActorRef[
        TimeAdvancer.Incoming
      ],
      override val args: Array[String] = Array.empty[String]
  ) extends SimonaSetup {

    override val buildActorSystem: () => ActorSystem = () => actorSystem

    override def runtimeEventListener(
        context: ActorContext
    ): org.apache.pekko.actor.typed.ActorRef[RuntimeEvent] =
      org.apache.pekko.actor.testkit.typed.scaladsl
        .TestProbe[RuntimeEvent]()(actorSystem.toTyped)
        .ref

    override def systemParticipantsListener(
        context: ActorContext
    ): Seq[ActorRef] = Seq.empty[ActorRef]

    override def primaryServiceProxy(
        context: ActorContext,
        scheduler: ActorRef,
        extSimSetupData: ExtSimSetupData
    ): ActorRef =
      TestProbe("primaryService")(actorSystem).ref

    override def weatherService(
        context: ActorContext,
        scheduler: ActorRef
    ): ActorRef =
      TestProbe("weatherService")(actorSystem).ref

    override def timeAdvancer(
        context: ActorContext,
        simulation: ActorRef,
        runtimeEventListener: org.apache.pekko.actor.typed.ActorRef[
          RuntimeEvent
        ]
    ): org.apache.pekko.actor.typed.ActorRef[TimeAdvancer.Incoming] =
      timeAdvancer

    override def scheduler(
        context: ActorContext,
        timeAdvancer: org.apache.pekko.actor.typed.ActorRef[
          TimeAdvancer.Incoming
        ]
    ): ActorRef = TestProbe("scheduler")(actorSystem).ref

    override def gridAgents(
        context: ActorContext,
        environmentRefs: EnvironmentRefs,
        systemParticipantListener: Seq[ActorRef]
    ): Iterable[ActorRef] = Iterable.empty

    override def extSimulations(
        context: ActorContext,
        scheduler: ActorRef
    ): ExtSimSetupData =
      ExtSimSetupData(Iterable.empty, Map.empty)
  }
}
