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
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Primary
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  InitSimMessage,
  SimulationFailureMessage,
  StartScheduleMessage
}
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.SimonaSimFailSpec.FailSim
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}
import edu.ie3.simona.test.common.AgentSpec

import java.time.ZonedDateTime

class SimonaSimFailSpec
    extends AgentSpec(
      ActorSystem(
        "SimonaSimFailSpec",
        ConfigFactory
          .parseString("""
                     |akka.loggers = ["akka.testkit.TestEventListener"]
                     |akka.loglevel="OFF"
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
            timeAdvancer.ref.toTyped[SchedulerMessage]
          )
        )
      )

      /* Init the simulation */
      failSim ! InitSimMessage

      /* The sim asks the scheduler to start it's schedule */
      timeAdvancer.expectMsg(StartScheduleMessage())

      /* Trigger the child to fail */
      failSim.underlyingActor.getChild ! "fail"

      expectMsg(SimulationFailureMessage)
    }
  }
}

object SimonaSimFailSpec {
  class FailSim(
      actorSystem: ActorSystem,
      timeAdvancer: org.apache.pekko.actor.typed.ActorRef[SchedulerMessage]
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
      timeAdvancer: org.apache.pekko.actor.typed.ActorRef[SchedulerMessage],
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
        scheduler: ActorRef
    ): (ActorRef, PrimaryServiceProxy.InitPrimaryServiceProxyStateData) =
      (
        TestProbe("primaryService")(actorSystem).ref,
        InitPrimaryServiceProxyStateData(
          new Primary(None, None, None, None),
          ZonedDateTime.now()
        )
      )

    override def weatherService(
        context: ActorContext,
        scheduler: ActorRef
    ): (ActorRef, WeatherService.InitWeatherServiceStateData) =
      (
        TestProbe("weatherService")(actorSystem).ref,
        InitWeatherServiceStateData(
          new SimonaConfig.Simona.Input.Weather.Datasource(
            new SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource(
              None,
              "foo",
              None,
              None
            ),
            None,
            None,
            None,
            50000d,
            None,
            None,
            "bar",
            None,
            None
          )
        )
      )

    override def timeAdvancer(
        context: ActorContext,
        simulation: ActorRef,
        runtimeEventListener: org.apache.pekko.actor.typed.ActorRef[
          RuntimeEvent
        ]
    ): org.apache.pekko.actor.typed.ActorRef[SchedulerMessage] = timeAdvancer

    override def scheduler(
        context: ActorContext,
        timeAdvancer: org.apache.pekko.actor.typed.ActorRef[SchedulerMessage]
    ): ActorRef = TestProbe("scheduler")(actorSystem).ref

    override def gridAgents(
        context: ActorContext,
        environmentRefs: EnvironmentRefs,
        systemParticipantListener: Seq[ActorRef]
    ): Map[ActorRef, GridAgentInitData] = Map.empty[ActorRef, GridAgentInitData]

    override def extSimulations(
        context: ActorContext,
        scheduler: ActorRef
    ): ExtSimSetupData =
      ExtSimSetupData(Iterable.empty, Iterable.empty)
  }
}
