/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.actor.{Actor, ActorContext, ActorRef, ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Primary
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  InitSimMessage,
  ScheduleTriggerMessage,
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
      val primaryService = TestProbe("primaryService").ref
      val weatherService = TestProbe("weatherService").ref

      val failSim = TestActorRef.create[FailSim](
        system,
        Props(
          new FailSim(
            system,
            primaryService,
            weatherService,
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
      primaryService: ActorRef,
      weatherService: ActorRef,
      timeAdvancer: akka.actor.typed.ActorRef[SchedulerMessage]
  ) extends SimonaSim(
        new DummySetup(
          actorSystem,
          primaryService,
          weatherService,
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
      primaryService: ActorRef,
      weatherService: ActorRef,
      timeAdvancer: akka.actor.typed.ActorRef[SchedulerMessage],
      override val args: Array[String] = Array.empty[String]
  ) extends SimonaSetup {

    /** A function, that constructs the [[ActorSystem]], the simulation shall
      * live in
      */
    override val buildActorSystem: () => ActorSystem = () => actorSystem

    /** Creates a sequence of runtime event listeners
      *
      * @param context
      *   Actor context to use
      * @return
      *   A sequence of actor references to runtime event listeners
      */
    override def runtimeEventListener(
        context: ActorContext
    ): akka.actor.typed.ActorRef[RuntimeEvent] =
      ActorRef.noSender.toTyped[RuntimeEvent]

    /** Creates a sequence of system participant event listeners
      *
      * @param context
      *   Actor context to use
      * @return
      *   A sequence of actor references to runtime event listeners
      */
    override def systemParticipantsListener(
        context: ActorContext
    ): Seq[ActorRef] = Seq.empty[ActorRef]

    /** Creates a primary service proxy. The proxy is the first instance to ask
      * for primary data. If necessary, it delegates the registration request to
      * it's subordinate workers.
      *
      * @param context
      *   Actor context to use
      * @param scheduler
      *   Actor reference to it's according scheduler to use
      * @return
      *   An actor reference to the service as well as matching data to
      *   initialize the service
      */
    override def primaryServiceProxy(
        context: ActorContext,
        scheduler: ActorRef
    ): (ActorRef, PrimaryServiceProxy.InitPrimaryServiceProxyStateData) =
      (
        primaryService,
        InitPrimaryServiceProxyStateData(
          new Primary(None, None, None, None),
          ZonedDateTime.now()
        )
      )

    /** Creates a weather service
      *
      * @param context
      *   Actor context to use
      * @param scheduler
      *   Actor reference to it's according scheduler to use
      * @return
      *   An actor reference to the service as well as matching data to
      *   initialize the service
      */
    override def weatherService(
        context: ActorContext,
        scheduler: ActorRef
    ): (ActorRef, WeatherService.InitWeatherServiceStateData) =
      (
        weatherService,
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
        runtimeEventListener: akka.actor.typed.ActorRef[RuntimeEvent]
    ): akka.actor.typed.ActorRef[SchedulerMessage] = timeAdvancer

    /** Creates a scheduler service
      *
      * @param context
      *   Actor context to use
      * @return
      *   An actor reference to the scheduler
      */
    override def scheduler(
        context: ActorContext,
        timeAdvancer: akka.actor.typed.ActorRef[SchedulerMessage]
    ): ActorRef = ActorRef.noSender

    /** Creates all the needed grid agents
      *
      * @param context
      *   Actor context to use
      * @param environmentRefs
      *   EnvironmentRefs to use
      * @param systemParticipantListener
      *   Listeners that await events from system participants
      * @return
      *   A mapping from actor reference to it's according initialization data
      *   to be used when setting up the agents
      */
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
