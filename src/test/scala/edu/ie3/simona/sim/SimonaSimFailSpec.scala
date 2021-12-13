/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import akka.actor.{Actor, ActorRef, ActorRefFactory, ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.akka.SimonaActorRef.RichActorRef
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Primary
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  InitSimMessage,
  ScheduleTriggerMessage,
  SimulationFailureMessage
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
      val scheduler = TestProbe("scheduler")
      val primaryService = TestProbe("primaryService").ref.asLocal
      val weatherService = TestProbe("weatherService").ref.asLocal

      val failSim = TestActorRef.create[FailSim](
        system,
        Props(
          new FailSim(primaryService, weatherService, scheduler.ref.asLocal)
        )
      )
      /*Expect the initialization triggers for weather and the primary service */
      scheduler.expectMsgType[ScheduleTriggerMessage]
      scheduler.expectMsgType[ScheduleTriggerMessage]

      /* Init the simulation */
      failSim ! InitSimMessage

      /* The sim asks the scheduler to start it's schedule */
      scheduler.expectMsg(InitSimMessage)

      /* Trigger the child to fail */
      failSim.underlyingActor.getChild ! "fail"

      expectMsg(SimulationFailureMessage)
    }
  }
}

object SimonaSimFailSpec {
  class FailSim(
      primaryService: SimonaActorRef,
      weatherService: SimonaActorRef,
      scheduler: SimonaActorRef
  ) extends SimonaSim(
        new DummySetup(primaryService, weatherService, scheduler)
      ) {
    val child: ActorRef = context.actorOf(Props(new Loser))
    context.watch(child)

    def getChild: SimonaActorRef = child.asLocal
  }

  class Loser extends Actor {
    override def receive: Receive = { case _ =>
      throw new RuntimeException("Nah, I'm not gonna do that!")
    }
  }

  class DummySetup(
      primaryService: SimonaActorRef,
      weatherService: SimonaActorRef,
      scheduler: SimonaActorRef,
      override val args: Array[String] = Array.empty[String]
  ) extends SimonaSetup {

    /** Creates a sequence of runtime event listeners
      *
      * @param refFactory
      *   ActorContext or ActorSystem to use
      * @return
      *   A sequence of actor references to runtime event listeners
      */
    override def runtimeEventListener(
        refFactory: ActorRefFactory
    ): Seq[SimonaActorRef] =
      Seq.empty[SimonaActorRef]

    /** Creates a sequence of system participant event listeners
      *
      * @param refFactory
      *   ActorContext or ActorSystem to use
      * @return
      *   A sequence of actor references to runtime event listeners
      */
    override def systemParticipantsListener(
        refFactory: ActorRefFactory,
        supervisor: SimonaActorRef
    ): Seq[SimonaActorRef] = Seq.empty[SimonaActorRef]

    /** Creates a primary service proxy. The proxy is the first instance to ask
      * for primary data. If necessary, it delegates the registration request to
      * it's subordinate workers.
      *
      * @param refFactory
      *   ActorContext or ActorSystem to use
      * @param scheduler
      *   Actor reference to it's according scheduler to use
      * @return
      *   An actor reference to the service as well as matching data to
      *   initialize the service
      */
    override def primaryServiceProxy(
        refFactory: ActorRefFactory,
        scheduler: SimonaActorRef
    ): (SimonaActorRef, PrimaryServiceProxy.InitPrimaryServiceProxyStateData) =
      (
        primaryService,
        InitPrimaryServiceProxyStateData(
          new Primary(None, None, None, None),
          ZonedDateTime.now()
        )
      )

    /** Creates a weather service
      *
      * @param refFactory
      *   ActorContext or ActorSystem to use
      * @param scheduler
      *   Actor reference to it's according scheduler to use
      * @return
      *   An actor reference to the service as well as matching data to
      *   initialize the service
      */
    override def weatherService(
        refFactory: ActorRefFactory,
        scheduler: SimonaActorRef
    ): (SimonaActorRef, WeatherService.InitWeatherServiceStateData) =
      (
        weatherService,
        InitWeatherServiceStateData(
          new SimonaConfig.Simona.Input.Weather.Datasource(
            new SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource(
              None,
              "foo",
              None
            ),
            None,
            None,
            None,
            None,
            None,
            "bar",
            None,
            None
          )
        )
      )

    /** Loads external simulations and provides corresponding actors and init
      * data
      *
      * @param refFactory
      *   ActorContext or ActorSystem to use
      * @param scheduler
      *   Actor reference to it's according scheduler to use
      * @return
      *   External simulations and their init data
      */
    override def extSimulations(
        refFactory: ActorRefFactory,
        scheduler: SimonaActorRef
    ): ExtSimSetupData =
      ExtSimSetupData(Iterable.empty, Iterable.empty)

    /** Creates a scheduler service
      *
      * @param refFactory
      *   ActorContext or ActorSystem to use
      * @return
      *   An actor reference to the scheduler
      */
    override def scheduler(
        refFactory: ActorRefFactory,
        runtimeEventListener: Seq[SimonaActorRef]
    ): SimonaActorRef = scheduler

    /** Creates all the needed grid agents
      *
      * @param refFactory
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
        refFactory: ActorRefFactory,
        environmentRefs: EnvironmentRefs,
        systemParticipantListener: Seq[SimonaActorRef]
    ): Map[SimonaActorRef, GridAgentInitData] =
      Map.empty[SimonaActorRef, GridAgentInitData]

  }
}
