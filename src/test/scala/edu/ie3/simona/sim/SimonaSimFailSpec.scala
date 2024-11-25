/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.config.InputConfig.{CoordinateSourceConfig, PrimaryConfig, WeatherDataSourceConfig}
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.SimonaSimFailSpec.FailSim
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}
import edu.ie3.simona.test.common.AgentSpec
import org.apache.pekko.actor.Props
import org.apache.pekko.actor.testkit.typed.scaladsl.TestProbe
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.testkit.TestActorRef

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
      val primaryService = TestProbe("primaryService").ref
      val weatherService = TestProbe("weatherService").ref

      val failSim = TestActorRef.create[FailSim](
        system,
        Props(
          new FailSim(system, primaryService, weatherService, scheduler.ref)
        ),
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
      actorSystem: ActorSystem,
      primaryService: ActorRef,
      weatherService: ActorRef,
      scheduler: ActorRef,
  ) extends SimonaSim(
        new DummySetup(actorSystem, primaryService, weatherService, scheduler)
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
      scheduler: ActorRef,
      override val args: Array[String] = Array.empty[String],
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
    override def runtimeEventListener(context: ActorContext): Seq[ActorRef] =
      Seq.empty[ActorRef]

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
        scheduler: ActorRef,
    ): (ActorRef, PrimaryServiceProxy.InitPrimaryServiceProxyStateData) =
      (
        primaryService,
        InitPrimaryServiceProxyStateData(
          PrimaryConfig(None, None, None, None),
          ZonedDateTime.now(),
        ),
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
        scheduler: ActorRef,
    ): (ActorRef, WeatherService.InitWeatherServiceStateData) =
      (
        weatherService,
        InitWeatherServiceStateData(
          WeatherDataSourceConfig(
            "bar",
            None,
            None,
            None,
            50000d,
            None,
            None,
            None,
            None,
            CoordinateSourceConfig(
              "foo",
              None,
              None,
              None,
            ),
          )
        ),
      )

    /** Creates a scheduler service
      *
      * @param context
      *   Actor context to use
      * @return
      *   An actor reference to the scheduler
      */
    override def scheduler(
        context: ActorContext,
        runtimeEventListener: Seq[ActorRef],
    ): ActorRef = scheduler

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
        systemParticipantListener: Seq[ActorRef],
    ): Map[ActorRef, GridAgentInitData] = Map.empty[ActorRef, GridAgentInitData]

    override def extSimulations(
        context: ActorContext,
        scheduler: ActorRef,
    ): ExtSimSetupData =
      ExtSimSetupData(Iterable.empty, Iterable.empty)
  }
}
