/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.adapter.TypedActorRefOps
import akka.actor.{ActorContext, DeadLetter, ActorRef => ClassicActorRef}
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Primary
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.SimonaSimSpec.DummySetup
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}
import edu.ie3.simona.test.common.UnitSpec

import java.time.ZonedDateTime

class SimonaSimSpec extends ScalaTestWithActorTestKit with UnitSpec {
  "SimonaSim" should {
    "work as expected when everything goes smoothly" in {

      val primaryService = createTestProbe("primaryService").ref.toClassic
      val weatherService = createTestProbe("weatherService").ref.toClassic
      val scheduler = createTestProbe[SchedulerMessage]("scheduler")

      val simonaSetup = DummySetup(
        primaryService,
        weatherService,
        () => scheduler.ref.toClassic
      )

      val simonaSim = spawn(
        SimonaSim(simonaSetup)
      )

      simonaSim ! SimonaSim.InitSimMessage

      1 to 2 foreach { _ =>
        scheduler.expectMessageType[SchedulerMessage.ScheduleTriggerMessage]
      }
      scheduler.expectMessageType[SchedulerMessage.InitSimMessage]

    }

    "fail and stop upon thrown Exception while handling a message" in {

      val simonaSetup = DummySetup(
        ClassicActorRef.noSender,
        ClassicActorRef.noSender,
        () => { throw new RuntimeException("This failed!") }
      )
      val simonaSim = spawn(SimonaSim(simonaSetup))

      // checking for the death of SimonaSim by expecting a dead letter
      val deadLetter = createDeadLetterProbe()

      simonaSim ! SimonaSim.InitSimMessage

      // actor must be dead by now
      val deadMessage = SimonaSim.ServiceInitComplete(ClassicActorRef.noSender)
      simonaSim ! deadMessage

      deadLetter.expectMessageType[DeadLetter].message shouldBe deadMessage
    }

    // TODO testing stopping a child and expecting SimonaSim to stop as well
  }
}

object SimonaSimSpec {

  case class DummySetup(
      primaryService: ClassicActorRef,
      weatherService: ClassicActorRef,
      createScheduler: () => ClassicActorRef,
      override val args: Array[String] = Array.empty[String]
  ) extends SimonaSetup {

    /** Creates a sequence of runtime event listeners
      *
      * @param context
      *   Actor context to use
      * @return
      *   A sequence of actor references to runtime event listeners
      */
    override def runtimeEventListener(
        context: ActorContext
    ): Seq[ClassicActorRef] =
      Seq.empty[ClassicActorRef]

    /** Creates a sequence of system participant event listeners
      *
      * @param context
      *   Actor context to use
      * @return
      *   A sequence of actor references to runtime event listeners
      */
    override def systemParticipantsListener(
        context: ActorContext,
        supervisor: ActorRef[SimonaSim.Request]
    ): Seq[ClassicActorRef] = Seq.empty[ClassicActorRef]

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
        scheduler: ClassicActorRef
    ): (ClassicActorRef, PrimaryServiceProxy.InitPrimaryServiceProxyStateData) =
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
        scheduler: ClassicActorRef
    ): (ClassicActorRef, WeatherService.InitWeatherServiceStateData) =
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

    /** Creates a scheduler service
      *
      * @param context
      *   Actor context to use
      * @return
      *   An actor reference to the scheduler
      */
    override def scheduler(
        context: ActorContext,
        runtimeEventListener: Seq[ClassicActorRef]
    ): ClassicActorRef = createScheduler()

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
        systemParticipantListener: Seq[ClassicActorRef]
    ): Map[ClassicActorRef, GridAgentInitData] =
      Map.empty[ClassicActorRef, GridAgentInitData]

    override def extSimulations(
        context: ActorContext,
        scheduler: ClassicActorRef
    ): ExtSimSetupData =
      ExtSimSetupData(Iterable.empty, Iterable.empty)
  }
}
