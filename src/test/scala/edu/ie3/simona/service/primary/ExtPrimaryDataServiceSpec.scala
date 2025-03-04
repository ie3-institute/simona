/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.RegisterForWeatherMessage
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.primary.ExtPrimaryDataService.InitExtPrimaryData
import edu.ie3.simona.test.common.{AgentSpec, TestSpawnerClassic}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.{TestActorRef, TestProbe}

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._

class ExtPrimaryDataServiceSpec
    extends AgentSpec(
      ActorSystem(
        "ExtPrimaryDataServiceSpec",
        ConfigFactory
          .parseString("""
        |pekko.loggers = ["org.apache.pekko.testkit.TestEventListener"]
        |pekko.loglevel = "INFO"
        |""".stripMargin),
      )
    )
    with TestSpawnerClassic {

  private val scheduler = TestProbe("scheduler")
  private val extSimAdapter = TestProbe("extSimAdapter")

  private val extPrimaryDataConnection = new ExtPrimaryDataConnection(
    Map.empty[String, UUID].asJava
  )

  "An uninitialized external primary data service" must {

    "send correct completion message after initialisation" in {
      val primaryDataService = TestActorRef(
        new ExtPrimaryDataService(
          scheduler.ref
        )
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      extPrimaryDataConnection.setActorRefs(
        primaryDataService,
        extSimAdapter.ref,
      )

      scheduler.send(
        primaryDataService,
        SimonaService.Create(
          InitExtPrimaryData(extPrimaryDataConnection),
          key,
        ),
      )
      scheduler.expectMsg(
        ScheduleActivation(primaryDataService.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(primaryDataService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(primaryDataService.toTyped))
    }
  }

  "An external primary service actor" should {
    val serviceRef =
      TestActorRef(
        new ExtPrimaryDataService(
          scheduler.ref
        )
      )

    "refuse registration for wrong registration request" in {
      serviceRef ! RegisterForWeatherMessage(self, 51.4843281, 7.4116482)
      expectNoMessage()
    }

    val systemParticipant: TestProbe = TestProbe("dummySystemParticipant")
    "correctly register a forwarded request" ignore {
      serviceRef ! PrimaryServiceRegistrationMessage(
        systemParticipant.ref,
        UUID.randomUUID(),
      )
      println("Try to register")

      /* Wait for request approval */
      val msg = systemParticipant.expectMsgType[RegistrationSuccessfulMessage](
        10.seconds
      )
      msg.serviceRef shouldBe serviceRef.ref
      msg.firstDataTick shouldBe 0L

      /* We cannot directly check, if the requesting actor is among the subscribers, therefore we ask the actor to
       * provide data to all subscribed actors and check, if the subscribed probe gets one */
      scheduler.send(serviceRef, Activation(0))
      scheduler.expectMsgType[Completion]
      systemParticipant.expectMsgAllClassOf(classOf[DataProvision[PrimaryData]])
    }
  }
}
