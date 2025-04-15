/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.value.Value
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant2.ParticipantAgent.{DataProvision, RegistrationSuccessfulMessage}
import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.api.simulation.ontology.ControlResponseMessageFromExt
import edu.ie3.simona.ontology.messages.SchedulerMessage.{Completion, ScheduleActivation}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{PrimaryServiceRegistrationMessage, WrappedActivation}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.RegisterForWeatherMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.primary.ExtPrimaryDataService.InitExtPrimaryData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._

class ExtPrimaryDataServiceSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers
    with PrivateMethodTester
    with LazyLogging
    with TestSpawnerTyped {

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")
  private val extSimAdapter =
    TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

  private val extPrimaryDataConnection = new ExtPrimaryDataConnection(
    Map.empty[UUID, Class[_ <: Value]].asJava
  )

  "An uninitialized external primary data service" must {

    "send correct completion message after initialisation" in {
      val primaryDataService = spawn(ExtPrimaryDataService.apply(scheduler.ref))
      val adapter = spawn(ExtPrimaryDataService.adapter(primaryDataService))

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      extPrimaryDataConnection.setActorRefs(
        adapter,
        extSimAdapter.ref,
      )

      primaryDataService ! ServiceMessage.Create(
        InitExtPrimaryData(extPrimaryDataConnection),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      val serviceActivation = activationMsg.actor

      serviceActivation ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(serviceActivation))
    }
  }

  "An external primary service actor" should {
    val serviceRef = spawn(ExtPrimaryDataService.apply(scheduler.ref))
    val systemParticipant = TestProbe[Any]("dummySystemParticipant")

    "refuse registration for wrong registration request" in {
      val schedulerProbe = TestProbe[SchedulerMessage]("schedulerProbe")

      // we need to create another service, since we want to continue using the other in later tests
      val service = spawn(ExtPrimaryDataService(schedulerProbe.ref))

      val key =
        ScheduleLock.singleKey(TSpawner, schedulerProbe.ref, INIT_SIM_TICK)

      service ! ServiceMessage.Create(
        InitExtPrimaryData(extPrimaryDataConnection),
        key,
      )

      service ! WrappedActivation(Activation(INIT_SIM_TICK))

      service ! RegisterForWeatherMessage(
        systemParticipant.ref,
        51.4843281,
        7.4116482,
      )

      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(service.ref)
    }

    "correctly register a forwarded request" ignore {
      serviceRef ! PrimaryServiceRegistrationMessage(
        systemParticipant.ref,
        UUID.randomUUID(),
      )

      /* Wait for request approval */
      val msg =
        systemParticipant.expectMessageType[RegistrationSuccessfulMessage](
          10.seconds
        )
      msg.serviceRef shouldBe serviceRef.ref
      msg.firstDataTick shouldBe 0L

      /* We cannot directly check, if the requesting actor is among the subscribers, therefore we ask the actor to
       * provide data to all subscribed actors and check, if the subscribed probe gets one */
      serviceRef ! WrappedActivation(Activation(0))
      scheduler.expectMessageType[Completion]

      systemParticipant.expectMessageType[DataProvision[PrimaryData]]
    }
  }
}
