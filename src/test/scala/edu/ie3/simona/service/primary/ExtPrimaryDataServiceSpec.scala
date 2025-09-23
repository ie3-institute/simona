/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.value.{PValue, Value}
import edu.ie3.simona.agent.participant.ParticipantAgent.{
  DataProvision,
  PrimaryRegistrationSuccessfulMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.api.data.connection.ExtPrimaryDataConnection
import edu.ie3.simona.api.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.ontology.primary.ProvidePrimaryData
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  Create,
  PrimaryServiceRegistrationMessage,
  SecondaryServiceRegistrationMessage,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.Data.PrimaryData.{ActivePower, ActivePowerExtra}
import edu.ie3.simona.service.primary.ExtPrimaryDataService.InitExtPrimaryData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.QuantityUtils.asKiloWatt
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike
import squants.energy.Kilowatts

import java.util.{Optional, UUID}
import scala.jdk.CollectionConverters.*

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

  private val systemParticipant = TestProbe[Any]("dummySystemParticipant")

  private val validUuid =
    UUID.fromString("b73a7e3f-9045-40cd-b518-c11a9a6a1025")
  private val invalidUuid =
    UUID.fromString("46be1e57-e4ed-4ef7-95f1-b2b321cb2047")

  "An uninitialized external primary data service" must {

    "send correct completion message after initialisation" in {
      val extPrimaryDataConnection = new ExtPrimaryDataConnection(
        Map(validUuid -> classOf[PValue]).asJava
      )

      val primaryDataService = spawn(ExtPrimaryDataService(scheduler.ref))
      extPrimaryDataConnection.setActorRefs(
        primaryDataService,
        extSimAdapter.ref,
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      extPrimaryDataConnection.setActorRefs(
        primaryDataService,
        extSimAdapter.ref,
      )

      primaryDataService ! Create(
        InitExtPrimaryData(extPrimaryDataConnection),
        key,
      )

      scheduler.expectMessage(
        ScheduleActivation(primaryDataService, INIT_SIM_TICK, Some(key))
      )

      primaryDataService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(primaryDataService))
    }

    "refuse registration for wrong registration request" in {
      val schedulerProbe = TestProbe[SchedulerMessage]("schedulerProbe")

      val extPrimaryDataConnection = new ExtPrimaryDataConnection(
        Map(validUuid -> classOf[PValue]).asJava
      )

      // we need to create another service, since we want to continue using the other in later tests
      val service = spawn(ExtPrimaryDataService(schedulerProbe.ref))
      extPrimaryDataConnection.setActorRefs(service, extSimAdapter.ref)

      val key =
        ScheduleLock.singleKey(TSpawner, schedulerProbe.ref, INIT_SIM_TICK)

      service ! Create(
        InitExtPrimaryData(extPrimaryDataConnection),
        key,
      )

      service ! Activation(INIT_SIM_TICK)

      service ! PrimaryServiceRegistrationMessage(
        systemParticipant.ref,
        UUID.randomUUID(),
      )

      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(service.ref)
    }

    "refuse registration for unknown participant uuid" in {
      val schedulerProbe = TestProbe[SchedulerMessage]("schedulerProbe")

      val extPrimaryDataConnection = new ExtPrimaryDataConnection(
        Map(validUuid -> classOf[PValue]).asJava
      )

      // we need to create another service, since we want to continue using the other in later tests
      val service = spawn(ExtPrimaryDataService(schedulerProbe.ref))
      extPrimaryDataConnection.setActorRefs(service, extSimAdapter.ref)

      val key =
        ScheduleLock.singleKey(TSpawner, schedulerProbe.ref, INIT_SIM_TICK)

      service ! Create(
        InitExtPrimaryData(extPrimaryDataConnection),
        key,
      )

      service ! Activation(INIT_SIM_TICK)

      service ! SecondaryServiceRegistrationMessage(
        systemParticipant.ref,
        invalidUuid,
      )

      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(service.ref)
    }
  }

  "An external primary service actor" should {

    val extPrimaryDataConnection = new ExtPrimaryDataConnection(
      Map(validUuid -> classOf[PValue]).asJava
    )

    val serviceRef = spawn(ExtPrimaryDataService(scheduler.ref))
    extPrimaryDataConnection.setActorRefs(serviceRef, extSimAdapter.ref)

    "init the service actor" in {
      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      serviceRef ! Create(InitExtPrimaryData(extPrimaryDataConnection), key)

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      serviceRef ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(serviceRef, None))
    }

    "correctly register a forwarded request" in {
      serviceRef ! PrimaryServiceRegistrationMessage(
        systemParticipant.ref,
        validUuid,
      )

      systemParticipant.expectMessage(
        PrimaryRegistrationSuccessfulMessage(serviceRef, 0L, ActivePowerExtra)
      )
    }

    "announce primary data correctly" in {
      extPrimaryDataConnection.sendExtMsg(
        new ProvidePrimaryData(
          0L,
          Map(validUuid -> new PValue(10.asKiloWatt)).asJava,
          Optional.of(900L),
        )
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(serviceRef))
      serviceRef ! Activation(0)

      systemParticipant.expectMessage(
        DataProvision(
          0L,
          serviceRef,
          ActivePower(Kilowatts(10)),
          Some(900L),
        )
      )
    }

  }
}
