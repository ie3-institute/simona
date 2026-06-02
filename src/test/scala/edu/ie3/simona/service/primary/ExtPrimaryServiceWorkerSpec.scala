/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.api.data.connection.ExtPrimaryDataConnection
import edu.ie3.simona.api.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.ontology.primary.ProvidePrimaryData
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.ontology.messages.{
  Activation,
  SchedulerMessage,
  ServiceMessage,
}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.Data.PrimaryData.{ActivePower, ActivePowerExtra}
import edu.ie3.simona.service.primary.ExtPrimaryServiceWorker.InitExtPrimaryData
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

import java.util.{Optional, OptionalLong, UUID}
import scala.jdk.CollectionConverters.*

class ExtPrimaryServiceWorkerSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers
    with PrivateMethodTester
    with LazyLogging
    with TestSpawnerTyped {

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")
  private val extSimAdapter =
    TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

  private val systemParticipant =
    TestProbe[ServiceMessage.Response]("dummySystemParticipant")

  private val validUuid =
    UUID.fromString("b73a7e3f-9045-40cd-b518-c11a9a6a1025")
  private val invalidUuid =
    UUID.fromString("46be1e57-e4ed-4ef7-95f1-b2b321cb2047")

  "An uninitialized external primary data service" must {

    "send correct completion message after initialisation" in {
      val extPrimaryDataConnection = new ExtPrimaryDataConnection(
        Map(validUuid -> classOf[PValue]).asJava
      )

      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      val serviceRef = spawn(
        ExtPrimaryServiceWorker(
          scheduler.ref,
          InitExtPrimaryData(extPrimaryDataConnection),
          serviceKey,
        )
      )
      extPrimaryDataConnection.setActorRefs(serviceRef, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()
    }

    "refuse registration for wrong registration request" in {
      val schedulerProbe = TestProbe[SchedulerMessage]("schedulerProbe")

      val extPrimaryDataConnection = new ExtPrimaryDataConnection(
        Map(validUuid -> classOf[PValue]).asJava
      )

      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      // we need to create another service, since we want to continue using the other in later tests
      val serviceRef = spawn(
        ExtPrimaryServiceWorker(
          scheduler.ref,
          InitExtPrimaryData(extPrimaryDataConnection),
          serviceKey,
        )
      )
      extPrimaryDataConnection.setActorRefs(serviceRef, extSimAdapter.ref)

      serviceRef ! Activation(INIT_SIM_TICK)

      serviceRef ! PrimaryServiceRegistrationMessage(
        systemParticipant.ref,
        UUID.randomUUID(),
      )

      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(serviceRef.ref)
    }

    "refuse registration for unknown participant uuid" in {
      val schedulerProbe = TestProbe[SchedulerMessage]("schedulerProbe")

      val extPrimaryDataConnection = new ExtPrimaryDataConnection(
        Map(validUuid -> classOf[PValue]).asJava
      )

      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      // we need to create another service, since we want to continue using the other in later tests
      val serviceRef = spawn(
        ExtPrimaryServiceWorker(
          scheduler.ref,
          InitExtPrimaryData(extPrimaryDataConnection),
          serviceKey,
        )
      )
      extPrimaryDataConnection.setActorRefs(serviceRef, extSimAdapter.ref)

      serviceRef ! Activation(INIT_SIM_TICK)

      serviceRef ! PrimaryServiceRegistrationMessage(
        systemParticipant.ref,
        invalidUuid,
      )

      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(serviceRef.ref)
    }
  }

  "An external primary service actor" should {

    val extPrimaryDataConnection = new ExtPrimaryDataConnection(
      Map(validUuid -> classOf[PValue]).asJava
    )

    val serviceKey =
      ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
    // lock activation scheduled
    scheduler.expectMessageType[ScheduleActivation]

    val serviceRef = spawn(
      ExtPrimaryServiceWorker(
        scheduler.ref,
        InitExtPrimaryData(extPrimaryDataConnection),
        serviceKey,
      )
    )
    extPrimaryDataConnection.setActorRefs(serviceRef, extSimAdapter.ref)

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
          OptionalLong.of(900L),
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
