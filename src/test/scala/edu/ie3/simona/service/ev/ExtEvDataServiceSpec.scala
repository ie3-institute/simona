/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.ev

import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{ActorRef, ActorSystem}
import org.apache.pekko.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.api.data.ev.ExtEvData
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology.*
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.ontology.messages.services.EvMessage.*
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.test.common.{
  EvTestData,
  TestKitWithShutdown,
  TestSpawnerClassic
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.wordspec.AnyWordSpecLike
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*

class ExtEvDataServiceSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "ExtEvDataServiceSpec",
        ConfigFactory
          .parseString("""
        |pekko.loggers = ["org.apache.pekko.testkit.TestEventListener"]
        |pekko.loglevel = "INFO"
        |""".stripMargin)
      )
    )
    with AnyWordSpecLike
    with EvTestData
    with TestSpawnerClassic {

  private val scheduler = TestProbe("scheduler")
  private val extSimAdapter = TestProbe("extSimAdapter")

  private val extEvData = (dataService: ActorRef) =>
    new ExtEvData(
      dataService,
      extSimAdapter.ref
    )

  private val evcs1UUID =
    UUID.fromString("06a14909-366e-4e94-a593-1016e1455b30")
  private val evcs2UUID =
    UUID.fromString("104acdaa-5dc5-4197-aed2-2fddb3c4f237")

  "An uninitialized ev movement service" must {
    "send correct completion message after initialisation" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        evService,
        SimonaService.Create(InitExtEvData(extEvData(evService)), key)
      )
      scheduler.expectMsg(
        ScheduleActivation(evService.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(evService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(evService.toTyped))
    }

    "stash registration request and handle it correctly once initialized" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val evcs1 = TestProbe("evcs1")

      // this one should be stashed
      evcs1.send(
        evService,
        RegisterForEvDataMessage(evcs1UUID)
      )

      evcs1.expectNoMessage()
      scheduler.expectNoMessage()

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        evService,
        SimonaService.Create(InitExtEvData(extEvData(evService)), key)
      )
      scheduler.expectMsg(
        ScheduleActivation(evService.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(evService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(evService.toTyped))

      evcs1.expectMsg(RegistrationSuccessfulMessage(None))
    }
  }

  "An idle ev movements service" must {
    // TODO enhance with tests for cases where no EVCS are applicable and answer is sent right away
    "handle duplicate registrations correctly" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        evService,
        SimonaService.Create(InitExtEvData(extEvData(evService)), key)
      )
      scheduler.expectMsgType[ScheduleActivation]

      scheduler.send(evService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(evService.toTyped))

      val evcs1 = TestProbe("evcs1")
      val evcs2 = TestProbe("evcs2")

      evcs1.send(
        evService,
        RegisterForEvDataMessage(evcs1UUID)
      )
      evcs1.expectMsg(RegistrationSuccessfulMessage(None))

      evcs2.send(
        evService,
        RegisterForEvDataMessage(evcs2UUID)
      )
      evcs2.expectMsg(RegistrationSuccessfulMessage(None))

      // register first one again
      evcs1.send(
        evService,
        RegisterForEvDataMessage(evcs1UUID)
      )
      evcs1.expectNoMessage()
      evcs2.expectNoMessage()
    }

    "fail when activated without having received ExtEvMessage" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        evService,
        SimonaService.Create(InitExtEvData(extEvData(evService)), key)
      )
      scheduler.expectMsg(
        ScheduleActivation(evService.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(evService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(evService.toTyped))

      // we trigger ev service and expect an exception
      assertThrows[ServiceException] {
        evService.receive(
          Activation(0),
          scheduler.ref
        )
      }

      scheduler.expectNoMessage()
    }

    "handle free lots requests correctly and forward them to the correct evcs" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val extData = extEvData(evService)

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        evService,
        SimonaService.Create(InitExtEvData(extData), key)
      )
      scheduler.expectMsgType[ScheduleActivation]

      scheduler.send(evService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(evService.toTyped))

      val evcs1 = TestProbe("evcs1")
      val evcs2 = TestProbe("evcs2")

      evcs1.send(
        evService,
        RegisterForEvDataMessage(evcs1UUID)
      )
      evcs1.expectMsgType[RegistrationSuccessfulMessage]

      evcs2.send(
        evService,
        RegisterForEvDataMessage(evcs2UUID)
      )
      evcs2.expectMsgType[RegistrationSuccessfulMessage]

      extData.sendExtMsg(
        new RequestEvcsFreeLots()
      )

      // ev service should receive request at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(evService))

      val tick = 0L

      // we trigger ev service
      scheduler.send(evService, Activation(tick))

      evcs1.expectMsg(
        EvFreeLotsRequest(tick)
      )

      evcs2.expectMsg(
        EvFreeLotsRequest(tick)
      )

      scheduler.expectMsg(Completion(evService.toTyped))

      extData.receiveTriggerQueue shouldBe empty

      // return free lots to ev service
      evcs1.send(
        evService,
        FreeLotsResponse(
          evcs1UUID,
          2
        )
      )

      // nothing should happen yet, waiting for second departed ev
      extData.receiveTriggerQueue shouldBe empty

      evcs2.send(
        evService,
        FreeLotsResponse(
          evcs2UUID,
          0
        )
      )

      // ev service should recognize that all evcs that are expected are returned,
      // thus should send ProvidePublicEvcs
      awaitCond(
        !extData.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
        message = "No message received"
      )
      extData.receiveTriggerQueue.size() shouldBe 1
      // only evcs 1 should be included, the other one is full
      extData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        Map(evcs1UUID -> Integer.valueOf(2)).asJava
      )
    }

    "return free lots requests right away if there are no evcs registered" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val extData = extEvData(evService)

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        evService,
        SimonaService.Create(InitExtEvData(extData), key)
      )
      scheduler.expectMsgType[ScheduleActivation]

      scheduler.send(evService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(evService.toTyped))

      extData.sendExtMsg(new RequestEvcsFreeLots())

      // ev service should receive movements msg at this moment
      // scheduler receives schedule msg
      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(evService))

      val tick = 0L

      // we trigger ev service
      scheduler.send(evService, Activation(tick))

      scheduler.expectMsg(Completion(evService.toTyped))

      // ev service should send ProvidePublicEvcs right away
      awaitCond(
        !extData.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
        message = "No message received"
      )
      extData.receiveTriggerQueue.size() shouldBe 1
      extData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots()
    }

    "handle ev departure requests correctly and return departed evs" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val extData = extEvData(evService)

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        evService,
        SimonaService.Create(InitExtEvData(extData), key)
      )
      scheduler.expectMsgType[ScheduleActivation]

      scheduler.send(evService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(evService.toTyped))

      val evcs1 = TestProbe("evcs1")
      val evcs2 = TestProbe("evcs1")

      evcs1.send(
        evService,
        RegisterForEvDataMessage(evcs1UUID)
      )
      evcs1.expectMsgType[RegistrationSuccessfulMessage]

      evcs2.send(
        evService,
        RegisterForEvDataMessage(evcs2UUID)
      )
      evcs2.expectMsgType[RegistrationSuccessfulMessage]

      val departures = Map(
        evcs1UUID -> List(evA.getUuid).asJava,
        evcs2UUID -> List(evB.getUuid).asJava
      ).asJava

      extData.sendExtMsg(
        new RequestDepartingEvs(departures)
      )

      // ev service should receive departure msg at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(evService))

      val tick = 0L

      // we trigger ev service
      scheduler.send(evService, Activation(tick))

      evcs1.expectMsg(
        DepartingEvsRequest(tick, scala.collection.immutable.Seq(evA.getUuid))
      )
      evcs2.expectMsg(
        DepartingEvsRequest(tick, scala.collection.immutable.Seq(evB.getUuid))
      )

      scheduler.expectMsg(Completion(evService.toTyped))

      // return evs to ev service
      val updatedEvA = evA.copyWith(
        Quantities.getQuantity(6.0, PowerSystemUnits.KILOWATTHOUR)
      )

      evcs1.send(
        evService,
        DepartingEvsResponse(evcs1UUID, Set(updatedEvA))
      )

      // nothing should happen yet, waiting for second departed ev
      extData.receiveTriggerQueue shouldBe empty

      val updatedEvB = evB.copyWith(
        Quantities.getQuantity(4.0, PowerSystemUnits.KILOWATTHOUR)
      )

      evcs2.send(
        evService,
        DepartingEvsResponse(evcs2UUID, Set(updatedEvB))
      )

      // ev service should recognize that all evs that are expected are returned,
      // thus should send AllDepartedEvsResponse
      awaitCond(
        !extData.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
        message = "No message received"
      )
      extData.receiveTriggerQueue.size() shouldBe 1
      extData.receiveTriggerQueue.take() shouldBe new ProvideDepartingEvs(
        List[EvModel](updatedEvA, updatedEvB).asJava
      )
    }

    "handle ev arrivals correctly and forward them to the correct evcs" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val extData = extEvData(evService)

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        evService,
        SimonaService.Create(InitExtEvData(extData), key)
      )
      scheduler.expectMsgType[ScheduleActivation]

      scheduler.send(evService, Activation(INIT_SIM_TICK))
      scheduler.expectMsgType[Completion]

      val evcs1 = TestProbe("evcs1")
      val evcs2 = TestProbe("evcs2")

      evcs1.send(
        evService,
        RegisterForEvDataMessage(evcs1UUID)
      )
      evcs1.expectMsgType[RegistrationSuccessfulMessage]

      evcs2.send(
        evService,
        RegisterForEvDataMessage(evcs2UUID)
      )
      evcs2.expectMsgType[RegistrationSuccessfulMessage]

      val arrivals = Map(
        evcs1UUID -> List[EvModel](evA).asJava,
        evcs2UUID -> List[EvModel](evB).asJava
      ).asJava

      extData.sendExtMsg(
        new ProvideArrivingEvs(arrivals)
      )

      // ev service should receive movements msg at this moment
      // scheduler receive schedule msg
      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(evService))

      val tick = 0L

      // we trigger ev service
      scheduler.send(evService, Activation(tick))
      // schedule lock is scheduled
      scheduler.expectMsgType[ScheduleActivation].tick shouldBe tick

      val evsMessage1 = evcs1.expectMsgType[ProvideEvDataMessage]
      evsMessage1.tick shouldBe tick
      evsMessage1.data shouldBe ArrivingEvsData(Seq(evA))
      evsMessage1.unlockKey should not be empty

      val evsMessage2 = evcs2.expectMsgType[ProvideEvDataMessage]
      evsMessage2.tick shouldBe tick
      evsMessage2.data shouldBe ArrivingEvsData(Seq(evB))
      evsMessage2.unlockKey should not be empty

      scheduler.expectMsg(Completion(evService.toTyped))

      // no response expected
      extData.receiveTriggerQueue shouldBe empty
    }

    "skip a movements provision from an evcs that is not registered" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val extData = extEvData(evService)

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        evService,
        SimonaService.Create(InitExtEvData(extData), key)
      )
      scheduler.expectMsgType[ScheduleActivation]

      scheduler.send(evService, Activation(INIT_SIM_TICK))
      scheduler.expectMsgType[Completion]

      val evcs1 = TestProbe("evcs1")

      evcs1.send(
        evService,
        RegisterForEvDataMessage(evcs1UUID)
      )
      evcs1.expectMsgType[RegistrationSuccessfulMessage]

      val arrivals = Map(
        evcs1UUID -> List[EvModel](evA).asJava,
        evcs2UUID -> List[EvModel](evB).asJava
      ).asJava

      extData.sendExtMsg(
        new ProvideArrivingEvs(arrivals)
      )

      // ev service should receive movements msg at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMsgType[ScheduleDataServiceMessage]

      val tick = 0L

      // we trigger ev service
      scheduler.send(evService, Activation(tick))
      // schedule lock is scheduled
      scheduler.expectMsgType[ScheduleActivation].tick shouldBe tick

      val evsMessage1 = evcs1.expectMsgType[ProvideEvDataMessage]
      evsMessage1.tick shouldBe tick
      evsMessage1.data shouldBe ArrivingEvsData(Seq(evA))
      evsMessage1.unlockKey should not be empty

      scheduler.expectMsg(Completion(evService.toTyped))

      // no response expected
      extData.receiveTriggerQueue shouldBe empty
    }
  }
}
