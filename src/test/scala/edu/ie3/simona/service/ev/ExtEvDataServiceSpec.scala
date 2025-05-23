/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.ev

import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.{
  DataProvision,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology._
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ontology.ControlResponseMessageFromExt
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  Create,
  RegisterForEvDataMessage,
  WrappedActivation,
  WrappedExternalMessage,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.test.common.{EvTestData, TestSpawnerTyped, UnitSpec}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.PowerSystemUnits
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.testkit.TestKit.awaitCond
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.language.implicitConversions

class ExtEvDataServiceSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with EvTestData
    with TestSpawnerTyped {

  implicit def wrap(msg: Activation): WrappedActivation =
    WrappedActivation(msg)

  implicit def wrap(
      msg: EvDataMessageFromExt
  ): WrappedExternalMessage = WrappedExternalMessage(msg)

  private val evcs1UUID =
    UUID.fromString("06a14909-366e-4e94-a593-1016e1455b30")
  private val evcs2UUID =
    UUID.fromString("104acdaa-5dc5-4197-aed2-2fddb3c4f237")

  "An uninitialized ev movement service" must {
    "send correct completion message after initialisation" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val evService = spawn(ExtEvDataService(scheduler.ref))
      val adapter = spawn(ExtEvDataService.adapter(evService))
      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(adapter, extSimAdapter.ref)

      /* INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(InitExtEvData(extEvData), key)

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))
    }

    "stash registration request and handle it correctly once initialized" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val evService = spawn(ExtEvDataService(scheduler.ref))
      val adapter = spawn(ExtEvDataService.adapter(evService))
      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(adapter, extSimAdapter.ref)

      val evcs1 = TestProbe[ParticipantAgent.Request]("evcs1")

      /* INIT */

      // this one should be stashed
      evService ! RegisterForEvDataMessage(evcs1.ref, evcs1UUID)

      evcs1.expectNoMessage()
      scheduler.expectNoMessage()

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(InitExtEvData(extEvData), key)

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))
    }
  }

  "An idle ev movements service" must {

    "handle duplicate registrations correctly" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val evService = spawn(ExtEvDataService(scheduler.ref))
      val adapter = spawn(ExtEvDataService.adapter(evService))
      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(adapter, extSimAdapter.ref)

      val evcs1 = TestProbe[ParticipantAgent.Request]("evcs1")
      val evcs2 = TestProbe[ParticipantAgent.Request]("evcs2")

      /* INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(InitExtEvData(extEvData), key)
      val activationMsg = scheduler.expectMessageType[ScheduleActivation]

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      evService ! RegisterForEvDataMessage(evcs1.ref, evcs1UUID)
      evcs1.expectNoMessage()

      evService ! RegisterForEvDataMessage(evcs2.ref, evcs2UUID)
      evcs2.expectNoMessage()

      // register first one again
      evService ! RegisterForEvDataMessage(evcs1.ref, evcs1UUID)
      evcs1.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          Some(long2Long(0L)).toJava,
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      scheduler.expectNoMessage()

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      evcs2.expectMessage(RegistrationSuccessfulMessage(evService, 0L))
    }

    "fail when activated without having received ExtEvMessage" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val evService = spawn(ExtEvDataService(scheduler.ref))
      val adapter = spawn(ExtEvDataService.adapter(evService))
      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(adapter, extSimAdapter.ref)

      /* INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(InitExtEvData(extEvData), key)

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      // we trigger ev service and expect an exception
      evService ! Activation(0L)
      scheduler.expectNoMessage()

      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(evService.ref)
    }

    "handle free lots requests correctly and forward them to the correct evcs" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val evService = spawn(ExtEvDataService(scheduler.ref))
      val adapter = spawn(ExtEvDataService.adapter(evService))
      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(adapter, extSimAdapter.ref)

      val evcs1 = TestProbe[ParticipantAgent.Request]("evcs1")
      val evcs2 = TestProbe[ParticipantAgent.Request]("evcs2")

      /* INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(InitExtEvData(extEvData), key)
      val activationMsg = scheduler.expectMessageType[ScheduleActivation]

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      evService ! RegisterForEvDataMessage(evcs1.ref, evcs1UUID)
      evcs1.expectNoMessage()

      evService ! RegisterForEvDataMessage(evcs2.ref, evcs2UUID)
      evcs2.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          Some(long2Long(0L)).toJava,
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      scheduler.expectNoMessage()

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      evcs2.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      /* TICK 0 */

      extEvData.sendExtMsg(
        new RequestEvcsFreeLots()
      )

      // ev service should receive request at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      // we trigger ev service
      evService ! Activation(0L)

      evcs1.expectMessage(EvFreeLotsRequest(0L, evService))

      evcs2.expectMessage(EvFreeLotsRequest(0L, evService))

      scheduler.expectMessage(Completion(activationMsg.actor))

      extEvData.receiveTriggerQueue shouldBe empty

      // return free lots to ev service
      evService ! FreeLotsResponse(evcs1UUID, 2)

      // nothing should happen yet, waiting for second departed ev
      extEvData.receiveTriggerQueue shouldBe empty

      evService ! FreeLotsResponse(evcs2UUID, 0)

      // ev service should recognize that all evcs that are expected are returned,
      // thus should send ProvideEvcsFreeLots
      awaitCond(
        !extEvData.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
      )
      extEvData.receiveTriggerQueue.size() shouldBe 1
      // only evcs 1 should be included, the other one is full
      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        Map(evcs1UUID -> int2Integer(2)).asJava
      )
    }

    "handle price requests correctly by returning dummy values" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val evService = spawn(ExtEvDataService(scheduler.ref))
      val adapter = spawn(ExtEvDataService.adapter(evService))
      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(adapter, extSimAdapter.ref)

      val evcs1 = TestProbe[ParticipantAgent.Request]("evcs1")
      val evcs2 = TestProbe[ParticipantAgent.Request]("evcs2")

      /* INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(InitExtEvData(extEvData), key)
      val activationMsg = scheduler.expectMessageType[ScheduleActivation]

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      evService ! RegisterForEvDataMessage(evcs1.ref, evcs1UUID)
      evcs1.expectNoMessage()

      evService ! RegisterForEvDataMessage(evcs2.ref, evcs2UUID)
      evcs2.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          Some(long2Long(0L)).toJava,
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      evcs2.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      /* TICK 0 */

      extEvData.sendExtMsg(new RequestCurrentPrices())

      // ev service should receive request at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      // we trigger ev service
      evService ! Activation(0L)

      evcs1.expectNoMessage()
      evcs2.expectNoMessage()

      // ev service should recognize that all evcs that are expected are returned,
      // thus should send ProvideEvcsFreeLots
      awaitCond(
        !extEvData.receiveTriggerQueue.isEmpty,
        max = 10.seconds,
      )
      extEvData.receiveTriggerQueue.size() shouldBe 1
      // only evcs 1 should be included, the other one is full
      extEvData.receiveTriggerQueue.take() shouldBe new ProvideCurrentPrices(
        Map(
          evcs1UUID -> double2Double(0d),
          evcs2UUID -> double2Double(0d),
        ).asJava
      )

      scheduler.expectMessage(Completion(activationMsg.actor))
    }

    "return free lots requests right away if there are no evcs registered" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val evService = spawn(ExtEvDataService(scheduler.ref))
      val adapter = spawn(ExtEvDataService.adapter(evService))
      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(adapter, extSimAdapter.ref)

      /* INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(InitExtEvData(extEvData), key)

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      /* TICK 0 */

      extEvData.sendExtMsg(new RequestEvcsFreeLots())

      // ev service should receive movements msg at this moment
      // scheduler receives schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      // we trigger ev service
      evService ! Activation(0L)

      scheduler.expectMessage(
        Completion(activationMsg.actor)
      )

      // ev service should send ProvideEvcsFreeLots right away
      awaitCond(
        !extEvData.receiveTriggerQueue.isEmpty,
        max = 10.seconds,
      )
      extEvData.receiveTriggerQueue.size() shouldBe 1
      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots()
    }

    "handle ev departure requests correctly and return departed evs" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val evService = spawn(ExtEvDataService(scheduler.ref))
      val adapter = spawn(ExtEvDataService.adapter(evService))
      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(adapter, extSimAdapter.ref)

      val evcs1 = TestProbe[ParticipantAgent.Request]("evcs1")
      val evcs2 = TestProbe[ParticipantAgent.Request]("evcs2")

      /* INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(InitExtEvData(extEvData), key)
      val activationMsg = scheduler.expectMessageType[ScheduleActivation]

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      evService ! RegisterForEvDataMessage(evcs1.ref, evcs1UUID)
      evcs1.expectNoMessage()

      evService ! RegisterForEvDataMessage(evcs2.ref, evcs2UUID)
      evcs2.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          Some(long2Long(0L)).toJava,
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      evcs2.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      /* TICK 0 */

      val departures = Map(
        evcs1UUID -> List(evA.getUuid).asJava,
        evcs2UUID -> List(evB.getUuid).asJava,
      ).asJava

      extEvData.sendExtMsg(
        new RequestDepartingEvs(departures)
      )

      // ev service should receive departure msg at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      // we trigger ev service
      evService ! Activation(0L)

      evcs1.expectMessage(
        DepartingEvsRequest(0L, Seq(evA.getUuid), evService)
      )
      evcs2.expectMessage(
        DepartingEvsRequest(0L, Seq(evB.getUuid), evService)
      )

      scheduler.expectMessage(Completion(activationMsg.actor))

      // return evs to ev service
      val updatedEvA = evA.copyWith(
        Quantities.getQuantity(6.0, PowerSystemUnits.KILOWATTHOUR)
      )

      evService ! DepartingEvsResponse(
        evcs1UUID,
        Seq(EvModelWrapper(updatedEvA)),
      )

      // nothing should happen yet, waiting for second departed ev
      extEvData.receiveTriggerQueue shouldBe empty

      val updatedEvB = evB.copyWith(
        Quantities.getQuantity(4.0, PowerSystemUnits.KILOWATTHOUR)
      )

      evService ! DepartingEvsResponse(
        evcs2UUID,
        Seq(EvModelWrapper(updatedEvB)),
      )

      // ev service should recognize that all evs that are expected are returned,
      // thus should send ProvideDepartingEvs
      awaitCond(
        !extEvData.receiveTriggerQueue.isEmpty,
        max = 10.seconds,
      )
      extEvData.receiveTriggerQueue.size() shouldBe 1
      extEvData.receiveTriggerQueue.take() shouldBe new ProvideDepartingEvs(
        List[EvModel](updatedEvA, updatedEvB).asJava
      )
    }

    "return ev departure requests right away if request list is empty" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val evService = spawn(ExtEvDataService(scheduler.ref))
      val adapter = spawn(ExtEvDataService.adapter(evService))
      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(adapter, extSimAdapter.ref)

      /* INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(InitExtEvData(extEvData), key)
      val activationMsg = scheduler.expectMessageType[ScheduleActivation]

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      /* TICK 0 */

      extEvData.sendExtMsg(
        new RequestDepartingEvs(Map.empty[UUID, java.util.List[UUID]].asJava)
      )

      // ev service should receive departure msg at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      // we trigger ev service
      evService ! Activation(0L)

      scheduler.expectMessage(Completion(activationMsg.actor))

      // ev service should send ProvideDepartingEvs right away
      awaitCond(
        !extEvData.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
      )
      extEvData.receiveTriggerQueue.size() shouldBe 1
      extEvData.receiveTriggerQueue.take() shouldBe new ProvideDepartingEvs(
        List.empty[EvModel].asJava
      )
    }

    "handle ev arrivals correctly and forward them to the correct evcs" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val evService = spawn(ExtEvDataService(scheduler.ref))
      val adapter = spawn(ExtEvDataService.adapter(evService))
      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(adapter, extSimAdapter.ref)

      val evcs1 = TestProbe[ParticipantAgent.Request]("evcs1")
      val evcs2 = TestProbe[ParticipantAgent.Request]("evcs2")

      /* INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(InitExtEvData(extEvData), key)
      val activationMsg = scheduler.expectMessageType[ScheduleActivation]

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessageType[Completion]

      evService ! RegisterForEvDataMessage(evcs1.ref, evcs1UUID)
      evcs1.expectNoMessage()

      evService ! RegisterForEvDataMessage(evcs2.ref, evcs2UUID)
      evcs2.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          Some(long2Long(0L)).toJava,
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      evcs2.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      /* TICK 0 */

      val arrivals = Map(
        evcs1UUID -> List[EvModel](evA).asJava,
        evcs2UUID -> List[EvModel](evB).asJava,
      ).asJava

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(arrivals, None.toJava)
      )

      // ev service should receive movements msg at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      // we trigger ev service
      evService ! Activation(0L)

      val evsMessage1 =
        evcs1.expectMessageType[DataProvision[EvData]]
      evsMessage1.tick shouldBe 0L
      evsMessage1.data shouldBe ArrivingEvs(
        Seq(EvModelWrapper(evA))
      )

      val evsMessage2 =
        evcs2.expectMessageType[DataProvision[EvData]]
      evsMessage2.tick shouldBe 0L
      evsMessage2.data shouldBe ArrivingEvs(
        Seq(EvModelWrapper(evB))
      )

      scheduler.expectMessage(Completion(activationMsg.actor))

      // no response expected
      extEvData.receiveTriggerQueue shouldBe empty
    }

    "skip a movements provision from an evcs that is not registered" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val evService = spawn(ExtEvDataService(scheduler.ref))
      val adapter = spawn(ExtEvDataService.adapter(evService))
      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(adapter, extSimAdapter.ref)

      val evcs1 = TestProbe[ParticipantAgent.Request]("evcs1")

      /* INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(InitExtEvData(extEvData), key)
      val activationMsg = scheduler.expectMessageType[ScheduleActivation]

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessageType[Completion]

      evService ! RegisterForEvDataMessage(evcs1.ref, evcs1UUID)
      evcs1.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          Some(long2Long(0L)).toJava,
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      /* TICK 0 */

      val arrivals = Map(
        evcs1UUID -> List[EvModel](evA).asJava,
        evcs2UUID -> List[EvModel](evB).asJava,
      ).asJava

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(arrivals, None.toJava)
      )

      // ev service should receive movements msg at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessageType[ScheduleDataServiceMessage]

      // we trigger ev service
      evService ! Activation(0L)

      val evsMessage1 =
        evcs1.expectMessageType[DataProvision[EvData]]
      evsMessage1.tick shouldBe 0L
      evsMessage1.data shouldBe ArrivingEvs(
        Seq(EvModelWrapper(evA))
      )

      scheduler.expectMessage(Completion(activationMsg.actor))

      // no response expected
      extEvData.receiveTriggerQueue shouldBe empty
    }
  }
}
