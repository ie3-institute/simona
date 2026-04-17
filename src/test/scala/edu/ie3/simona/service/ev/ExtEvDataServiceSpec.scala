/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.ev

import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.api.data.connection.ExtEvDataConnection
import edu.ie3.simona.api.data.model.ev.EvModel
import edu.ie3.simona.api.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.ontology.ev.*
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.Data.SecondaryData.ArrivingEvs
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.common.{TestSpawnerTyped, UnitSpec}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.PowerSystemUnits
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.testkit.TestKit.awaitCond
import tech.units.indriya.quantity.Quantities

import java.util.{OptionalLong, UUID}
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions

class ExtEvDataServiceSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with EvcsInputTestData
    with TestSpawnerTyped {

  private val evcs1UUID =
    UUID.fromString("06a14909-366e-4e94-a593-1016e1455b30")
  private val evcs2UUID =
    UUID.fromString("104acdaa-5dc5-4197-aed2-2fddb3c4f237")

  "An uninitialized ev movement service" must {
    "send correct completion message after initialisation" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val extEvData = new ExtEvDataConnection()
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService =
        spawn(
          ExtEvDataService(scheduler.ref, InitExtEvData(extEvData), serviceKey)
        )
      extEvData.setActorRefs(evService, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()
    }

    "stash registration request and handle it correctly once initialized" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val extEvData = new ExtEvDataConnection()
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService =
        spawn(
          ExtEvDataService(scheduler.ref, InitExtEvData(extEvData), serviceKey)
        )
      extEvData.setActorRefs(evService, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      val evcs1 = TestProbe[ParticipantAgent.Message]("evcs1")

      /* INIT */

      // this one should be stashed
      evService ! SecondaryServiceRegistrationMessage(
        evcs1.ref,
        DataTimeType.Current,
        evcs1UUID,
      )

      evcs1.expectNoMessage()
      scheduler.expectNoMessage()
    }
  }

  "An idle ev movements service" must {

    "handle duplicate registrations correctly" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val extEvData = new ExtEvDataConnection()
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService =
        spawn(
          ExtEvDataService(scheduler.ref, InitExtEvData(extEvData), serviceKey)
        )
      extEvData.setActorRefs(evService, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      val evcs1 = TestProbe[ParticipantAgent.Message]("evcs1")
      val evcs2 = TestProbe[ParticipantAgent.Message]("evcs2")

      /* INIT */

      evService ! SecondaryServiceRegistrationMessage(
        evcs1.ref,
        DataTimeType.Current,
        evcs1UUID,
      )
      evcs1.expectNoMessage()

      evService ! SecondaryServiceRegistrationMessage(
        evcs2.ref,
        DataTimeType.Current,
        evcs2UUID,
      )
      evcs2.expectNoMessage()

      // register first one again
      evService ! SecondaryServiceRegistrationMessage(
        evcs1.ref,
        DataTimeType.Current,
        evcs1UUID,
      )
      evcs1.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          OptionalLong.of(0L),
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      scheduler.expectNoMessage()

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(evService))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      evcs2.expectMessage(RegistrationSuccessfulMessage(evService, 0L))
    }

    "fail when activated without having received ExtEvMessage" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val extEvData = new ExtEvDataConnection()
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService =
        spawn(
          ExtEvDataService(scheduler.ref, InitExtEvData(extEvData), serviceKey)
        )
      extEvData.setActorRefs(evService, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

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

      val extEvData = new ExtEvDataConnection()
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService =
        spawn(
          ExtEvDataService(scheduler.ref, InitExtEvData(extEvData), serviceKey)
        )
      extEvData.setActorRefs(evService, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      val evcs1 = TestProbe[ParticipantAgent.Message]("evcs1")
      val evcs2 = TestProbe[ParticipantAgent.Message]("evcs2")

      /* INIT */

      evService ! SecondaryServiceRegistrationMessage(
        evcs1.ref,
        DataTimeType.Current,
        evcs1UUID,
      )
      evcs1.expectNoMessage()

      evService ! SecondaryServiceRegistrationMessage(
        evcs2.ref,
        DataTimeType.Current,
        evcs2UUID,
      )
      evcs2.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          OptionalLong.of(0L),
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      scheduler.expectNoMessage()

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(evService))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      evcs2.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      /* TICK 0 */

      extEvData.sendExtMsg(
        new RequestEvcsFreeLots()
      )

      // ev service should receive request at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // we trigger ev service
      evService ! Activation(0L)

      evcs1.expectMessage(EvFreeLotsRequest(0L, evService))

      evcs2.expectMessage(EvFreeLotsRequest(0L, evService))

      scheduler.expectMessage(Completion(evService))

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

      val extEvData = new ExtEvDataConnection()
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService =
        spawn(
          ExtEvDataService(scheduler.ref, InitExtEvData(extEvData), serviceKey)
        )
      extEvData.setActorRefs(evService, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      val evcs1 = TestProbe[ParticipantAgent.Message]("evcs1")
      val evcs2 = TestProbe[ParticipantAgent.Message]("evcs2")

      /* INIT */

      evService ! SecondaryServiceRegistrationMessage(
        evcs1.ref,
        DataTimeType.Current,
        evcs1UUID,
      )
      evcs1.expectNoMessage()

      evService ! SecondaryServiceRegistrationMessage(
        evcs2.ref,
        DataTimeType.Current,
        evcs2UUID,
      )
      evcs2.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          OptionalLong.of(0L),
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(evService))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      evcs2.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      /* TICK 0 */

      extEvData.sendExtMsg(new RequestCurrentPrices())

      // ev service should receive request at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

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

      scheduler.expectMessage(Completion(evService))
    }

    "return free lots requests right away if there are no evcs registered" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val extEvData = new ExtEvDataConnection()
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService =
        spawn(
          ExtEvDataService(scheduler.ref, InitExtEvData(extEvData), serviceKey)
        )
      extEvData.setActorRefs(evService, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      /* TICK 0 */

      extEvData.sendExtMsg(new RequestEvcsFreeLots())

      // ev service should receive movements msg at this moment
      // scheduler receives schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // we trigger ev service
      evService ! Activation(0L)

      scheduler.expectMessage(Completion(evService))

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

      val extEvData = new ExtEvDataConnection()
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService =
        spawn(
          ExtEvDataService(scheduler.ref, InitExtEvData(extEvData), serviceKey)
        )
      extEvData.setActorRefs(evService, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      val evcs1 = TestProbe[ParticipantAgent.Message]("evcs1")
      val evcs2 = TestProbe[ParticipantAgent.Message]("evcs2")

      /* INIT */

      evService ! SecondaryServiceRegistrationMessage(
        evcs1.ref,
        DataTimeType.Current,
        evcs1UUID,
      )
      evcs1.expectNoMessage()

      evService ! SecondaryServiceRegistrationMessage(
        evcs2.ref,
        DataTimeType.Current,
        evcs2UUID,
      )
      evcs2.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          OptionalLong.of(0L),
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(evService))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      evcs2.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      /* TICK 0 */

      val departures = Map(
        evcs1UUID -> List(ev1.getUuid).asJava,
        evcs2UUID -> List(ev2.getUuid).asJava,
      ).asJava

      extEvData.sendExtMsg(
        new RequestDepartingEvs(departures)
      )

      // ev service should receive departure msg at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // we trigger ev service
      evService ! Activation(0L)

      evcs1.expectMessage(
        DepartingEvsRequest(0L, Seq(ev1.getUuid), evService)
      )
      evcs2.expectMessage(
        DepartingEvsRequest(0L, Seq(ev2.getUuid), evService)
      )

      scheduler.expectMessage(Completion(evService))

      // return evs to ev service
      val updatedEv1 = ev1.copyWith(
        Quantities.getQuantity(6.0, PowerSystemUnits.KILOWATTHOUR)
      )

      evService ! DepartingEvsResponse(
        evcs1UUID,
        Seq(EvModelWrapper(updatedEv1)),
      )

      // nothing should happen yet, waiting for second departed ev
      extEvData.receiveTriggerQueue shouldBe empty

      val updatedEv2 = ev2.copyWith(
        Quantities.getQuantity(4.0, PowerSystemUnits.KILOWATTHOUR)
      )

      evService ! DepartingEvsResponse(
        evcs2UUID,
        Seq(EvModelWrapper(updatedEv2)),
      )

      // ev service should recognize that all evs that are expected are returned,
      // thus should send ProvideDepartingEvs
      awaitCond(
        !extEvData.receiveTriggerQueue.isEmpty,
        max = 10.seconds,
      )
      extEvData.receiveTriggerQueue.size() shouldBe 1
      extEvData.receiveTriggerQueue.take() shouldBe new ProvideDepartingEvs(
        List[EvModel](updatedEv1, updatedEv2).asJava
      )
    }

    "return ev departure requests right away if request list is empty" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val extEvData = new ExtEvDataConnection()
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService =
        spawn(
          ExtEvDataService(scheduler.ref, InitExtEvData(extEvData), serviceKey)
        )
      extEvData.setActorRefs(evService, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      /* TICK 0 */

      extEvData.sendExtMsg(
        new RequestDepartingEvs(Map.empty[UUID, java.util.List[UUID]].asJava)
      )

      // ev service should receive departure msg at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // we trigger ev service
      evService ! Activation(0L)

      scheduler.expectMessage(Completion(evService))

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

      val extEvData = new ExtEvDataConnection()
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService =
        spawn(
          ExtEvDataService(scheduler.ref, InitExtEvData(extEvData), serviceKey)
        )
      extEvData.setActorRefs(evService, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      val evcs1 = TestProbe[ParticipantAgent.Message]("evcs1")
      val evcs2 = TestProbe[ParticipantAgent.Message]("evcs2")

      /* INIT */

      evService ! SecondaryServiceRegistrationMessage(
        evcs1.ref,
        DataTimeType.Current,
        evcs1UUID,
      )
      evcs1.expectNoMessage()

      evService ! SecondaryServiceRegistrationMessage(
        evcs2.ref,
        DataTimeType.Current,
        evcs2UUID,
      )
      evcs2.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          OptionalLong.of(0L),
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(evService))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      evcs2.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      /* TICK 0 */

      val arrivals = Map(
        evcs1UUID -> List[EvModel](ev1).asJava,
        evcs2UUID -> List[EvModel](ev2).asJava,
      ).asJava

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(arrivals, OptionalLong.empty())
      )

      // ev service should receive movements msg at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // we trigger ev service
      evService ! Activation(0L)

      val evsMessage1 = evcs1.expectMessageType[DataProvision]
      evsMessage1.tick shouldBe 0L
      evsMessage1.data shouldBe ArrivingEvs(
        Seq(EvModelWrapper(ev1))
      )

      val evsMessage2 = evcs2.expectMessageType[DataProvision]
      evsMessage2.tick shouldBe 0L
      evsMessage2.data shouldBe ArrivingEvs(
        Seq(EvModelWrapper(ev2))
      )

      scheduler.expectMessage(Completion(evService))

      // no response expected
      extEvData.receiveTriggerQueue shouldBe empty
    }

    "skip a movements provision from an evcs that is not registered" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val extEvData = new ExtEvDataConnection()
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService =
        spawn(
          ExtEvDataService(scheduler.ref, InitExtEvData(extEvData), serviceKey)
        )
      extEvData.setActorRefs(evService, extSimAdapter.ref)

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      val evcs1 = TestProbe[ParticipantAgent.Message]("evcs1")

      /* INIT */

      evService ! SecondaryServiceRegistrationMessage(
        evcs1.ref,
        DataTimeType.Current,
        evcs1UUID,
      )
      evcs1.expectNoMessage()

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(
          Map.empty[UUID, java.util.List[EvModel]].asJava,
          OptionalLong.of(0L),
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(evService))

      evcs1.expectMessage(RegistrationSuccessfulMessage(evService, 0L))

      /* TICK 0 */

      val arrivals = Map(
        evcs1UUID -> List[EvModel](ev1).asJava,
        evcs2UUID -> List[EvModel](ev2).asJava,
      ).asJava

      extEvData.sendExtMsg(
        new ProvideArrivingEvs(arrivals, OptionalLong.empty())
      )

      // ev service should receive movements msg at this moment
      // scheduler should receive schedule msg
      extSimAdapter.expectMessageType[ScheduleDataServiceMessage]

      // we trigger ev service
      evService ! Activation(0L)

      val evsMessage1 = evcs1.expectMessageType[DataProvision]
      evsMessage1.tick shouldBe 0L
      evsMessage1.data shouldBe ArrivingEvs(
        Seq(EvModelWrapper(ev1))
      )

      scheduler.expectMessage(Completion(evService))

      // no response expected
      extEvData.receiveTriggerQueue shouldBe empty
    }
  }
}
