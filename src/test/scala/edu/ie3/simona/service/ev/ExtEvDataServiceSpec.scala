/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.ev

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.api.data.ev.ExtEvData
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology.EvMovementsMessage.EvcsMovements
import edu.ie3.simona.api.data.ev.ontology.builder.{
  EvMovementsMessageBuilder,
  EvcsMovementsBuilder
}
import edu.ie3.simona.api.data.ev.ontology.{
  AllDepartedEvsResponse,
  ProvideEvcsFreeLots,
  RequestEvcsFreeLots
}
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.EvMessage.{
  DepartedEvsResponse,
  EvFreeLotsRequest,
  EvMovementData,
  FreeLotsResponse,
  ProvideEvDataMessage,
  RegisterForEvDataMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeServiceTrigger
}
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.test.common.{EvTestData, TestKitWithShutdown}
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.wordspec.AnyWordSpecLike
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.{MapHasAsJava, SeqHasAsJava}

class ExtEvDataServiceSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "ExtEvDataServiceSpec",
        ConfigFactory
          .parseString("""
        |akka.loggers = ["akka.testkit.TestEventListener"]
        |akka.loglevel = "INFO"
        |""".stripMargin)
      )
    )
    with AnyWordSpecLike
    with EvTestData {

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

      val triggerId = 1L

      scheduler.send(
        evService,
        TriggerWithIdMessage(
          InitializeServiceTrigger(
            InitExtEvData(
              extEvData(evService)
            )
          ),
          triggerId,
          evService
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          None
        )
      )
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

      scheduler.send(
        evService,
        TriggerWithIdMessage(
          InitializeServiceTrigger(
            InitExtEvData(
              extEvData(evService)
            )
          ),
          1L,
          evService
        )
      )

      scheduler.expectMsgType[CompletionMessage]
      evcs1.expectMsg(RegistrationSuccessfulMessage(None))
    }
  }

  "An idle ev movements service" must {
    "handle duplicate registrations correctly" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      scheduler.send(
        evService,
        TriggerWithIdMessage(
          InitializeServiceTrigger(
            InitExtEvData(
              extEvData(evService)
            )
          ),
          1L,
          evService
        )
      )
      scheduler.expectMsgType[CompletionMessage]

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

      val extData = extEvData(evService)

      scheduler.send(
        evService,
        TriggerWithIdMessage(
          InitializeServiceTrigger(
            InitExtEvData(
              extData
            )
          ),
          1L,
          evService
        )
      )
      scheduler.expectMsgType[CompletionMessage]

      // we trigger ev service and expect an exception
      assertThrows[ServiceException] {
        evService.receive(
          TriggerWithIdMessage(
            ActivityStartTrigger(
              0L
            ),
            2L,
            evService
          ),
          scheduler.ref
        )
      }

      scheduler.expectNoMessage()
    }

    "handle ev movements provisions correctly and forward them to the correct evcs" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val extData = extEvData(evService)

      scheduler.send(
        evService,
        TriggerWithIdMessage(
          InitializeServiceTrigger(
            InitExtEvData(
              extData
            )
          ),
          1L,
          evService
        )
      )
      scheduler.expectMsgType[CompletionMessage]

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
        new EvMovementsMessageBuilder()
          .addArrival(evcs1UUID, evA)
          .addArrival(evcs2UUID, evB)
          .build()
      )

      // ev service should receive movements msg at this moment
      // scheduler receive schedule msg
      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(evService))

      val tick = 0L
      val triggerId = 2L

      // we trigger ev service
      scheduler.send(
        evService,
        TriggerWithIdMessage(
          ActivityStartTrigger(
            tick
          ),
          triggerId,
          evService
        )
      )

      evcs1.expectMsg(
        ProvideEvDataMessage(
          tick,
          EvMovementData(
            new EvcsMovementsBuilder().addArrival(evA).build()
          )
        )
      )

      evcs2.expectMsg(
        ProvideEvDataMessage(
          tick,
          EvMovementData(
            new EvcsMovementsBuilder().addArrival(evB).build()
          )
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(tick),
                evcs1.ref
              ),
              ScheduleTriggerMessage(
                ActivityStartTrigger(tick),
                evcs2.ref
              )
            )
          )
        )
      )

      // AllDepartedEvsResponse should be sent right away, since no evs departed
      awaitCond(
        !extData.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
        message = "No message received"
      )
      extData.receiveTriggerQueue.size() shouldBe 1
      extData.receiveTriggerQueue.take() shouldBe new AllDepartedEvsResponse()
    }

    "handle ev movements provisions correctly and return departed evs" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val extData = extEvData(evService)

      scheduler.send(
        evService,
        TriggerWithIdMessage(
          InitializeServiceTrigger(
            InitExtEvData(
              extData
            )
          ),
          1L,
          evService
        )
      )
      scheduler.expectMsgType[CompletionMessage]

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

      // first tick, arrivals
      extData.sendExtMsg(
        new EvMovementsMessageBuilder()
          .addArrival(evcs1UUID, evA)
          .addArrival(evcs2UUID, evB)
          .build()
      )

      // ev service should receive movements msg at this moment
      // scheduler receive schedule msg
      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(evService))

      val tick = 0L
      val triggerId = 2L

      // we trigger ev service
      scheduler.send(
        evService,
        TriggerWithIdMessage(
          ActivityStartTrigger(
            tick
          ),
          triggerId,
          evService
        )
      )

      evcs1.expectMsg(
        ProvideEvDataMessage(
          tick,
          EvMovementData(
            new EvcsMovements(
              List.empty[UUID].asJava,
              List[EvModel](evA).asJava
            )
          )
        )
      )
      evcs2.expectMsg(
        ProvideEvDataMessage(
          tick,
          EvMovementData(
            new EvcsMovementsBuilder().addArrival(evB).build()
          )
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(tick),
                evcs1.ref
              ),
              ScheduleTriggerMessage(
                ActivityStartTrigger(tick),
                evcs2.ref
              )
            )
          )
        )
      )

      // AllDepartedEvsResponse should be sent right away, since no evs departed
      awaitCond(
        !extData.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
        message = "No message received"
      )
      extData.receiveTriggerQueue.size() shouldBe 1
      extData.receiveTriggerQueue.take() shouldBe new AllDepartedEvsResponse()

      // next tick, departures
      extData.sendExtMsg(
        new EvMovementsMessageBuilder()
          .addDeparture(evcs1UUID, evA.getUuid)
          .addDeparture(evcs2UUID, evB.getUuid)
          .build()
      )

      // ev service should receive movements msg at this moment
      // scheduler receive schedule msg
      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(evService))

      val tick2 = 900L
      val triggerId2 = 2L

      // we trigger ev service
      scheduler.send(
        evService,
        TriggerWithIdMessage(
          ActivityStartTrigger(
            tick2
          ),
          triggerId2,
          evService
        )
      )

      evcs1.expectMsg(
        ProvideEvDataMessage(
          tick2,
          EvMovementData(
            new EvcsMovementsBuilder().addDeparture(evA.getUuid).build()
          )
        )
      )
      evcs2.expectMsg(
        ProvideEvDataMessage(
          tick2,
          EvMovementData(
            new EvcsMovementsBuilder().addDeparture(evB.getUuid).build()
          )
        )
      )

      scheduler.expectMsgType[CompletionMessage]

      extData.receiveTriggerQueue shouldBe empty

      // return evs to ev service
      val updatedEvA = evA.copyWith(
        Quantities.getQuantity(6, PowerSystemUnits.KILOWATTHOUR)
      )

      evcs1.send(
        evService,
        DepartedEvsResponse(
          evcs1UUID,
          Set(updatedEvA)
        )
      )

      // nothing should happen yet, waiting for second departed ev
      extData.receiveTriggerQueue shouldBe empty

      val updatedEvB = evB.copyWith(
        Quantities.getQuantity(4, PowerSystemUnits.KILOWATTHOUR)
      )

      evcs2.send(
        evService,
        DepartedEvsResponse(
          evcs2UUID,
          Set(updatedEvB)
        )
      )

      // ev service should recognize that all evs that are expected are returned,
      // thus should send AllDepartedEvsResponse
      awaitCond(
        !extData.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
        message = "No message received"
      )
      extData.receiveTriggerQueue.size() shouldBe 1
      extData.receiveTriggerQueue.take() shouldBe new AllDepartedEvsResponse(
        List[EvModel](updatedEvA, updatedEvB).asJava
      )
    }

    "skip a movements provision from an evcs that is not registered" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val extData = extEvData(evService)

      scheduler.send(
        evService,
        TriggerWithIdMessage(
          InitializeServiceTrigger(
            InitExtEvData(
              extData
            )
          ),
          1L,
          evService
        )
      )
      scheduler.expectMsgType[CompletionMessage]

      val evcs1 = TestProbe("evcs1")

      evcs1.send(
        evService,
        RegisterForEvDataMessage(evcs1UUID)
      )
      evcs1.expectMsgType[RegistrationSuccessfulMessage]

      extData.sendExtMsg(
        new EvMovementsMessageBuilder()
          .addArrival(evcs1UUID, evA)
          .addDeparture(evcs2UUID, evB.getUuid)
          .build()
      )

      // ev service should receive movements msg at this moment
      // scheduler receive schedule msg
      extSimAdapter.expectMsgType[ScheduleDataServiceMessage]

      val tick = 0L
      val triggerId = 2L

      // we trigger ev service
      scheduler.send(
        evService,
        TriggerWithIdMessage(
          ActivityStartTrigger(
            tick
          ),
          triggerId,
          evService
        )
      )

      evcs1.expectMsg(
        ProvideEvDataMessage(
          tick,
          EvMovementData(
            new EvcsMovementsBuilder().addArrival(evA).build()
          )
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(tick),
                evcs1.ref
              )
            )
          )
        )
      )

      // AllDepartedEvsResponse should be sent right away, since no evs departed (evcs2 was skipped)
      awaitCond(
        !extData.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
        message = "No message received"
      )
      extData.receiveTriggerQueue.size() shouldBe 1
      extData.receiveTriggerQueue.take() shouldBe new AllDepartedEvsResponse()
    }

    "handle free lots requests correctly and forward them to the correct evcs" in {
      val evService = TestActorRef(
        new ExtEvDataService(
          scheduler.ref
        )
      )

      val extData = extEvData(evService)

      scheduler.send(
        evService,
        TriggerWithIdMessage(
          InitializeServiceTrigger(
            InitExtEvData(
              extData
            )
          ),
          1L,
          evService
        )
      )
      scheduler.expectMsgType[CompletionMessage]

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
      // scheduler receives schedule msg
      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(evService))

      val tick = 0L
      val triggerId = 2L

      // we trigger ev service
      scheduler.send(
        evService,
        TriggerWithIdMessage(
          ActivityStartTrigger(
            tick
          ),
          triggerId,
          evService
        )
      )

      evcs1.expectMsg(
        EvFreeLotsRequest(tick)
      )

      evcs2.expectMsg(
        EvFreeLotsRequest(tick)
      )

      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          None
        )
      )

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

      scheduler.send(
        evService,
        TriggerWithIdMessage(
          InitializeServiceTrigger(
            InitExtEvData(
              extData
            )
          ),
          1L,
          evService
        )
      )
      scheduler.expectMsgType[CompletionMessage]

      extData.sendExtMsg(
        new RequestEvcsFreeLots()
      )

      // ev service should receive movements msg at this moment
      // scheduler receives schedule msg
      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(evService))

      val tick = 0L
      val triggerId = 2L

      // we trigger ev service
      scheduler.send(
        evService,
        TriggerWithIdMessage(
          ActivityStartTrigger(
            tick
          ),
          triggerId,
          evService
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          None
        )
      )

      // ev service should send ProvidePublicEvcs right away
      awaitCond(
        !extData.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
        message = "No message received"
      )
      extData.receiveTriggerQueue.size() shouldBe 1
      extData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots()
    }
  }
}
