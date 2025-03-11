/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.api.data.em.ExtEmDataConnection
import edu.ie3.simona.api.data.em.model.{EmSetPointResult, FlexOptionValue}
import edu.ie3.simona.api.data.em.ontology.{
  EmSetPointDataResponse,
  FlexOptionsResponse,
  ProvideEmFlexOptionData,
  ProvideEmSetPointData,
  RequestEmFlexResults,
  RequestEmSetPoints,
}
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexActivation,
  IssuePowerControl,
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.EmMessage.{
  WrappedFlexRequest,
  WrappedFlexResponse,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegisterForEmDataService
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.test.common.{TestKitWithShutdown, TestSpawnerClassic}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.QuantityUtils._
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.{TestActorRef, TestProbe}
import org.scalatest.wordspec.AnyWordSpecLike
import squants.energy.Kilowatts

import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOption

class ExtEmDataServiceSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "ExtEvDataServiceSpec",
        ConfigFactory
          .parseString("""
        |pekko.loggers = ["org.apache.pekko.testkit.TestEventListener"]
        |pekko.loglevel = "INFO"
        |""".stripMargin),
      )
    )
    with AnyWordSpecLike
    with EmInputTestData
    with TestSpawnerClassic {

  implicit val simulationStart: ZonedDateTime = ZonedDateTime.now()

  private val emAgent1UUID =
    UUID.fromString("06a14909-366e-4e94-a593-1016e1455b30")
  private val emAgent2UUID =
    UUID.fromString("104acdaa-5dc5-4197-aed2-2fddb3c4f237")

  "An uninitialized em service" must {
    "send correct completion message after initialisation" in {
      val scheduler = TestProbe("scheduler")
      val extSimAdapter = TestProbe("extSimAdapter")

      val emService = TestActorRef(new ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(Map.empty[String, UUID].asJava)
      extEmDataConnection.setActorRefs(emService, extSimAdapter.ref)

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        emService,
        SimonaService.Create(InitExtEmData(extEmDataConnection), key),
      )
      scheduler.expectMsg(
        ScheduleActivation(emService.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(emService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(emService.toTyped))
    }

    "stash registration request and handle it correctly once initialized" in {
      val scheduler = TestProbe("scheduler")
      val extSimAdapter = TestProbe("extSimAdapter")

      val emService = TestActorRef(new ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(Map.empty[String, UUID].asJava)
      extEmDataConnection.setActorRefs(emService, extSimAdapter.ref)

      val emAgent = TestProbe("emAgent")

      // this one should be stashed
      emAgent.send(
        emService,
        RegisterForEmDataService(
          emInput.getUuid,
          emAgent.ref.toTyped,
          emAgent.ref.toTyped,
          None,
          None,
        ),
      )

      scheduler.expectNoMessage()

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        emService,
        SimonaService.Create(InitExtEmData(extEmDataConnection), key),
      )
      scheduler.expectMsg(
        ScheduleActivation(emService.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(emService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(emService.toTyped))
    }
  }

  "An idle em service" must {

    "fail when activated without having received ExtEmMessage" in {
      val scheduler = TestProbe("scheduler")
      val extSimAdapter = TestProbe("extSimAdapter")

      val emService = TestActorRef(new ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(Map.empty[String, UUID].asJava)
      extEmDataConnection.setActorRefs(emService, extSimAdapter.ref)

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        emService,
        SimonaService.Create(InitExtEmData(extEmDataConnection), key),
      )
      scheduler.expectMsg(
        ScheduleActivation(emService.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(emService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(emService.toTyped))

      // we trigger ev service and expect an exception
      assertThrows[ServiceException] {
        emService.receive(
          Activation(0),
          scheduler.ref,
        )
      }

      scheduler.expectNoMessage()
    }

    "handle flex option request correctly" in {
      val scheduler = TestProbe("scheduler")
      val extSimAdapter = TestProbe("extSimAdapter")

      val emService = TestActorRef(new ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(Map.empty[String, UUID].asJava)
      extEmDataConnection.setActorRefs(emService, extSimAdapter.ref)

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        emService,
        SimonaService.Create(InitExtEmData(extEmDataConnection), key),
      )
      scheduler.expectMsgType[ScheduleActivation]

      scheduler.send(emService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(emService.toTyped))

      val emAgent1 = TestProbe("emAgent1")
      val emAgent2 = TestProbe("emAgent2")

      emAgent1.send(
        emService,
        RegisterForEmDataService(
          emAgent1UUID,
          emAgent1.ref.toTyped,
          emAgent1.ref.toTyped,
          None,
          None,
        ),
      )
      emAgent1.expectNoMessage()

      emAgent2.send(
        emService,
        RegisterForEmDataService(
          emAgent2UUID,
          emAgent2.ref.toTyped,
          emAgent2.ref.toTyped,
          None,
          None,
        ),
      )
      emAgent2.expectNoMessage()

      extEmDataConnection.sendExtMsg(
        new RequestEmFlexResults(
          INIT_SIM_TICK,
          List.empty.asJava,
        )
      )

      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(emService))
      scheduler.send(emService, Activation(INIT_SIM_TICK))

      emAgent1.expectNoMessage()
      emAgent2.expectNoMessage()

      scheduler.expectMsg(Completion(emService.toTyped))

      extEmDataConnection.sendExtMsg(
        new RequestEmFlexResults(
          0,
          List(emAgent1UUID).asJava,
        )
      )

      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(emService))
      scheduler.send(emService, Activation(0))

      emAgent1.expectMsg(FlexActivation(0))
      emAgent2.expectNoMessage()

      scheduler.expectMsg(Completion(emService.toTyped))

      extEmDataConnection.receiveTriggerQueue shouldBe empty

      emAgent1.send(
        emService,
        WrappedFlexResponse(
          ProvideMinMaxFlexOptions(
            emAgent1UUID,
            Kilowatts(5),
            Kilowatts(0),
            Kilowatts(10),
          ),
          Left(emAgent1UUID),
        ),
      )

      awaitCond(
        !extEmDataConnection.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
        message = "No message received",
      )

      extEmDataConnection.receiveTriggerQueue.size() shouldBe 1

      extEmDataConnection.receiveTriggerQueue
        .take() shouldBe new FlexOptionsResponse(
        Map(
          emAgent1UUID -> new FlexOptionsResult(
            simulationStart,
            emAgent1UUID,
            0.asKiloWatt,
            5.asKiloWatt,
            10.asKiloWatt,
          )
        ).asJava
      )
    }

    "handle flex option provision correctly" in {
      val scheduler = TestProbe("scheduler")
      val extSimAdapter = TestProbe("extSimAdapter")

      val emService = TestActorRef(new ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(Map.empty[String, UUID].asJava)
      extEmDataConnection.setActorRefs(emService, extSimAdapter.ref)

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        emService,
        SimonaService.Create(InitExtEmData(extEmDataConnection), key),
      )
      scheduler.expectMsgType[ScheduleActivation]

      scheduler.send(emService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(emService.toTyped))

      val emAgent1 = TestProbe("emAgent1")
      val emAgent2 = TestProbe("emAgent2")

      emAgent1.send(
        emService,
        RegisterForEmDataService(
          emAgent1UUID,
          emAgent1.ref.toTyped,
          emAgent1.ref.toTyped,
          None,
          None,
        ),
      )
      emAgent1.expectNoMessage()

      emAgent2.send(
        emService,
        RegisterForEmDataService(
          emAgent2UUID,
          emAgent2.ref.toTyped,
          emAgent2.ref.toTyped,
          None,
          None,
        ),
      )
      emAgent2.expectNoMessage()

      extEmDataConnection.sendExtMsg(
        new ProvideEmFlexOptionData(
          0,
          Map(
            emAgent2UUID -> new FlexOptionValue(
              emAgent1UUID,
              -3.asKiloWatt,
              -1.asKiloWatt,
              1.asKiloWatt,
            )
          ).asJava,
          None.toJava,
        )
      )

      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(emService))
      scheduler.send(emService, Activation(0))

      emAgent1.expectNoMessage()
      emAgent2.expectMsg(
        ProvideMinMaxFlexOptions(
          emAgent1UUID,
          Kilowatts(-1),
          Kilowatts(-3),
          Kilowatts(1),
        )
      )

      scheduler.expectMsg(Completion(emService.toTyped))
    }

    "handle set point provision correctly" in {
      val scheduler = TestProbe("scheduler")
      val extSimAdapter = TestProbe("extSimAdapter")

      val emService = TestActorRef(new ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(Map.empty[String, UUID].asJava)
      extEmDataConnection.setActorRefs(emService, extSimAdapter.ref)

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        emService,
        SimonaService.Create(InitExtEmData(extEmDataConnection), key),
      )
      scheduler.expectMsgType[ScheduleActivation]

      scheduler.send(emService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(emService.toTyped))

      val emAgent1 = TestProbe("emAgent1")
      val emAgent2 = TestProbe("emAgent2")

      emAgent1.send(
        emService,
        RegisterForEmDataService(
          emAgent1UUID,
          emAgent1.ref.toTyped,
          emAgent1.ref.toTyped,
          None,
          None,
        ),
      )
      emAgent1.expectNoMessage()

      emAgent2.send(
        emService,
        RegisterForEmDataService(
          emAgent2UUID,
          emAgent2.ref.toTyped,
          emAgent2.ref.toTyped,
          None,
          None,
        ),
      )
      emAgent2.expectNoMessage()

      extEmDataConnection.sendExtMsg(
        new ProvideEmSetPointData(
          0,
          Map(
            emAgent1UUID -> new PValue(-3.asKiloWatt),
            emAgent2UUID -> new PValue(0.asKiloWatt),
          ).asJava,
          None.toJava,
        )
      )

      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(emService))
      scheduler.send(emService, Activation(0))

      emAgent1.expectMsg(
        IssuePowerControl(0, Kilowatts(-3))
      )
      emAgent2.expectMsg(
        IssuePowerControl(0, zeroKW)
      )

      scheduler.expectMsg(Completion(emService.toTyped))
    }

    "handle set point request correctly" in {
      val scheduler = TestProbe("scheduler")
      val extSimAdapter = TestProbe("extSimAdapter")

      val emService = TestActorRef(new ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(Map.empty[String, UUID].asJava)
      extEmDataConnection.setActorRefs(emService, extSimAdapter.ref)

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        emService,
        SimonaService.Create(InitExtEmData(extEmDataConnection), key),
      )
      scheduler.expectMsgType[ScheduleActivation]

      scheduler.send(emService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(emService.toTyped))

      val emAgent1 = TestProbe("emAgent1")
      val emAgent2 = TestProbe("emAgent2")

      emAgent1.send(
        emService,
        RegisterForEmDataService(
          emAgent1UUID,
          emAgent1.ref.toTyped,
          emAgent1.ref.toTyped,
          None,
          None,
        ),
      )
      emAgent1.expectNoMessage()

      emAgent2.send(
        emService,
        RegisterForEmDataService(
          emAgent2UUID,
          emAgent2.ref.toTyped,
          emAgent2.ref.toTyped,
          None,
          None,
        ),
      )
      emAgent2.expectNoMessage()

      // parent em normally sets itself a set point to 0 kW
      // we replace this behavior with the external data service
      extEmDataConnection.sendExtMsg(
        new ProvideEmSetPointData(
          0,
          Map(emAgent2UUID -> new PValue(0.asKiloWatt)).asJava,
          None.toJava,
        )
      )

      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(emService))
      scheduler.send(emService, Activation(0))

      emAgent1.expectNoMessage()
      emAgent2.expectMsg(
        IssuePowerControl(0, zeroKW)
      )

      scheduler.expectMsg(Completion(emService.toTyped))

      // we now request the set points of the controlled em agent
      extEmDataConnection.sendExtMsg(
        new RequestEmSetPoints(
          0,
          List(emAgent1UUID).asJava,
        )
      )

      extSimAdapter.expectMsg(new ScheduleDataServiceMessage(emService))
      scheduler.send(emService, Activation(0))

      emAgent1.expectNoMessage()
      emAgent2.expectNoMessage()

      scheduler.expectMsg(Completion(emService.toTyped))

      extEmDataConnection.receiveTriggerQueue shouldBe empty

      // the parent em agent sends the controlled em agent an IssuePowerControl message through the service
      emAgent2.send(
        emService,
        WrappedFlexRequest(
          IssuePowerControl(0, Kilowatts(2)),
          emAgent1.ref.toTyped,
        ),
      )

      awaitCond(
        !extEmDataConnection.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
        message = "No message received",
      )

      extEmDataConnection.receiveTriggerQueue.size() shouldBe 1

      extEmDataConnection.receiveTriggerQueue
        .take() shouldBe new EmSetPointDataResponse(
        Map(
          emAgent1UUID -> new EmSetPointResult(
            simulationStart,
            emAgent1UUID,
            Some(new PValue(2.asKiloWatt)).toJava,
          )
        ).asJava
      )
    }

  }
}
