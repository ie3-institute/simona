/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.em.model.{
  EmSetPointResult,
  ExtendedFlexOptionsResult,
  FlexOptions,
}
import edu.ie3.simona.api.data.em.ontology._
import edu.ie3.simona.api.data.em.{EmMode, ExtEmDataConnection}
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ontology.ControlResponseMessageFromExt
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexActivation,
  FlexRequest,
  IssuePowerControl,
  ProvideFlexOptions,
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.EmMessage.{
  WrappedFlexRequest,
  WrappedFlexResponse,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  Create,
  RegisterForEmDataService,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.QuantityUtils._
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.apache.pekko.testkit.TestKit.awaitCond
import org.scalatest.wordspec.AnyWordSpecLike
import squants.energy.Kilowatts

import java.time.ZonedDateTime
import java._
import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOption

class ExtEmDataServiceSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with EmInputTestData
    with TestSpawnerTyped {

  implicit val simulationStart: ZonedDateTime = ZonedDateTime.now()

  private val emptyControlled = List.empty[UUID].asJava

  private val emAgentSupUUID =
    UUID.fromString("d797fe9c-e4af-49a3-947d-44f81933887e")
  private val emAgent1UUID =
    UUID.fromString("06a14909-366e-4e94-a593-1016e1455b30")
  private val emAgent2UUID =
    UUID.fromString("104acdaa-5dc5-4197-aed2-2fddb3c4f237")

  "An uninitialized em service" must {
    "send correct completion message after initialisation" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService.apply(scheduler.ref))
      val adapter = spawn(ExtEmDataService.adapter(emService))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)
      extEmDataConnection.setActorRefs(
        adapter,
        extSimAdapter.ref,
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      emService ! Create(
        InitExtEmData(extEmDataConnection, simulationStart),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      val serviceActivation = activationMsg.actor

      serviceActivation ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(serviceActivation))
    }

    "stash registration request and handle it correctly once initialized" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService.apply(scheduler.ref))
      val adapter = spawn(ExtEmDataService.adapter(emService))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)
      extEmDataConnection.setActorRefs(
        adapter,
        extSimAdapter.ref,
      )

      val emAgent = TestProbe[EmAgent.Request]("emAgent")
      val emAgentFlex = TestProbe[FlexRequest]("emAgentFlex")

      // this one should be stashed
      emService ! RegisterForEmDataService(
        emInput.getUuid,
        emAgent.ref,
        emAgentFlex.ref,
        None,
        None,
      )

      scheduler.expectNoMessage()

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      emService ! Create(
        InitExtEmData(extEmDataConnection, simulationStart),
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

  "An idle em service" must {

    "fail when activated without having received ExtEmMessage" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService.apply(scheduler.ref))
      val adapter = spawn(ExtEmDataService.adapter(emService))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)
      extEmDataConnection.setActorRefs(
        adapter,
        extSimAdapter.ref,
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      emService ! Create(
        InitExtEmData(extEmDataConnection, simulationStart),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      val serviceActivation = activationMsg.actor

      serviceActivation ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(serviceActivation))

      // we trigger em service and expect an exception
      serviceActivation ! Activation(0)
      scheduler.expectNoMessage()

      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(emService.ref)
    }

    "handle flex option request correctly" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService.apply(scheduler.ref))
      val adapter = spawn(ExtEmDataService.adapter(emService))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)
      extEmDataConnection.setActorRefs(
        adapter,
        extSimAdapter.ref,
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      emService ! Create(
        InitExtEmData(extEmDataConnection, simulationStart),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      val serviceActivation = activationMsg.actor

      serviceActivation ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(serviceActivation))

      val emAgent1 = TestProbe[EmAgent.Request]("emAgent1")
      val emAgentFlex1 = TestProbe[FlexRequest]("emAgentFlex1")
      val emAgent2 = TestProbe[EmAgent.Request]("emAgent2")
      val emAgentFlex2 = TestProbe[FlexRequest]("emAgentFlex2")

      emService ! RegisterForEmDataService(
        emAgent1UUID,
        emAgent1.ref,
        emAgentFlex1.ref,
        None,
        None,
      )
      emAgent1.expectNoMessage()

      emService ! RegisterForEmDataService(
        emAgent2UUID,
        emAgent2.ref,
        emAgentFlex2.ref,
        None,
        None,
      )
      emAgent2.expectNoMessage()

      extEmDataConnection.sendExtMsg(
        new RequestEmFlexResults(
          INIT_SIM_TICK,
          List.empty[UUID].asJava,
        )
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      scheduler.expectNoMessage()

      serviceActivation ! Activation(INIT_SIM_TICK)

      emAgent1.expectNoMessage()
      emAgentFlex1.expectMessage(FlexActivation(-1))

      emAgent2.expectNoMessage()
      emAgentFlex2.expectMessage(FlexActivation(-1))

      extEmDataConnection.sendExtMsg(
        new RequestEmFlexResults(
          0,
          List(emAgentSupUUID).asJava,
        )
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      // scheduler.expectMessage(Completion(serviceActivation))

      serviceActivation ! Activation(0)

      emAgent1.expectNoMessage()
      emAgentFlex1.expectMessage(FlexActivation(0))

      emAgent2.expectNoMessage()
      emAgentFlex2.expectNoMessage()

      scheduler.expectMessage(Completion(serviceActivation))

      extEmDataConnection.receiveTriggerQueue shouldBe empty

      emService ! WrappedFlexResponse(
        ProvideFlexOptions(
          emAgentSupUUID,
          MinMaxFlexOptions(
            Kilowatts(5),
            Kilowatts(0),
            Kilowatts(10),
          ),
        ),
        Left(emAgentSupUUID),
      )

      awaitCond(
        !extEmDataConnection.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
      )

      extEmDataConnection.receiveTriggerQueue.size() shouldBe 1

      extEmDataConnection.receiveTriggerQueue
        .take() shouldBe new FlexOptionsResponse(
        Map(
          emAgentSupUUID -> new ExtendedFlexOptionsResult(
            simulationStart,
            emAgent1UUID,
            emAgentSupUUID,
            0.asKiloWatt,
            5.asKiloWatt,
            10.asKiloWatt,
          )
        ).asJava
      )
    }

    "handle flex option provision correctly" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService.apply(scheduler.ref))
      val adapter = spawn(ExtEmDataService.adapter(emService))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)
      extEmDataConnection.setActorRefs(
        adapter,
        extSimAdapter.ref,
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      emService ! Create(
        InitExtEmData(extEmDataConnection, simulationStart),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      val serviceActivation = activationMsg.actor

      serviceActivation ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(serviceActivation))

      val emAgent1 = TestProbe[EmAgent.Request]("emAgent1")
      val emAgentFlex1 = TestProbe[FlexRequest]("emAgentFlex1")
      val emAgent2 = TestProbe[EmAgent.Request]("emAgent2")
      val emAgentFlex2 = TestProbe[FlexRequest]("emAgentFlex2")

      emService ! RegisterForEmDataService(
        emAgent1UUID,
        emAgent1.ref,
        emAgentFlex1.ref,
        None,
        None,
      )
      emAgent1.expectNoMessage()

      emService ! RegisterForEmDataService(
        emAgent2UUID,
        emAgent2.ref,
        emAgentFlex2.ref,
        None,
        None,
      )
      emAgent2.expectNoMessage()

      extEmDataConnection.sendExtMsg(
        new ProvideEmFlexOptionData(
          0,
          Map(
            emAgent2UUID ->
              List(
                new FlexOptions(
                  emAgentSupUUID,
                  emAgent1UUID,
                  -3.asKiloWatt,
                  -1.asKiloWatt,
                  1.asKiloWatt,
                )
              ).asJava
          ).asJava,
          None.toJava,
        )
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      serviceActivation ! Activation(0)

      emAgent1.expectNoMessage()
      emAgent2.expectMessage(
        ProvideFlexOptions(
          emAgent1UUID,
          MinMaxFlexOptions(
            Kilowatts(-1),
            Kilowatts(-3),
            Kilowatts(1),
          ),
        )
      )

      scheduler.expectMessage(Completion(serviceActivation))
    }

    "handle set point provision correctly" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService.apply(scheduler.ref))
      val adapter = spawn(ExtEmDataService.adapter(emService))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)
      extEmDataConnection.setActorRefs(
        adapter,
        extSimAdapter.ref,
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      emService ! Create(
        InitExtEmData(extEmDataConnection, simulationStart),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      val serviceActivation = activationMsg.actor

      serviceActivation ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(serviceActivation))

      val emAgent1 = TestProbe[EmAgent.Request]("emAgent1")
      val emAgentFlex1 = TestProbe[FlexRequest]("emAgentFlex1")
      val emAgent2 = TestProbe[EmAgent.Request]("emAgent2")
      val emAgentFlex2 = TestProbe[FlexRequest]("emAgentFlex2")

      emService ! RegisterForEmDataService(
        emAgent1UUID,
        emAgent1.ref,
        emAgentFlex1.ref,
        None,
        None,
      )
      emAgent1.expectNoMessage()
      emAgentFlex1.expectMessage(FlexActivation(-1))

      emService ! RegisterForEmDataService(
        emAgent2UUID,
        emAgent2.ref,
        emAgentFlex2.ref,
        None,
        None,
      )
      emAgent2.expectNoMessage()
      emAgentFlex2.expectMessage(FlexActivation(-1))

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

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      serviceActivation ! Activation(0)

      emAgent1.expectNoMessage()
      emAgentFlex1.expectMessage(
        IssuePowerControl(0, Kilowatts(-3))
      )

      emAgent2.expectNoMessage()
      emAgentFlex2.expectMessage(
        IssuePowerControl(0, zeroKW)
      )

      scheduler.expectMessage(Completion(serviceActivation))
    }

    "handle set point request correctly" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService.apply(scheduler.ref))
      val adapter = spawn(ExtEmDataService.adapter(emService))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)
      extEmDataConnection.setActorRefs(
        adapter,
        extSimAdapter.ref,
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      emService ! Create(
        InitExtEmData(extEmDataConnection, simulationStart),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      val serviceActivation = activationMsg.actor

      serviceActivation ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(serviceActivation))

      val emAgent1 = TestProbe[EmAgent.Request]("emAgent1")
      val emAgentFlex1 = TestProbe[FlexRequest]("emAgentFlex1")
      val emAgent2 = TestProbe[EmAgent.Request]("emAgent2")
      val emAgentFlex2 = TestProbe[FlexRequest]("emAgentFlex2")

      emService ! RegisterForEmDataService(
        emAgent1UUID,
        emAgent1.ref,
        emAgentFlex1.ref,
        None,
        None,
      )
      emAgent1.expectNoMessage()
      emAgentFlex1.expectMessage(FlexActivation(-1))

      emService ! RegisterForEmDataService(
        emAgent2UUID,
        emAgent2.ref,
        emAgentFlex2.ref,
        None,
        None,
      )
      emAgent2.expectNoMessage()
      emAgentFlex2.expectMessage(FlexActivation(-1))

      // parent em normally sets itself a set point to 0 kW
      // we replace this behavior with the external data service
      extEmDataConnection.sendExtMsg(
        new ProvideEmSetPointData(
          0,
          Map(emAgent2UUID -> new PValue(0.asKiloWatt)).asJava,
          None.toJava,
        )
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      serviceActivation ! Activation(0)

      emAgent1.expectNoMessage()
      emAgentFlex1.expectNoMessage()

      emAgent2.expectNoMessage()
      emAgentFlex2.expectMessage(
        IssuePowerControl(0, zeroKW)
      )

      scheduler.expectMessage(Completion(serviceActivation))

      // we now request the set points of the controlled em agent
      extEmDataConnection.sendExtMsg(
        new RequestEmSetPoints(
          0,
          List(emAgent1UUID).asJava,
        )
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))

      serviceActivation ! Activation(0)

      emAgent1.expectNoMessage()
      emAgent2.expectNoMessage()

      // scheduler.expectMessage(Completion(serviceActivation))

      extEmDataConnection.receiveTriggerQueue shouldBe empty

      // the parent em agent sends the controlled em agent an IssuePowerControl message through the service
      emService ! WrappedFlexRequest(
        IssuePowerControl(0, Kilowatts(2)),
        emAgentFlex1.ref,
      )

      awaitCond(
        !extEmDataConnection.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
      )

      extEmDataConnection.receiveTriggerQueue.size() shouldBe 1

      extEmDataConnection.receiveTriggerQueue
        .take() shouldBe new EmSetPointDataResponse(
        Map(
          emAgent1UUID -> new EmSetPointResult(
            simulationStart,
            emAgentSupUUID,
            Map(emAgent1UUID -> new PValue(2.asKiloWatt)).asJava,
          )
        ).asJava
      )
    }

  }
}
