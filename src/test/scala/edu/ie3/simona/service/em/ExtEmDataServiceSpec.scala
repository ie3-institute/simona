/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.em.model.{
  EmSetPoint,
  ExtendedFlexOptionsResult,
  FlexOptions,
}
import edu.ie3.simona.api.data.em.ontology.*
import edu.ie3.simona.api.data.em.{EmMode, ExtEmDataConnection}
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ontology.ControlResponseMessageFromExt
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  Create,
  EmFlexMessage,
  EmServiceRegistration,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexActivation,
  FlexCompletion,
  IssuePowerControl,
  ProvideFlexOptions,
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.QuantityUtils.*
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.testkit.TestKit.awaitCond
import org.scalatest.wordspec.AnyWordSpecLike
import squants.energy.Kilowatts

import java.time.ZonedDateTime
import java.util.{Optional, UUID}
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
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

      val emService = spawn(ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)

      extEmDataConnection.setActorRefs(
        emService,
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

      scheduler.expectMessage(
        ScheduleActivation(emService, INIT_SIM_TICK, Some(key))
      )

      emService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(emService))
    }

    "stash registration request and handle it correctly once initialized" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)

      extEmDataConnection.setActorRefs(
        emService,
        extSimAdapter.ref,
      )

      val emAgent = TestProbe[EmAgent.Message]("emAgent")

      // this one should be stashed
      emService ! EmServiceRegistration(
        emAgent.ref,
        emInput.getUuid,
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

      scheduler.expectMessage(
        ScheduleActivation(emService, INIT_SIM_TICK, Some(key))
      )

      emService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(emService))
    }
  }

  "An idle em service" must {

    "fail when activated without having received ExtEmMessage" in {
      val emAgent = TestProbe[EmAgent.Message]("emAgent")

      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)

      extEmDataConnection.setActorRefs(
        emService,
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

      scheduler.expectMessage(
        ScheduleActivation(emService, INIT_SIM_TICK, Some(key))
      )

      emService ! Activation(INIT_SIM_TICK)

      emService ! EmServiceRegistration(
        emAgent.ref,
        emAgent1UUID,
        None,
        None,
      )
      emAgent.expectMessage(FlexActivation(-1))
      emService ! EmFlexMessage(
        FlexCompletion(emAgent1UUID, requestAtTick = Some(0)),
        Left(emAgent1UUID),
      )

      scheduler.expectMessage(Completion(emService))

      // we trigger em service and expect an exception
      emService ! Activation(0)
      scheduler.expectNoMessage()

      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(emService)
    }

    "handle flex option request correctly" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.BASE)

      extEmDataConnection.setActorRefs(
        emService,
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

      scheduler.expectMessage(
        ScheduleActivation(emService, INIT_SIM_TICK, Some(key))
      )

      emService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(emService))

      val emAgent1 = TestProbe[EmAgent.Message]("emAgent1")
      val emAgent2 = TestProbe[EmAgent.Message]("emAgent2")

      emService ! EmServiceRegistration(
        emAgent1.ref,
        emAgent1UUID,
        None,
        None,
      )
      emAgent1.expectMessage(FlexActivation(-1))
      emService ! EmFlexMessage(
        FlexCompletion(emAgent1UUID, requestAtTick = Some(0)),
        Left(emAgent1UUID),
      )

      emService ! EmServiceRegistration(
        emAgent2.ref,
        emAgent2UUID,
        None,
        None,
      )
      emAgent2.expectMessage(FlexActivation(-1))
      emService ! EmFlexMessage(
        FlexCompletion(emAgent2UUID, requestAtTick = Some(0)),
        Left(emAgent2UUID),
      )

      // scheduler.expectMessage(Completion(emService))

      extEmDataConnection.sendExtMsg(
        new RequestEmFlexResults(
          0,
          List(emAgent1UUID).asJava,
          false,
        )
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(0)

      emAgent1.expectMessage(FlexActivation(0))
      emAgent2.expectNoMessage()

      scheduler.expectMessage(Completion(emService))

      extEmDataConnection.receiveTriggerQueue shouldBe empty

      emService ! EmFlexMessage(
        ProvideFlexOptions(
          emAgent1UUID,
          MinMaxFlexOptions(
            Kilowatts(5),
            Kilowatts(0),
            Kilowatts(10),
          ),
        ),
        Left(emAgent1UUID),
      )

      awaitCond(
        !extEmDataConnection.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
      )

      extEmDataConnection.receiveTriggerQueue.size() shouldBe 1

      extEmDataConnection.receiveTriggerQueue
        .take() shouldBe new FlexOptionsResponse(
        Map(
          emAgent1UUID -> new ExtendedFlexOptionsResult(
            simulationStart,
            emAgent1UUID,
            emAgent1UUID,
            0.asMegaWatt,
            0.005.asMegaWatt,
            0.01.asMegaWatt,
          )
        ).asJava
      )
    }

    "handle flex option provision correctly" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)

      extEmDataConnection.setActorRefs(
        emService,
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

      scheduler.expectMessage(
        ScheduleActivation(emService, INIT_SIM_TICK, Some(key))
      )

      emService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(emService))

      val emAgentSup = TestProbe[EmAgent.Message]("emAgentSup")
      val emAgent1 = TestProbe[EmAgent.Message]("emAgent1")

      emService ! EmServiceRegistration(
        emAgent1.ref,
        emAgent1UUID,
        Some(emAgentSup.ref),
        Some(emAgentSupUUID),
      )
      emAgent1.expectNoMessage()

      emService ! EmServiceRegistration(
        emAgentSup.ref,
        emAgentSupUUID,
        None,
        None,
      )
      emAgentSup.expectMessage(FlexActivation(-1))

      // the em agent sup will send an activation message to the em agent1
      emService ! EmFlexMessage(FlexActivation(-1), Right(emAgent1.ref))

      emAgent1.expectMessage(FlexActivation(-1))

      // the em agent 1 will answer the activation with a flex completion message
      emService ! EmFlexMessage(
        FlexCompletion(emAgent1UUID, requestAtTick = Some(0)),
        Right(emAgentSup.ref),
      )

      emAgentSup
        .expectMessageType[FlexCompletion]
        .modelUuid shouldBe emAgent1UUID

      emService ! EmFlexMessage(
        FlexCompletion(emAgentSupUUID, requestAtTick = Some(0)),
        Left(emAgentSupUUID),
      )

      extEmDataConnection.sendExtMsg(
        new ProvideEmFlexOptionData(
          0,
          Map(
            emAgentSupUUID ->
              List(
                new FlexOptions(
                  emAgentSupUUID,
                  emAgent1UUID,
                  -3.asKiloWatt,
                  -1.asKiloWatt,
                  1.asKiloWatt,
                  Optional.empty,
                )
              ).asJava
          ).asJava,
          None.toJava,
        )
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(0)

      emAgent1.expectNoMessage()
      emAgentSup.expectMessage(
        60.seconds,
        ProvideFlexOptions(
          emAgent1UUID,
          MinMaxFlexOptions(
            Kilowatts(-1),
            Kilowatts(-3),
            Kilowatts(1),
          ),
        ),
      )

      scheduler.expectMessage(Completion(emService))
    }

    "handle set point provision correctly" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val emService = spawn(ExtEmDataService(scheduler.ref))
      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.EM_COMMUNICATION)

      extEmDataConnection.setActorRefs(
        emService,
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

      scheduler.expectMessage(
        ScheduleActivation(emService, INIT_SIM_TICK, Some(key))
      )

      emService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(emService))

      val emAgent1 = TestProbe[EmAgent.Message]("emAgent1")
      val emAgent2 = TestProbe[EmAgent.Message]("emAgent2")

      emService ! EmServiceRegistration(
        emAgent1.ref,
        emAgent1UUID,
        None,
        None,
      )
      emAgent1.expectMessage(FlexActivation(-1))
      emService ! EmFlexMessage(
        FlexCompletion(emAgent1UUID, requestAtTick = Some(0)),
        Left(emAgent1UUID),
      )

      emService ! EmServiceRegistration(
        emAgent2.ref,
        emAgent2UUID,
        None,
        None,
      )
      emAgent2.expectMessage(FlexActivation(-1))
      emService ! EmFlexMessage(
        FlexCompletion(emAgent2UUID, requestAtTick = Some(0)),
        Left(emAgent2UUID),
      )

      extEmDataConnection.sendExtMsg(
        new ProvideEmSetPointData(
          0,
          Map(
            emAgent1UUID -> new EmSetPoint(emAgent1UUID, -3.asKiloWatt),
            emAgent2UUID -> new EmSetPoint(emAgent2UUID, 0.asKiloWatt),
          ).asJava,
          None.toJava,
        )
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))

      emService ! Activation(0)

      emAgent1.expectMessage(
        IssuePowerControl(0, Kilowatts(-3))
      )

      emAgent2.expectMessage(
        IssuePowerControl(0, zeroKW)
      )

      scheduler.expectMessage(Completion(emService))
    }

  }
}
