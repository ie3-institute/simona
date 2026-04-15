/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.connection.ExtEmDataConnection
import edu.ie3.simona.api.data.connection.ExtEmDataConnection.EmMode
import edu.ie3.simona.api.data.model.em
import edu.ie3.simona.api.data.model.em.{EmSetPoint, FlexOptionRequest}
import edu.ie3.simona.api.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  EmFlexMessage,
  EmServiceRegistration,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.flex.{FlexType, PowerLimitFlexOptions}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.QuantityUtils.{asKiloWatt, asMegaWatt}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.testkit.TestKit.awaitCond
import org.scalatest.wordspec.AnyWordSpecLike
import squants.energy.Kilowatts

import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*

class ExtEmDataServiceSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with EmInputTestData
    with TestSpawnerTyped {

  implicit val simulationStart: ZonedDateTime = ZonedDateTime.now()

  private val emptyControlled = List.empty[UUID].asJava

  private val emAgent1UUID =
    UUID.fromString("06a14909-366e-4e94-a593-1016e1455b30")
  private val emAgent2UUID =
    UUID.fromString("104acdaa-5dc5-4197-aed2-2fddb3c4f237")

  "An uninitialized em service" must {
    "send correct completion message after initialisation" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.BASE)
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val emService = spawn(
        ExtEmDataService(
          scheduler.ref,
          InitExtEmData(extEmDataConnection, simulationStart),
          serviceKey,
        )
      )

      extEmDataConnection.setActorRefs(
        emService,
        extSimAdapter.ref,
      )

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()
    }

    "stash registration request and handle it correctly once initialized" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.BASE)
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val emService = spawn(
        ExtEmDataService(
          scheduler.ref,
          InitExtEmData(extEmDataConnection, simulationStart),
          serviceKey,
        )
      )

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

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()
    }
  }

  "An idle em service" must {

    "fail when activated without having received ExtEmMessage" in {
      val emAgent = TestProbe[EmAgent.Message]("emAgent")

      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.BASE)
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val emService = spawn(
        ExtEmDataService(
          scheduler.ref,
          InitExtEmData(extEmDataConnection, simulationStart),
          serviceKey,
        )
      )

      extEmDataConnection.setActorRefs(
        emService,
        extSimAdapter.ref,
      )

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      emService ! EmServiceRegistration(
        emAgent.ref,
        emAgent1UUID,
        None,
        None,
      )
      emAgent.expectMessage(FlexInit(FlexType.PowerLimit, DataTimeType.Current))
      emService ! EmFlexMessage(
        FlexCompletion(emAgent1UUID, requestAtTick = Some(0)),
        emAgent1UUID,
      )

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

      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.BASE)
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val emService = spawn(
        ExtEmDataService(
          scheduler.ref,
          InitExtEmData(extEmDataConnection, simulationStart),
          serviceKey,
        )
      )

      extEmDataConnection.setActorRefs(
        emService,
        extSimAdapter.ref,
      )

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      val emAgent1 = TestProbe[EmAgent.Message]("emAgent1")
      val emAgent2 = TestProbe[EmAgent.Message]("emAgent2")

      emService ! EmServiceRegistration(
        emAgent1.ref,
        emAgent1UUID,
        None,
        None,
      )
      emAgent1.expectMessage(
        FlexInit(FlexType.PowerLimit, DataTimeType.Current)
      )
      emService ! EmFlexMessage(
        FlexCompletion(emAgent1UUID, requestAtTick = Some(0)),
        emAgent1UUID,
      )

      emService ! EmServiceRegistration(
        emAgent2.ref,
        emAgent2UUID,
        None,
        None,
      )
      emAgent2.expectMessage(
        FlexInit(FlexType.PowerLimit, DataTimeType.Current)
      )
      emService ! EmFlexMessage(
        FlexCompletion(emAgent2UUID, requestAtTick = Some(0)),
        emAgent2UUID,
      )

      // scheduler.expectMessage(Completion(emService))

      extEmDataConnection.sendExtMsg(
        new ProvideEmData(
          0L,
          Map(
            emAgent1UUID -> new FlexOptionRequest(emAgent1UUID, false)
          ).asJava,
          Map.empty.asJava,
          Map.empty.asJava,
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
          PowerLimitFlexOptions(
            Kilowatts(5),
            Kilowatts(0),
            Kilowatts(10),
          ),
        ),
        emAgent1UUID,
      )

      awaitCond(
        !extEmDataConnection.receiveTriggerQueue.isEmpty,
        max = 3.seconds,
      )

      extEmDataConnection.receiveTriggerQueue.size() shouldBe 1

      extEmDataConnection.receiveTriggerQueue
        .take() shouldBe new FlexOptionsResponse(
        Map(
          emAgent1UUID -> List(
            new em.PowerLimitFlexOptions(
              emAgent1UUID,
              emAgent1UUID,
              0.005.asMegaWatt,
              0.asMegaWatt,
              0.01.asMegaWatt,
            )
          )
        ).asJava
      )
    }

    "handle set point provision correctly" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      val extEmDataConnection =
        new ExtEmDataConnection(emptyControlled, EmMode.BASE)
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val emService = spawn(
        ExtEmDataService(
          scheduler.ref,
          InitExtEmData(extEmDataConnection, simulationStart),
          serviceKey,
        )
      )

      extEmDataConnection.setActorRefs(
        emService,
        extSimAdapter.ref,
      )

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      val emAgent1 = TestProbe[EmAgent.Message]("emAgent1")
      val emAgent2 = TestProbe[EmAgent.Message]("emAgent2")

      emService ! EmServiceRegistration(
        emAgent1.ref,
        emAgent1UUID,
        None,
        None,
      )
      emAgent1.expectMessage(
        FlexInit(FlexType.PowerLimit, DataTimeType.Current)
      )
      emService ! EmFlexMessage(
        FlexCompletion(emAgent1UUID, requestAtTick = Some(0)),
        emAgent1UUID,
      )

      emService ! EmServiceRegistration(
        emAgent2.ref,
        emAgent2UUID,
        None,
        None,
      )
      emAgent2.expectMessage(
        FlexInit(FlexType.PowerLimit, DataTimeType.Current)
      )
      emService ! EmFlexMessage(
        FlexCompletion(emAgent2UUID, requestAtTick = Some(0)),
        emAgent2UUID,
      )

      extEmDataConnection.sendExtMsg(
        new ProvideEmData(
          0,
          Map.empty.asJava,
          Map.empty.asJava,
          Map(
            emAgent1UUID -> new EmSetPoint(
              emAgent1UUID,
              -3d.asKiloWatt,
            ),
            emAgent2UUID -> new EmSetPoint(
              emAgent2UUID,
              0d.asKiloWatt,
            ),
          ).asJava,
        )
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(0)

      // first the em agents are activated by the service
      emAgent1.expectMessageType[FlexActivation]
      emAgent2.expectMessageType[FlexActivation]

      // then the agents sent their option
      emService ! EmFlexMessage(
        ProvideFlexOptions(
          emAgent1UUID,
          PowerLimitFlexOptions(zeroKW, zeroKW, zeroKW),
        ),
        emAgent1UUID, // prevents from sending the message back
      )

      emService ! EmFlexMessage(
        ProvideFlexOptions(
          emAgent2UUID,
          PowerLimitFlexOptions(zeroKW, zeroKW, zeroKW),
        ),
        emAgent2UUID, // prevents from sending the message back
      )

      // now the agents are able to receive em set points
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
