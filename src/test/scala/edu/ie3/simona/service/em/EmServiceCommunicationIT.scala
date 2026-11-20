/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.util.quantities.QuantityUtils.*
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.{EmInput, OperatorInput}
import edu.ie3.simona.agent.em.EmAgentInit
import edu.ie3.simona.agent.participant.ParticipantAgentInit.SimulationParameters
import edu.ie3.simona.api.data.connection.ExtEmDataConnection
import edu.ie3.simona.api.data.connection.ExtEmDataConnection.EmMode
import edu.ie3.simona.api.data.model.em
import edu.ie3.simona.api.data.model.em.{
  EmCommunicationMessage,
  FlexOptionRequest,
}
import edu.ie3.simona.api.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.ontology.em.{EmCompletion, EmResultResponse}
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.config.RuntimeConfig.*
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.flex.PowerLimitFlexOptions
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.service.results.ResultServiceProxy.{
  ExpectResult,
  NoResult,
}
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.scalatest.wordspec.AnyWordSpecLike
import squants.Each
import squants.energy.Kilowatts

import java.time.ZonedDateTime
import java.util as ju
import java.util.{OptionalLong, UUID}
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*

class EmServiceCommunicationIT
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with EmInputTestData
    with TestSpawnerTyped {

  // start a bit later so the sun is up
  protected given simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T10:00:00Z")

  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02T02:00:00Z")

  given simulationParams: SimulationParameters = SimulationParameters(
    expectedPowerRequestTick = Long.MaxValue,
    requestVoltageDeviationTolerance = Each(1e-14d),
    simulationStart = simulationStartDate,
    simulationEnd = simulationEndDate,
  )

  private val outputConfigOn = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false,
    flexResult = false,
  )

  private val modelConfig: EmRuntimeConfig = EmRuntimeConfig(
    aggregateFlex = "SELF_OPT"
  )

  protected val controlledEmInput = new EmInput(
    UUID.randomUUID(),
    "ControlledEmModel",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    "PROPORTIONAL",
    emInput,
  )

  "An em service with external communication" should {
    "work correctly with one uncontrolled and one controlled EmAgent in one tick" in {
      val resultServiceProxy =
        TestProbe[ResultEvent | ExpectResult | NoResult]("ResultServiceProxy")

      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")
      val storage = TestProbe[FlexRequest]("storage")

      val extEmDataConnection =
        new ExtEmDataConnection(
          ju.List.of(emInput.getUuid),
          EmMode.EM_COMMUNICATION,
        )
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      val emService = spawn(
        ExtEmDataService(
          scheduler.ref,
          InitExtEmData(scheduler.ref, extEmDataConnection),
          serviceKey,
        )
      )

      extEmDataConnection.setActorRefs(
        emService,
        extSimAdapter.ref,
      )

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      val uncontrolledEmAgent = spawn(
        EmAgentInit(
          emInput,
          modelConfig,
          outputConfigOn,
          simulationStartDate,
          parent = Left(scheduler.ref),
          listener = resultServiceProxy.ref,
          emDataService = Some(emService),
        ),
        "UncontrolledEmAgent",
      )

      val controlledEmAgent = spawn(
        EmAgentInit(
          controlledEmInput,
          modelConfig,
          outputConfigOn,
          simulationStartDate,
          parent = Right(uncontrolledEmAgent),
          listener = resultServiceProxy.ref,
          emDataService = Some(emService),
        ),
        "ControlledEmAgent",
      )

      controlledEmAgent ! RegisterControlledAsset(storage.ref, storageInput)
      controlledEmAgent ! ScheduleFlexActivation(
        storageInput.getUuid,
        INIT_SIM_TICK,
        None,
      )

      scheduler.expectMessage(
        ScheduleActivation(uncontrolledEmAgent, INIT_SIM_TICK)
      )
      uncontrolledEmAgent ! Activation(INIT_SIM_TICK)

      // finish initialization
      storage.expectMessageType[FlexInit](10.seconds)
      controlledEmAgent ! FlexCompletion(
        storageInput.getUuid,
        requestAtTick = Some(0),
      )
      scheduler.expectMessage(Completion(uncontrolledEmAgent))

      // start the communication
      extEmDataConnection.sendEmData(
        0L,
        Map(emInput.getUuid -> new FlexOptionRequest(emInput.getUuid)).asJava,
        Map.empty.asJava,
        Map.empty.asJava,
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(0L)
      scheduler.expectMessage(Completion(emService))

      // we receive a communication message
      val res1 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res1.size shouldBe 1
      val list1 = res1(emInput.getUuid).asScala
      val msg1 = list1.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe controlledEmInput.getUuid
          sender shouldBe emInput.getUuid
          content shouldBe new FlexOptionRequest(controlledEmInput.getUuid)

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(0L, ju.List.of(msg1))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(0L)
      scheduler.expectMessage(Completion(emService))

      // the controlled em should send a request to the storage now
      storage.expectMessage(FlexActivation(0L))

      // the storage answers with some flex options
      controlledEmAgent ! ProvideFlexOptions(
        storageInput.getUuid,
        PowerLimitFlexOptions(Kilowatts(1), Kilowatts(-5), Kilowatts(5)),
      )

      // we receive a communication message
      val res2 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res2.size shouldBe 1
      val list2 = res2(controlledEmInput.getUuid).asScala
      val msg2 = list2.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe emInput.getUuid
          sender shouldBe controlledEmInput.getUuid
          content shouldBe new em.PowerLimitFlexOptions(
            emInput.getUuid,
            controlledEmInput.getUuid,
            0.asMegaWatt,
            (-0.005).asMegaWatt,
            0.005.asMegaWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(0L, ju.List.of(msg2))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(0L)
      scheduler.expectMessage(Completion(emService))

      // we receive flex options
      val res3 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res3.size shouldBe 1
      val list3 = res3(emInput.getUuid).asScala
      list3.headOption shouldBe Some(
        new em.PowerLimitFlexOptions(
          emInput.getUuid,
          emInput.getUuid,
          0.asMegaWatt,
          (-0.005).asMegaWatt,
          0.005.asMegaWatt,
        )
      )

      // we will now answer with a new set point
      extEmDataConnection.sendEmData(
        0L,
        Map.empty.asJava,
        Map.empty.asJava,
        Map(
          emInput.getUuid -> new em.SetPoint.AggregatedSetPoint(
            emInput.getUuid,
            (-1).asKiloWatt,
          )
        ).asJava,
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(0L)
      scheduler.expectMessage(Completion(emService))

      // we receive a communication message
      val res4 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res4.size shouldBe 1
      val list4 = res4(emInput.getUuid).asScala
      val msg4 = list4.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe controlledEmInput.getUuid
          sender shouldBe emInput.getUuid
          content shouldBe new em.SetPoint.AggregatedSetPoint(
            controlledEmInput.getUuid,
            (-1).asKiloWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(0L, ju.List.of(msg4))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(0L)
      scheduler.expectMessage(Completion(emService))

      // the storage should now receive the set point
      storage.expectMessage(IssuePowerControl(0L, Kilowatts(-1)))

      // storage will send a FlexCompletion message back
      controlledEmAgent ! FlexCompletion(
        storageInput.getUuid,
        requestAtTick = Some(300L),
      )

      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.of(300L)

      // start the communication
      extEmDataConnection.sendEmData(
        300L,
        Map(emInput.getUuid -> new FlexOptionRequest(emInput.getUuid)).asJava,
        Map.empty.asJava,
        Map.empty.asJava,
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(300L)
      scheduler.expectMessage(Completion(emService))

      // we receive a communication message
      val res5 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res5.size shouldBe 1
      val list5 = res5(emInput.getUuid).asScala
      val msg5 = list5.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe controlledEmInput.getUuid
          sender shouldBe emInput.getUuid
          content shouldBe new FlexOptionRequest(controlledEmInput.getUuid)

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(300L, ju.List.of(msg5))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(300L)
      scheduler.expectMessage(Completion(emService))

      // the controlled em should send a request to the storage now
      storage.expectMessage(FlexActivation(300L))

      // the storage answers with some flex options
      controlledEmAgent ! ProvideFlexOptions(
        storageInput.getUuid,
        PowerLimitFlexOptions(Kilowatts(-1), Kilowatts(-5), Kilowatts(5)),
      )

      // we receive a communication message
      val res6 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res6.size shouldBe 1
      val list6 = res6(controlledEmInput.getUuid).asScala
      val msg6 = list6.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe emInput.getUuid
          sender shouldBe controlledEmInput.getUuid
          content shouldBe new em.PowerLimitFlexOptions(
            emInput.getUuid,
            controlledEmInput.getUuid,
            0.asMegaWatt,
            (-0.005).asMegaWatt,
            0.005.asMegaWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(300L, ju.List.of(msg6))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(300L)
      scheduler.expectMessage(Completion(emService))

      // we receive flex options
      val res7 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res7.size shouldBe 1
      val list7 = res7(emInput.getUuid).asScala
      list7.headOption shouldBe Some(
        new em.PowerLimitFlexOptions(
          emInput.getUuid,
          emInput.getUuid,
          0.asMegaWatt,
          (-0.005).asMegaWatt,
          0.005.asMegaWatt,
        )
      )

      // we will now answer with a new set point
      extEmDataConnection.sendEmData(
        300L,
        Map.empty.asJava,
        Map.empty.asJava,
        Map(
          emInput.getUuid -> new em.SetPoint.AggregatedSetPoint(
            emInput.getUuid,
            1.asKiloWatt,
          )
        ).asJava,
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(300L)
      scheduler.expectMessage(Completion(emService))

      // we receive a communication message
      val res8 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res8.size shouldBe 1
      val list8 = res8(emInput.getUuid).asScala
      val msg8 = list8.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe controlledEmInput.getUuid
          sender shouldBe emInput.getUuid
          content shouldBe new em.SetPoint.AggregatedSetPoint(
            controlledEmInput.getUuid,
            1.asKiloWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(300L, ju.List.of(msg8))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(300L)
      scheduler.expectMessage(Completion(emService))

      // the storage should now receive the set point
      storage.expectMessage(IssuePowerControl(300L, Kilowatts(1)))

      // storage will send a FlexCompletion message back
      controlledEmAgent ! FlexCompletion(storageInput.getUuid)

      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.empty
    }

    "work correctly with one uncontrolled and one controlled EmAgent with multiple tick" in {
      val resultServiceProxy =
        TestProbe[ResultEvent | ExpectResult | NoResult]("ResultServiceProxy")

      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")
      val storage = TestProbe[FlexRequest]("storage")

      val extEmDataConnection =
        new ExtEmDataConnection(
          ju.List.of(emInput.getUuid),
          EmMode.EM_COMMUNICATION,
        )
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      val emService = spawn(
        ExtEmDataService(
          scheduler.ref,
          InitExtEmData(scheduler.ref, extEmDataConnection),
          serviceKey,
        )
      )

      extEmDataConnection.setActorRefs(
        emService,
        extSimAdapter.ref,
      )

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      val uncontrolledEmAgent = spawn(
        EmAgentInit(
          emInput,
          modelConfig,
          outputConfigOn,
          simulationStartDate,
          parent = Left(scheduler.ref),
          listener = resultServiceProxy.ref,
          emDataService = Some(emService),
        ),
        "UncontrolledEmAgent_2",
      )

      val controlledEmAgent = spawn(
        EmAgentInit(
          controlledEmInput,
          modelConfig,
          outputConfigOn,
          simulationStartDate,
          parent = Right(uncontrolledEmAgent),
          listener = resultServiceProxy.ref,
          emDataService = Some(emService),
        ),
        "ControlledEmAgent_2",
      )

      controlledEmAgent ! RegisterControlledAsset(storage.ref, storageInput)
      controlledEmAgent ! ScheduleFlexActivation(
        storageInput.getUuid,
        INIT_SIM_TICK,
        None,
      )

      scheduler.expectMessage(
        ScheduleActivation(uncontrolledEmAgent, INIT_SIM_TICK)
      )
      uncontrolledEmAgent ! Activation(INIT_SIM_TICK)

      // finish initialization
      storage.expectMessageType[FlexInit](10.seconds)
      controlledEmAgent ! FlexCompletion(
        storageInput.getUuid,
        requestAtTick = Some(0),
      )
      scheduler.expectMessage(Completion(uncontrolledEmAgent))

      // start the communication
      extEmDataConnection.sendEmData(
        0L,
        Map(emInput.getUuid -> new FlexOptionRequest(emInput.getUuid)).asJava,
        Map.empty.asJava,
        Map.empty.asJava,
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(0L)
      scheduler.expectMessage(Completion(emService))

      // we receive a communication message
      val res1 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res1.size shouldBe 1
      val list1 = res1(emInput.getUuid).asScala
      val msg1 = list1.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe controlledEmInput.getUuid
          sender shouldBe emInput.getUuid
          content shouldBe new FlexOptionRequest(controlledEmInput.getUuid)

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(1L, ju.List.of(msg1))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(1L)
      scheduler.expectMessage(Completion(emService))

      // the controlled em should send a request to the storage now
      storage.expectMessage(FlexActivation(1L))

      // the storage answers with some flex options
      controlledEmAgent ! ProvideFlexOptions(
        storageInput.getUuid,
        PowerLimitFlexOptions(Kilowatts(1), Kilowatts(-5), Kilowatts(5)),
      )

      // we receive a communication message
      val res2 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res2.size shouldBe 1
      val list2 = res2(controlledEmInput.getUuid).asScala
      val msg2 = list2.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe emInput.getUuid
          sender shouldBe controlledEmInput.getUuid
          content shouldBe new em.PowerLimitFlexOptions(
            emInput.getUuid,
            controlledEmInput.getUuid,
            0.asMegaWatt,
            (-0.005).asMegaWatt,
            0.005.asMegaWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(2L, ju.List.of(msg2))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(2L)
      scheduler.expectMessage(Completion(emService))

      // we receive flex options
      val res3 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res3.size shouldBe 1
      val list3 = res3(emInput.getUuid).asScala
      list3.headOption shouldBe Some(
        new em.PowerLimitFlexOptions(
          emInput.getUuid,
          emInput.getUuid,
          0.asMegaWatt,
          (-0.005).asMegaWatt,
          0.005.asMegaWatt,
        )
      )

      // we will now answer with a new set point
      extEmDataConnection.sendEmData(
        2L,
        Map.empty.asJava,
        Map.empty.asJava,
        Map(
          emInput.getUuid -> new em.SetPoint.AggregatedSetPoint(
            emInput.getUuid,
            (-1).asKiloWatt,
          )
        ).asJava,
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(2L)
      scheduler.expectMessage(Completion(emService))

      // we receive a communication message
      val res4 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res4.size shouldBe 1
      val list4 = res4(emInput.getUuid).asScala
      val msg4 = list4.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe controlledEmInput.getUuid
          sender shouldBe emInput.getUuid
          content shouldBe new em.SetPoint.AggregatedSetPoint(
            controlledEmInput.getUuid,
            (-1).asKiloWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(3L, ju.List.of(msg4))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(3L)
      scheduler.expectMessage(Completion(emService))

      // the storage should now receive the set point
      storage.expectMessage(IssuePowerControl(3L, Kilowatts(-1)))

      // storage will send a FlexCompletion message back
      controlledEmAgent ! FlexCompletion(
        storageInput.getUuid,
        requestAtTick = Some(300L),
      )

      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.of(300L)

      // start the communication
      extEmDataConnection.sendEmData(
        300L,
        Map(emInput.getUuid -> new FlexOptionRequest(emInput.getUuid)).asJava,
        Map.empty.asJava,
        Map.empty.asJava,
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(300L)
      scheduler.expectMessage(Completion(emService))

      // we receive a communication message
      val res5 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res5.size shouldBe 1
      val list5 = res5(emInput.getUuid).asScala
      val msg5 = list5.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe controlledEmInput.getUuid
          sender shouldBe emInput.getUuid
          content shouldBe new FlexOptionRequest(controlledEmInput.getUuid)

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(300L, ju.List.of(msg5))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(300L)
      scheduler.expectMessage(Completion(emService))

      // the controlled em should send a request to the storage now
      storage.expectMessage(FlexActivation(300L))

      // the storage answers with some flex options
      controlledEmAgent ! ProvideFlexOptions(
        storageInput.getUuid,
        PowerLimitFlexOptions(Kilowatts(-1), Kilowatts(-5), Kilowatts(5)),
      )

      // we receive a communication message
      val res6 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res6.size shouldBe 1
      val list6 = res6(controlledEmInput.getUuid).asScala
      val msg6 = list6.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe emInput.getUuid
          sender shouldBe controlledEmInput.getUuid
          content shouldBe new em.PowerLimitFlexOptions(
            emInput.getUuid,
            controlledEmInput.getUuid,
            0.asMegaWatt,
            (-0.005).asMegaWatt,
            0.005.asMegaWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(301L, ju.List.of(msg6))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(301L)
      scheduler.expectMessage(Completion(emService))

      // we receive flex options
      val res7 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res7.size shouldBe 1
      val list7 = res7(emInput.getUuid).asScala
      list7.headOption shouldBe Some(
        new em.PowerLimitFlexOptions(
          emInput.getUuid,
          emInput.getUuid,
          0.asMegaWatt,
          (-0.005).asMegaWatt,
          0.005.asMegaWatt,
        )
      )

      // we will now answer with a new set point
      extEmDataConnection.sendEmData(
        301L,
        Map.empty.asJava,
        Map.empty.asJava,
        Map(
          emInput.getUuid -> new em.SetPoint.AggregatedSetPoint(
            emInput.getUuid,
            1.asKiloWatt,
          )
        ).asJava,
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(301L)
      scheduler.expectMessage(Completion(emService))

      // we receive a communication message
      val res8 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res8.size shouldBe 1
      val list8 = res8(emInput.getUuid).asScala
      val msg8 = list8.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe controlledEmInput.getUuid
          sender shouldBe emInput.getUuid
          content shouldBe new em.SetPoint.AggregatedSetPoint(
            controlledEmInput.getUuid,
            1.asKiloWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(302L, ju.List.of(msg8))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(302L)
      scheduler.expectMessage(Completion(emService))

      // the storage should now receive the set point
      storage.expectMessage(IssuePowerControl(302L, Kilowatts(1)))

      // storage will send a FlexCompletion message back
      controlledEmAgent ! FlexCompletion(storageInput.getUuid)

      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.empty
    }

  }

}
