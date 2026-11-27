/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.input.system.`type`.StorageTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.{EmInput, OperatorInput}
import edu.ie3.simona.agent.em.EmAgentInit
import edu.ie3.simona.agent.participant.ParticipantAgentInit
import edu.ie3.simona.agent.participant.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.api.data.connection.ExtEmDataConnection
import edu.ie3.simona.api.data.connection.ExtEmDataConnection.EmMode
import edu.ie3.simona.api.data.model.em
import edu.ie3.simona.api.data.model.em.{
  EmCommunicationMessage,
  EmData,
  FlexOptionRequest,
}
import edu.ie3.simona.api.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.ontology.em.{EmCompletion, EmResultResponse}
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.config.RuntimeConfig.*
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.InputModelContainer.SimpleInputContainer
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  PrimaryServiceRegistrationMessage,
  RegistrationFailedMessage,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.flex.PowerLimitFlexOptions
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy.{
  ExpectResult,
  NoResult,
}
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.*
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.scalatest.wordspec.AnyWordSpecLike
import squants.Each
import squants.energy.Kilowatts
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.{util, util as ju}
import java.util.{OptionalLong, UUID}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*

class EmServiceCommunicationIT
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with EmServiceTestData
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

  private val outputConfigOff = NotifierConfig(
    simulationResultInfo = false,
    powerRequestReply = false,
    flexResult = false,
  )

  private val modelConfig: EmRuntimeConfig = EmRuntimeConfig(
    aggregateFlex = "SELF_OPT"
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
      storage.expectMessage(FlexActivation(0L, true))

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
      storage.expectMessage(FlexActivation(300L, true))

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
            (-0.0).asMegaWatt,
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
      storage.expectMessage(FlexActivation(1L, true))

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
      storage.expectMessage(
        IssuePowerControl(1L, Kilowatts(-1))
      ) // TODO: Fix the active tick of EmAgent

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
      storage.expectMessage(FlexActivation(300L, true))

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
            (-0.0).asMegaWatt,
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
      storage.expectMessage(IssuePowerControl(300L, Kilowatts(1)))

      // storage will send a FlexCompletion message back
      controlledEmAgent ! FlexCompletion(storageInput.getUuid)

      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.empty
    }

    "work correctly with one uncontrolled, one controlled EmAgent and a StorageAgent in one tick" in {
      val resultServiceProxy =
        TestProbe[ResultEvent | ExpectResult | NoResult]("ResultServiceProxy")
      val primaryServiceProxy =
        TestProbe[PrimaryServiceProxy.Message]("PrimaryServiceProxy")

      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      given ParticipantRefs = ParticipantRefs(
        primaryServiceProxy = primaryServiceProxy.ref,
        resultServiceProxy = resultServiceProxy.ref,
        services = Map.empty,
      )

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

      val keys = ScheduleLock
        .multiKey(TSpawner, scheduler.ref, PRE_INIT_TICK, 1)
        .iterator
      val lockActivation =
        scheduler.expectMessageType[ScheduleActivation].actor
      lockActivation ! Activation(PRE_INIT_TICK)

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
        "UncontrolledEmAgent_3",
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
        "ControlledEmAgent_3",
      )

      val storageAgent = spawn(
        ParticipantAgentInit(
          storageInputContainer,
          StorageRuntimeConfig(),
          outputConfigOff,
          Right(controlledEmAgent),
          keys.next(),
        ),
        "StorageAgent_3",
      )

      val initSchedule = scheduler.expectMessageType[ScheduleActivation]
      initSchedule.tick shouldBe INIT_SIM_TICK
      initSchedule.actor shouldBe uncontrolledEmAgent

      scheduler.expectNoMessage()

      initSchedule.unlockKey.foreach(_.unlock())
      scheduler.expectMessage(Completion(lockActivation))

      /* INIT */
      uncontrolledEmAgent ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(
          storageAgent,
          storageInput.getUuid,
        )
      )

      // storage
      storageAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      // finish initialization
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
      // the storage answers with some flex options
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
            0.asMegaWatt,
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
          0.asMegaWatt,
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
            1.asKiloWatt,
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
            1.asKiloWatt,
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
      // storage will send a FlexCompletion message back
      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.of(56842L)

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
      // the storage answers with some flex options
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
            0.asKiloWatt,
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
            controlledEmInput.getUuid
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
      // storage will send a FlexCompletion message back
      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.of(57600L)
    }

    "work correctly with one uncontrolled, one controlled EmAgent and a StorageAgent with multiple tick" in {
      val resultServiceProxy =
        TestProbe[ResultEvent | ExpectResult | NoResult]("ResultServiceProxy")
      val primaryServiceProxy =
        TestProbe[PrimaryServiceProxy.Message]("PrimaryServiceProxy")

      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      given ParticipantRefs = ParticipantRefs(
        primaryServiceProxy = primaryServiceProxy.ref,
        resultServiceProxy = resultServiceProxy.ref,
        services = Map.empty,
      )

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

      val keys = ScheduleLock
        .multiKey(TSpawner, scheduler.ref, PRE_INIT_TICK, 1)
        .iterator
      val lockActivation =
        scheduler.expectMessageType[ScheduleActivation].actor
      lockActivation ! Activation(PRE_INIT_TICK)

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
        "UncontrolledEmAgent_4",
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
        "ControlledEmAgent_4",
      )

      val storageAgent = spawn(
        ParticipantAgentInit(
          storageInputContainer,
          StorageRuntimeConfig(),
          outputConfigOff,
          Right(controlledEmAgent),
          keys.next(),
        ),
        "StorageAgent_4",
      )

      val initSchedule = scheduler.expectMessageType[ScheduleActivation]
      initSchedule.tick shouldBe INIT_SIM_TICK
      initSchedule.actor shouldBe uncontrolledEmAgent

      scheduler.expectNoMessage()

      initSchedule.unlockKey.foreach(_.unlock())
      scheduler.expectMessage(Completion(lockActivation))

      /* INIT */
      uncontrolledEmAgent ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(
          storageAgent,
          storageInput.getUuid,
        )
      )

      // storage
      storageAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      // finish initialization
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
      // the storage answers with some flex options
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
            0.asMegaWatt,
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
          0.asMegaWatt,
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
            1.asKiloWatt,
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
            1.asKiloWatt,
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
      // storage will send a FlexCompletion message back
      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.of(56843L)

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
      // the storage answers with some flex options
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
      // storage will send a FlexCompletion message back
      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.of(56843L)
    }

    "work correctly with one uncontrolled and two controlled EmAgent in one tick" in {
      val resultServiceProxy =
        TestProbe[ResultEvent | ExpectResult | NoResult]("ResultServiceProxy")
      val primaryServiceProxy =
        TestProbe[PrimaryServiceProxy.Message]("PrimaryServiceProxy")

      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      given ParticipantRefs = ParticipantRefs(
        primaryServiceProxy = primaryServiceProxy.ref,
        resultServiceProxy = resultServiceProxy.ref,
        services = Map.empty,
      )

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

      val keys = ScheduleLock
        .multiKey(TSpawner, scheduler.ref, PRE_INIT_TICK, 2)
        .iterator
      val lockActivation =
        scheduler.expectMessageType[ScheduleActivation].actor
      lockActivation ! Activation(PRE_INIT_TICK)

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
        "UncontrolledEmAgent_5",
      )

      val controlledEmAgent1 = spawn(
        EmAgentInit(
          controlledEmInput,
          modelConfig,
          outputConfigOn,
          simulationStartDate,
          parent = Right(uncontrolledEmAgent),
          listener = resultServiceProxy.ref,
          emDataService = Some(emService),
        ),
        "ControlledEmAgent_5_1",
      )

      val controlledEmAgent2 = spawn(
        EmAgentInit(
          controlledEmInput2,
          modelConfig,
          outputConfigOn,
          simulationStartDate,
          parent = Right(uncontrolledEmAgent),
          listener = resultServiceProxy.ref,
          emDataService = Some(emService),
        ),
        "ControlledEmAgent_5_2",
      )

      val storageAgent = spawn(
        ParticipantAgentInit(
          storageInputContainer,
          StorageRuntimeConfig(),
          outputConfigOff,
          Right(controlledEmAgent1),
          keys.next(),
        ),
        "StorageAgent_5",
      )

      val loadAgent = spawn(
        ParticipantAgentInit(
          loadInputContainer,
          LoadRuntimeConfig(),
          outputConfigOff,
          Right(controlledEmAgent2),
          keys.next(),
        ),
        "LoadAgent_5",
      )

      val initSchedule = scheduler.expectMessageType[ScheduleActivation]
      initSchedule.tick shouldBe INIT_SIM_TICK
      initSchedule.actor shouldBe uncontrolledEmAgent

      scheduler.expectNoMessage()

      initSchedule.unlockKey.foreach(_.unlock())
      scheduler.expectMessage(Completion(lockActivation))

      /* INIT */
      uncontrolledEmAgent ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.receiveMessages(2) should contain allOf (
        PrimaryServiceRegistrationMessage(
          loadAgent,
          loadInput.getUuid,
        ),
        PrimaryServiceRegistrationMessage(
          storageAgent,
          storageInput.getUuid,
        )
      )

      // load
      loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      // storage
      storageAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      // finish initialization
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
      val map1 = res1(emInput.getUuid).asScala.collect {
        case msg: EmCommunicationMessage => msg.receiver -> msg
      }.toMap
      val msg11 = map1(controlledEmInput.getUuid)
      msg11.sender shouldBe emInput.getUuid
      msg11.content shouldBe new FlexOptionRequest(controlledEmInput.getUuid)

      val msg12 = map1(controlledEmInput2.getUuid)
      msg12.sender shouldBe emInput.getUuid
      msg12.content shouldBe new FlexOptionRequest(controlledEmInput2.getUuid)

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(0L, ju.List.of(msg11, msg12))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(0L)
      scheduler.expectMessage(Completion(emService))

      // the controlled em should send a request to the storage now
      // the storage answers with some flex options
      // we receive a communication message
      val res2 = mutable.Map.empty[UUID, ju.List[EmData]]
      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach(res2.put)

      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach(res2.put)

      res2.size shouldBe 2
      val msg21 = res2(controlledEmInput.getUuid).asScala.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe emInput.getUuid
          sender shouldBe controlledEmInput.getUuid
          content shouldBe new em.PowerLimitFlexOptions(
            emInput.getUuid,
            controlledEmInput.getUuid,
            0.asMegaWatt,
            0.asMegaWatt,
            0.005.asMegaWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      val msg22 = res2(controlledEmInput2.getUuid).asScala.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe emInput.getUuid
          sender shouldBe controlledEmInput2.getUuid
          content shouldBe new em.PowerLimitFlexOptions(
            emInput.getUuid,
            controlledEmInput2.getUuid,
            2.68603e-4.asMegaWatt,
            2.68603e-4.asMegaWatt,
            2.68603e-4.asMegaWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(0L, ju.List.of(msg21, msg22))

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
          2.68603e-4.asMegaWatt,
          2.68603e-4.asMegaWatt,
          0.005268603.asMegaWatt,
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
            0.001268603.asMegaWatt,
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
      list4.size shouldBe 2
      val map4 = list4.collect { case msg: EmCommunicationMessage =>
        msg.receiver -> msg
      }.toMap

      val msg41 = map4(controlledEmInput.getUuid)
      msg41.receiver shouldBe controlledEmInput.getUuid
      msg41.sender shouldBe emInput.getUuid
      msg41.content shouldBe new em.SetPoint.AggregatedSetPoint(
        controlledEmInput.getUuid,
        1.asKiloWatt,
      )

      val msg42 = map4(controlledEmInput2.getUuid)
      msg42.receiver shouldBe controlledEmInput2.getUuid
      msg42.sender shouldBe emInput.getUuid
      msg42.content shouldBe new em.SetPoint.AggregatedSetPoint(
        controlledEmInput2.getUuid
      )

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(0L, ju.List.of(msg41, msg42))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(0L)
      scheduler.expectMessage(Completion(emService))

      // the storage should now receive the set point
      // storage will send a FlexCompletion message back
      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.of(56842L)

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
      val res5 = mutable.Map.empty[UUID, ju.List[EmData]]
      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach(res5.put)
      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach { case (key, r) =>
          val tmp = new util.ArrayList(res5(key))
          tmp.addAll(r)
          res5.put(key, tmp)
        }
      res5.size shouldBe 1
      val map5 = res5(emInput.getUuid).asScala.collect {
        case msg: EmCommunicationMessage => msg.receiver -> msg
      }.toMap
      val msg51 = map5(controlledEmInput.getUuid)
      msg51.sender shouldBe emInput.getUuid
      msg51.content shouldBe new FlexOptionRequest(controlledEmInput.getUuid)

      val msg52 = map5(controlledEmInput2.getUuid)
      msg52.sender shouldBe emInput.getUuid
      msg52.content shouldBe new FlexOptionRequest(controlledEmInput2.getUuid)

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(
        300L,
        ju.List.of(msg51, msg52),
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(300L)
      scheduler.expectMessage(Completion(emService))

      // the controlled em should send a request to the storage now
      // the storage answers with some flex options
      // we receive a communication message
      val res6 = mutable.Map.empty[UUID, ju.List[EmData]]
      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach(res6.put)

      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach(res6.put)

      res6.size shouldBe 2
      val msg61 = res6(controlledEmInput.getUuid).asScala.headOption match {
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

      val msg62 = res6(controlledEmInput2.getUuid).asScala.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe emInput.getUuid
          sender shouldBe controlledEmInput2.getUuid
          content shouldBe new em.PowerLimitFlexOptions(
            emInput.getUuid,
            controlledEmInput2.getUuid,
            2.68603e-4.asMegaWatt,
            2.68603e-4.asMegaWatt,
            2.68603e-4.asMegaWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(
        300L,
        ju.List.of(msg61, msg62),
      )

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
          (-0.004731397).asMegaWatt,
          0.005268603.asMegaWatt,
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
            0.268603.asKiloWatt,
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
      list8.size shouldBe 2
      val map8 = list8.collect { case msg: EmCommunicationMessage =>
        msg.receiver -> msg
      }.toMap

      val msg81 = map8(controlledEmInput.getUuid)
      msg81.receiver shouldBe controlledEmInput.getUuid
      msg81.sender shouldBe emInput.getUuid
      msg81.content shouldBe new em.SetPoint.AggregatedSetPoint(
        controlledEmInput.getUuid
      )

      val msg82 = map8(controlledEmInput2.getUuid)
      msg82.receiver shouldBe controlledEmInput2.getUuid
      msg82.sender shouldBe emInput.getUuid
      msg82.content shouldBe new em.SetPoint.AggregatedSetPoint(
        controlledEmInput2.getUuid
      )

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(
        300L,
        ju.List.of(msg81, msg82),
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(300L)
      scheduler.expectMessage(Completion(emService))

      // the storage should now receive the set point
      // storage will send a FlexCompletion message back
      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.of(57600L)
    }

    "work correctly with one uncontrolled and two controlled EmAgent in multiple tick" in {
      val resultServiceProxy =
        TestProbe[ResultEvent | ExpectResult | NoResult]("ResultServiceProxy")
      val primaryServiceProxy =
        TestProbe[PrimaryServiceProxy.Message]("PrimaryServiceProxy")

      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

      given ParticipantRefs = ParticipantRefs(
        primaryServiceProxy = primaryServiceProxy.ref,
        resultServiceProxy = resultServiceProxy.ref,
        services = Map.empty,
      )

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

      val keys = ScheduleLock
        .multiKey(TSpawner, scheduler.ref, PRE_INIT_TICK, 2)
        .iterator
      val lockActivation =
        scheduler.expectMessageType[ScheduleActivation].actor
      lockActivation ! Activation(PRE_INIT_TICK)

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
        "UncontrolledEmAgent_6",
      )

      val controlledEmAgent1 = spawn(
        EmAgentInit(
          controlledEmInput,
          modelConfig,
          outputConfigOn,
          simulationStartDate,
          parent = Right(uncontrolledEmAgent),
          listener = resultServiceProxy.ref,
          emDataService = Some(emService),
        ),
        "ControlledEmAgent_6_1",
      )

      val controlledEmAgent2 = spawn(
        EmAgentInit(
          controlledEmInput2,
          modelConfig,
          outputConfigOn,
          simulationStartDate,
          parent = Right(uncontrolledEmAgent),
          listener = resultServiceProxy.ref,
          emDataService = Some(emService),
        ),
        "ControlledEmAgent_6_2",
      )

      val storageAgent = spawn(
        ParticipantAgentInit(
          storageInputContainer,
          StorageRuntimeConfig(),
          outputConfigOff,
          Right(controlledEmAgent1),
          keys.next(),
        ),
        "StorageAgent_6",
      )

      val loadAgent = spawn(
        ParticipantAgentInit(
          loadInputContainer,
          LoadRuntimeConfig(),
          outputConfigOff,
          Right(controlledEmAgent2),
          keys.next(),
        ),
        "LoadAgent_6",
      )

      val initSchedule = scheduler.expectMessageType[ScheduleActivation]
      initSchedule.tick shouldBe INIT_SIM_TICK
      initSchedule.actor shouldBe uncontrolledEmAgent

      scheduler.expectNoMessage()

      initSchedule.unlockKey.foreach(_.unlock())
      scheduler.expectMessage(Completion(lockActivation))

      /* INIT */
      uncontrolledEmAgent ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.receiveMessages(2) should contain allOf (
        PrimaryServiceRegistrationMessage(
          loadAgent,
          loadInput.getUuid,
        ),
        PrimaryServiceRegistrationMessage(
          storageAgent,
          storageInput.getUuid,
        )
      )

      // load
      loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      // storage
      storageAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      // finish initialization
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
      val map1 = res1(emInput.getUuid).asScala.collect {
        case msg: EmCommunicationMessage => msg.receiver -> msg
      }.toMap
      val msg11 = map1(controlledEmInput.getUuid)
      msg11.sender shouldBe emInput.getUuid
      msg11.content shouldBe new FlexOptionRequest(controlledEmInput.getUuid)

      val msg12 = map1(controlledEmInput2.getUuid)
      msg12.sender shouldBe emInput.getUuid
      msg12.content shouldBe new FlexOptionRequest(controlledEmInput2.getUuid)

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(1L, ju.List.of(msg11, msg12))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(1L)
      scheduler.expectMessage(Completion(emService))

      // the controlled em should send a request to the storage now
      // the storage answers with some flex options
      // we receive a communication message
      val res2 = mutable.Map.empty[UUID, ju.List[EmData]]
      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach(res2.put)

      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach(res2.put)

      res2.size shouldBe 2
      val msg21 = res2(controlledEmInput.getUuid).asScala.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe emInput.getUuid
          sender shouldBe controlledEmInput.getUuid
          content shouldBe new em.PowerLimitFlexOptions(
            emInput.getUuid,
            controlledEmInput.getUuid,
            0.asMegaWatt,
            0.asMegaWatt,
            0.005.asMegaWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      val msg22 = res2(controlledEmInput2.getUuid).asScala.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe emInput.getUuid
          sender shouldBe controlledEmInput2.getUuid
          content shouldBe new em.PowerLimitFlexOptions(
            emInput.getUuid,
            controlledEmInput2.getUuid,
            2.68603e-4.asMegaWatt,
            2.68603e-4.asMegaWatt,
            2.68603e-4.asMegaWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(2L, ju.List.of(msg21, msg22))

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
          2.68603e-4.asMegaWatt,
          2.68603e-4.asMegaWatt,
          0.005268603.asMegaWatt,
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
            0.001268603.asMegaWatt,
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
      list4.size shouldBe 2
      val map4 = list4.collect { case msg: EmCommunicationMessage =>
        msg.receiver -> msg
      }.toMap

      val msg41 = map4(controlledEmInput.getUuid)
      msg41.receiver shouldBe controlledEmInput.getUuid
      msg41.sender shouldBe emInput.getUuid
      msg41.content shouldBe new em.SetPoint.AggregatedSetPoint(
        controlledEmInput.getUuid,
        1.asKiloWatt,
      )

      val msg42 = map4(controlledEmInput2.getUuid)
      msg42.receiver shouldBe controlledEmInput2.getUuid
      msg42.sender shouldBe emInput.getUuid
      msg42.content shouldBe new em.SetPoint.AggregatedSetPoint(
        controlledEmInput2.getUuid
      )

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(3L, ju.List.of(msg41, msg42))

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(3L)
      scheduler.expectMessage(Completion(emService))

      // the storage should now receive the set point
      // storage will send a FlexCompletion message back
      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.of(56843L)

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
      val res5 = mutable.Map.empty[UUID, ju.List[EmData]]
      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach(res5.put)
      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach { case (key, r) =>
          val tmp = new util.ArrayList(res5(key))
          tmp.addAll(r)
          res5.put(key, tmp)
        }
      res5.size shouldBe 1
      val map5 = res5(emInput.getUuid).asScala.collect {
        case msg: EmCommunicationMessage => msg.receiver -> msg
      }.toMap
      val msg51 = map5(controlledEmInput.getUuid)
      msg51.sender shouldBe emInput.getUuid
      msg51.content shouldBe new FlexOptionRequest(controlledEmInput.getUuid)

      val msg52 = map5(controlledEmInput2.getUuid)
      msg52.sender shouldBe emInput.getUuid
      msg52.content shouldBe new FlexOptionRequest(controlledEmInput2.getUuid)

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(
        330L,
        ju.List.of(msg51, msg52),
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(330L)
      scheduler.expectMessage(Completion(emService))

      // the controlled em should send a request to the storage now
      // the storage answers with some flex options
      // we receive a communication message
      val res6 = mutable.Map.empty[UUID, ju.List[EmData]]
      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach(res6.put)

      extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .forEach(res6.put)

      res6.size shouldBe 2
      val msg61 = res6(controlledEmInput.getUuid).asScala.headOption match {
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

      val msg62 = res6(controlledEmInput2.getUuid).asScala.headOption match {
        case Some(msg @ EmCommunicationMessage(receiver, sender, _, content)) =>
          receiver shouldBe emInput.getUuid
          sender shouldBe controlledEmInput2.getUuid
          content shouldBe new em.PowerLimitFlexOptions(
            emInput.getUuid,
            controlledEmInput2.getUuid,
            2.68603e-4.asMegaWatt,
            2.68603e-4.asMegaWatt,
            2.68603e-4.asMegaWatt,
          )

          msg
        case other =>
          fail(s"Wrong value: $other")
      }

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(
        360L,
        ju.List.of(msg61, msg62),
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(360L)
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
          (-0.004731397).asMegaWatt,
          0.005268603.asMegaWatt,
        )
      )

      // we will now answer with a new set point
      extEmDataConnection.sendEmData(
        360L,
        Map.empty.asJava,
        Map.empty.asJava,
        Map(
          emInput.getUuid -> new em.SetPoint.AggregatedSetPoint(
            emInput.getUuid,
            0.268603.asKiloWatt,
          )
        ).asJava,
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(360L)
      scheduler.expectMessage(Completion(emService))

      // we receive a communication message
      val res8 = extEmDataConnection
        .receiveWithType(classOf[EmResultResponse])
        .emResults
        .asScala
        .toMap
      res8.size shouldBe 1
      val list8 = res8(emInput.getUuid).asScala
      list8.size shouldBe 2
      val map8 = list8.collect { case msg: EmCommunicationMessage =>
        msg.receiver -> msg
      }.toMap

      val msg81 = map8(controlledEmInput.getUuid)
      msg81.receiver shouldBe controlledEmInput.getUuid
      msg81.sender shouldBe emInput.getUuid
      msg81.content shouldBe new em.SetPoint.AggregatedSetPoint(
        controlledEmInput.getUuid
      )

      val msg82 = map8(controlledEmInput2.getUuid)
      msg82.receiver shouldBe controlledEmInput2.getUuid
      msg82.sender shouldBe emInput.getUuid
      msg82.content shouldBe new em.SetPoint.AggregatedSetPoint(
        controlledEmInput2.getUuid
      )

      // we send the communication message to the receiver
      extEmDataConnection.sendCommunicationMessage(
        400L,
        ju.List.of(msg81, msg82),
      )

      // the service should be activated
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(emService))
      emService ! Activation(400L)
      scheduler.expectMessage(Completion(emService))

      // the storage should now receive the set point
      // storage will send a FlexCompletion message back
      // we are finished for the tick
      extEmDataConnection
        .receiveWithType(classOf[EmCompletion])
        .maybeNextTick shouldBe OptionalLong.of(57600L)
    }

  }

}
