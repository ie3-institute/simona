/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.datamodel.models.result.system.{EvResult, EvcsResult}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
}
import edu.ie3.simona.agent.participant.ParticipantAgent.RequestAssetPowerMessage
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorEvMovementsService
import edu.ie3.simona.agent.participant.evcs.EvcsAgent
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.DataCollectionStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData._
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent,
}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsState,
  ScheduleEntry,
}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  ProvideFlexOptions,
  _,
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.common.{EvTestData, TestSpawnerClassic}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKVAr
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower, Vars}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.{TestFSMRef, TestProbe}
import squants.energy._
import squants.{Each, Energy, Power}

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.immutable.{SortedMap, SortedSet}

class EvcsAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "EvcsAgentModelCalculationSpec",
        ConfigFactory
          .parseString("""
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="DEBUG"
        """.stripMargin),
      )
    )
    with EvcsInputTestData
    with EvTestData
    with TestSpawnerClassic {

  private val requestVoltageDeviationThreshold = 1e-14d

  private val defaultOutputConfig =
    NotifierConfig(
      simulationResultInfo = false,
      powerRequestReply = false,
      flexResult = false,
    )

  private val modelConfig =
    EvcsRuntimeConfig.apply(
      calculateMissingReactivePowerWithModel = false,
      scaling = 1.0,
      uuids = List("default"),
      chargingStrategy = "maxPower",
      lowestEvSoc = 0.2,
    )

  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T02:00:00Z")

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  private val evcsInputModelQv = evcsInputModel
    .copy()
    .qCharacteristics(new QV("qV:{(0.95,-0.625),(1.05,0.625)}"))
    .build()

  private val resolution = 3600L

  private implicit val powerTolerance: Power = Watts(0.1)
  private implicit val energyTolerance: Energy = WattHours(0.1)
  private implicit val reactivePowerTolerance: ReactivePower = Vars(0.1)

  "An evcs agent with model calculation depending on no secondary data service" should {
    val initStateData = ParticipantInitializeStateData[
      EvcsInput,
      EvcsRuntimeConfig,
      ApparentPower,
    ](
      inputModel = evcsInputModel,
      modelConfig = modelConfig,
      secondaryDataServices = None,
      simulationStartDate = simulationStartDate,
      simulationEndDate = simulationEndDate,
      resolution = resolution,
      requestVoltageDeviationThreshold = requestVoltageDeviationThreshold,
      outputConfig = defaultOutputConfig,
      primaryServiceProxy = primaryServiceProxy.ref,
    )

    "be instantiated correctly" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable.empty,
        )
      )
      evcsAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(evcsAgent.stateData) {
        case _: ParticipantUninitializedStateData[_] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${evcsAgent.stateData}."
          )
      }
    }

    "fail initialisation and stop agent" in {
      val deathProbe = TestProbe("deathProbe")

      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable.empty,
        )
      )

      scheduler.send(evcsAgent, Activation(INIT_SIM_TICK))

      deathProbe.watch(evcsAgent.ref)

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      deathProbe.expectTerminated(evcsAgent.ref)
    }
  }

  "An evcs agent with model calculation depending on one secondary data service" should {
    val evService = TestProbe("evService")

    val initStateData = ParticipantInitializeStateData[
      EvcsInput,
      EvcsRuntimeConfig,
      ApparentPower,
    ](
      inputModel = evcsInputModel,
      modelConfig = modelConfig,
      secondaryDataServices = Iterable(
        ActorEvMovementsService(evService.ref)
      ),
      simulationStartDate = simulationStartDate,
      simulationEndDate = simulationEndDate,
      resolution = resolution,
      requestVoltageDeviationThreshold = requestVoltageDeviationThreshold,
      outputConfig = defaultOutputConfig,
      primaryServiceProxy = primaryServiceProxy.ref,
    )

    "be instantiated correctly" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable.empty,
        )
      )

      evcsAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(evcsAgent.stateData) {
        case _: ParticipantUninitializedStateData[_] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${evcsAgent.stateData}."
          )
      }
    }

    "end in correct state with correct state data after initialisation" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable.empty,
        )
      )

      scheduler.send(evcsAgent, Activation(INIT_SIM_TICK))

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(evcsInputModel.getUuid)
      )
      /* State should be information handling and having correct state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case ParticipantInitializingStateData(
              inputModel,
              modelConfig,
              secondaryDataServices,
              simulationStartDate,
              simulationEndDate,
              timeBin,
              requestVoltageDeviationThreshold,
              outputConfig,
              maybeEmAgent,
            ) =>
          inputModel shouldBe SimpleInputContainer(evcsInputModel)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe Iterable(
            ActorEvMovementsService(evService.ref)
          )
          simulationStartDate shouldBe simulationStartDate
          simulationEndDate shouldBe simulationEndDate
          timeBin shouldBe resolution
          requestVoltageDeviationThreshold shouldBe requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
          maybeEmAgent shouldBe None
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* Expect a registration message */
      evService.expectMsg(RegisterForEvDataMessage(evcsInputModel.getUuid))

      /* ... as well as corresponding state and state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case CollectRegistrationConfirmMessages(
              ParticipantModelBaseStateData(
                startDate,
                endDate,
                _,
                services,
                outputConfig,
                additionalActivationTicks,
                foreseenDataTicks,
                _,
                voltageValueStore,
                resultValueStore,
                requestValueStore,
                _,
                _,
                _,
              ),
              awaitRegistrationResponsesFrom,
              foreseenNextDataTicks,
            ) =>
          /* Base state data */
          startDate shouldBe simulationStartDate
          endDate shouldBe simulationEndDate
          services shouldBe Iterable(
            ActorEvMovementsService(evService.ref)
          )
          outputConfig shouldBe NotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false,
            flexResult = false,
          )
          additionalActivationTicks shouldBe empty
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution,
            SortedMap(0L -> Each(1.0)),
          )
          resultValueStore shouldBe ValueStore(resolution)
          requestValueStore shouldBe ValueStore[ApparentPower](resolution)

          /* Additional information */
          awaitRegistrationResponsesFrom shouldBe Iterable(evService.ref)
          foreseenNextDataTicks shouldBe Map.empty
        case _ =>
          fail(
            s"Did not find expected state data $CollectRegistrationConfirmMessages, but ${evcsAgent.stateData}"
          )
      }

      /* Reply, that registration was successful */
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(evService.ref, None),
      )

      /* Expect a completion message */
      scheduler.expectMsg(Completion(evcsAgent.toTyped, None))

      /* ... as well as corresponding state and state data */
      evcsAgent.stateName shouldBe Idle
      evcsAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          /* Only check the awaited next data ticks, as the rest has yet been checked */
          baseStateData.foreseenDataTicks shouldBe Map(evService.ref -> None)
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${evcsAgent.stateData}"
          )
      }
    }

    "answer with zero power, if asked directly after initialisation" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable.empty,
        )
      )

      scheduler.send(evcsAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* Expect a registration message */
      evService.expectMsg(RegisterForEvDataMessage(evcsInputModel.getUuid))
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(evService.ref, Some(900L)),
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]

      evcsAgent.stateName shouldBe Idle
      /* State data has already been tested */

      evcsAgent ! RequestAssetPowerMessage(
        0L,
        Each(1.0),
        Each(0.0),
      )
      expectMsg(
        AssetPowerChangedMessage(
          Megawatts(0.0),
          Megavars(0.0),
        )
      )

      inside(evcsAgent.stateData) {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          baseStateData.requestValueStore shouldBe ValueStore[
            ApparentPower
          ](
            resolution,
            SortedMap(
              0L -> ApparentPower(
                Megawatts(0.0),
                Megavars(0.0),
              )
            ),
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${evcsAgent.stateData}"
          )
      }
    }

    "do correct transitions faced with new data in Idle" in {
      val lock = TestProbe("lock")

      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable.empty,
        )
      )

      scheduler.send(
        evcsAgent,
        Activation(INIT_SIM_TICK),
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(evService.ref, None),
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(evcsAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out new data */
      val arrivingEvsData =
        ArrivingEvsData(Seq(EvModelWrapper(evA), EvModelWrapper(evB)))

      val key1 = Some(ScheduleKey(lock.ref.toTyped, UUID.randomUUID()))
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          evService.ref,
          arrivingEvsData,
          unlockKey = key1,
        ),
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 0, key1))

      /* Find yourself in corresponding state and state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[_, _, _, _],
              expectedSenders,
              isYetTriggered,
            ) =>
          /* The next data tick is already registered */
          baseStateData.foreseenDataTicks shouldBe Map(evService.ref -> None)

          /* The yet sent data is also registered */
          expectedSenders shouldBe Map(
            evService.ref -> Some(arrivingEvsData)
          )

          /* It is not yet triggered */
          isYetTriggered shouldBe false
        case _ =>
          fail(
            s"Did not find expected state data $DataCollectionStateData, but ${evcsAgent.stateData}"
          )
      }

      /* Trigger the agent */
      scheduler.send(
        evcsAgent,
        Activation(0),
      )

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(
        Completion(evcsAgent.toTyped, None)
      )
      evcsAgent.stateName shouldBe Idle
      evcsAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.stateDataStore match {
            case ValueStore(_, store) =>
              store.keys should contain only 0L
              store.get(0L) match {
                case Some(EvcsState(currentEvs, schedule, tick)) =>
                  currentEvs should contain theSameElementsAs Set(
                    EvModelWrapper(evA),
                    EvModelWrapper(evB),
                  )

                  schedule shouldBe Map(
                    evA.getUuid -> SortedSet(
                      ScheduleEntry(
                        0,
                        200,
                        Kilowatts(11.0),
                      )
                    ),
                    evB.getUuid -> SortedSet(
                      ScheduleEntry(
                        0,
                        200,
                        Kilowatts(11.0),
                      )
                    ),
                  )

                  tick shouldBe 0L
                case None => fail("Entry for tick 0 expected.")
              }
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              // Since departures and arrivals are considered separately,
              // EvcsAgent calculates results only with the next activation
              store.keys shouldBe empty
          }
        case _ =>
          fail(
            s"Did not found the expected state data $ParticipantModelBaseStateData, but ${evcsAgent.stateData}"
          )
      }
    }

    "do correct transitions triggered for activation in idle" in {
      val lock = TestProbe("lock")

      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable.empty,
        )
      )

      scheduler.send(
        evcsAgent,
        Activation(INIT_SIM_TICK),
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(evService.ref, None),
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(evcsAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out activation */
      scheduler.send(
        evcsAgent,
        Activation(0),
      )

      /* Find yourself in corresponding state and state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[_, _, _, _],
              expectedSenders,
              isYetTriggered,
            ) =>
          /* The next data tick is already registered */
          baseStateData.foreseenDataTicks shouldBe Map(evService.ref -> None)

          /* The yet sent data is also registered */
          expectedSenders shouldBe Map.empty

          /* It is not yet triggered */
          isYetTriggered shouldBe true
        case _ =>
          fail(
            s"Did not find expected state data $DataCollectionStateData, but ${evcsAgent.stateData}"
          )
      }

      /* Send out new data */
      val arrivingEvsData =
        ArrivingEvsData(Seq(EvModelWrapper(evA), EvModelWrapper(evB)))

      val key1 = Some(ScheduleKey(lock.ref.toTyped, UUID.randomUUID()))
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          evService.ref,
          arrivingEvsData,
          unlockKey = key1,
        ),
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 0, key1))

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(
        Completion(evcsAgent.toTyped, None)
      )
      evcsAgent.stateName shouldBe Idle
      evcsAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.stateDataStore match {
            case ValueStore(_, store) =>
              store.keys should contain only 0L
              store.get(0L) match {
                case Some(EvcsState(currentEvs, schedule, tick)) =>
                  currentEvs should contain theSameElementsAs Set(
                    EvModelWrapper(evA),
                    EvModelWrapper(evB),
                  )
                  schedule shouldBe Map(
                    evA.getUuid ->
                      SortedSet(
                        ScheduleEntry(
                          0,
                          200,
                          Kilowatts(11.0),
                        )
                      ),
                    evB.getUuid ->
                      SortedSet(
                        ScheduleEntry(
                          0,
                          200,
                          Kilowatts(11.0),
                        )
                      ),
                  )

                  tick shouldBe 0L
                case None => fail("Entry for tick 0 expected.")
              }
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              // Since departures and arrivals are considered separately,
              // EvcsAgent calculates results only with the next activation
              store shouldBe empty
          }
        case _ =>
          fail(
            s"Did not found the expected state data $ParticipantModelBaseStateData, but ${evcsAgent.stateData}"
          )
      }
    }

    "provide power right away because no data is awaited" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable.empty,
        )
      )

      scheduler.send(
        evcsAgent,
        Activation(INIT_SIM_TICK),
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(evService.ref, None),
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(evcsAgent.stateName shouldBe Idle)

      evcsAgent ! RequestAssetPowerMessage(
        7200L,
        Each(1.0),
        Each(0.0),
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0.0))
          q should approximate(Megavars(0.0))
      }
    }

    "provide number of free lots when asked to" in {
      val lock = TestProbe("lock")

      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable.empty,
        )
      )

      scheduler.send(
        evcsAgent,
        Activation(INIT_SIM_TICK),
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(evService.ref, None),
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(evcsAgent.stateName shouldBe Idle)

      /* Send out public evcs request */
      evService.send(
        evcsAgent,
        EvFreeLotsRequest(0L),
      )

      evService.expectMsg(
        FreeLotsResponse(
          evcsInputModel.getUuid,
          2,
        )
      )

      scheduler.expectNoMessage()

      /* Send ev for this tick */
      val key1 = Some(ScheduleKey(lock.ref.toTyped, UUID.randomUUID()))
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          evService.ref,
          ArrivingEvsData(Seq(EvModelWrapper(evA))),
          unlockKey = key1,
        ),
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 0, key1))

      scheduler.send(
        evcsAgent,
        Activation(0),
      )
      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* Ask for public evcs lot count again with a later tick */
      evService.send(
        evcsAgent,
        EvFreeLotsRequest(3600L),
      )

      // this time, only one is still free
      evService.expectMsg(
        FreeLotsResponse(
          evcsInputModel.getUuid,
          1,
        )
      )

      scheduler.expectNoMessage()
    }

    val evcsAgent = TestFSMRef(
      new EvcsAgent(
        scheduler = scheduler.ref,
        initStateData = ParticipantInitializeStateData(
          inputModel = evcsInputModelQv,
          modelConfig = modelConfig,
          secondaryDataServices = Iterable(
            ActorEvMovementsService(evService.ref)
          ),
          simulationStartDate = simulationStartDate,
          simulationEndDate = simulationEndDate,
          resolution = resolution,
          requestVoltageDeviationThreshold = requestVoltageDeviationThreshold,
          outputConfig = defaultOutputConfig,
          primaryServiceProxy = primaryServiceProxy.ref,
        ),
        listener = systemListener,
      )
    )

    "provide correct average power after three data ticks are available" in {
      val lock = TestProbe("lock")

      scheduler.send(evcsAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(evService.ref, None),
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(evcsAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 0 */
      val key1 = Some(ScheduleKey(lock.ref.toTyped, UUID.randomUUID()))
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          evService.ref,
          ArrivingEvsData(Seq(EvModelWrapper(evA.copyWithDeparture(3600L)))),
          unlockKey = key1,
        ),
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 0, key1))
      scheduler.send(evcsAgent, Activation(0))
      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* ... for tick 3600 */

      // departures first
      evService.send(
        evcsAgent,
        DepartingEvsRequest(3600L, Seq(evA.getUuid)),
      )
      evService.expectMsgType[DepartingEvsResponse] match {
        case DepartingEvsResponse(evcs, evModels) =>
          evcs shouldBe evcsInputModelQv.getUuid
          evModels should have size 1
          evModels.headOption match {
            case Some(evModel) =>
              evModel.uuid shouldBe evA.getUuid
              evModel.storedEnergy should approximate(KilowattHours(11.0))
            case None => fail("Expected to get at least one ev.")
          }
      }

      // arrivals second
      val key2 = Some(ScheduleKey(lock.ref.toTyped, UUID.randomUUID()))
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          3600L,
          evService.ref,
          ArrivingEvsData(Seq(EvModelWrapper(evB.copyWithDeparture(7200L)))),
          unlockKey = key2,
        ),
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 3600, key2))

      scheduler.send(evcsAgent, Activation(3600))
      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* ... for tick 7200 */

      // departures first
      evService.send(
        evcsAgent,
        DepartingEvsRequest(7200L, Seq(evB.getUuid)),
      )
      evService.expectMsgType[DepartingEvsResponse] match {
        case DepartingEvsResponse(evcs, evModels) =>
          evcs shouldBe evcsInputModelQv.getUuid
          evModels should have size 1
          evModels.headOption match {
            case Some(evModel) =>
              evModel.uuid shouldBe evB.getUuid
              evModel.storedEnergy should approximate(KilowattHours(11.0))
            case None => fail("Expected to get at least one ev.")
          }
      }

      val key3 = Some(ScheduleKey(lock.ref.toTyped, UUID.randomUUID()))
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          7200L,
          evService.ref,
          ArrivingEvsData(Seq(EvModelWrapper(evA.copyWithDeparture(10800L)))),
          unlockKey = key3,
        ),
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 7200, key3))

      scheduler.send(evcsAgent, Activation(7200))

      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* Ask the agent for average power in tick 7500 */
      evcsAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1.0),
        Each(0.0),
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0.011))
          q should approximate(Megavars(0.0))
        case answer => fail(s"Did not expect to get that answer: $answer")
      }
    }

    "answer unchanged power values after asking a second time with considerably same voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with (nearly) unchanged information */
      evcsAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1.000000000000001d),
        Each(0.0),
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should approximate(Megawatts(0.011))
          q should approximate(Megavars(0.0))
      }
    }

    "answer changed power values after asking a second time with different voltage" in {
      /* Ask again with changed information */
      evcsAgent ! RequestAssetPowerMessage(
        7500L,
        Each(0.98),
        Each(0.0),
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0.011))
          q should approximate(Megavars(-0.0067133728))
      }
    }
  }

  "An evcs agent with model calculation controlled by an EmAgent" should {

    "be initialized correctly" in {
      val evService = TestProbe("evService")
      val emAgent = TestProbe("EmAgentProbe")

      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = ParticipantInitializeStateData(
            inputModel = evcsInputModelQv,
            modelConfig = modelConfig,
            secondaryDataServices = Iterable(
              ActorEvMovementsService(evService.ref)
            ),
            simulationStartDate = simulationStartDate,
            simulationEndDate = simulationEndDate,
            resolution = resolution,
            requestVoltageDeviationThreshold = requestVoltageDeviationThreshold,
            outputConfig = defaultOutputConfig,
            primaryServiceProxy = primaryServiceProxy.ref,
            maybeEmAgent = Some(emAgent.ref.toTyped),
          ),
          listener = Iterable.empty,
        )
      )

      scheduler.send(evcsAgent, Activation(INIT_SIM_TICK))

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(evcsInputModelQv.getUuid)
      )
      /* State should be information handling and having correct state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case ParticipantInitializingStateData(
              inputModel,
              modelConfig,
              secondaryDataServices,
              simulationStartDate,
              simulationEndDate,
              resolution,
              requestVoltageDeviationThreshold,
              outputConfig,
              maybeEmAgent,
            ) =>
          inputModel shouldBe SimpleInputContainer(evcsInputModelQv)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe Iterable(
            ActorEvMovementsService(evService.ref)
          )
          simulationStartDate shouldBe simulationStartDate
          simulationEndDate shouldBe simulationEndDate
          resolution shouldBe resolution
          requestVoltageDeviationThreshold shouldBe requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
          maybeEmAgent shouldBe Some(emAgent.ref.toTyped)
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      emAgent.expectMsg(
        RegisterParticipant(
          evcsInputModelQv.getUuid,
          evcsAgent.toTyped,
          evcsInputModelQv,
        )
      )
      // only receive registration message. ScheduleFlexRequest after secondary service initialized
      emAgent.expectNoMessage()

      evService.expectMsg(RegisterForEvDataMessage(evcsInputModelQv.getUuid))
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(evService.ref, None),
      )

      emAgent.expectMsg(
        ScheduleFlexRequest(evcsInputModelQv.getUuid, 0)
      )

      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* ... as well as corresponding state and state data */
      evcsAgent.stateName shouldBe Idle
      evcsAgent.stateData match {
        case ParticipantModelBaseStateData(
              startDate,
              endDate,
              _,
              services,
              outputConfig,
              additionalActivationTicks,
              foreseenDataTicks,
              _,
              voltageValueStore,
              resultValueStore,
              requestValueStore,
              _,
              _,
              _,
            ) =>
          /* Base state data */
          startDate shouldBe simulationStartDate
          endDate shouldBe simulationEndDate
          services shouldBe Iterable(
            ActorEvMovementsService(evService.ref)
          )
          outputConfig shouldBe defaultOutputConfig
          additionalActivationTicks shouldBe empty
          foreseenDataTicks shouldBe Map(evService.ref -> None)
          voltageValueStore shouldBe ValueStore(
            resolution,
            SortedMap(0L -> Each(1.0)),
          )
          resultValueStore shouldBe ValueStore(
            resolution
          )
          requestValueStore shouldBe ValueStore[ApparentPower](
            resolution
          )
        case unrecognized =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but $unrecognized"
          )
      }
    }

    "provide correct flex options when in Idle" in {
      val evService = TestProbe("evService")
      val emAgent = TestProbe("EmAgentProbe")
      val resultListener = TestProbe("ResultListener")

      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = ParticipantInitializeStateData(
            inputModel = SimpleInputContainer(evcsInputModelQv),
            modelConfig = modelConfig,
            secondaryDataServices = Iterable(
              ActorEvMovementsService(evService.ref)
            ),
            simulationStartDate = simulationStartDate,
            simulationEndDate = simulationEndDate,
            resolution = resolution,
            requestVoltageDeviationThreshold = requestVoltageDeviationThreshold,
            outputConfig = NotifierConfig(
              simulationResultInfo = true,
              powerRequestReply = false,
              flexResult = true,
            ),
            primaryServiceProxy = primaryServiceProxy.ref,
            maybeEmAgent = Some(emAgent.ref.toTyped),
          ),
          listener = Iterable(resultListener.ref),
        )
      )

      scheduler.send(evcsAgent, Activation(INIT_SIM_TICK))

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(evcsInputModelQv.getUuid)
      )
      /* State should be information handling and having correct state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case ParticipantInitializingStateData(
              inputModel,
              modelConfig,
              secondaryDataServices,
              simulationStartDate,
              simulationEndDate,
              resolution,
              requestVoltageDeviationThreshold,
              outputConfig,
              maybeEmAgent,
            ) =>
          inputModel shouldBe SimpleInputContainer(evcsInputModelQv)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe Iterable(
            ActorEvMovementsService(evService.ref)
          )
          simulationStartDate shouldBe simulationStartDate
          simulationEndDate shouldBe simulationEndDate
          resolution shouldBe resolution
          requestVoltageDeviationThreshold shouldBe requestVoltageDeviationThreshold
          outputConfig shouldBe NotifierConfig(
            simulationResultInfo = true,
            powerRequestReply = false,
            flexResult = true,
          )
          maybeEmAgent shouldBe Some(emAgent.ref.toTyped)
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      emAgent.expectMsg(
        RegisterParticipant(
          evcsInputModelQv.getUuid,
          evcsAgent.toTyped,
          evcsInputModelQv,
        )
      )
      emAgent.expectNoMessage()

      evService.expectMsg(RegisterForEvDataMessage(evcsInputModelQv.getUuid))
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(evService.ref, None),
      )

      emAgent.expectMsg(
        ScheduleFlexRequest(evcsInputModelQv.getUuid, 0)
      )

      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* TICK 0 (expected activation)
         - currently no cars
       */

      emAgent.send(evcsAgent, RequestFlexOptions(0))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              refPower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          refPower should approximate(Kilowatts(0.0))
          minPower should approximate(Kilowatts(0.0))
          maxPower should approximate(Kilowatts(0.0))
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModelQv.getUuid
        flexResult.getTime shouldBe 0.toDateTime
        flexResult.getpRef should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMin should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(0d.asKiloWatt)
      }

      emAgent.send(
        evcsAgent,
        IssueNoControl(0),
      )

      // next potential activation at fully charged battery:
      // net power = 12.961kW * 0.92 = 11.92412kW
      // time to charge fully ~= 16.7727262054h = 60382 ticks (rounded)
      emAgent.expectMsgPF() {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          result.p should approximate(Kilowatts(0))
          result.q should approximate(Megavars(0))
          requestAtNextActivation shouldBe false
          requestAtTick shouldBe None
      }

      // results arrive after next activation
      resultListener.expectNoMessage()

      /* TICK 900
         - ev 900 arrives
         - charging with 11 kW
       */

      val ev900 = EvModelWrapper(evA.copyWithDeparture(4500))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          900,
          evService.ref,
          ArrivingEvsData(Seq(ev900)),
        ),
      )

      emAgent.expectMsg(ScheduleFlexRequest(evcsInputModelQv.getUuid, 900))
      emAgent.send(evcsAgent, RequestFlexOptions(900))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          referencePower shouldBe ev900.sRatedAc
          minPower shouldBe ev900.sRatedAc // battery is empty
          maxPower shouldBe ev900.sRatedAc
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModelQv.getUuid
        flexResult.getTime shouldBe 900.toDateTime
        flexResult.getpRef should beEquivalentTo(ev900.unwrap().getSRatedAC)
        flexResult.getpMin should beEquivalentTo(ev900.unwrap().getSRatedAC)
        flexResult.getpMax should beEquivalentTo(ev900.unwrap().getSRatedAC)
      }

      emAgent.send(evcsAgent, IssueNoControl(900))

      // at 4500 ev is departing
      emAgent.expectMsgPF() {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          result.p should approximate(
            Kilowatts(
              ev900
                .unwrap()
                .getSRatedAC
                .to(PowerSystemUnits.KILOWATT)
                .getValue
                .doubleValue
            )
          )
          result.q should approximate(Megavars(0))
          requestAtNextActivation shouldBe true
          requestAtTick shouldBe Some(4500)
      }

      // result of tick 0
      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModelQv.getUuid
          result.getTime shouldBe 0.toDateTime
          result.getP should beEquivalentTo(0d.asMegaWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 4500
         - ev900 departs, ev4500 arrives
         - charging with 11 kW
       */

      // departure first
      evService.send(
        evcsAgent,
        DepartingEvsRequest(4500, Seq(ev900.uuid)),
      )

      evService.expectMsgPF() { case DepartingEvsResponse(uuid, evs) =>
        evs.size shouldBe 1
        uuid shouldBe evcsInputModelQv.getUuid
        evs.headOption.foreach { ev =>
          ev.uuid shouldBe ev900.uuid
          ev.storedEnergy should approximate(KilowattHours(11.0))
        }
      }

      // results arrive right after departure request
      Range(0, 2)
        .map { _ =>
          resultListener.expectMsgType[ParticipantResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(result: EvResult)
              if result.getTime.equals(900.toDateTime) =>
            result.getInputModel shouldBe ev900.uuid
            result.getP should beEquivalentTo(ev900.unwrap().getSRatedAC)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(0d.asPercent)
          case ParticipantResultEvent(result: EvResult)
              if result.getTime.equals(4500.toDateTime) =>
            result.getInputModel shouldBe ev900.uuid
            result.getP should beEquivalentTo(0d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(18.96551724137931d.asPercent)
        }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModelQv.getUuid
          result.getTime shouldBe 900.toDateTime
          result.getP should beEquivalentTo(ev900.unwrap().getSRatedAC)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      val ev4500 = EvModelWrapper(evB.copyWithDeparture(72000))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          4500,
          evService.ref,
          ArrivingEvsData(Seq(ev4500)),
        ),
      )

      emAgent.expectMsg(ScheduleFlexRequest(evcsInputModelQv.getUuid, 4500))
      emAgent.send(evcsAgent, RequestFlexOptions(4500))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          referencePower shouldBe ev4500.sRatedAc
          minPower shouldBe ev900.sRatedAc // battery is empty
          maxPower shouldBe ev4500.sRatedAc
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModelQv.getUuid
        flexResult.getTime shouldBe 4500.toDateTime
        flexResult.getpRef should beEquivalentTo(ev4500.unwrap().getSRatedAC)
        flexResult.getpMin should beEquivalentTo(ev4500.unwrap().getSRatedAC)
        flexResult.getpMax should beEquivalentTo(ev4500.unwrap().getSRatedAC)
      }

      emAgent.send(evcsAgent, IssueNoControl(4500))

      // we currently have an empty battery in ev4500
      // time to charge to minimal soc ~= 1.45454545455h = 5236 ticks (rounded) from now
      // current tick is 4500, thus: 4500 + 5236 = 9736
      emAgent.expectMsgPF() {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          result.p should approximate(Kilowatts(11))
          result.q should approximate(Megavars(0))
          requestAtNextActivation shouldBe true
          requestAtTick shouldBe Some(9736)
      }

      // already sent out after EV departed
      resultListener.expectNoMessage()

      /* TICK 9736
         - flex control changes
         - charging with 10 kW
       */

      // sending flex request at very next activated tick
      emAgent.send(evcsAgent, RequestFlexOptions(9736))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          referencePower shouldBe ev4500.sRatedAc
          minPower shouldBe Kilowatts(0.0) // battery is exactly at margin
          maxPower shouldBe ev4500.sRatedAc
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModelQv.getUuid
        flexResult.getTime shouldBe 9736.toDateTime
        flexResult.getpRef should beEquivalentTo(ev4500.unwrap().getSRatedAC)
        flexResult.getpMin should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(ev4500.unwrap().getSRatedAC)
      }

      emAgent.send(evcsAgent, IssuePowerControl(9736, Kilowatts(10.0)))

      evService.expectNoMessage()

      // ev4500 is now at 16 kWh
      // time to charge fully = 6.4 h = 23040 ticks from now
      // current tick is 9736, thus: 9736 + 23040 = 32776
      emAgent.expectMsgPF() {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          result.p should approximate(Kilowatts(10))
          result.q should approximate(Megavars(0))
          // since battery is still below lowest soc, it's still considered empty
          requestAtNextActivation shouldBe true
          requestAtTick shouldBe Some(32776)
      }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvResult) =>
          result.getInputModel shouldBe ev4500.uuid
          result.getTime shouldBe 4500.toDateTime
          result.getP should beEquivalentTo(11d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(0d.asPercent)
      }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModelQv.getUuid
          result.getTime shouldBe 4500.toDateTime
          result.getP should beEquivalentTo(11d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 11700
         - ev11700 arrives
         - charging with 16 kW
       */

      // with stored energy right at minimal SOC
      val ev11700 = EvModelWrapper(
        evA.copyWithDeparture(36000).copyWith(11.6d.asKiloWattHour)
      )

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          11700,
          evService.ref,
          ArrivingEvsData(Seq(ev11700)),
        ),
      )

      emAgent.expectMsg(ScheduleFlexRequest(evcsInputModelQv.getUuid, 11700))
      emAgent.send(evcsAgent, RequestFlexOptions(11700))

      val combinedChargingPower =
        ev11700.unwrap().getSRatedAC.add(ev4500.unwrap().getSRatedAC)
      val combinedChargingPowerSq = Kilowatts(
        combinedChargingPower.to(PowerSystemUnits.KILOWATT).getValue.doubleValue
      )

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              refPower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          refPower shouldBe combinedChargingPowerSq
          minPower shouldBe ev4500.sRatedAc * -1 // battery of earlier ev is above lowest soc now
          maxPower shouldBe combinedChargingPowerSq
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModelQv.getUuid
        flexResult.getTime shouldBe 11700.toDateTime
        flexResult.getpRef should beEquivalentTo(combinedChargingPower)
        flexResult.getpMin should beEquivalentTo(
          ev4500.unwrap().getSRatedAC.multiply(-1)
        )
        flexResult.getpMax should beEquivalentTo(combinedChargingPower)
      }

      emAgent.send(evcsAgent, IssuePowerControl(11700, Kilowatts(16)))

      // no departing evs here
      evService.expectNoMessage()

      // ev4500 is now at ~ 21.45555556 kWh, ev11700 just arrived with 11.6 kWh
      // ev4500: time to charge fully ~= 7.3180556 h = 26345 ticks from now
      // ev11700: time to charge fully = 5.8 h = 20880 ticks from now
      // current tick is 11700, thus: 11700 + 20880 = 32580
      emAgent.expectMsgPF() {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          result.p should approximate(Kilowatts(16))
          result.q should approximate(Megavars(0))
          // since battery is still below lowest soc, it's still considered empty
          requestAtNextActivation shouldBe true
          requestAtTick shouldBe Some(32580)
      }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvResult) =>
          result.getInputModel shouldBe ev4500.uuid
          result.getTime shouldBe 9736.toDateTime
          result.getP should beEquivalentTo(10d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(20d.asPercent, 1e-2)
      }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModelQv.getUuid
          result.getTime shouldBe 9736.toDateTime
          result.getP should beEquivalentTo(10d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 18000
         - flex control changes
         - discharging with 20 kW
       */

      // sending flex request at very next activated tick
      emAgent.send(evcsAgent, RequestFlexOptions(18000))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          referencePower shouldBe combinedChargingPowerSq
          minPower shouldBe combinedChargingPowerSq * -1 // battery of both evs is above lowest soc now
          maxPower shouldBe combinedChargingPowerSq
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModelQv.getUuid
        flexResult.getTime shouldBe 18000.toDateTime
        flexResult.getpRef should beEquivalentTo(combinedChargingPower)
        flexResult.getpMin should beEquivalentTo(
          combinedChargingPower.multiply(
            -1
          )
        )
        flexResult.getpMax should beEquivalentTo(combinedChargingPower)
      }

      emAgent.send(evcsAgent, IssuePowerControl(18000, Kilowatts(-20)))

      // no departing evs here
      evService.expectNoMessage()

      // ev4500 is now at ~ 35.455556 kWh, ev11700 at 25.6 kWh
      // ev4500: time to discharge to lowest soc ~= 1.9455556 h = 7004 ticks from now
      // ev11700: time to discharge to lowest soc ~= 1.4 h = 5040 ticks from now
      // current tick is 18000, thus: 18000 + 5040 = 23040
      emAgent.expectMsgPF() {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          result.p should approximate(Kilowatts(-20))
          result.q should approximate(Megavars(0))
          requestAtNextActivation shouldBe false
          requestAtTick shouldBe Some(23040)
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMsgType[ParticipantResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev4500.uuid =>
            result.getTime shouldBe 11700.toDateTime
            result.getP should beEquivalentTo(8d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(26.819d.asPercent, 1e-2)
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev11700.uuid =>
            result.getTime shouldBe 11700.toDateTime
            result.getP should beEquivalentTo(8d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(20.0d.asPercent)
        }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModelQv.getUuid
          result.getTime shouldBe 11700.toDateTime
          result.getP should beEquivalentTo(16d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 23040
         - ev11700 at lowest soc
         - discharging with 10 kW
       */

      emAgent.send(evcsAgent, RequestFlexOptions(23040))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          referencePower shouldBe combinedChargingPowerSq
          minPower shouldBe ev4500.sRatedAc * -1 // battery of ev11700 is below lowest soc now
          maxPower shouldBe combinedChargingPowerSq
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModelQv.getUuid
        flexResult.getTime shouldBe 23040.toDateTime
        flexResult.getpRef should beEquivalentTo(combinedChargingPower)
        flexResult.getpMin should beEquivalentTo(
          ev4500
            .unwrap()
            .getSRatedAC
            .multiply(
              -1
            )
        )
        flexResult.getpMax should beEquivalentTo(combinedChargingPower)
      }

      emAgent.send(evcsAgent, IssuePowerControl(23040, Kilowatts(-10)))

      // no departing evs here
      evService.expectNoMessage()

      // ev4500 is now at 21.455556 kWh, ev11700 at 11.6 kWh (lowest soc)
      // ev4500: time to discharge to lowest soc =  0.5455556 h = 1964 ticks from now
      // current tick is 18864, thus: 23040 + 1964 = 25004
      emAgent.expectMsgPF() {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          result.p should approximate(Kilowatts(-10))
          result.q should approximate(Megavars(0))
          requestAtNextActivation shouldBe false
          requestAtTick shouldBe Some(25004)
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMsgType[ParticipantResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev4500.uuid =>
            result.getTime shouldBe 18000.toDateTime
            result.getP should beEquivalentTo((-10d).asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(44.3194d.asPercent, 1e-2)
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev11700.uuid =>
            result.getTime shouldBe 18000.toDateTime
            result.getP should beEquivalentTo((-10d).asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(44.137931034d.asPercent, 1e-6)
        }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModelQv.getUuid
          result.getTime shouldBe 18000.toDateTime
          result.getP should beEquivalentTo((-20d).asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 25004
         - both evs at lowest soc
         - no power
       */

      emAgent.send(evcsAgent, RequestFlexOptions(25004))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          referencePower shouldBe combinedChargingPowerSq
          minPower shouldBe Kilowatts(0.0) // both at lowest soc
          maxPower shouldBe combinedChargingPowerSq
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModelQv.getUuid
        flexResult.getTime shouldBe 25004.toDateTime
        flexResult.getpRef should beEquivalentTo(combinedChargingPower)
        flexResult.getpMin should beEquivalentTo(0.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(combinedChargingPower)
      }

      emAgent.send(evcsAgent, IssuePowerControl(25004, Kilowatts(0.0)))

      // no departing evs here
      evService.expectNoMessage()

      // no new activation
      emAgent.expectMsgPF() {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          result.p should approximate(Kilowatts(0))
          result.q should approximate(Megavars(0))
          requestAtNextActivation shouldBe false
          requestAtTick shouldBe None
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMsgType[ParticipantResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev4500.uuid =>
            result.getTime shouldBe 23040.toDateTime
            result.getP should beEquivalentTo((-10d).asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(26.819445d.asPercent, 1e-2)
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev11700.uuid =>
            result.getTime shouldBe 23040.toDateTime
            result.getP should beEquivalentTo(0d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(20d.asPercent)
        }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModelQv.getUuid
          result.getTime shouldBe 23040.toDateTime
          result.getP should beEquivalentTo((-10d).asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 36000
         - ev11700 departs
         - charging with 4 kW
       */

      // departure first
      evService.send(
        evcsAgent,
        DepartingEvsRequest(36000, Seq(ev900.uuid)),
      )

      evService.expectMsgPF() { case DepartingEvsResponse(uuid, evs) =>
        evs.size shouldBe 1
        uuid shouldBe evcsInputModelQv.getUuid
        evs.headOption.foreach { ev =>
          ev.uuid shouldBe ev11700.uuid
          ev.storedEnergy should approximate(KilowattHours(11.6))
        }
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMsgType[ParticipantResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev4500.uuid =>
            result.getTime shouldBe 25004.toDateTime
            result.getP should beEquivalentTo(0d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(20d.asPercent, 1e-2)
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev11700.uuid =>
            result.getTime shouldBe 25004.toDateTime
            result.getP should beEquivalentTo(0d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(20d.asPercent)
        }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModelQv.getUuid
          result.getTime shouldBe 25004.toDateTime
          result.getP should beEquivalentTo(0d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      // sending flex request at very next activated tick
      emAgent.send(evcsAgent, RequestFlexOptions(36000))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          referencePower shouldBe ev4500.sRatedAc
          minPower shouldBe Kilowatts(0d)
          maxPower shouldBe ev4500.sRatedAc
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModelQv.getUuid
        flexResult.getTime shouldBe 36000.toDateTime
        flexResult.getpRef should beEquivalentTo(ev4500.unwrap().getSRatedAC)
        flexResult.getpMin should beEquivalentTo(0.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(ev4500.unwrap().getSRatedAC)
      }

      emAgent.send(evcsAgent, IssuePowerControl(36000, Kilowatts(4.0)))

      // ev11700 is now at 16 kWh
      // ev11700: time to charge fully = 16 h = 57600 ticks from now
      // current tick is 36000, thus: 36000 + 57600 = 93600
      // BUT: departing tick 72000 is earlier
      emAgent.expectMsgPF() {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          result.p should approximate(Kilowatts(4))
          result.q should approximate(Megavars(0))
          // since we're starting from lowest again
          requestAtNextActivation shouldBe true
          requestAtTick shouldBe Some(72000)
      }

      // expect no more messages after completion of initialization
      scheduler.expectNoMessage()

    }

    //TODO DF: Finalize Test
  /*  "provide correct results for three evs charging at same time" in {
      val evService = TestProbe("evService")
      val emAgent = TestProbe("EmAgentProbe")
      val resultListener = TestProbe("ResultListener")

      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = ParticipantInitializeStateData(
            inputModel = SimpleInputContainer(evcsInputModelQv),
            modelConfig = modelConfig,
            secondaryDataServices = Iterable(
              ActorEvMovementsService(evService.ref)
            ),
            simulationStartDate = simulationStartDate,
            simulationEndDate = simulationEndDate,
            resolution = resolution,
            requestVoltageDeviationThreshold = requestVoltageDeviationThreshold,
            outputConfig = NotifierConfig(
              simulationResultInfo = true,
              powerRequestReply = false,
              flexResult = true,
            ),
            primaryServiceProxy = primaryServiceProxy.ref,
            maybeEmAgent = Some(emAgent.ref.toTyped),
          ),
          listener = Iterable(resultListener.ref),
        )
      )

      scheduler.send(evcsAgent, Activation(INIT_SIM_TICK))

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(evcsInputModelQv.getUuid)
      )
      /* State should be information handling and having correct state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case ParticipantInitializingStateData(
              inputModel,
              modelConfig,
              secondaryDataServices,
              simulationStartDate,
              simulationEndDate,
              resolution,
              requestVoltageDeviationThreshold,
              outputConfig,
              maybeEmAgent,
            ) =>
          inputModel shouldBe SimpleInputContainer(evcsInputModelQv)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe Iterable(
            ActorEvMovementsService(evService.ref)
          )
          simulationStartDate shouldBe simulationStartDate
          simulationEndDate shouldBe simulationEndDate
          resolution shouldBe resolution
          requestVoltageDeviationThreshold shouldBe requestVoltageDeviationThreshold
          outputConfig shouldBe NotifierConfig(
            simulationResultInfo = true,
            powerRequestReply = false,
            flexResult = true,
          )
          maybeEmAgent shouldBe Some(emAgent.ref.toTyped)
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      emAgent.expectMsg(
        RegisterParticipant(
          evcsInputModelQv.getUuid,
          evcsAgent.toTyped,
          evcsInputModelQv,
        )
      )
      emAgent.expectNoMessage()

      evService.expectMsg(RegisterForEvDataMessage(evcsInputModelQv.getUuid))
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(evService.ref, None),
      )

      emAgent.expectMsg(
        ScheduleFlexRequest(evcsInputModelQv.getUuid, 0)
      )

      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* TICK 0 (expected activation)
         - currently no cars
       */

      emAgent.send(evcsAgent, RequestFlexOptions(0))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              refPower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          refPower should approximate(Kilowatts(0.0))
          minPower should approximate(Kilowatts(0.0))
          maxPower should approximate(Kilowatts(0.0))
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModelQv.getUuid
        flexResult.getTime shouldBe 0.toDateTime
        flexResult.getpRef should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMin should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(0d.asKiloWatt)
      }

      emAgent.send(
        evcsAgent,
        IssueNoControl(0),
      )

      // next potential activation at fully charged battery:
      // net power = 12.961kW * 0.92 = 11.92412kW
      // time to charge fully ~= 16.7727262054h = 60382 ticks (rounded)
      emAgent.expectMsgPF() {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          result.p should approximate(Kilowatts(0))
          result.q should approximate(Megavars(0))
          requestAtNextActivation shouldBe false
          requestAtTick shouldBe None
      }

      // results arrive after next activation
      resultListener.expectNoMessage()

      /* TICK 900
         - ev 900 arrives
         - charging with 11 kW
       */

      val ev900 = EvModelWrapper(evA.copyWithDeparture(3600))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          900,
          evService.ref,
          ArrivingEvsData(Seq(ev900)),
        ),
      )

      emAgent.expectMsg(ScheduleFlexRequest(evcsInputModelQv.getUuid, 900))
      emAgent.send(evcsAgent, RequestFlexOptions(900))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe evcsInputModelQv.getUuid
          referencePower shouldBe ev900.sRatedAc
          minPower shouldBe ev900.sRatedAc // battery is empty
          maxPower shouldBe ev900.sRatedAc
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModelQv.getUuid
        flexResult.getTime shouldBe 900.toDateTime
        flexResult.getpRef should beEquivalentTo(ev900.unwrap().getSRatedAC)
        flexResult.getpMin should beEquivalentTo(ev900.unwrap().getSRatedAC)
        flexResult.getpMax should beEquivalentTo(ev900.unwrap().getSRatedAC)
      }

      emAgent.send(evcsAgent, IssueNoControl(900))

      // result of tick 0
      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModelQv.getUuid
          result.getTime shouldBe 0.toDateTime
          result.getP should beEquivalentTo(0d.asMegaWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 1800
        - ev 1800 arrives
        - charging with 11 kW
       */

      val ev1800 = EvModelWrapper(evA.copyWithDeparture(4500))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          1800,
          evService.ref,
          ArrivingEvsData(Seq(ev1800)),
        ),
      )

      emAgent.expectMsg(
        FlexCtrlCompletion(
          evcsInputModelQv.getUuid,
          ApparentPower(ev900.sRatedAc, zeroKVAr),
          requestAtNextActivation = true,
          Some(3600),
        )
      )
      emAgent.expectMsg(ScheduleFlexRequest(evcsInputModelQv.getUuid, 1800))

      /* TICK 2700
  - ev 2700 arrives
  - charging with 11 kW
       */

   */

    "provide correct results for three evs charging at same time without Em" in {
      val evService = TestProbe("evService")
      val resultListener = TestProbe("ResultListener")

      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = ParticipantInitializeStateData(
            inputModel = SimpleInputContainer(evcsInputModelQv),
            modelConfig = modelConfig,
            secondaryDataServices = Iterable(
              ActorEvMovementsService(evService.ref)
            ),
            simulationStartDate = simulationStartDate,
            simulationEndDate = simulationEndDate,
            resolution = resolution,
            requestVoltageDeviationThreshold = requestVoltageDeviationThreshold,
            outputConfig = NotifierConfig(
              simulationResultInfo = true,
              powerRequestReply = false,
              flexResult = true,
            ),
            primaryServiceProxy = primaryServiceProxy.ref,
            maybeEmAgent = None,
          ),
          listener = Iterable(resultListener.ref),
        )
      )

      scheduler.send(evcsAgent, Activation(INIT_SIM_TICK))

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(evcsInputModelQv.getUuid)
      )
      /* State should be information handling and having correct state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case ParticipantInitializingStateData(
        inputModel,
        modelConfig,
        secondaryDataServices,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        maybeEmAgent,
        ) =>
          inputModel shouldBe SimpleInputContainer(evcsInputModelQv)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe Iterable(
            ActorEvMovementsService(evService.ref)
          )
          simulationStartDate shouldBe simulationStartDate
          simulationEndDate shouldBe simulationEndDate
          resolution shouldBe resolution
          requestVoltageDeviationThreshold shouldBe requestVoltageDeviationThreshold
          outputConfig shouldBe NotifierConfig(
            simulationResultInfo = true,
            powerRequestReply = false,
            flexResult = true,
          )
          maybeEmAgent shouldBe None
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      evService.expectMsg(RegisterForEvDataMessage(evcsInputModelQv.getUuid))
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(evService.ref, None),
      )

      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* TICK 0 (expected activation)
         - currently no cars
     */
      scheduler.send(evcsAgent, Activation(0))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(0, evService.ref, ArrivingEvsData(Seq.empty)),
      )

      /* TICK 900
         - ev 900 arrives
         - charging with 11 kW
     */
      scheduler.send(evcsAgent, Activation(900))

      val ev900 = EvModelWrapper(evA.copyWithDeparture(3600))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          900,
          evService.ref,
          ArrivingEvsData(Seq(ev900)),
        ),
      )

      // result of tick 0
      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModelQv.getUuid
          result.getTime shouldBe 0.toDateTime
          result.getP should beEquivalentTo(0d.asMegaWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 1800
        - ev 1800 arrives
        - charging with 11 kW
     */
      scheduler.send(evcsAgent, Activation(1800))
      val ev1800 = EvModelWrapper(evA.copyWithDeparture(4500))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          1800,
          evService.ref,
          ArrivingEvsData(Seq(ev1800)),
        ),
      )

      // result of tick 9ß0



      Range(0, 3)
        .map { _ =>
          resultListener.expectMsgType[ParticipantResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(result: EvResult)
            if result.getInputModel == ev1800.uuid =>
            result.getTime shouldBe 900.toDateTime
            result.getP should beEquivalentTo(11d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(0d.asPercent, 1e-2)
          case ParticipantResultEvent(result: EvcsResult) =>
            result.getTime shouldBe 0.toDateTime
            result.getP should beEquivalentTo(0d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
        }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModelQv.getUuid
          result.getTime shouldBe 900.toDateTime
          result.getP should beEquivalentTo(ev900.unwrap().getSRatedAC)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }





      /* TICK 2700
  - ev 2700 arrives
  - charging with 11 kW
     */
      scheduler.send(evcsAgent, Activation(2700))
      val ev2700 = EvModelWrapper(evA.copyWithDeparture(5400))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          2700,
          evService.ref,
          ArrivingEvsData(Seq(ev2700)),
        ),
      )


    }
  }
}
