/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.testkit.{TestFSMRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorEvMovementsService
import edu.ie3.simona.agent.participant.evcs.EvcsAgent
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.DataCollectionStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData._
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.EvcsModel.EvcsRelevantData
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  RequestAssetPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage
}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.ev.ExtEvDataService.FALLBACK_EV_MOVEMENTS_STEM_DISTANCE
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.common.{EvTestData, TestSpawnerClassic}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower, Vars}
import squants.energy.{Megawatts, Watts}
import squants.{Each, Power}

import java.util.UUID

class EvcsAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "EvcsAgentModelCalculationSpec",
        ConfigFactory
          .parseString("""
                     |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
                     |akka.loglevel="DEBUG"
        """.stripMargin)
      )
    )
    with EvcsInputTestData
    with EvTestData
    with TestSpawnerClassic {

  private implicit val powerTolerance: Power = Watts(0.1)
  private implicit val reactivePowerTolerance: ReactivePower = Vars(0.1)

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  private val voltageSensitiveInput = evcsInputModel
    .copy()
    .qCharacteristics(new QV("qV:{(0.95,-0.625),(1.05,0.625)}"))
    .build()

  private val evService = TestProbe("evService")

  private val noServices = None
  private val withServices = Some(
    Vector(
      ActorEvMovementsService(evService.ref)
    )
  )

  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  "An evcs agent with model calculation depending on no secondary data service" should {
    val initStateData = ParticipantInitializeStateData[
      EvcsInput,
      EvcsRuntimeConfig,
      ApparentPower
    ](
      inputModel = evcsInputModel,
      modelConfig = modelConfig,
      secondaryDataServices = noServices,
      simulationStartDate = simulationStartDate,
      simulationEndDate = simulationEndDate,
      resolution = resolution,
      requestVoltageDeviationThreshold =
        simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
      outputConfig = defaultOutputConfig,
      primaryServiceProxy = primaryServiceProxy.ref
    )

    "be instantiated correctly" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener
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
          listener = systemListener
        )
      )

      scheduler.send(evcsAgent, Activation(INIT_SIM_TICK))

      deathProbe.watch(evcsAgent.ref)

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      deathProbe.expectTerminated(evcsAgent.ref)
    }
  }

  "An evcs agent with model calculation depending on one secondary data service" should {
    val initStateData = ParticipantInitializeStateData[
      EvcsInput,
      EvcsRuntimeConfig,
      ApparentPower
    ](
      inputModel = evcsInputModel,
      modelConfig = modelConfig,
      secondaryDataServices = withServices,
      simulationStartDate = simulationStartDate,
      simulationEndDate = simulationEndDate,
      resolution = resolution,
      requestVoltageDeviationThreshold =
        simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
      outputConfig = defaultOutputConfig,
      primaryServiceProxy = primaryServiceProxy.ref
    )

    "be instantiated correctly" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener
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
          listener = systemListener
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
              outputConfig
            ) =>
          inputModel shouldBe SimpleInputContainer(evcsInputModel)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe withServices
          simulationStartDate shouldBe this.simulationStartDate
          simulationEndDate shouldBe this.simulationEndDate
          timeBin shouldBe this.resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* Expect a registration message */
      evService.expectMsg(
        RegisterForEvDataMessage(evcsInputModel.getUuid)
      )

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
                _
              ),
              awaitRegistrationResponsesFrom,
              foreseenNextDataTicks
            ) =>
          /* Base state data */
          startDate shouldBe simulationStartDate
          endDate shouldBe simulationEndDate
          services shouldBe Some(
            Vector(
              ActorEvMovementsService(evService.ref)
            )
          )
          outputConfig shouldBe NotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false
          )
          additionalActivationTicks shouldBe empty
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution * 10,
            Map(0L -> Each(1.0))
          )
          resultValueStore shouldBe ValueStore.forResult(resolution, 10)
          requestValueStore shouldBe ValueStore[ApparentPower](resolution * 10)

          /* Additional information */
          awaitRegistrationResponsesFrom shouldBe Vector(evService.ref)
          foreseenNextDataTicks shouldBe Map.empty
        case _ =>
          fail(
            s"Did not find expected state data $CollectRegistrationConfirmMessages, but ${evcsAgent.stateData}"
          )
      }

      /* Reply, that registration was successful */
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

      /* Expect a completion message */
      scheduler.expectMsg(Completion(evcsAgent.toTyped, None))

      /* ... as well as corresponding state and state data */
      evcsAgent.stateName shouldBe Idle
      evcsAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _] =>
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
          listener = systemListener
        )
      )

      scheduler.send(evcsAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* Expect a registration message */
      evService.expectMsg(
        RegisterForEvDataMessage(evcsInputModel.getUuid)
      )
      evService.send(evcsAgent, RegistrationSuccessfulMessage(Some(900L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]

      evcsAgent.stateName shouldBe Idle
      /* State data has already been tested */

      evcsAgent ! RequestAssetPowerMessage(
        0L,
        Each(1.0),
        Each(0.0)
      )
      expectMsg(
        AssetPowerChangedMessage(
          Megawatts(0.0),
          Megavars(0.0)
        )
      )

      inside(evcsAgent.stateData) {
        case modelBaseStateData: ParticipantModelBaseStateData[_, _, _] =>
          modelBaseStateData.requestValueStore shouldBe ValueStore[
            ApparentPower
          ](
            resolution * 10,
            Map(
              0L -> ApparentPower(
                Megawatts(0.0),
                Megavars(0.0)
              )
            )
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
          listener = systemListener
        )
      )

      scheduler.send(
        evcsAgent,
        Activation(INIT_SIM_TICK)
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(evcsAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out new data */
      val arrivingEvsData =
        ArrivingEvsData(scala.collection.immutable.Seq(evA, evB))

      val key1 = Some(ScheduleKey(lock.ref.toTyped, UUID.randomUUID()))
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(0L, arrivingEvsData, unlockKey = key1)
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 0, key1))

      /* Find yourself in corresponding state and state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[_, _, _],
              expectedSenders,
              isYetTriggered
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
        Activation(0)
      )

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(
        Completion(evcsAgent.toTyped, None)
      )
      evcsAgent.stateName shouldBe Idle
      evcsAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.calcRelevantDateStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                0L -> EvcsRelevantData(
                  FALLBACK_EV_MOVEMENTS_STEM_DISTANCE,
                  Set(evA, evB)
                )
              )
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                0L,
                fail("Expected a simulation result for tick 900.")
              ) match {
                case ApparentPower(p, q) =>
                  (p ~= Megawatts(0d)) shouldBe true
                  (q ~= Megavars(0d)) shouldBe true
              }
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
          listener = systemListener
        )
      )

      scheduler.send(
        evcsAgent,
        Activation(INIT_SIM_TICK)
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(evcsAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out activation */
      scheduler.send(
        evcsAgent,
        Activation(0)
      )

      /* Find yourself in corresponding state and state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[_, _, _],
              expectedSenders,
              isYetTriggered
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
        ArrivingEvsData(Seq(evA, evB))

      val key1 = Some(ScheduleKey(lock.ref.toTyped, UUID.randomUUID()))
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(0L, arrivingEvsData, unlockKey = key1)
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 0, key1))

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(
        Completion(evcsAgent.toTyped, None)
      )
      evcsAgent.stateName shouldBe Idle
      evcsAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.calcRelevantDateStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                0L -> EvcsRelevantData(
                  FALLBACK_EV_MOVEMENTS_STEM_DISTANCE,
                  Set(evA, evB)
                )
              )
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                0L,
                fail("Expected a simulation result for tick 900.")
              ) match {
                case ApparentPower(p, q) =>
                  (p ~= Megawatts(0d)) shouldBe true
                  (q ~= Megavars(0d)) shouldBe true
              }
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
          listener = systemListener
        )
      )

      scheduler.send(
        evcsAgent,
        Activation(INIT_SIM_TICK)
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(evcsAgent.stateName shouldBe Idle)

      evcsAgent ! RequestAssetPowerMessage(
        7200L,
        Each(1.0),
        Each(0.0)
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          (p ~= Megawatts(0.0)) shouldBe true
          (q ~= Megavars(0.0)) shouldBe true
      }
    }

    "provide number of free lots when asked to" in {
      val lock = TestProbe("lock")

      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener
        )
      )

      scheduler.send(
        evcsAgent,
        Activation(INIT_SIM_TICK)
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(evcsAgent.stateName shouldBe Idle)

      /* Send out public evcs request */
      evService.send(
        evcsAgent,
        EvFreeLotsRequest(0L)
      )

      evService.expectMsg(
        FreeLotsResponse(
          evcsInputModel.getUuid,
          2
        )
      )

      scheduler.expectNoMessage()

      /* Send ev for this tick */
      val key1 = Some(ScheduleKey(lock.ref.toTyped, UUID.randomUUID()))
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          ArrivingEvsData(Seq(evA)),
          unlockKey = key1
        )
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 0, key1))

      scheduler.send(
        evcsAgent,
        Activation(0)
      )
      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* Ask for public evcs lot count again with a later tick */
      evService.send(
        evcsAgent,
        EvFreeLotsRequest(3600L)
      )

      // this time, only one is still free
      evService.expectMsg(
        FreeLotsResponse(
          evcsInputModel.getUuid,
          1
        )
      )

      scheduler.expectNoMessage()
    }

    val evcsAgent = TestFSMRef(
      new EvcsAgent(
        scheduler = scheduler.ref,
        initStateData = ParticipantInitializeStateData(
          inputModel = voltageSensitiveInput,
          modelConfig = modelConfig,
          secondaryDataServices = withServices,
          simulationStartDate = simulationStartDate,
          simulationEndDate = simulationEndDate,
          resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
          requestVoltageDeviationThreshold =
            simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
          outputConfig = defaultOutputConfig,
          primaryServiceProxy = primaryServiceProxy.ref
        ),
        listener = systemListener
      )
    )

    "provide correct average power after three data ticks are available" in {
      val lock = TestProbe("lock")

      scheduler.send(evcsAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

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
          ArrivingEvsData(Seq(evA)),
          unlockKey = key1
        )
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 0, key1))
      scheduler.send(evcsAgent, Activation(0))
      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* ... for tick 3600 */

      // departures first
      evService.send(
        evcsAgent,
        DepartingEvsRequest(3600L, Seq(evA.getUuid))
      )
      evService.expectMsgType[DepartingEvsResponse] match {
        case DepartingEvsResponse(evcs, evs) =>
          evcs shouldBe evcsInputModel.getUuid

          evs should have size 1

          val ev = evs.headOption.getOrElse(fail("No EV departed"))
          ev.getUuid shouldBe evA.getUuid
          ev.getStoredEnergy should equalWithTolerance(11d.asKiloWattHour)
      }

      // arrivals second
      val key2 = Some(ScheduleKey(lock.ref.toTyped, UUID.randomUUID()))
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          3600L,
          ArrivingEvsData(Seq(evB)),
          unlockKey = key2
        )
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 3600, key2))

      scheduler.send(evcsAgent, Activation(3600))
      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* ... for tick 7200 */

      // departures first
      evService.send(
        evcsAgent,
        DepartingEvsRequest(7200L, Seq(evB.getUuid))
      )
      evService.expectMsgType[DepartingEvsResponse] match {
        case DepartingEvsResponse(evcs, evModels) =>
          evcs shouldBe evcsInputModel.getUuid
          evModels should have size 1
          evModels.headOption match {
            case Some(evModel) =>
              evModel.getUuid shouldBe evB.getUuid
              evModel.getStoredEnergy should equalWithTolerance(
                11d.asKiloWattHour
              )
            case None => fail("Expected to get at least one ev.")
          }
      }

      val key3 = Some(ScheduleKey(lock.ref.toTyped, UUID.randomUUID()))
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          7200L,
          ArrivingEvsData(Seq(evA)),
          unlockKey = key3
        )
      )
      scheduler.expectMsg(ScheduleActivation(evcsAgent.toTyped, 7200, key3))

      scheduler.send(evcsAgent, Activation(7200))

      scheduler.expectMsg(Completion(evcsAgent.toTyped))

      /* Ask the agent for average power in tick 7500 */
      evcsAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1.0),
        Each(0.0)
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          (p ~= Megawatts(0.00572)) shouldBe true
          (q ~= Megavars(0.0)) shouldBe true
        case answer => fail(s"Did not expect to get that answer: $answer")
      }
    }

    "answer unchanged power values after asking a second time with considerably same voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with (nearly) unchanged information */
      evcsAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1.000000000000001d),
        Each(0.0)
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          (p ~= Megawatts(0.00572)) shouldBe true
          (q ~= Megavars(0.0)) shouldBe true
      }
    }

    "answer changed power values after asking a second time with different voltage" in {
      /* Ask again with changed information */
      evcsAgent ! RequestAssetPowerMessage(
        7500L,
        Each(0.98),
        Each(0.0)
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          (p ~= Megawatts(0.00572)) shouldBe true
          (q ~= Megavars(-0.00335669)) shouldBe true
      }
    }
  }
}
