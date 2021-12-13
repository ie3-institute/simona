/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.ActorSystem
import akka.testkit.{TestFSMRef, TestProbe}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorEvMovementsService
import edu.ie3.simona.agent.participant.evcs.EvcsAgent
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.DataCollectionStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  CollectRegistrationConfirmMessages,
  ParticipantInitializeStateData,
  ParticipantInitializingStateData,
  ParticipantUninitializedStateData
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.akka.SimonaActorRef.RichActorRef
import edu.ie3.simona.api.data.ev.ontology.builder.EvcsMovementsBuilder
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.EvcsModel.EvcsRelevantData
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  RequestAssetPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  IllegalTriggerMessage,
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
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger
}
import edu.ie3.simona.service.ev.ExtEvDataService.FALLBACK_EV_MOVEMENTS_STEM_DISTANCE
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.EvTestData
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.{PowerSystemUnits, QuantityUtil}
import tech.units.indriya.quantity.Quantities

import scala.concurrent.duration.{DurationInt, FiniteDuration}

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
    with EvTestData {
  private implicit val receiveTimeout: FiniteDuration = 10.seconds
  private implicit val noReceiveTimeOut: Timeout = 1.second

  private val testingTolerance = 1e-6 // Equality on the basis of 1 W

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  private val voltageSensitiveInput = evcsInputModel
    .copy()
    .qCharacteristics(new QV("qV:{(0.95,-0.625),(1.05,0.625)}"))
    .build()

  private val evService = TestProbe("evService")

  private val noServices = None
  private val withServices = Some(
    Vector(
      ActorEvMovementsService(evService.ref.asLocal)
    )
  )

  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  "An evcs agent with model calculation depending on no secondary data service" should {
    "be instantiated correctly" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref.asLocal,
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

    "fail initialisation and stay in uninitialized state" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref.asLocal,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              EvcsInput,
              EvcsRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = evcsInputModel,
              modelConfig = modelConfig,
              secondaryDataServices = noServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref.asLocal
            )
          ),
          triggerId,
          evcsAgent.asLocal
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref.asLocal)
      )

      scheduler.receiveOne(receiveTimeout) match {
        case IllegalTriggerMessage(_, _) => logger.debug("Got correct message")
        case m =>
          fail(
            s"Did not fail initialization because of missing weather service. Received: $m"
          )
      }

      /* agent should stay uninitialized */
      evcsAgent.stateName shouldBe Uninitialized
      evcsAgent.stateData match {
        case _: ParticipantInitializingStateData[_, _, _] => succeed
        case _ => fail("Expected to get initializing state data")
      }
    }
  }

  "An evcs agent with model calculation depending on one secondary data service" should {
    "be instantiated correctly" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref.asLocal,
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
          scheduler = scheduler.ref.asLocal,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              EvcsInput,
              EvcsRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = evcsInputModel,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref.asLocal
            )
          ),
          triggerId,
          evcsAgent.asLocal
        )
      )

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(
          evcsInputModel.getUuid,
          evcsAgent.asLocal
        )
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
          inputModel shouldBe evcsInputModel
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
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref.asLocal)
      )

      /* Expect a registration message */
      evService.expectMsg(
        RegisterForEvDataMessage(evcsInputModel.getUuid, evcsAgent.asLocal)
      )

      /* ... as well as corresponding state and state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case CollectRegistrationConfirmMessages(
              ParticipantModelBaseStateData(
                startDate,
                endDate,
                _,
                subnetNo,
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
          subnetNo shouldBe evcsInputModel.getNode.getSubnet
          services shouldBe Some(
            Vector(
              ActorEvMovementsService(evService.ref.asLocal)
            )
          )
          outputConfig shouldBe ParticipantNotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false
          )
          additionalActivationTicks shouldBe Array.emptyLongArray
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution * 10,
            Map(0L -> Quantities.getQuantity(1d, PU))
          )
          resultValueStore shouldBe ValueStore.forResult(resolution, 10)
          requestValueStore shouldBe ValueStore[ApparentPower](resolution * 10)

          /* Additional information */
          awaitRegistrationResponsesFrom shouldBe Vector(evService.ref.asLocal)
          foreseenNextDataTicks shouldBe Map.empty
        case _ =>
          fail(
            s"Did not find expected state data $CollectRegistrationConfirmMessages, but ${evcsAgent.stateData}"
          )
      }

      /* Reply, that registration was successful */
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(None, evService.ref.asLocal)
      )

      /* Expect a completion message */
      scheduler.expectMsg(
        CompletionMessage(triggerId, evcsAgent.asLocal, None)
      )

      /* ... as well as corresponding state and state data */
      evcsAgent.stateName shouldBe Idle
      evcsAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _] =>
          /* Only check the awaited next data ticks, as the rest has yet been checked */
          baseStateData.foreseenDataTicks shouldBe Map(
            evService.ref.asLocal -> None
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${evcsAgent.stateData}"
          )
      }
    }

    "answer with zero power, if asked directly after initialisation" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref.asLocal,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              EvcsInput,
              EvcsRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = evcsInputModel,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref.asLocal
            )
          ),
          triggerId,
          evcsAgent.asLocal
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref.asLocal)
      )

      /* Expect a registration message */
      evService.expectMsg(
        RegisterForEvDataMessage(evcsInputModel.getUuid, evcsAgent.asLocal)
      )
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(Some(900L), evService.ref.asLocal)
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]

      evcsAgent.stateName shouldBe Idle
      /* State data has already been tested */

      evcsAgent ! RequestAssetPowerMessage(
        0L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU),
        self.asLocal
      )
      expectMsg(
        AssetPowerChangedMessage(
          Quantities.getQuantity(0d, MEGAWATT),
          Quantities.getQuantity(0d, MEGAVAR)
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
                Quantities.getQuantity(0d, MEGAWATT),
                Quantities.getQuantity(0d, MEGAVAR)
              )
            )
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${evcsAgent.stateData}"
          )
      }
    }

    "do correct transitions faced to new data in Idle" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref.asLocal,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              EvcsInput,
              EvcsRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = evcsInputModel,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref.asLocal
            )
          ),
          initialiseTriggerId,
          evcsAgent.asLocal
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref.asLocal)
      )

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(None, evService.ref.asLocal)
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(evcsAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out new data */
      val evMovementData = EvMovementData(
        new EvcsMovementsBuilder().addArrival(evA).addArrival(evB).build()
      )

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          evMovementData,
          serviceRef = evService.ref.asLocal
        )
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
          baseStateData.foreseenDataTicks shouldBe Map(
            evService.ref.asLocal -> None
          )

          /* The yet sent data is also registered */
          expectedSenders shouldBe Map(
            evService.ref.asLocal -> Some(evMovementData)
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
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1L,
          evcsAgent.asLocal
        )
      )

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(
        CompletionMessage(1L, evcsAgent.asLocal, None)
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
                  QuantityUtil.isEquivalentAbs(
                    p,
                    Quantities.getQuantity(0, MEGAWATT),
                    1e-16
                  ) shouldBe true
                  QuantityUtil.isEquivalentAbs(
                    q,
                    Quantities.getQuantity(0, MEGAVAR),
                    1e-16
                  ) shouldBe true
              }
          }
        case _ =>
          fail(
            s"Did not found the expected state data $ParticipantModelBaseStateData, but ${evcsAgent.stateData}"
          )
      }
    }

    "do correct transitions triggered for activation in idle" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref.asLocal,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              EvcsInput,
              EvcsRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = evcsInputModel,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref.asLocal
            )
          ),
          initialiseTriggerId,
          evcsAgent.asLocal
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref.asLocal)
      )

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(None, evService.ref.asLocal)
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(evcsAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out an activity start trigger */
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1L,
          evcsAgent.asLocal
        )
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
          baseStateData.foreseenDataTicks shouldBe Map(
            evService.ref.asLocal -> None
          )

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
      val evMovementData = EvMovementData(
        new EvcsMovementsBuilder().addArrival(evA).addArrival(evB).build()
      )

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          evMovementData,
          serviceRef = evService.ref.asLocal
        )
      )

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(
        CompletionMessage(1L, evcsAgent.asLocal, None)
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
                  QuantityUtil.isEquivalentAbs(
                    p,
                    Quantities.getQuantity(0, MEGAWATT),
                    1e-16
                  ) shouldBe true
                  QuantityUtil.isEquivalentAbs(
                    q,
                    Quantities.getQuantity(0, MEGAVAR),
                    1e-16
                  ) shouldBe true
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
          scheduler = scheduler.ref.asLocal,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              EvcsInput,
              EvcsRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = evcsInputModel,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref.asLocal
            )
          ),
          initialiseTriggerId,
          evcsAgent.asLocal
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref.asLocal)
      )

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(None, evService.ref.asLocal)
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(evcsAgent.stateName shouldBe Idle)

      evcsAgent ! RequestAssetPowerMessage(
        7200L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU),
        self.asLocal
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAVAR),
            testingTolerance
          )
      }
    }

    "provide number of free lots when asked to" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref.asLocal,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              EvcsInput,
              EvcsRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = evcsInputModel,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref.asLocal
            )
          ),
          initialiseTriggerId,
          evcsAgent.asLocal
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref.asLocal)
      )

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(None, evService.ref.asLocal)
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(evcsAgent.stateName shouldBe Idle)

      /* Send out public evcs request */
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          EvFreeLotsRequest,
          serviceRef = evService.ref.asLocal
        )
      )
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          3L,
          evcsAgent.asLocal
        )
      )

      evService.expectMsg(
        FreeLotsResponse(
          evcsInputModel.getUuid,
          2
        )
      )
      scheduler.expectMsg(CompletionMessage(3L, evcsAgent.asLocal))

      /* Send ev for this tick */
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          EvMovementData(
            new EvcsMovementsBuilder().addArrival(evA).build()
          ),
          serviceRef = evService.ref.asLocal
        )
      )

      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          4L,
          evcsAgent.asLocal
        )
      )
      scheduler.expectMsg(CompletionMessage(4L, evcsAgent.asLocal))

      /* Ask for public evcs count again */
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          3600L,
          EvFreeLotsRequest,
          serviceRef = evService.ref.asLocal
        )
      )
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600L),
          5L,
          evcsAgent.asLocal
        )
      )

      // this time, only one is still free
      evService.expectMsg(
        FreeLotsResponse(
          evcsInputModel.getUuid,
          1
        )
      )
      scheduler.expectMsg(CompletionMessage(5L, evcsAgent.asLocal))
    }

    val evcsAgent = TestFSMRef(
      new EvcsAgent(
        scheduler = scheduler.ref.asLocal,
        listener = systemListener
      )
    )

    "provide correct average power after three data ticks are available" in {
      val initialiseTriggerId = 0
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              EvcsInput,
              EvcsRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref.asLocal
            )
          ),
          initialiseTriggerId,
          evcsAgent.asLocal
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        evcsAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref.asLocal)
      )

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(
        evcsAgent,
        RegistrationSuccessfulMessage(None, evService.ref.asLocal)
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(evcsAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 0 */
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          EvMovementData(
            new EvcsMovementsBuilder().addArrival(evA).build()
          ),
          serviceRef = evService.ref.asLocal
        )
      )
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          3L,
          evcsAgent.asLocal
        )
      )
      scheduler.expectMsg(CompletionMessage(3L, evcsAgent.asLocal))

      /* ... for tick 3600 */
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          3600L,
          EvMovementData(
            new EvcsMovementsBuilder()
              .addDeparture(evA.getUuid)
              .addArrival(evB)
              .build()
          ),
          serviceRef = evService.ref.asLocal
        )
      )
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600L),
          4L,
          evcsAgent.asLocal
        )
      )
      evService.expectMsg(
        DepartedEvsResponse(
          evcsInputModel.getUuid,
          Set(
            evA.copyWith(
              Quantities.getQuantity(11d, PowerSystemUnits.KILOWATTHOUR)
            )
          )
        )
      )
      scheduler.expectMsg(CompletionMessage(4L, evcsAgent.asLocal))

      /* ... for tick 7200 */
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          7200L,
          EvMovementData(
            new EvcsMovementsBuilder()
              .addDeparture(evB.getUuid)
              .addArrival(evA)
              .build()
          ),
          serviceRef = evService.ref.asLocal
        )
      )
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(7200L),
          5L,
          evcsAgent.asLocal
        )
      )
      evService.expectMsg(
        DepartedEvsResponse(
          evcsInputModel.getUuid,
          Set(
            evB.copyWith(
              Quantities.getQuantity(11d, PowerSystemUnits.KILOWATTHOUR)
            )
          )
        )
      )
      scheduler.expectMsg(CompletionMessage(5L, evcsAgent.asLocal))

      /* Ask the agent for average power in tick 7500 */
      evcsAgent ! RequestAssetPowerMessage(
        7500L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU),
        self.asLocal
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0.00572d, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAVAR),
            testingTolerance
          )
        case answer => fail(s"Did not expect to get that answer: $answer")
      }
    }

    "answer unchanged power values after asking a second time with considerably same voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with (nearly) unchanged information */
      evcsAgent ! RequestAssetPowerMessage(
        7500L,
        Quantities.getQuantity(1.000000000000001d, PU),
        Quantities.getQuantity(0d, PU),
        self.asLocal
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0.00572d, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAVAR),
            testingTolerance
          )
      }
    }

    "answer changed power values after asking a second time with different voltage" in {
      /* Ask again with changed information */
      evcsAgent ! RequestAssetPowerMessage(
        7500L,
        Quantities.getQuantity(0.98, PU),
        Quantities.getQuantity(0d, PU),
        self.asLocal
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0.00572d, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(-3.35669e-3, MEGAVAR),
            testingTolerance
          )
      }
    }
  }
}
