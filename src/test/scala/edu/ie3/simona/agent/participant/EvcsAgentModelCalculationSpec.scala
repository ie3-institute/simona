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
import edu.ie3.datamodel.models.result.system.{EvResult, EvcsResult}
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
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent
}
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.evcs.ChargingSchedule
import edu.ie3.simona.model.participant.evcs.EvcsModel.EvcsState
import edu.ie3.simona.ontology.messages.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  RequestAssetPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  IllegalTriggerMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger
}
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.EvTestData
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import tech.units.indriya.quantity.Quantities

import java.time.temporal.ChronoUnit
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
      ActorEvMovementsService(evService.ref)
    )
  )

  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  // FIXME: Shall be temp only!
  /* Adapt start time of simulation to meet requirements of market price source */
  private val adaptedSimulationStart =
    simulationStartDate.plus(5, ChronoUnit.YEARS)
  private val adaptedSimulationEnd = simulationEndDate.plus(5, ChronoUnit.YEARS)

  "An evcs agent with model calculation depending on no secondary data service" should {
    "be instantiated correctly" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          listener = Iterable.empty
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
          scheduler = scheduler.ref,
          listener = Iterable.empty
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
              simulationStartDate = adaptedSimulationStart,
              simulationEndDate = adaptedSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          evcsAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

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
          scheduler = scheduler.ref,
          listener = Iterable.empty
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
          listener = Iterable.empty
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
              simulationStartDate = adaptedSimulationStart,
              simulationEndDate = adaptedSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          evcsAgent
        )
      )

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
              maybeEmAgent
            ) =>
          inputModel shouldBe evcsInputModel
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe withServices
          simulationStartDate shouldBe this.adaptedSimulationStart
          simulationEndDate shouldBe this.adaptedSimulationEnd
          timeBin shouldBe this.resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
          maybeEmAgent shouldBe None
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* Expect a registration message */
      evService.expectMsgPF() {
        case RegisterForEvDataMessage(
              uuid,
              departureScheduleFunc,
              arrivalScheduleFunc
            ) =>
          uuid shouldBe evcsInputModel.getUuid
          departureScheduleFunc(3L) shouldBe None
          arrivalScheduleFunc(3L) shouldBe
            ScheduleTriggerMessage(ActivityStartTrigger(3L), evcsAgent)
      }

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
                _
              ),
              awaitRegistrationResponsesFrom,
              foreseenNextDataTicks
            ) =>
          /* Base state data */
          startDate shouldBe adaptedSimulationStart
          endDate shouldBe adaptedSimulationEnd
          services shouldBe Some(
            Vector(
              ActorEvMovementsService(evService.ref)
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
      scheduler.expectMsg(
        CompletionMessage(triggerId, None)
      )

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
          listener = Iterable.empty
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
              simulationStartDate = adaptedSimulationStart,
              simulationEndDate = adaptedSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          evcsAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* Expect a registration message */
      evService.expectMsgPF() {
        case RegisterForEvDataMessage(
              uuid,
              departureScheduleFunc,
              arrivalScheduleFunc
            ) =>
          uuid shouldBe evcsInputModel.getUuid
          departureScheduleFunc(3L) shouldBe None
          arrivalScheduleFunc(3L) shouldBe
            ScheduleTriggerMessage(ActivityStartTrigger(3L), evcsAgent)
      }
      evService.send(evcsAgent, RegistrationSuccessfulMessage(Some(900L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]

      evcsAgent.stateName shouldBe Idle
      /* State data has already been tested */

      evcsAgent ! RequestAssetPowerMessage(
        0L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )
      expectMsg(
        AssetPowerChangedMessage(
          Quantities.getQuantity(0d, MEGAWATT),
          Quantities.getQuantity(0d, MEGAVAR)
        )
      )

      inside(evcsAgent.stateData) {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          baseStateData.requestValueStore shouldBe ValueStore[
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

    "do correct transitions faced with new data in Idle" in {
      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = scheduler.ref,
          listener = Iterable.empty
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
              simulationStartDate = adaptedSimulationStart,
              simulationEndDate = adaptedSimulationEnd,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          evcsAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(evcsAgent, RegistrationSuccessfulMessage(Some(0L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(evcsAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out new data */
      val arrivingEvsData = ArrivingEvsData(Seq(evA, evB))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(0L, arrivingEvsData, None)
      )

      /* Find yourself in corresponding state and state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[_, _, _, _],
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
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1L,
          evcsAgent
        )
      )

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(
        CompletionMessage(1L, None)
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
                  currentEvs should contain theSameElementsAs Set(evA, evB)

                  schedule.values.flatten should contain allOf (
                    ChargingSchedule(
                      evA,
                      Seq(
                        ChargingSchedule.Entry(
                          0,
                          200,
                          Quantities
                            .getQuantity(11, PowerSystemUnits.KILOWATT)
                        )
                      )
                    ),
                    ChargingSchedule(
                      evB,
                      Seq(
                        ChargingSchedule.Entry(
                          0,
                          200,
                          Quantities.getQuantity(
                            11,
                            PowerSystemUnits.KILOWATT
                          )
                        )
                      )
                    )
                  )

                  tick shouldBe 0L
                case None => fail("Entry for tick 0 expected.")
              }
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              // FIXME: Please double-check if an empty result store is actually correct here!
              store.keys shouldBe empty
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
          scheduler = scheduler.ref,
          listener = Iterable.empty
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
              simulationStartDate = adaptedSimulationStart,
              simulationEndDate = adaptedSimulationEnd,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          evcsAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

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
          evcsAgent
        )
      )

      /* Find yourself in corresponding state and state data */
      evcsAgent.stateName shouldBe HandleInformation
      evcsAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[_, _, _, _],
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
      val arrivingEvsData = ArrivingEvsData(Seq(evA, evB))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(0L, arrivingEvsData)
      )

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(
        CompletionMessage(1L, None)
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
                  currentEvs should contain theSameElementsAs Set(evA, evB)
                  schedule.values.flatten should contain allOf (
                    ChargingSchedule(
                      evA,
                      Seq(
                        ChargingSchedule.Entry(
                          0,
                          200,
                          Quantities
                            .getQuantity(11, PowerSystemUnits.KILOWATT)
                        )
                      )
                    ),
                    ChargingSchedule(
                      evB,
                      Seq(
                        ChargingSchedule.Entry(
                          0,
                          200,
                          Quantities
                            .getQuantity(11, PowerSystemUnits.KILOWATT)
                        )
                      )
                    )
                  )

                  tick shouldBe 0L
                case None => fail("Entry for tick 0 expected.")
              }
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              // FIXME: Please double-check if an empty result store is actually correct here!
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
          listener = Iterable.empty
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
              simulationStartDate = adaptedSimulationStart,
              simulationEndDate = adaptedSimulationEnd,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          evcsAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(evcsAgent.stateName shouldBe Idle)

      evcsAgent ! RequestAssetPowerMessage(
        7200L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
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
          scheduler = scheduler.ref,
          listener = Iterable.empty
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
              simulationStartDate = adaptedSimulationStart,
              simulationEndDate = adaptedSimulationEnd,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          evcsAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
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
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          ArrivingEvsData(Seq(evA))
        )
      )

      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          4L,
          evcsAgent
        )
      )
      scheduler.expectMsg(CompletionMessage(4L))

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
        listener = Iterable.empty
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
              simulationStartDate = adaptedSimulationStart,
              simulationEndDate = adaptedSimulationEnd,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          evcsAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      evService.expectMsgType[RegisterForEvDataMessage]
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(evcsAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 0 */
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          0L,
          ArrivingEvsData(Seq(evA.copyWithDeparture(3600L)))
        )
      )
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          3L,
          evcsAgent
        )
      )
      scheduler.expectMsg(CompletionMessage(3L))

      /* ... for tick 3600 */

      // departures first
      evService.send(
        evcsAgent,
        DepartingEvsRequest(3600L, Seq(evA.getUuid))
      )
      evService.expectMsgType[DepartingEvsResponse] match {
        case DepartingEvsResponse(evcs, evModels) =>
          evcs shouldBe evcsInputModel.getUuid
          evModels should have size 1
          evModels.headOption match {
            case Some(evModel) =>
              evModel.getUuid shouldBe evA.getUuid
              evModel.getStoredEnergy should equalWithTolerance(
                11d.asKiloWattHour,
                testingTolerance
              )
            case None => fail("Expected to get at least one ev.")
          }
      }

      // arrivals second
      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          3600L,
          ArrivingEvsData(Seq(evB.copyWithDeparture(7200L)))
        )
      )

      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600L),
          4L,
          evcsAgent
        )
      )
      scheduler.expectMsg(CompletionMessage(4L))

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
                11d.asKiloWattHour,
                testingTolerance
              )
            case None => fail("Expected to get at least one ev.")
          }
      }

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          7200L,
          ArrivingEvsData(Seq(evA.copyWithDeparture(10800L)))
        )
      )
      scheduler.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(7200L),
          5L,
          evcsAgent
        )
      )

      scheduler.expectMsg(CompletionMessage(5L))

      /* Ask the agent for average power in tick 7500 */
      evcsAgent ! RequestAssetPowerMessage(
        7500L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0.011d, MEGAWATT),
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
        Quantities.getQuantity(0d, PU)
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0.011d, MEGAWATT),
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
        Quantities.getQuantity(0d, PU)
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0.011d, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(-0.0067133728, MEGAVAR),
            testingTolerance
          )
      }
    }
  }

  "An evcs agent with model calculation controlled by an EmAgent" should {

    "be initialized correctly" in {
      val emAgent = TestProbe("EmAgentProbe")

      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = emAgent.ref,
          listener = Iterable.empty
        )
      )

      val triggerId = 0
      emAgent.send(
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
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref,
              maybeEmAgent = Some(emAgent.ref)
            )
          ),
          triggerId,
          evcsAgent
        )
      )

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(voltageSensitiveInput.getUuid)
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
              maybeEmAgent
            ) =>
          inputModel shouldBe voltageSensitiveInput
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe withServices
          simulationStartDate shouldBe simulationStartDate
          simulationEndDate shouldBe simulationEndDate
          resolution shouldBe resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
          maybeEmAgent shouldBe Some(emAgent.ref)
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      emAgent.expectMsg(
        ScheduleTriggerMessage(
          RequestFlexOptions(0L),
          evcsAgent
        )
      )

      evService.expectMsgPF() {
        case RegisterForEvDataMessage(
              uuid,
              departureScheduleFunc,
              arrivalScheduleFunc
            ) =>
          uuid shouldBe evcsInputModel.getUuid
          departureScheduleFunc(3L) shouldBe Some(
            ScheduleTriggerMessage(
              ScheduleTriggerMessage(RequestFlexOptions(3L), evcsAgent),
              emAgent.ref,
              priority = true
            )
          )
          arrivalScheduleFunc(3L) shouldBe
            ScheduleTriggerMessage(
              ScheduleTriggerMessage(ActivityStartTrigger(3L), evcsAgent),
              emAgent.ref,
              priority = true
            )
      }
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

      emAgent.expectMsg(
        CompletionMessage(
          triggerId,
          None
        )
      )

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
              _
            ) =>
          /* Base state data */
          startDate shouldBe simulationStartDate
          endDate shouldBe simulationEndDate
          services shouldBe withServices
          outputConfig shouldBe defaultOutputConfig
          additionalActivationTicks shouldBe Array.emptyLongArray
          foreseenDataTicks shouldBe Map(evService.ref -> None)
          voltageValueStore shouldBe ValueStore(
            resolution * 10,
            Map(0L -> Quantities.getQuantity(1d, PU))
          )
          resultValueStore shouldBe ValueStore(
            resolution * 10
          )
          requestValueStore shouldBe ValueStore[ApparentPower](
            resolution * 10
          )
        case unrecognized =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but $unrecognized"
          )
      }
    }

    "provide correct flex options when in Idle" in {
      val emAgent = TestProbe("EmAgentProbe")
      val resultListener = TestProbe("ResultListener")

      val evcsAgent = TestFSMRef(
        new EvcsAgent(
          scheduler = emAgent.ref,
          listener = Iterable(resultListener.ref)
        )
      )

      val triggerId = 0
      emAgent.send(
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
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = simResultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref,
              maybeEmAgent = Some(emAgent.ref)
            )
          ),
          triggerId,
          evcsAgent
        )
      )

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(voltageSensitiveInput.getUuid)
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
              maybeEmAgent
            ) =>
          inputModel shouldBe voltageSensitiveInput
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe withServices
          simulationStartDate shouldBe simulationStartDate
          simulationEndDate shouldBe simulationEndDate
          resolution shouldBe resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe simResultOutputConfig
          maybeEmAgent shouldBe Some(emAgent.ref)
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(evcsAgent, RegistrationFailedMessage)

      emAgent.expectMsg(
        ScheduleTriggerMessage(
          RequestFlexOptions(0L),
          evcsAgent
        )
      )

      evService.expectMsgPF() {
        case RegisterForEvDataMessage(
              uuid,
              departureScheduleFunc,
              arrivalScheduleFunc
            ) =>
          uuid shouldBe evcsInputModel.getUuid
          departureScheduleFunc(4L) shouldBe Some(
            ScheduleTriggerMessage(
              ScheduleTriggerMessage(RequestFlexOptions(4L), evcsAgent),
              emAgent.ref,
              priority = true
            )
          )
          arrivalScheduleFunc(4L) shouldBe
            ScheduleTriggerMessage(
              ScheduleTriggerMessage(ActivityStartTrigger(4L), evcsAgent),
              emAgent.ref,
              priority = true
            )
      }
      evService.send(evcsAgent, RegistrationSuccessfulMessage(None))

      emAgent.expectMsg(
        CompletionMessage(
          triggerId,
          None
        )
      )

      /* TICK 0 (expected activation)
         - currently no cars
       */

      emAgent.send(evcsAgent, RequestFlexOptions(0L))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe evcsInputModel.getUuid
          referencePower shouldBe 0d.asKiloWatt
          minPower shouldBe 0d.asKiloWatt
          maxPower shouldBe 0d.asKiloWatt
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModel.getUuid
        flexResult.getTime shouldBe 0L.toDateTime
        flexResult.getpRef should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMin should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(0d.asKiloWatt)
      }

      emAgent.send(
        evcsAgent,
        IssueNoCtrl(0L)
      )

      // next potential activation at fully charged battery:
      // net power = 12.961kW * 0.92 = 11.92412kW
      // time to charge fully ~= 16.7727262054h = 60382 ticks (rounded)
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = evcsInputModel.getUuid
        )
      )

      emAgent.expectMsgType[ParticipantResultEvent] match {
        case result =>
          result.systemParticipantResult.getP should equalWithTolerance(
            0.asKiloWatt,
            testingTolerance
          )
          result.systemParticipantResult.getQ should equalWithTolerance(
            0.asMegaVar,
            testingTolerance
          )
      }

      // results arrive after next activation
      resultListener.expectNoMessage()

      /* TICK 900
         - ev 900 arrives
         - charging with 11 kW
       */

      val ev900 = evA.copyWithDeparture(4500L)

      val activation1 = 1L

      emAgent.send(
        evcsAgent,
        TriggerWithIdMessage(ActivityStartTrigger(900L), activation1, evcsAgent)
      )

      emAgent.send(evcsAgent, RequestFlexOptions(900L))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          900L,
          ArrivingEvsData(Seq(ev900))
        )
      )

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe evcsInputModel.getUuid
          referencePower shouldBe ev900.getSRatedAC
          minPower shouldBe ev900.getSRatedAC // battery is empty
          maxPower shouldBe ev900.getSRatedAC
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModel.getUuid
        flexResult.getTime shouldBe 900L.toDateTime
        flexResult.getpRef should beEquivalentTo(ev900.getSRatedAC)
        flexResult.getpMin should beEquivalentTo(ev900.getSRatedAC)
        flexResult.getpMax should beEquivalentTo(ev900.getSRatedAC)
      }

      emAgent.send(evcsAgent, IssueNoCtrl(900L))

      // at 4500 ev is departing
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = evcsInputModel.getUuid,
          requestAtNextActivation = true,
          requestAtTick = Some(4500L)
        )
      )

      emAgent.expectMsgType[ParticipantResultEvent] match {
        case result =>
          result.systemParticipantResult.getP should equalWithTolerance(
            ev900.getSRatedAC,
            testingTolerance
          )
          result.systemParticipantResult.getQ should equalWithTolerance(
            0.asMegaVar,
            testingTolerance
          )
      }

      emAgent.expectMsg(CompletionMessage(activation1, None))

      // result of tick 0
      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModel.getUuid
          result.getTime shouldBe 0L.toDateTime
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
        DepartingEvsRequest(4500L, Seq(ev900.getUuid))
      )

      evService.expectMsgPF() { case DepartingEvsResponse(uuid, evs) =>
        evs.size shouldBe 1
        uuid shouldBe evcsInputModel.getUuid
        evs.headOption.foreach { ev =>
          ev.getUuid shouldBe ev900.getUuid
          ev.getStoredEnergy should equalWithTolerance(
            11.asKiloWattHour,
            testingTolerance
          )
        }
      }

      // results arrive right after departure request
      Range(0, 2)
        .map { _ =>
          resultListener.expectMsgType[ParticipantResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(result: EvResult)
              if result.getTime.equals(900L.toDateTime) =>
            result.getInputModel shouldBe ev900.getUuid
            result.getP should beEquivalentTo(ev900.getSRatedAC)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(0d.asPercent)
          case ParticipantResultEvent(result: EvResult)
              if result.getTime.equals(4500L.toDateTime) =>
            result.getInputModel shouldBe ev900.getUuid
            result.getP should beEquivalentTo(0d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(18.96551724137931d.asPercent)
        }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModel.getUuid
          result.getTime shouldBe 900L.toDateTime
          result.getP should beEquivalentTo(ev900.getSRatedAC)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      val ev4500 = evB.copyWithDeparture(72000L)

      val activation2 = 2L

      emAgent.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(4500L),
          activation2,
          evcsAgent
        )
      )

      emAgent.send(evcsAgent, RequestFlexOptions(4500L))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          4500L,
          ArrivingEvsData(Seq(ev4500))
        )
      )

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe evcsInputModel.getUuid
          referencePower shouldBe ev4500.getSRatedAC
          minPower shouldBe ev900.getSRatedAC // battery is empty
          maxPower shouldBe ev4500.getSRatedAC
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModel.getUuid
        flexResult.getTime shouldBe 4500L.toDateTime
        flexResult.getpRef should beEquivalentTo(ev4500.getSRatedAC)
        flexResult.getpMin should beEquivalentTo(ev4500.getSRatedAC)
        flexResult.getpMax should beEquivalentTo(ev4500.getSRatedAC)
      }

      emAgent.send(evcsAgent, IssueNoCtrl(4500L))

      // we currently have an empty battery in ev4500
      // time to charge to minimal soc ~= 1.45454545455h = 5236 ticks (rounded) from now
      // current tick is 4500, thus: 4500 + 5236 = 9736
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = evcsInputModel.getUuid,
          requestAtNextActivation = true,
          requestAtTick = Some(9736L)
        )
      )

      emAgent.expectMsgType[ParticipantResultEvent] match {
        case result =>
          result.systemParticipantResult.getP should equalWithTolerance(
            11.asKiloWatt,
            testingTolerance
          )
          result.systemParticipantResult.getQ should equalWithTolerance(
            0.asMegaVar,
            testingTolerance
          )
      }

      emAgent.expectMsg(CompletionMessage(activation2, None))

      // already sent out after EV departed
      resultListener.expectNoMessage()

      /* TICK 9736
         - flex control changes
         - charging with 10 kW
       */

      // sending flex request at very next activated tick
      emAgent.send(evcsAgent, RequestFlexOptions(9736L))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe evcsInputModel.getUuid
          referencePower shouldBe ev4500.getSRatedAC
          minPower shouldBe 0d.asKiloWatt // battery is exactly at margin
          maxPower shouldBe ev4500.getSRatedAC
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModel.getUuid
        flexResult.getTime shouldBe 9736L.toDateTime
        flexResult.getpRef should beEquivalentTo(ev4500.getSRatedAC)
        flexResult.getpMin should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(ev4500.getSRatedAC)
      }

      emAgent.send(evcsAgent, IssuePowerCtrl(9736L, 10.asKiloWatt))

      evService.expectNoMessage()

      // ev4500 is now at 16 kWh
      // time to charge fully = 6.4 h = 23040 ticks from now
      // current tick is 9736, thus: 9736 + 23040 = 32776
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = evcsInputModel.getUuid,
          requestAtTick = Some(32776L),
          requestAtNextActivation =
            true // since battery is still below lowest soc, it's still considered empty
        )
      )

      emAgent.expectMsgType[ParticipantResultEvent] match {
        case result =>
          result.systemParticipantResult.getP should equalWithTolerance(
            10.asKiloWatt,
            testingTolerance
          )
          result.systemParticipantResult.getQ should equalWithTolerance(
            0.asMegaVar,
            testingTolerance
          )
      }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvResult) =>
          result.getInputModel shouldBe ev4500.getUuid
          result.getTime shouldBe 4500L.toDateTime
          result.getP should beEquivalentTo(11d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(0d.asPercent)
      }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModel.getUuid
          result.getTime shouldBe 4500L.toDateTime
          result.getP should beEquivalentTo(11d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 11700
         - ev11700 arrives
         - charging with 16 kW
       */

      // with stored energy right at minimal SOC
      val ev11700 = evA.copyWithDeparture(36000L).copyWith(11.6d.asKiloWattHour)

      val activation3 = 3L

      emAgent.send(
        evcsAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(11700L),
          activation3,
          evcsAgent
        )
      )

      // sending flex request at very next activated tick
      emAgent.send(evcsAgent, RequestFlexOptions(11700L))

      evService.send(
        evcsAgent,
        ProvideEvDataMessage(
          11700L,
          ArrivingEvsData(Seq(ev11700))
        )
      )

      val combinedChargingPower =
        ev11700.getSRatedAC.add(ev4500.getSRatedAC)

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe evcsInputModel.getUuid
          referencePower shouldBe combinedChargingPower
          minPower shouldBe ev4500.getSRatedAC.multiply(
            -1
          ) // battery of earlier ev is above lowest soc now
          maxPower shouldBe combinedChargingPower
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModel.getUuid
        flexResult.getTime shouldBe 11700L.toDateTime
        flexResult.getpRef should beEquivalentTo(combinedChargingPower)
        flexResult.getpMin should beEquivalentTo(
          ev4500.getSRatedAC.multiply(-1)
        )
        flexResult.getpMax should beEquivalentTo(combinedChargingPower)
      }

      emAgent.send(evcsAgent, IssuePowerCtrl(11700L, 16.asKiloWatt))

      // no departing evs here
      evService.expectNoMessage()

      // ev4500 is now at ~ 21.45555556 kWh, ev11700 just arrived with 11.6 kWh
      // ev4500: time to charge fully ~= 7.3180556 h = 26345 ticks from now
      // ev11700: time to charge fully = 5.8 h = 20880 ticks from now
      // current tick is 11700, thus: 11700 + 20880 = 32580
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = evcsInputModel.getUuid,
          requestAtTick = Some(32580L),
          revokeRequestAtTick = Some(32776L),
          requestAtNextActivation =
            true // since battery is still below lowest soc, it's still considered empty
        )
      )

      emAgent.expectMsgType[ParticipantResultEvent] match {
        case result =>
          result.systemParticipantResult.getP should equalWithTolerance(
            16.asKiloWatt,
            testingTolerance
          )
          result.systemParticipantResult.getQ should equalWithTolerance(
            0.asMegaVar,
            testingTolerance
          )
      }

      emAgent.expectMsg(CompletionMessage(activation3, None))

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvResult) =>
          result.getInputModel shouldBe ev4500.getUuid
          result.getTime shouldBe 9736L.toDateTime
          result.getP should beEquivalentTo(10d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(20d.asPercent, 1e-2)
      }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModel.getUuid
          result.getTime shouldBe 9736L.toDateTime
          result.getP should beEquivalentTo(10d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 18000
         - flex control changes
         - discharging with 20 kW
       */

      // sending flex request at very next activated tick
      emAgent.send(evcsAgent, RequestFlexOptions(18000L))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe evcsInputModel.getUuid
          referencePower shouldBe combinedChargingPower
          minPower shouldBe combinedChargingPower.multiply(
            -1
          ) // battery of both evs is above lowest soc now
          maxPower shouldBe combinedChargingPower
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModel.getUuid
        flexResult.getTime shouldBe 18000L.toDateTime
        flexResult.getpRef should beEquivalentTo(combinedChargingPower)
        flexResult.getpMin should beEquivalentTo(
          combinedChargingPower.multiply(
            -1
          )
        )
        flexResult.getpMax should beEquivalentTo(combinedChargingPower)
      }

      emAgent.send(evcsAgent, IssuePowerCtrl(18000L, (-20).asKiloWatt))

      // no departing evs here
      evService.expectNoMessage()

      // ev4500 is now at ~ 35.455556 kWh, ev11700 at 25.6 kWh
      // ev4500: time to discharge to lowest soc ~= 1.9455556 h = 7004 ticks from now
      // ev11700: time to discharge to lowest soc ~= 1.4 h = 5040 ticks from now
      // current tick is 18000, thus: 18000 + 5040 = 23040
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = evcsInputModel.getUuid,
          requestAtTick = Some(23040L),
          revokeRequestAtTick = Some(32580L)
        )
      )

      emAgent.expectMsgType[ParticipantResultEvent] match {
        case result =>
          result.systemParticipantResult.getP should equalWithTolerance(
            (-20).asKiloWatt,
            testingTolerance
          )
          result.systemParticipantResult.getQ should equalWithTolerance(
            0.asMegaVar,
            testingTolerance
          )
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMsgType[ParticipantResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev4500.getUuid =>
            result.getTime shouldBe 11700L.toDateTime
            result.getP should beEquivalentTo(8d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(26.819d.asPercent, 1e-2)
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev11700.getUuid =>
            result.getTime shouldBe 11700L.toDateTime
            result.getP should beEquivalentTo(8d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(20.0d.asPercent)
        }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModel.getUuid
          result.getTime shouldBe 11700L.toDateTime
          result.getP should beEquivalentTo(16d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 23040
         - ev11700 at lowest soc
         - discharging with 10 kW
       */

      emAgent.send(evcsAgent, RequestFlexOptions(23040L))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe evcsInputModel.getUuid
          referencePower shouldBe combinedChargingPower
          minPower shouldBe ev4500.getSRatedAC.multiply(
            -1
          ) // battery of ev11700 is below lowest soc now
          maxPower shouldBe combinedChargingPower
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModel.getUuid
        flexResult.getTime shouldBe 23040L.toDateTime
        flexResult.getpRef should beEquivalentTo(combinedChargingPower)
        flexResult.getpMin should beEquivalentTo(
          ev4500.getSRatedAC.multiply(
            -1
          )
        )
        flexResult.getpMax should beEquivalentTo(combinedChargingPower)
      }

      emAgent.send(evcsAgent, IssuePowerCtrl(23040L, (-10).asKiloWatt))

      // no departing evs here
      evService.expectNoMessage()

      // ev4500 is now at 21.455556 kWh, ev11700 at 11.6 kWh (lowest soc)
      // ev4500: time to discharge to lowest soc =  0.5455556 h = 1964 ticks from now
      // current tick is 18864, thus: 23040 + 1964 = 25004
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = evcsInputModel.getUuid,
          requestAtTick = Some(25004L)
        )
      )

      emAgent.expectMsgType[ParticipantResultEvent] match {
        case result =>
          result.systemParticipantResult.getP should equalWithTolerance(
            (-10).asKiloWatt,
            testingTolerance
          )
          result.systemParticipantResult.getQ should equalWithTolerance(
            0.asMegaVar,
            testingTolerance
          )
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMsgType[ParticipantResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev4500.getUuid =>
            result.getTime shouldBe 18000L.toDateTime
            result.getP should beEquivalentTo((-10d).asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(44.3194d.asPercent, 1e-2)
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev11700.getUuid =>
            result.getTime shouldBe 18000L.toDateTime
            result.getP should beEquivalentTo((-10d).asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(44.137931034d.asPercent, 1e-6)
        }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModel.getUuid
          result.getTime shouldBe 18000L.toDateTime
          result.getP should beEquivalentTo((-20d).asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      /* TICK 25004L
         - both evs at lowest soc
         - no power
       */

      emAgent.send(evcsAgent, RequestFlexOptions(25004L))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe evcsInputModel.getUuid
          referencePower shouldBe combinedChargingPower
          minPower shouldBe 0.asKiloWatt // both at lowest soc
          maxPower shouldBe combinedChargingPower
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModel.getUuid
        flexResult.getTime shouldBe 25004L.toDateTime
        flexResult.getpRef should beEquivalentTo(combinedChargingPower)
        flexResult.getpMin should beEquivalentTo(0.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(combinedChargingPower)
      }

      emAgent.send(evcsAgent, IssuePowerCtrl(25004L, 0.asKiloWatt))

      // no departing evs here
      evService.expectNoMessage()

      // no new activation
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = evcsInputModel.getUuid
        )
      )

      emAgent.expectMsgType[ParticipantResultEvent] match {
        case result =>
          result.systemParticipantResult.getP should equalWithTolerance(
            0.asKiloWatt,
            testingTolerance
          )
          result.systemParticipantResult.getQ should equalWithTolerance(
            0.asMegaVar,
            testingTolerance
          )
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMsgType[ParticipantResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev4500.getUuid =>
            result.getTime shouldBe 23040L.toDateTime
            result.getP should beEquivalentTo((-10d).asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(26.819445d.asPercent, 1e-2)
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev11700.getUuid =>
            result.getTime shouldBe 23040L.toDateTime
            result.getP should beEquivalentTo(0d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(20d.asPercent)
        }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModel.getUuid
          result.getTime shouldBe 23040L.toDateTime
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
        DepartingEvsRequest(36000L, Seq(ev900.getUuid))
      )

      evService.expectMsgPF() { case DepartingEvsResponse(uuid, evs) =>
        evs.size shouldBe 1
        uuid shouldBe evcsInputModel.getUuid
        evs.headOption.foreach { ev =>
          ev.getUuid shouldBe ev11700.getUuid
          ev.getStoredEnergy should equalWithTolerance(
            11.6.asKiloWattHour,
            testingTolerance
          )
        }
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMsgType[ParticipantResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev4500.getUuid =>
            result.getTime shouldBe 25004L.toDateTime
            result.getP should beEquivalentTo(0d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(20d.asPercent, 1e-2)
          case ParticipantResultEvent(result: EvResult)
              if result.getInputModel == ev11700.getUuid =>
            result.getTime shouldBe 25004L.toDateTime
            result.getP should beEquivalentTo(0d.asKiloWatt)
            result.getQ should beEquivalentTo(0d.asMegaVar)
            result.getSoc should beEquivalentTo(20d.asPercent)
        }

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: EvcsResult) =>
          result.getInputModel shouldBe evcsInputModel.getUuid
          result.getTime shouldBe 25004L.toDateTime
          result.getP should beEquivalentTo(0d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      // sending flex request at very next activated tick
      emAgent.send(evcsAgent, RequestFlexOptions(36000L))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe evcsInputModel.getUuid
          referencePower shouldBe ev4500.getSRatedAC
          minPower shouldBe 0.asKiloWatt
          maxPower shouldBe ev4500.getSRatedAC
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe evcsInputModel.getUuid
        flexResult.getTime shouldBe 36000L.toDateTime
        flexResult.getpRef should beEquivalentTo(ev4500.getSRatedAC)
        flexResult.getpMin should beEquivalentTo(0.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(ev4500.getSRatedAC)
      }

      emAgent.send(evcsAgent, IssuePowerCtrl(36000L, 4.asKiloWatt))

      // ev11700 is now at 16 kWh
      // ev11700: time to charge fully = 16 h = 57600 ticks from now
      // current tick is 36000, thus: 36000 + 57600 = 93600
      // BUT: departing tick 72000 is earlier
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = evcsInputModel.getUuid,
          requestAtTick = Some(72000),
          requestAtNextActivation =
            true // since we're starting from lowest again
        )
      )

      emAgent.expectMsgType[ParticipantResultEvent] match {
        case result =>
          result.systemParticipantResult.getP should equalWithTolerance(
            4.asKiloWatt,
            testingTolerance
          )
          result.systemParticipantResult.getQ should equalWithTolerance(
            0.asMegaVar,
            testingTolerance
          )
      }

    }

  }

}
