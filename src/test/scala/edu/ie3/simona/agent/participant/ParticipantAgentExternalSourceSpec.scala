/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestFSMRef
import akka.util.Timeout
import breeze.numerics.{acos, tan}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ActivePower,
  ActivePowerAndHeat,
  ApparentPower,
  ApparentPowerAndHeat
}
import edu.ie3.simona.agent.participant.statedata.BaseStateData.FromOutsideBaseStateData
import edu.ie3.simona.agent.participant.statedata.DataCollectionStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  ParticipantInitializeStateData,
  ParticipantInitializingStateData,
  ParticipantUninitializedStateData,
  SimpleInputContainer
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.BaseRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.{CalcRelevantData, SystemParticipant}
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  RequestAssetPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger
}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.ProvidePrimaryDataMessage
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits._
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import java.util.concurrent.TimeUnit
import javax.measure.quantity.{Dimensionless, Power}
import scala.util.{Failure, Success}

/** Tests a mock participant agent with external data (primary data). Since
  * primary data is exclusively handled on the level of ParticipantAgent, it
  * only needs to be tested once.
  */
class ParticipantAgentExternalSourceSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "ParticipantAgentExternalSourceSpec",
        ConfigFactory
          .parseString("""
                       |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
                       |akka.loglevel="DEBUG"
        """.stripMargin)
      )
    )
    with DefaultTestData
    with MockitoSugar {
  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  private val testUUID = UUID.randomUUID
  private val testID = "PartAgentExternalMock"
  private val mockNode = mock[NodeInput]
  when(mockNode.getvTarget())
    .thenReturn(Quantities.getQuantity(1d, PowerSystemUnits.PU))
  private val mockInputModel = mock[SystemParticipantInput]
  when(mockInputModel.getUuid).thenReturn(testUUID)
  when(mockInputModel.getId).thenReturn(testID)
  when(mockInputModel.getNode).thenReturn(mockNode)
  private val mockModel =
    mock[SystemParticipant[CalcRelevantData.FixedRelevantData.type]]
  when(mockModel.getUuid).thenReturn(testUUID)
  private val activeToReactivePowerFunction
      : ComparableQuantity[Power] => ComparableQuantity[Power] =
    (p: ComparableQuantity[Power]) => p.multiply(tan(acos(0.9)))
  when(
    mockModel.activeToReactivePowerFunc(
      any(classOf[ComparableQuantity[Dimensionless]])
    )
  ).thenReturn(activeToReactivePowerFunction)

  private val testingTolerance = 1e-6 // Equality on the basis of 1 W

  private val simonaConfig: SimonaConfig = createSimonaConfig(
    LoadModelBehaviour.FIX,
    LoadReference.ActivePower(Quantities.getQuantity(0d, KILOWATT))
  )
  private val defaultOutputConfig = ParticipantNotifierConfig(
    simulationResultInfo = false,
    powerRequestReply = false
  )

  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  "A participant agent with externally given data provider" should {
    "be instantiated correctly" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref
        )
      )

      mockAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(mockAgent.stateData) {
        case _: ParticipantUninitializedStateData[_] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${mockAgent.stateData}."
          )
      }
    }

    "end in correct state with correct state data after initialisation" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref
        )
      )

      val triggerId = 0L
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              SystemParticipantInput,
              BaseRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = mockInputModel,
              modelConfig = mock[BaseRuntimeConfig],
              secondaryDataServices = None,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          mockAgent
        )
      )

      /* Expect a registration message */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(testUUID)
      )

      /* ... as well as corresponding state and state data */
      mockAgent.stateName shouldBe HandleInformation
      mockAgent.stateData match {
        case ParticipantInitializingStateData(
              inputModel,
              modelConfig,
              secondaryDataServices,
              simulationStartDate,
              simulationEndDate,
              resolution,
              requestVoltageDeviationThreshold,
              outputConfig
            ) =>
          inputModel shouldBe SimpleInputContainer(mockInputModel)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe None
          simulationStartDate shouldBe defaultSimulationStart
          simulationEndDate shouldBe defaultSimulationEnd
          resolution shouldBe this.resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Reply, that registration was successful */
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(Some(4711L))
      )

      scheduler.expectMsgClass(classOf[CompletionMessage]) match {
        case CompletionMessage(actualTriggerId, newTriggers) =>
          actualTriggerId shouldBe triggerId
          newTriggers match {
            case Some(triggers) =>
              triggers.size shouldBe 1
              triggers.exists {
                case ScheduleTriggerMessage(
                      ActivityStartTrigger(tick),
                      actorToBeScheduled
                    ) =>
                  tick == 4711L && actorToBeScheduled == mockAgent
              } shouldBe true
            case None => fail("Expected to get a trigger for tick 4711.")
          }
      }

      /* ... as well as corresponding state and state data */
      mockAgent.stateName shouldBe Idle
      mockAgent.stateData match {
        case baseStateData: FromOutsideBaseStateData[SystemParticipant[
              FixedRelevantData.type
            ], ApparentPower] =>
          /* Only check the awaited next data ticks, as the rest has yet been checked */
          baseStateData.foreseenDataTicks shouldBe Map(
            primaryServiceProxy.ref -> Some(4711L)
          )
        case _ =>
          fail(
            s"Did not find expected state data $FromOutsideBaseStateData, but ${mockAgent.stateData}"
          )
      }
    }

    "answer with zero power, if asked directly after initialisation" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref
        )
      )

      val triggerId = 0
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              SystemParticipantInput,
              BaseRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = mockInputModel,
              modelConfig = mock[BaseRuntimeConfig],
              secondaryDataServices = None,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          mockAgent
        )
      )

      /* I'm not interested in the content of the RegistrationMessage */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(Some(900L))
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(mockAgent.stateName shouldBe Idle)

      /* State data has already been tested */

      mockAgent ! RequestAssetPowerMessage(
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

      inside(mockAgent.stateData) {
        case FromOutsideBaseStateData(
              _,
              _,
              _,
              _,
              _,
              _,
              _,
              _,
              _,
              _,
              requestValueStore
            ) =>
          requestValueStore shouldBe ValueStore[ApparentPower](
            resolution,
            Map(
              0L -> ApparentPower(
                Quantities.getQuantity(0d, MEGAWATT),
                Quantities.getQuantity(0d, MEGAVAR)
              )
            )
          )
        case _ =>
          fail(
            s"Did not found the expected state data $FromOutsideBaseStateData, but ${mockAgent.stateData}"
          )
      }
    }

    "do correct transitions faced to new data in Idle" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              SystemParticipantInput,
              BaseRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = mockInputModel,
              modelConfig = mock[BaseRuntimeConfig],
              secondaryDataServices = None,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          mockAgent
        )
      )

      /* I'm not interested in the content of the RegistrationMessage */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(Some(900L))
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(mockAgent.stateName shouldBe Idle)

      /* Send out new data */
      primaryServiceProxy.send(
        mockAgent,
        ProvidePrimaryDataMessage(
          900L,
          ApparentPower(
            Quantities.getQuantity(0, KILOWATT),
            Quantities.getQuantity(900, KILOVAR)
          ),
          Some(1800L)
        )
      )

      /* Find yourself in corresponding state and state data */
      mockAgent.stateName shouldBe HandleInformation
      mockAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: FromOutsideBaseStateData[SystemParticipant[
                CalcRelevantData
              ], ApparentPower],
              expectedSenders,
              isYetTriggered
            ) =>
          /* The next data tick is already registered */
          baseStateData.foreseenDataTicks shouldBe Map(
            primaryServiceProxy.ref -> Some(1800L)
          )

          /* The yet sent data is also registered */
          expectedSenders shouldBe Map(
            primaryServiceProxy.ref -> Some(
              ApparentPower(
                Quantities.getQuantity(0, KILOWATT),
                Quantities.getQuantity(900, KILOVAR)
              )
            )
          )

          /* It is not yet triggered */
          isYetTriggered shouldBe false
        case _ =>
          fail(
            s"Did not find expected state data $DataCollectionStateData, but ${mockAgent.stateData}"
          )
      }

      /* Trigger the agent */
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(900L),
          1L,
          scheduler.ref
        )
      )

      /* Expect confirmation */
      scheduler.expectMsg(
        CompletionMessage(
          1L,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(1800L),
                mockAgent
              )
            )
          )
        )
      )

      /* Expect the state change to idle with updated base state data */
      mockAgent.stateName shouldBe Idle
      mockAgent.stateData match {
        case baseStateData: FromOutsideBaseStateData[SystemParticipant[
              CalcRelevantData
            ], ApparentPower] =>
          /* The new data is apparent in the result value store */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                900L -> ApparentPower(
                  Quantities.getQuantity(0, KILOWATT),
                  Quantities.getQuantity(900, KILOVAR)
                )
              )
          }
        case _ =>
          fail(
            s"Did not found the expected state data $FromOutsideBaseStateData, but ${mockAgent.stateData}"
          )
      }
    }

    "do correct transitions triggered for activation in idle" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              SystemParticipantInput,
              BaseRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = mockInputModel,
              modelConfig = mock[BaseRuntimeConfig],
              secondaryDataServices = None,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          mockAgent
        )
      )

      /* I'm not interested in the content of the RegistrationMessage */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(Some(900L))
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(mockAgent.stateName shouldBe Idle)

      /* Send out an activity start trigger */
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(900L),
          1L,
          scheduler.ref
        )
      )

      /* Find yourself in appropriate state with state data */
      mockAgent.stateName shouldBe HandleInformation
      mockAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: FromOutsideBaseStateData[SystemParticipant[
                CalcRelevantData
              ], ApparentPower],
              expectedSenders,
              isYetTriggered
            ) =>
          /* The next data tick is already registered */
          baseStateData.foreseenDataTicks shouldBe Map(
            primaryServiceProxy.ref -> Some(900L)
          )

          /* The yet sent data is also registered */
          expectedSenders shouldBe Map(primaryServiceProxy.ref -> None)

          /* It is yet triggered */
          isYetTriggered shouldBe true
        case _ =>
          fail(
            s"Did not find expected state data $DataCollectionStateData, but ${mockAgent.stateData}"
          )
      }

      /* Providing the awaited data will lead to the foreseen transitions */
      primaryServiceProxy.send(
        mockAgent,
        ProvidePrimaryDataMessage(
          900L,
          ApparentPower(
            Quantities.getQuantity(0, KILOWATT),
            Quantities.getQuantity(900, KILOVAR)
          ),
          Some(1800L)
        )
      )

      /* Expect confirmation */
      scheduler.expectMsg(
        CompletionMessage(
          1L,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(1800L),
                mockAgent
              )
            )
          )
        )
      )

      /* Expect the state change to idle with updated base state data */
      mockAgent.stateName shouldBe Idle
      mockAgent.stateData match {
        case baseStateData: FromOutsideBaseStateData[SystemParticipant[
              CalcRelevantData
            ], ApparentPower] =>
          /* The new data is apparent in the result value store */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                900L -> ApparentPower(
                  Quantities.getQuantity(0, KILOWATT),
                  Quantities.getQuantity(900, KILOVAR)
                )
              )
          }
        case _ =>
          fail(
            s"Did not found the expected state data $FromOutsideBaseStateData, but ${mockAgent.stateData}"
          )
      }
    }

    "does not provide power if data is awaited in an earlier tick, but answers it, if all expected data is there" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref
        )
      )

      /* Trigger the initialisation */
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              SystemParticipantInput,
              BaseRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = mockInputModel,
              modelConfig = mock[BaseRuntimeConfig],
              secondaryDataServices = None,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0L,
          mockAgent
        )
      )

      /* I'm not interested in the content of the RegistrationMessage */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(Some(900L))
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(mockAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 1800 */
      mockAgent ! RequestAssetPowerMessage(
        1800L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )
      expectNoMessage(noReceiveTimeOut.duration)
      awaitAssert(mockAgent.stateName == Idle)

      /* Send out the expected data and wait for the reply */
      primaryServiceProxy.send(
        mockAgent,
        ProvidePrimaryDataMessage(
          900L,
          ApparentPower(
            Quantities.getQuantity(0, KILOWATT),
            Quantities.getQuantity(900, KILOVAR)
          ),
          Some(1800L)
        )
      )

      /* Trigger the agent */
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(900L),
          1L,
          scheduler.ref
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          1L,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(1800L),
                mockAgent
              )
            )
          )
        )
      )

      /* Appreciate the answer to my previous request */
      expectMsgType[AssetPowerChangedMessage]
    }

    val mockAgent = TestFSMRef(
      new ParticipantAgentMock(
        scheduler = scheduler.ref
      )
    )

    "correctly determine the reactive power function when trivial reactive power is requested" in {
      val baseStateData: FromOutsideBaseStateData[SystemParticipant[
        CalcRelevantData.FixedRelevantData.type
      ], ApparentPower] = FromOutsideBaseStateData(
        mockModel,
        defaultSimulationStart,
        defaultSimulationEnd,
        defaultOutputConfig,
        Array.emptyLongArray,
        Map.empty[ActorRef, Option[Long]],
        fillUpReactivePowerWithModelFunc = false,
        1e-4,
        ValueStore.forVoltage(
          900L,
          Quantities.getQuantity(1d, StandardUnits.VOLTAGE_MAGNITUDE)
        ),
        ValueStore.forResult(900L, 1L),
        ValueStore(900L)
      )

      val actualFunction =
        mockAgent.underlyingActor.getReactivePowerFunction(0L, baseStateData)
      actualFunction(
        Quantities.getQuantity(100d, StandardUnits.ACTIVE_POWER_IN)
      ) should equalWithTolerance(
        Quantities.getQuantity(0L, StandardUnits.REACTIVE_POWER_IN),
        1e-3
      )
    }

    "correctly determine the reactive power function from model when requested" in {
      val baseStateData: FromOutsideBaseStateData[SystemParticipant[
        CalcRelevantData.FixedRelevantData.type
      ], ApparentPower] = FromOutsideBaseStateData(
        mockModel,
        defaultSimulationStart,
        defaultSimulationEnd,
        defaultOutputConfig,
        Array.emptyLongArray,
        Map.empty[ActorRef, Option[Long]],
        fillUpReactivePowerWithModelFunc = true,
        1e-4,
        ValueStore.forVoltage(
          900L,
          Quantities.getQuantity(1d, StandardUnits.VOLTAGE_MAGNITUDE)
        ),
        ValueStore.forResult(900L, 1L),
        ValueStore(900L)
      )

      val actualFunction =
        mockAgent.underlyingActor.getReactivePowerFunction(0L, baseStateData)
      actualFunction(
        Quantities.getQuantity(100d, StandardUnits.ACTIVE_POWER_IN)
      ) should equalWithTolerance(
        Quantities.getQuantity(48.43221, StandardUnits.REACTIVE_POWER_IN),
        1e-3
      )
    }

    "provide correct average power after three data ticks are available" in {
      /* Trigger the initialisation */
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              SystemParticipantInput,
              BaseRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = mockInputModel,
              modelConfig = mock[BaseRuntimeConfig],
              secondaryDataServices = None,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0,
          mockAgent
        )
      )

      /* I'm not interested in the content of the RegistrationMessage */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(Some(900L))
      )

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(mockAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 900 */
      primaryServiceProxy.send(
        mockAgent,
        ProvidePrimaryDataMessage(
          900L,
          ApparentPower(
            Quantities.getQuantity(100d, KILOWATT),
            Quantities.getQuantity(33d, KILOVAR)
          ),
          Some(1800L)
        )
      )
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(900L),
          1L,
          scheduler.ref
        )
      )
      scheduler.expectMsg(
        CompletionMessage(
          1L,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(1800L),
                mockAgent
              )
            )
          )
        )
      )

      /* ... for tick 1800 */
      primaryServiceProxy.send(
        mockAgent,
        ProvidePrimaryDataMessage(
          1800L,
          ApparentPower(
            Quantities.getQuantity(150d, KILOWATT),
            Quantities.getQuantity(49d, KILOVAR)
          ),
          Some(2700L)
        )
      )
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(1800L),
          2L,
          scheduler.ref
        )
      )
      scheduler.expectMsg(
        CompletionMessage(
          2L,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(2700L),
                mockAgent
              )
            )
          )
        )
      )

      /* ... for tick 2700 */
      primaryServiceProxy.send(
        mockAgent,
        ProvidePrimaryDataMessage(
          2700L,
          ApparentPower(
            Quantities.getQuantity(200d, KILOWATT),
            Quantities.getQuantity(66d, KILOVAR)
          ),
          None
        )
      )
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(2700L),
          3L,
          scheduler.ref
        )
      )
      scheduler.expectMsg(CompletionMessage(3L, None))

      awaitAssert(mockAgent.stateName == Idle)

      /* Ask the agent for average power in tick 3000 */
      mockAgent ! RequestAssetPowerMessage(
        3000L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0.095, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0.0312, MEGAVAR),
            testingTolerance
          )
      }
    }

    "replies unchanged power values after asking a second time" in {
      /* Previous request stems from previous test */
      /* Ask again with unchanged information */
      mockAgent ! RequestAssetPowerMessage(
        3000L,
        Quantities.getQuantity(1.000000000000001d, PU),
        Quantities.getQuantity(0d, PU)
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0.095, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0.0312, MEGAVAR),
            testingTolerance
          )
      }
    }

    "replies unchanged power values after asking a second time with altered voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with unchanged information */
      mockAgent ! RequestAssetPowerMessage(
        3000L,
        Quantities.getQuantity(0.98d, PU),
        Quantities.getQuantity(0d, PU)
      )

      /* Expect, that nothing has changed, as this model is meant to forward information from outside */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0.095, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0.0312, MEGAVAR),
            testingTolerance
          )
      }
    }

    "preparing incoming primary data" when {
      val participantAgent =
        TestFSMRef(new ParticipantAgentMock(scheduler.ref)).underlyingActor
      val reactivePowerFunction = (_: ComparableQuantity[Power]) =>
        Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_IN)

      "sending unsupported data" should {
        "fail" in {
          val data = Map(
            primaryServiceProxy.ref -> Some(
              ApparentPowerAndHeat(
                Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN),
                Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_IN),
                Quantities.getQuantity(0d, StandardUnits.HEAT_DEMAND)
              )
            )
          )

          participantAgent.prepareData(data, reactivePowerFunction) match {
            case Failure(exception: IllegalStateException) =>
              exception.getMessage shouldBe "Got the wrong primary data. Expected: edu.ie3.simona.agent.participant.data.Data$PrimaryData$ApparentPower, got: edu.ie3.simona.agent.participant.data.Data$PrimaryData$ApparentPowerAndHeat"
            case Failure(exception) =>
              fail(s"Failed with wrong exception:\n\t$exception")
            case Success(_) => fail("Was meant to fail, but succeeded")
          }
        }
      }

      "sending enhanceable data" should {
        "fail, if enhanced data are not supported" in {
          val data = Map(
            primaryServiceProxy.ref -> Some(
              ActivePowerAndHeat(
                Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN),
                Quantities.getQuantity(0d, StandardUnits.HEAT_DEMAND)
              )
            )
          )

          participantAgent.prepareData(data, reactivePowerFunction) match {
            case Failure(exception: IllegalStateException) =>
              exception.getMessage shouldBe "Received primary data cannot be enriched to expected data. Expected: edu.ie3.simona.agent.participant.data.Data$PrimaryData$ApparentPower, got: edu.ie3.simona.agent.participant.data.Data$PrimaryData$ActivePowerAndHeat, enriched to: edu.ie3.simona.agent.participant.data.Data$PrimaryData$ApparentPowerAndHeat"
            case Failure(exception) =>
              fail(s"Failed with wrong exception:\n\t$exception")
            case Success(_) => fail("Was meant to fail, but succeeded")
          }
        }

        "lead to proper enriched data, if supported" in {
          val data = Map(
            primaryServiceProxy.ref -> Some(
              ActivePower(
                Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
              )
            )
          )

          participantAgent.prepareData(data, reactivePowerFunction) match {
            case Success(ApparentPower(p, q)) =>
              p should equalWithTolerance(
                Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
              )
              q should equalWithTolerance(
                Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_IN)
              )
            case Success(value) =>
              fail(s"Succeeded, but with wrong data: '$value'.")
            case Failure(exception) =>
              fail(
                "Was meant to succeed, but failed with exception.",
                exception
              )
          }
        }

        "lead to proper enriched data, if supported and utilizing a active to reactive power function" in {
          val data = Map(
            primaryServiceProxy.ref -> Some(
              ActivePower(
                Quantities.getQuantity(100d, StandardUnits.ACTIVE_POWER_IN)
              )
            )
          )

          participantAgent.prepareData(
            data,
            (p: ComparableQuantity[Power]) => p.multiply(tan(acos(0.9)))
          ) match {
            case Success(ApparentPower(p, q)) =>
              p should equalWithTolerance(
                Quantities.getQuantity(100d, StandardUnits.ACTIVE_POWER_IN)
              )
              q should equalWithTolerance(
                Quantities
                  .getQuantity(48.43221, StandardUnits.REACTIVE_POWER_IN),
                1e-4
              )
            case Success(value) =>
              fail(s"Succeeded, but with wrong data: '$value'.")
            case Failure(exception) =>
              fail(
                "Was meant to succeed, but failed with exception.",
                exception
              )
          }
        }
      }
    }
  }
}
