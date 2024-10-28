/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{ActorRef, ActorSystem}
import org.apache.pekko.testkit.TestFSMRef
import org.apache.pekko.util.Timeout
import breeze.numerics.{acos, tan}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
}
import edu.ie3.simona.agent.participant.ParticipantAgent.RequestAssetPowerMessageimport edu.ie3.simona.agent.participant.data.Data.PrimaryData.{ActivePower, ActivePowerAndHeat, ApparentPower, ApparentPowerAndHeat,
}
import edu.ie3.simona.agent.participant.statedata.BaseStateData.FromOutsideBaseStateData
import edu.ie3.simona.agent.participant.statedata.DataCollectionStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{ParticipantInitializeStateData, ParticipantInitializingStateData, ParticipantUninitializedStateData,
  SimpleInputContainer,
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.RuntimeConfig.BaseRuntimeConfig
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.model.participant.{CalcRelevantData, SystemParticipant}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.trigger.Trigger.{ActivityStartTrigger, InitializeParticipantAgentTrigger}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.ProvidePrimaryDataMessage
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.{Kilovars, Megavars, ReactivePower, Vars}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import squants.{Each, Power}
import squants.energy.{Kilowatts, Megawatts, Watts}
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.collection.{SortedMap, SortedSet}
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
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="DEBUG"
        """.stripMargin),
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
    mock[SystemParticipant[
      CalcRelevantData.FixedRelevantData.type,
      ApparentPower,
      ConstantState.type,
    ]]
  when(mockModel.getUuid).thenReturn(testUUID)
  private val activeToReactivePowerFunction: Power => ReactivePower =
    (p: Power) => Kilovars(p.toKilowatts * tan(acos(0.9)))
  when(
    mockModel.activeToReactivePowerFunc(
      any(classOf[squants.Dimensionless])
    )
  ).thenReturn(activeToReactivePowerFunction)

  private val simonaConfig: SimonaConfig = createSimonaConfig(
    LoadModelBehaviour.FIX,
    LoadReference.ActivePower(Kilowatts(0.0)),
  )
  private val defaultOutputConfig = NotifierConfig(
    simulationResultInfo = false,
    powerRequestReply = false,
    flexResult = false,
  )

  private val resolution = simonaConfig.powerflow.resolution.toSeconds

  private implicit val powerTolerance: Power = Watts(0.1)
  private implicit val reactivePowerTolerance: ReactivePower = Vars(0.1)

  "A participant agent with externally given data provider" should {
    val initStateData = ParticipantInitializeStateData[
      SystemParticipantInput,
      BaseRuntimeConfig,
      ApparentPower,
    ](
      inputModel = mockInputModel,
      modelConfig = mock[BaseRuntimeConfig],
      secondaryDataServices = Iterable.empty,
      simulationStartDate = defaultSimulationStart,
      simulationEndDate = defaultSimulationEnd,
      resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
      requestVoltageDeviationThreshold =
        simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
      outputConfig = defaultOutputConfig,
      primaryServiceProxy = primaryServiceProxy.ref,
    )

    "be instantiated correctly" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref,
          initStateData = initStateData,
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
          scheduler = scheduler.ref,
          initStateData = initStateData,
        )
      )

      scheduler.send(mockAgent, Activation(INIT_SIM_TICK))

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
              outputConfig,
              maybeEmAgent,
            ) =>
          inputModel shouldBe SimpleInputContainer(mockInputModel)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe Iterable.empty
          simulationStartDate shouldBe defaultSimulationStart
          simulationEndDate shouldBe defaultSimulationEnd
          resolution shouldBe this.resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
          maybeEmAgent shouldBe None
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Reply, that registration was successful */
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(primaryServiceProxy.ref, Some(4711L)),
      )

      scheduler.expectMsg(Completion(mockAgent.toTyped, Some(4711L)))

      /* ... as well as corresponding state and state data */
      mockAgent.stateName shouldBe Idle
      mockAgent.stateData match {
        case baseStateData: FromOutsideBaseStateData[SystemParticipant[
              FixedRelevantData.type,
              ApparentPower,
              ConstantState.type,
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
          scheduler = scheduler.ref,
          initStateData = initStateData,
        )
      )

      scheduler.send(mockAgent, Activation(INIT_SIM_TICK))

      /* I'm not interested in the content of the RegistrationMessage */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(primaryServiceProxy.ref, Some(900L)),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(mockAgent.stateName shouldBe Idle)

      /* State data has already been tested */

      mockAgent ! RequestAssetPowerMessage(
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
              requestValueStore,
            ) =>
          requestValueStore shouldBe ValueStore[ApparentPower](
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
            s"Did not found the expected state data $FromOutsideBaseStateData, but ${mockAgent.stateData}"
          )
      }
    }

    "do correct transitions faced to new data in Idle" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref,
          initStateData = initStateData,
        )
      )

      scheduler.send(mockAgent, Activation(INIT_SIM_TICK))

      /* I'm not interested in the content of the RegistrationMessage */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(primaryServiceProxy.ref, Some(900L)),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(mockAgent.stateName shouldBe Idle)

      /* Send out new data */
      primaryServiceProxy.send(
        mockAgent,
        ProvidePrimaryDataMessage(
          900L,
          primaryServiceProxy.ref,
          ApparentPower(
            Kilowatts(0.0),
            Kilovars(900.0),
          ),
          Some(1800L),
        ),
      )

      /* Find yourself in corresponding state and state data */
      mockAgent.stateName shouldBe HandleInformation
      mockAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: FromOutsideBaseStateData[SystemParticipant[
                CalcRelevantData,
                ApparentPower,
                ConstantState.type,
              ], ApparentPower],
              expectedSenders,
              isYetTriggered,
            ) =>
          /* The next data tick is already registered */
          baseStateData.foreseenDataTicks shouldBe Map(
            primaryServiceProxy.ref -> Some(1800L)
          )

          /* The yet sent data is also registered */
          expectedSenders shouldBe Map(
            primaryServiceProxy.ref -> Some(
              ApparentPower(
                Kilowatts(0.0),
                Kilovars(900.0),
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
      scheduler.send(mockAgent, Activation(900))

      /* Expect confirmation */
      scheduler.expectMsg(Completion(mockAgent.toTyped, Some(1800)))

      /* Expect the state change to idle with updated base state data */
      mockAgent.stateName shouldBe Idle
      mockAgent.stateData match {
        case baseStateData: FromOutsideBaseStateData[SystemParticipant[
              CalcRelevantData,
              ApparentPower,
              ConstantState.type,
            ], ApparentPower] =>
          /* The new data is apparent in the result value store */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                900L -> ApparentPower(
                  Kilowatts(0.0),
                  Kilovars(900.0),
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
          scheduler = scheduler.ref,
          initStateData = initStateData,
        )
      )

      scheduler.send(mockAgent, Activation(INIT_SIM_TICK))

      /* I'm not interested in the content of the RegistrationMessage */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(primaryServiceProxy.ref, Some(900L)),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(mockAgent.stateName shouldBe Idle)

      /* Send out an activity start trigger */
      scheduler.send(mockAgent, Activation(900))

      /* Find yourself in appropriate state with state data */
      mockAgent.stateName shouldBe HandleInformation
      mockAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: FromOutsideBaseStateData[SystemParticipant[
                CalcRelevantData,
                ApparentPower,
                ConstantState.type,
              ], ApparentPower],
              expectedSenders,
              isYetTriggered,
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
          primaryServiceProxy.ref,
          ApparentPower(
            Kilowatts(0.0),
            Kilovars(900.0),
          ),
          Some(1800L),
        ),
      )

      /* Expect confirmation */
      scheduler.expectMsg(Completion(mockAgent.toTyped, Some(1800)))

      /* Expect the state change to idle with updated base state data */
      mockAgent.stateName shouldBe Idle
      mockAgent.stateData match {
        case baseStateData: FromOutsideBaseStateData[SystemParticipant[
              CalcRelevantData,
              ApparentPower,
              ConstantState.type,
            ], ApparentPower] =>
          /* The new data is apparent in the result value store */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                900L -> ApparentPower(
                  Kilowatts(0.0),
                  Kilovars(900.0),
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
          scheduler = scheduler.ref,
          initStateData = initStateData,
        )
      )

      /* Trigger the initialisation */
      scheduler.send(mockAgent, Activation(INIT_SIM_TICK))

      /* I'm not interested in the content of the RegistrationMessage */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(primaryServiceProxy.ref, Some(900L)),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(mockAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 1800 */
      mockAgent ! RequestAssetPowerMessage(
        1800L,
        Each(1.0),
        Each(0.0),
      )
      expectNoMessage(noReceiveTimeOut.duration)
      awaitAssert(mockAgent.stateName == Idle)

      /* Send out the expected data and wait for the reply */
      primaryServiceProxy.send(
        mockAgent,
        ProvidePrimaryDataMessage(
          900L,
          primaryServiceProxy.ref,
          ApparentPower(
            Kilowatts(0.0),
            Kilovars(900.0),
          ),
          Some(1800L),
        ),
      )

      /* Trigger the agent */
      scheduler.send(mockAgent, Activation(900))

      scheduler.expectMsg(Completion(mockAgent.toTyped, Some(1800)))

      /* Appreciate the answer to my previous request */
      expectMsgType[AssetPowerChangedMessage]
    }

    val mockAgent = TestFSMRef(
      new ParticipantAgentMock(
        scheduler = scheduler.ref,
        initStateData = initStateData,
      )
    )

    "correctly determine the reactive power function when trivial reactive power is requested" in {
      val baseStateData: FromOutsideBaseStateData[SystemParticipant[
        CalcRelevantData.FixedRelevantData.type,
        ApparentPower,
        ConstantState.type,
      ], ApparentPower] = FromOutsideBaseStateData[SystemParticipant[
        CalcRelevantData.FixedRelevantData.type,
        ApparentPower,
        ConstantState.type,
      ], ApparentPower](
        mockModel,
        defaultSimulationStart,
        defaultSimulationEnd,
        defaultOutputConfig,
        SortedSet.empty,
        Map.empty[ActorRef, Option[Long]],
        fillUpReactivePowerWithModelFunc = false,
        1e-4,
        ValueStore.forVoltage(
          900L,
          Each(1.0),
        ),
        ValueStore.forResult(900L, 1L),
        ValueStore(900L),
      )

      val actualFunction =
        mockAgent.underlyingActor.getReactivePowerFunction(0L, baseStateData)
      actualFunction(Kilowatts(100.0)) should approximate(Kilovars(0.0))
    }

    "correctly determine the reactive power function from model when requested" in {
      val baseStateData: FromOutsideBaseStateData[SystemParticipant[
        CalcRelevantData.FixedRelevantData.type,
        ApparentPower,
        ConstantState.type,
      ], ApparentPower] = FromOutsideBaseStateData[SystemParticipant[
        CalcRelevantData.FixedRelevantData.type,
        ApparentPower,
        ConstantState.type,
      ], ApparentPower](
        mockModel,
        defaultSimulationStart,
        defaultSimulationEnd,
        defaultOutputConfig,
        SortedSet.empty,
        Map.empty[ActorRef, Option[Long]],
        fillUpReactivePowerWithModelFunc = true,
        1e-4,
        ValueStore.forVoltage(
          900L,
          Each(1.0),
        ),
        ValueStore.forResult(900L, 1L),
        ValueStore(900L),
      )

      val actualFunction =
        mockAgent.underlyingActor.getReactivePowerFunction(0L, baseStateData)
      actualFunction(Kilowatts(100.0)) should approximate(Kilovars(48.43221))
    }

    "provide correct average power after three data ticks are available" in {
      /* Trigger the initialisation */
      scheduler.send(mockAgent, Activation(INIT_SIM_TICK))

      /* I'm not interested in the content of the RegistrationMessage */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationSuccessfulMessage(primaryServiceProxy.ref, Some(900L)),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(mockAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 900 */
      primaryServiceProxy.send(
        mockAgent,
        ProvidePrimaryDataMessage(
          900L,
          primaryServiceProxy.ref,
          ApparentPower(
            Kilowatts(100.0),
            Kilovars(33.0),
          ),
          Some(1800L),
        ),
      )
      scheduler.send(mockAgent, Activation(900))
      scheduler.expectMsg(Completion(mockAgent.toTyped, Some(1800)))

      /* ... for tick 1800 */
      primaryServiceProxy.send(
        mockAgent,
        ProvidePrimaryDataMessage(
          1800L,
          primaryServiceProxy.ref,
          ApparentPower(
            Kilowatts(150.0),
            Kilovars(49.0),
          ),
          Some(2700L),
        ),
      )
      scheduler.send(mockAgent, Activation(1800))
      scheduler.expectMsg(Completion(mockAgent.toTyped, Some(2700)))

      /* ... for tick 2700 */
      primaryServiceProxy.send(
        mockAgent,
        ProvidePrimaryDataMessage(
          2700L,
          primaryServiceProxy.ref,
          ApparentPower(
            Kilowatts(200.0),
            Kilovars(66.0),
          ),
          None,
        ),
      )
      scheduler.send(mockAgent, Activation(2700))
      scheduler.expectMsg(Completion(mockAgent.toTyped))

      awaitAssert(mockAgent.stateName == Idle)

      /* Ask the agent for average power in tick 3000 */
      mockAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1.0),
        Each(0.0),
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0.095))
          q should approximate(Megavars(0.0312))
      }
    }

    "replies unchanged power values after asking a second time" in {
      /* Previous request stems from previous test */
      /* Ask again with unchanged information */
      mockAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1.000000000000001d),
        Each(0.0),
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should approximate(Megawatts(0.095))
          q should approximate(Megavars(0.0312))
      }
    }

    "replies unchanged power values after asking a second time with altered voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with unchanged information */
      mockAgent ! RequestAssetPowerMessage(
        3000L,
        Each(0.98d),
        Each(0.0),
      )

      /* Expect, that nothing has changed, as this model is meant to forward information from outside */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should approximate(Megawatts(0.095))
          q should approximate(Megavars(0.0312))
      }
    }

    "preparing incoming primary data" when {
      val participantAgent =
        TestFSMRef(
          new ParticipantAgentMock(scheduler.ref, initStateData = initStateData)
        ).underlyingActor
      val reactivePowerFunction = (_: squants.Power) => Kilovars(0.0)

      "sending unsupported data" should {
        "fail" in {
          val data = Map(
            primaryServiceProxy.ref -> Some(
              ApparentPowerAndHeat(
                Kilowatts(0.0),
                Kilovars(0.0),
                Kilowatts(0.0),
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
                Kilowatts(0.0),
                Kilowatts(0.0),
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
              ActivePower(Kilowatts(0.0))
            )
          )

          participantAgent.prepareData(data, reactivePowerFunction) match {
            case Success(ApparentPower(p, q)) =>
              p should approximate(Megawatts(0.0))
              q should approximate(Megavars(0.0))
            case Success(value) =>
              fail(s"Succeeded, but with wrong data: '$value'.")
            case Failure(exception) =>
              fail(
                "Was meant to succeed, but failed with exception.",
                exception,
              )
          }
        }

        "lead to proper enriched data, if supported and utilizing a active to reactive power function" in {
          val data = Map(
            primaryServiceProxy.ref -> Some(
              ActivePower(Kilowatts(100.0))
            )
          )

          participantAgent.prepareData(
            data,
            (p: squants.Power) => Kilovars(p.toKilowatts * tan(acos(0.9))),
          ) match {
            case Success(ApparentPower(p, q)) =>
              p should approximate(Kilowatts(100.0))
              q should approximate(Kilovars(48.43221))
            case Success(value) =>
              fail(s"Succeeded, but with wrong data: '$value'.")
            case Failure(exception) =>
              fail(
                "Was meant to succeed, but failed with exception.",
                exception,
              )
          }
        }
      }
    }
  }
}
