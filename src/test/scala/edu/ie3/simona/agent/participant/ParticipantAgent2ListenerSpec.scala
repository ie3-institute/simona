/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestFSMRef
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.RuntimeConfig.BaseRuntimeConfig
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  RequestAssetPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  FinishGridSimulationTrigger,
  InitializeParticipantAgentTrigger
}
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits.{MEGAVAR, MEGAWATT}
import org.mockito.Mockito.when
import org.scalatest.PrivateMethodTester
import org.scalatestplus.mockito.MockitoSugar
import squants.Each
import squants.energy.Kilowatts
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import java.util.concurrent.TimeUnit

class ParticipantAgent2ListenerSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "ParticipantAgent2ListenerSpec",
        ConfigFactory
          .parseString("""
            |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
            |akka.loglevel="OFF"
        """.stripMargin)
      )
    )
    with DefaultTestData
    with PrivateMethodTester
    with MockitoSugar {

  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  /* Assign this test to receive the result events from agent */
  override val systemListener: Iterable[ActorRef] = Vector(self)

  private val testUUID = UUID.randomUUID
  private val testID = "PartAgentExternalMock"

  private implicit val quantityTolerance: Double = 1e-6 // Equals to 1 W power
  private val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(Kilowatts(0d))
    )

  private val mockInputModel = mock[SystemParticipantInput]
  when(mockInputModel.getUuid).thenReturn(testUUID)
  when(mockInputModel.getId).thenReturn(testID)

  private val sources = None

  "A participant agent" should {
    "inform listeners about new simulation results, when asked to do" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      /* Let the agent send announcements, when there is anew request reply */
      val outputConfig = ParticipantNotifierConfig(
        simulationResultInfo = true,
        powerRequestReply = false
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
              secondaryDataServices = sources,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = simonaConfig.powerflow.resolution.toSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = outputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0,
          mockAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(mockAgent, RegistrationFailedMessage)

      scheduler.receiveOne(receiveTimeOut.duration) match {
        case _: CompletionMessage =>
          logger.debug("Agent completed initialization.")
      }

      /* Trigger the data generation in tick 0 */
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1,
          mockAgent
        )
      )

      /* Receive the completion message for the calculation */
      scheduler.expectMsgType[CompletionMessage]
      logger.debug("Agent completed model calculation.")

      /* Receive the listener announcement */
      expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(
              systemParticipantResult: SystemParticipantResult
            ) =>
          systemParticipantResult.getP should equalWithTolerance(
            Quantities.getQuantity(2, MEGAWATT),
            quantityTolerance
          )
          systemParticipantResult.getQ should equalWithTolerance(
            Quantities.getQuantity(1, MEGAVAR),
            quantityTolerance
          )
        case _ => fail("Expected a SystemParticipantResult")
      }
    }

    "not inform listeners about new simulation results, when not asked to do" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      /* Let the agent send announcements, when there is anew request reply */
      val outputConfig = ParticipantNotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false
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
              secondaryDataServices = sources,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = simonaConfig.powerflow.resolution.toSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = outputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0,
          mockAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(mockAgent, RegistrationFailedMessage)

      scheduler.receiveOne(receiveTimeOut.duration) match {
        case _: CompletionMessage =>
          logger.debug("Agent completed initialization.")
      }

      /* Trigger the data generation in tick 0 */
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1,
          mockAgent
        )
      )

      /* Receive the completion message for the calculation and no listener announcement */
      scheduler.expectMsgClass(
        receiveTimeOut.duration,
        classOf[CompletionMessage]
      )
      expectNoMessage(noReceiveTimeOut.duration)
    }

    "not inform listeners about request reply, when asked to do (currently not implemented)" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      /* Let the agent send announcements, when there is anew request reply */
      val outputConfig = ParticipantNotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = true
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
              secondaryDataServices = sources,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = simonaConfig.powerflow.resolution.toSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = outputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0,
          mockAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(mockAgent, RegistrationFailedMessage)

      /* Trigger the data generation in tick 0 */
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1,
          mockAgent
        )
      )

      /* Appreciate the existence of two CompletionMessages */
      val completedTriggerIds = scheduler.receiveWhile(messages = 2) {
        case msg: CompletionMessage => msg.triggerId
      }
      logger.debug(
        s"Received CompletionMessages for the following trigger ids: $completedTriggerIds"
      )

      /* Ask the agent for average power in tick 3000 */
      mockAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1d),
        Each(0d)
      )

      /* Wait for original reply (this is the querying agent) */
      receiveOne(receiveTimeOut.duration) match {
        case AssetPowerChangedMessage(p, q) =>
          logger.debug(s"Agent answered with changed power ($p, $q)")
        case AssetPowerUnchangedMessage(p, q) =>
          logger.debug(s"Agent answered with unchanged power ($p, $q)")
        case unknownMsg => fail(s"Received unexpected message: $unknownMsg")
      }

      scheduler.send(mockAgent, FinishGridSimulationTrigger(3000L))

      /* Wait for the result event (this is the event listener) */
      logger.warn(
        "Writing out power request replies is currently not implemented. Reimplement this test, as soon as" +
          "the function is available!"
      )
      expectNoMessage(noReceiveTimeOut.duration)
    }

    "not inform listeners about request reply, when not asked to do" in {
      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      /* Let the agent send announcements, when there is anew request reply */
      val outputConfig = ParticipantNotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false
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
              secondaryDataServices = sources,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = simonaConfig.powerflow.resolution.toSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = outputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0,
          mockAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(mockAgent, RegistrationFailedMessage)

      /* Trigger the data generation in tick 0 */
      scheduler.send(
        mockAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1,
          mockAgent
        )
      )

      /* Appreciate the existence of two CompletionMessages */
      val completedTriggerIds = scheduler.receiveWhile(messages = 2) {
        case msg: CompletionMessage => msg.triggerId
      }
      logger.debug(
        s"Received CompletionMessages for the following trigger ids: $completedTriggerIds"
      )

      /* Ask the agent for average power in tick 3000 */
      mockAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1d),
        Each(0d)
      )

      /* Wait for original reply (this is the querying agent) */
      receiveOne(receiveTimeOut.duration) match {
        case AssetPowerChangedMessage(p, q) =>
          logger.debug(s"Agent answered with changed power ($p, $q)")
        case AssetPowerUnchangedMessage(p, q) =>
          logger.debug(s"Agent answered with unchanged power ($p, $q)")
        case unknownMsg => fail(s"Received unexpected message: $unknownMsg")
      }

      scheduler.send(mockAgent, FinishGridSimulationTrigger(3000L))

      /* Make sure nothing else is sent */
      expectNoMessage(noReceiveTimeOut.duration)
    }
  }
}
