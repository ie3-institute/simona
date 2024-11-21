/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
}
import edu.ie3.simona.agent.participant.ParticipantAgent.{
  FinishParticipantSimulation,
  RequestAssetPowerMessage,
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.BaseRuntimeConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.PowerSystemUnits.{MEGAVAR, MEGAWATT}
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{ActorRef, ActorSystem}
import org.apache.pekko.testkit.TestFSMRef
import org.apache.pekko.util.Timeout
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
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="OFF"
        """.stripMargin),
      )
    )
    with DefaultTestData
    with PrivateMethodTester
    with MockitoSugar {

  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  /* Assign this test to receive the result events from agent */
  override val systemListener: Iterable[ActorRef] = Iterable(self)

  private val testUUID = UUID.randomUUID
  private val testID = "PartAgentExternalMock"

  private implicit val quantityTolerance: Double = 1e-6 // Equals to 1 W power
  private val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(Kilowatts(0d)),
    )

  private val mockInputModel = mock[SystemParticipantInput]
  when(mockInputModel.getUuid).thenReturn(testUUID)
  when(mockInputModel.getId).thenReturn(testID)

  private val services = Iterable.empty

  "A participant agent" should {
    val initStateData: NotifierConfig => ParticipantInitializeStateData[
      SystemParticipantInput,
      BaseRuntimeConfig,
      ComplexPower,
    ] = outputConfig =>
      ParticipantInitializeStateData[
        SystemParticipantInput,
        BaseRuntimeConfig,
        ComplexPower,
      ](
        inputModel = mockInputModel,
        modelConfig = mock[BaseRuntimeConfig],
        secondaryDataServices = services,
        simulationStartDate = defaultSimulationStart,
        simulationEndDate = defaultSimulationEnd,
        resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
        requestVoltageDeviationThreshold =
          simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
        outputConfig = outputConfig,
        primaryServiceProxy = primaryServiceProxy.ref,
      )

    "inform listeners about new simulation results, when asked to do" in {
      /* Let the agent send announcements, when there is anew request reply */
      val outputConfig = NotifierConfig(
        simulationResultInfo = true,
        powerRequestReply = false,
        flexResult = false,
      )

      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref,
          initStateData = initStateData(outputConfig),
          listener = systemListener,
        )
      )

      /* Trigger the initialisation */
      scheduler.send(mockAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      scheduler.expectMsg(Completion(mockAgent.toTyped))

      /* Trigger the data generation in tick 0 */
      scheduler.send(mockAgent, Activation(0))

      /* Receive the completion message for the calculation */
      scheduler.expectMsgType[Completion]
      logger.debug("Agent completed model calculation.")

      /* Receive the listener announcement */
      expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(
              systemParticipantResult: SystemParticipantResult
            ) =>
          systemParticipantResult.getP should equalWithTolerance(
            Quantities.getQuantity(2, MEGAWATT),
            quantityTolerance,
          )
          systemParticipantResult.getQ should equalWithTolerance(
            Quantities.getQuantity(1, MEGAVAR),
            quantityTolerance,
          )
        case _ => fail("Expected a SystemParticipantResult")
      }
    }

    "not inform listeners about new simulation results, when not asked to do" in {
      /* Let the agent send announcements, when there is anew request reply */
      val outputConfig = NotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false,
        flexResult = false,
      )

      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref,
          initStateData = initStateData(outputConfig),
          listener = systemListener,
        )
      )

      /* Trigger the initialisation */
      scheduler.send(mockAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      scheduler.expectMsg(Completion(mockAgent.toTyped))

      /* Trigger the data generation in tick 0 */
      scheduler.send(mockAgent, Activation(0))

      /* Receive the completion message for the calculation and no listener announcement */
      scheduler.expectMsgType[Completion]
      expectNoMessage(noReceiveTimeOut.duration)
    }

    "not inform listeners about request reply, when asked to do (currently not implemented)" in {
      /* Let the agent send announcements, when there is anew request reply */
      val outputConfig = NotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = true,
        flexResult = false,
      )

      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref,
          initStateData = initStateData(outputConfig),
          listener = systemListener,
        )
      )

      /* Trigger the initialisation */
      scheduler.send(mockAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      scheduler.expectMsg(Completion(mockAgent.toTyped))

      /* Trigger the data generation in tick 0 */
      scheduler.send(mockAgent, Activation(0))

      scheduler.expectMsg(Completion(mockAgent.toTyped))

      /* Ask the agent for average power in tick 3000 */
      mockAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1d),
        Each(0d),
      )

      /* Wait for original reply (this is the querying agent) */
      receiveOne(receiveTimeOut.duration) match {
        case AssetPowerChangedMessage(p, q) =>
          logger.debug(s"Agent answered with changed power ($p, $q)")
        case AssetPowerUnchangedMessage(p, q) =>
          logger.debug(s"Agent answered with unchanged power ($p, $q)")
        case unknownMsg => fail(s"Received unexpected message: $unknownMsg")
      }

      scheduler.send(mockAgent, FinishParticipantSimulation(3000L))

      /* Wait for the result event (this is the event listener) */
      logger.warn(
        "Writing out power request replies is currently not implemented. Reimplement this test, as soon as" +
          "the function is available!"
      )
      expectNoMessage(noReceiveTimeOut.duration)

      scheduler.expectNoMessage()
    }

    "not inform listeners about request reply, when not asked to do" in {
      /* Let the agent send announcements, when there is anew request reply */
      val outputConfig = NotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false,
        flexResult = false,
      )

      val mockAgent = TestFSMRef(
        new ParticipantAgentMock(
          scheduler = scheduler.ref,
          initStateData = initStateData(outputConfig),
          listener = systemListener,
        )
      )

      /* Trigger the initialisation */
      scheduler.send(mockAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        mockAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      scheduler.expectMsg(Completion(mockAgent.toTyped))

      /* Trigger the data generation in tick 0 */
      scheduler.send(mockAgent, Activation(0))

      /* Appreciate the existence of two Completion */
      scheduler.expectMsg(Completion(mockAgent.toTyped))

      /* Ask the agent for average power in tick 3000 */
      mockAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1d),
        Each(0d),
      )

      /* Wait for original reply (this is the querying agent) */
      receiveOne(receiveTimeOut.duration) match {
        case AssetPowerChangedMessage(p, q) =>
          logger.debug(s"Agent answered with changed power ($p, $q)")
        case AssetPowerUnchangedMessage(p, q) =>
          logger.debug(s"Agent answered with unchanged power ($p, $q)")
        case unknownMsg => fail(s"Received unexpected message: $unknownMsg")
      }

      scheduler.send(mockAgent, FinishParticipantSimulation(3000L))

      /* Make sure nothing else is sent */
      expectNoMessage(noReceiveTimeOut.duration)
    }
  }
}
