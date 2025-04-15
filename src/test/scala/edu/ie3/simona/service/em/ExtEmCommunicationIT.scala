package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant2.ParticipantAgentInit.ParticipantRefs
import edu.ie3.simona.api.data.em.model.{ExtendedFlexOptionsResult, FlexOptionRequest, FlexOptions}
import edu.ie3.simona.api.data.em.ontology.{EmCompletion, EmSetPointDataResponse, FlexOptionsResponse, FlexRequestResponse}
import edu.ie3.simona.api.data.em.{EmMode, ExtEmDataConnection}
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ontology.ControlResponseMessageFromExt
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.RegisterControlledAsset
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.Create
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.EmCommunicationTestData
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.QuantityUtils._
import org.apache.pekko.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}

import java.util.{Optional, UUID}
import scala.jdk.CollectionConverters.{MapHasAsJava, MapHasAsScala, SeqHasAsJava}

class ExtEmCommunicationIT
  extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with EmCommunicationTestData
  with TestSpawnerTyped {

  protected val log: Logger = LoggerFactory.getLogger("ExtEmCommunicationIT")

  private val emSupUuid = UUID.fromString("858f3d3d-4189-49cd-9fe5-3cd49b88dc70")
  private val emNode3Uuid = UUID.fromString("fd1a8de9-722a-4304-8799-e1e976d9979c")
  private val emNode4Uuid = UUID.fromString("ff0b995a-86ff-4f4d-987e-e475a64f2180")

  private val connection = new ExtEmDataConnection(
    List(emSupUuid, emNode3Uuid, emNode4Uuid).asJava,
    EmMode.EM_COMMUNICATION,
  )

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")
  private val extSimAdapter = TestProbe[ControlResponseMessageFromExt]("extSimAdapter")
  private val resultListener = TestProbe[ResultEvent]("ResultListener")

  private val gridAgent = TestProbe[GridAgent.Request]("GridAgent")
  private val primaryServiceProxy =
    TestProbe[ServiceMessage]("PrimaryServiceProxy")
  private val weatherService = TestProbe[ServiceMessage]("WeatherService")

  private val participantRefs = ParticipantRefs(
    gridAgent = gridAgent.ref,
    primaryServiceProxy = primaryServiceProxy.ref,
    services = Map(ServiceType.WeatherService -> weatherService.ref),
    resultListener = Iterable(resultListener.ref),
  )

  "An ExtEmDataService im communication mode" should {

    "communicate correctly" in {
      val service = spawn(ExtEmDataService(scheduler.ref))
      val serviceRef = service.ref
      val adapter = spawn(ExtEmDataService.adapter(service))
      connection.setActorRefs(adapter, extSimAdapter.ref)


      val emAgentSup = spawn(
        EmAgent(
          emSup,
          modelConfig,
          outputConfig,
          "PROPORTIONAL",
          simulationStart,
          parent = Left(scheduler.ref),
          listener = Iterable(resultListener.ref),
          Some(serviceRef),
        )
      )

      val emAgentNode3 = spawn(
        EmAgent(
          emNode3,
          modelConfig,
          outputConfig,
          "PRIORITIZED",
          simulationStart,
          parent = Right(emAgentSup),
          listener = Iterable(resultListener.ref),
          Some(serviceRef),
        )
      )

      val emAgentNode4 = spawn(
        EmAgent(
          emNode4,
          modelConfig,
          outputConfig,
          "PRIORITIZED",
          simulationStart,
          parent = Right(emAgentSup),
          listener = Iterable(resultListener.ref),
          Some(serviceRef),
        )
      )

      val pvAgentNode3 = TestProbe[FlexibilityMessage.FlexRequest]("PvAgentNode3")
      val pvAgentNode4 = TestProbe[FlexibilityMessage.FlexRequest]("PvAgentNode4")
      val storageAgentNode3 = TestProbe[FlexibilityMessage.FlexRequest]("storageAgentNode3")
      val loadAgentNode4 = TestProbe[FlexibilityMessage.FlexRequest]("LoadAgentNode4")

      emAgentNode3 ! RegisterControlledAsset(pvAgentNode3.ref, pvNode3)
      emAgentNode3 ! RegisterControlledAsset(storageAgentNode3.ref, storageInput)

      emAgentNode4 ! RegisterControlledAsset(pvAgentNode4.ref, pvNode4)
      emAgentNode4 ! RegisterControlledAsset(loadAgentNode4.ref, loadInput)

      /* INIT */

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      service ! Create(
        InitExtEmData(connection, simulationStart),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)
      val serviceActivation = activationMsg.actor

      serviceActivation ! Activation(INIT_SIM_TICK)


      /* start communication */

      // we first send a flex option request to the superior em agent
      connection.sendFlexRequests(
        0,
        Map(emSupUuid -> new FlexOptionRequest(emSupUuid, Optional.empty())).asJava,
        Optional.of(900),
        log
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(0)

      // we expect to receive a request per inferior em agent
      val requestsToInferior = connection.receiveWithType(classOf[FlexRequestResponse])
      requestsToInferior.flexRequests().asScala shouldBe Map(emSupUuid -> List(emNode3Uuid, emNode4Uuid).asJava)

      // we send a request to each inferior em agent
      connection.sendFlexRequests(
        0,
        Map(
          emNode3Uuid -> new FlexOptionRequest(emNode3Uuid, Optional.of(emSupUuid)),
          emNode4Uuid -> new FlexOptionRequest(emNode4Uuid, Optional.of(emSupUuid)),
        ).asJava,
        Optional.of(900),
        log,
      )

      // we expect to receive flex options from the inferior em agents
      val flexOptionResponseInferior = connection.receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      flexOptionResponseInferior shouldBe Map(
        emNode3Uuid -> new ExtendedFlexOptionsResult(
          simulationStart,
          emNode3Uuid,
          emSupUuid,
          0.0.asKiloWatt,
          0.0.asKiloWatt,
          4.asKiloWatt
        ),
        emNode4Uuid -> new ExtendedFlexOptionsResult(
          simulationStart,
          emNode4Uuid,
          emSupUuid,
          2.200000413468004.asKiloWatt,
          2.200000413468004.asKiloWatt,
          2.200000413468004.asKiloWatt
        )
      )

      // we send the flex options to the superior em agent
      connection.sendFlexOptions(
        0,
        Map(
          emSupUuid -> List(
            new FlexOptions(
              emSupUuid,
              emNode3Uuid,
              0.0.asKiloWatt,
              0.0.asKiloWatt,
              4.asKiloWatt
            ),
            new FlexOptions(
              emSupUuid,
              emNode4Uuid,
              2.200000413468004.asKiloWatt,
              2.200000413468004.asKiloWatt,
              2.200000413468004.asKiloWatt
            ),
          ).asJava
        ).asJava,
        Optional.of(900),
        log
      )

      // we expect the total flex options of the grid from the superior em agent
      val totalFlexOptions = connection.receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      totalFlexOptions shouldBe Map(
        emSup -> new ExtendedFlexOptionsResult(
          simulationStart,
          emSupUuid,
          emSupUuid,
          2.200000413468004.asKiloWatt,
          2.200000413468004.asKiloWatt,
          6.200000413468004.asKiloWatt
        )
      )

      // after we received all options we will send a message, to keep the current set point
      connection.sendSetPoints(
        0,
        Map(emSupUuid -> new PValue(2.200000413468004.asKiloWatt)).asJava,
        Optional.of(900),
        log
      )

      // we expect a new set point for each inferior em agent
      val inferiorSetPoints = connection.receiveWithType(classOf[EmSetPointDataResponse])
        .emData().asScala

      inferiorSetPoints shouldBe Map(
        emNode3Uuid -> new PValue(0.0.asKiloWatt),
        emNode4Uuid -> new PValue(2.200000413468004.asKiloWatt)
      )

      // we send the new set points to the inferior em agents
      connection.sendSetPoints(
        0,
        Map(
          emNode3Uuid -> new PValue(0.0.asKiloWatt),
          emNode4Uuid -> new PValue(2.200000413468004.asKiloWatt),
        ).asJava,
        Optional.of(900),
        log
      )

      // we expect a finish message
      connection.receiveWithType(classOf[EmCompletion])


    }
  }
}
