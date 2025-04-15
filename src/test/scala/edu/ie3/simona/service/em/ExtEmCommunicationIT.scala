package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.SimpleInputContainer
import edu.ie3.simona.agent.participant2.ParticipantAgentInit
import edu.ie3.simona.agent.participant2.ParticipantAgentInit.{ParticipantRefs, SimulationParameters}
import edu.ie3.simona.api.data.em.model.{ExtendedFlexOptionsResult, FlexOptionRequest, FlexOptions, FlexRequestResult}
import edu.ie3.simona.api.data.em.ontology.{EmCompletion, EmSetPointDataResponse, FlexOptionsResponse, FlexRequestResponse}
import edu.ie3.simona.api.data.em.{EmMode, ExtEmDataConnection}
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ontology.ControlResponseMessageFromExt
import edu.ie3.simona.config.RuntimeConfig.{LoadRuntimeConfig, PvRuntimeConfig, StorageRuntimeConfig}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage.{Completion, ScheduleActivation}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.flex.{FlexibilityMessage, MinMaxFlexOptions}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.Create
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.EmCommunicationTestData
import edu.ie3.simona.test.matchers.QuantityMatchers
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.util.quantities.QuantityUtils._
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.apache.pekko.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}
import squants.Each
import squants.energy.Kilowatts

import java.util.{Optional, UUID}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.jdk.CollectionConverters.{MapHasAsJava, MapHasAsScala, SeqHasAsJava}
import scala.jdk.OptionConverters.RichOptional

class ExtEmCommunicationIT
  extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with EmCommunicationTestData
    with QuantityMatchers
  with TestSpawnerTyped {

  protected val messageTimeout: FiniteDuration = 30.seconds

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

  "An ExtEmDataService im communication mode" should {
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

    "with participant probes work correctly" in {
          val pvAgentNode3 = TestProbe[FlexibilityMessage.FlexRequest]("PvAgentNode3")
          val pvAgentNode4 = TestProbe[FlexibilityMessage.FlexRequest]("PvAgentNode4")
          val storageAgentNode3 = TestProbe[FlexibilityMessage.FlexRequest]("storageAgentNode3")
          val loadAgentNode4 = TestProbe[FlexibilityMessage.FlexRequest]("LoadAgentNode4")

      /* PRE_INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, PRE_INIT_TICK)
      scheduler.expectMessageType[ScheduleActivation] // lock activation scheduled

      service ! Create(
        InitExtEmData(connection, simulationStart),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)
      val serviceActivation = activationMsg.actor

      /* INIT */

      //register all system participant agents
      emAgentNode3 ! RegisterControlledAsset(pvAgentNode3.ref, pvNode3)
      emAgentNode3 ! ScheduleFlexActivation(pvNode3.getUuid, INIT_SIM_TICK)

      emAgentNode3 ! RegisterControlledAsset(storageAgentNode3.ref, storageInput)
      emAgentNode3 ! ScheduleFlexActivation(storageInput.getUuid, INIT_SIM_TICK)

      emAgentNode4 ! RegisterControlledAsset(pvAgentNode4.ref, pvNode4)
      emAgentNode4 ! ScheduleFlexActivation(pvNode4.getUuid, INIT_SIM_TICK)

      emAgentNode4 ! RegisterControlledAsset(loadAgentNode4.ref, loadInput)
      emAgentNode4 ! ScheduleFlexActivation(loadInput.getUuid, INIT_SIM_TICK)

      // activate the service for init tick
      serviceActivation ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(serviceActivation))

      // the agents will receive a flex activation
      pvAgentNode3.expectMessage(FlexActivation(INIT_SIM_TICK))
      storageAgentNode3.expectMessage(FlexActivation(INIT_SIM_TICK))

      pvAgentNode4.expectMessage(FlexActivation(INIT_SIM_TICK))
      loadAgentNode4.expectMessage(FlexActivation(INIT_SIM_TICK))

      // all agents should answer with a flex completion for the init tick
      emAgentNode3 ! FlexCompletion(pvNode3.getUuid, requestAtTick = Some(0))
      emAgentNode3 ! FlexCompletion(storageInput.getUuid, requestAtTick = Some(0))

      emAgentNode4 ! FlexCompletion(pvNode4.getUuid, requestAtTick = Some(0))
      emAgentNode4 ! FlexCompletion(loadInput.getUuid, requestAtTick = Some(0))


      /* TICK: 0 */

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
        .flexRequests()
        .asScala

      requestsToInferior shouldBe Map(emSupUuid -> new FlexRequestResult(simulationStart, emSupUuid, List(emNode3Uuid, emNode4Uuid).asJava))

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

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(0)

      // we expect flex request from inferior em agents
      pvAgentNode3.expectMessage(messageTimeout, FlexActivation(0))
      emAgentNode3 ! ProvideFlexOptions(pvNode3.getUuid, MinMaxFlexOptions(zeroKW, zeroKW, zeroKW))

      storageAgentNode3.expectMessage(messageTimeout, FlexActivation(0))
      emAgentNode3 ! ProvideFlexOptions(storageInput.getUuid, MinMaxFlexOptions(zeroKW, zeroKW, Kilowatts(4)))

      pvAgentNode4.expectMessage(messageTimeout, FlexActivation(0))
      emAgentNode4 ! ProvideFlexOptions(pvNode4.getUuid, MinMaxFlexOptions(zeroKW, zeroKW, zeroKW))

      loadAgentNode4.expectMessage(messageTimeout, FlexActivation(0))
      emAgentNode4 ! ProvideFlexOptions(loadInput.getUuid, MinMaxFlexOptions(Kilowatts(2.200000413468004), Kilowatts(2.200000413468004), Kilowatts(2.200000413468004)))

      // we expect to receive flex options from the inferior em agents
      val flexOptionResponseInferior = connection.receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      if (flexOptionResponseInferior.size == 1) {
        flexOptionResponseInferior.addAll(
          connection.receiveWithType(classOf[FlexOptionsResponse])
            .receiverToFlexOptions()
            .asScala
        )
      }

      flexOptionResponseInferior(emNode3Uuid) shouldBe new ExtendedFlexOptionsResult(
        simulationStart,
        emNode3Uuid,
        emSupUuid,
        0.0.asMegaWatt,
        0.0.asMegaWatt,
        0.004.asMegaWatt
      )

      flexOptionResponseInferior(emNode4Uuid) shouldBe new ExtendedFlexOptionsResult(
        simulationStart,
        emNode4Uuid,
        emSupUuid,
        0.002200000413468004.asMegaWatt,
        0.002200000413468004.asMegaWatt,
        0.002200000413468004.asMegaWatt
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

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(0)

      // we expect the total flex options of the grid from the superior em agent
      val totalFlexOptions = connection.receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      totalFlexOptions shouldBe Map(
        emSupUuid -> new ExtendedFlexOptionsResult(
          simulationStart,
          emSupUuid,
          emSupUuid,
          0.002200000413468004.asMegaWatt,
          0.002200000413468004.asMegaWatt,
          0.0062000004134680035.asMegaWatt
        )
      )

      // after we received all options we will send a message, to keep the current set point
      connection.sendSetPoints(
        0,
        Map(emSupUuid -> new PValue(2.200000413468004.asKiloWatt)).asJava,
        Optional.of(900),
        log
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(0)

      // we expect a new set point for each inferior em agent
      val inferiorSetPoints = connection.receiveWithType(classOf[EmSetPointDataResponse])
        .emData().asScala
        .flatMap(_._2.getReceiverToSetPoint.asScala)

      if (inferiorSetPoints.size == 1) {
        inferiorSetPoints.addAll(
          connection.receiveWithType(classOf[EmSetPointDataResponse])
            .emData().asScala
            .flatMap(_._2.getReceiverToSetPoint.asScala)
        )
      }

      inferiorSetPoints(emNode3Uuid).getP.toScala.value should equalWithTolerance(0.asKiloWatt)
      inferiorSetPoints(emNode4Uuid).getP.toScala.value should equalWithTolerance(2.200000413468004.asKiloWatt)

      // we send the new set points to the inferior em agents
      connection.sendSetPoints(
        0,
        Map(
          emNode3Uuid -> new PValue(0.0.asMegaWatt),
          emNode4Uuid -> new PValue(0.002200000413468004.asMegaWatt),
        ).asJava,
        Optional.of(900),
        log
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(0)

      // we expect new flex control message for the participant agents
      pvAgentNode3.expectMessage(IssueNoControl(0))
      emAgentNode3 ! FlexCompletion(pvNode3.getUuid)

      storageAgentNode3.expectMessage(IssueNoControl(0))
      emAgentNode3 ! FlexCompletion(storageInput.getUuid)


      pvAgentNode4.expectMessage(IssueNoControl(0))
      emAgentNode4 ! FlexCompletion(pvNode4.getUuid)

      loadAgentNode4.expectMessage(IssueNoControl(0))
      emAgentNode4 ! FlexCompletion(loadInput.getUuid)


      // we expect a finish message
      connection.receiveWithType(classOf[EmCompletion])


    }

    "with participant agents work correctly" in {
      val gridAgent = TestProbe[GridAgent.Request]("GridAgent")
      val resultListener = TestProbe[ResultEvent]("ResultListener")
      val primaryServiceProxy =
        TestProbe[ServiceMessage]("PrimaryServiceProxy")
      val weatherService = TestProbe[ServiceMessage]("WeatherService")

      val participantRefs = ParticipantRefs(
        gridAgent = gridAgent.ref,
        primaryServiceProxy = primaryServiceProxy.ref,
        services = Map(ServiceType.WeatherService -> weatherService.ref),
        resultListener = Iterable(resultListener.ref),
      )

      val simulationParams = SimulationParameters(
        expectedPowerRequestTick = Long.MaxValue,
        requestVoltageDeviationTolerance = Each(1e-14d),
        simulationStart = simulationStart,
        simulationEnd = simulationEnd,
      )

      val keys = ScheduleLock
        .multiKey(TSpawner, scheduler.ref, PRE_INIT_TICK, 4)
        .iterator
      val lockActivation =
        scheduler.expectMessageType[ScheduleActivation].actor
      lockActivation ! Activation(PRE_INIT_TICK)

      val pvAgentNode3 = spawn(
        ParticipantAgentInit(
          SimpleInputContainer(pvNode3),
          PvRuntimeConfig(),
          outputConfig,
          participantRefs,
          simulationParams,
          Right(emAgentNode3),
          keys.next()
        ),
        "PvAgentNode3"
      )

      val storageAgentNode3 = spawn(
        ParticipantAgentInit(
          SimpleInputContainer(storageInput),
          StorageRuntimeConfig(),
          outputConfig,
          participantRefs,
          simulationParams,
          Right(emAgentNode3),
          keys.next()
        ),
        "storageAgentNode3"
      )

      val pvAgentNode4 = spawn(
        ParticipantAgentInit(
          SimpleInputContainer(pvNode4),
          PvRuntimeConfig(),
          outputConfig,
          participantRefs,
          simulationParams,
          Right(emAgentNode4),
          keys.next()
        ),
        "PvAgentNode4"
      )

      val loadAgentNode4 = spawn(
        ParticipantAgentInit(
          SimpleInputContainer(loadInput),
          LoadRuntimeConfig(),
          outputConfig,
          participantRefs,
          simulationParams,
          Right(emAgentNode4),
          keys.next()
        ),
        "LoadAgentNode4"
      )

      /* PRE_INIT */

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, PRE_INIT_TICK)
      scheduler.expectMessageType[ScheduleActivation] // lock activation scheduled

      service ! Create(
        InitExtEmData(connection, simulationStart),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)
      val serviceActivation = activationMsg.actor

      // we expect a completion for the participant locks
      scheduler.expectMessage(Completion(lockActivation))


      /* INIT */

      // activate the service for init tick
      serviceActivation ! Activation(INIT_SIM_TICK)


      // TODO: Init participants



      scheduler.expectMessage(Completion(serviceActivation))

      /* TICK: 0 */

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
        .flexRequests()
        .asScala

      requestsToInferior shouldBe Map(emSupUuid -> new FlexRequestResult(simulationStart, emSupUuid, List(emNode3Uuid, emNode4Uuid).asJava))

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

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(0)

      // we expect to receive flex options from the inferior em agents
      val flexOptionResponseInferior = connection.receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      if (flexOptionResponseInferior.size == 1) {
        flexOptionResponseInferior.addAll(
          connection.receiveWithType(classOf[FlexOptionsResponse])
            .receiverToFlexOptions()
            .asScala
        )
      }

      flexOptionResponseInferior(emNode3Uuid) shouldBe new ExtendedFlexOptionsResult(
        simulationStart,
        emNode3Uuid,
        emSupUuid,
        0.0.asMegaWatt,
        0.0.asMegaWatt,
        0.004.asMegaWatt
      )

      flexOptionResponseInferior(emNode4Uuid) shouldBe new ExtendedFlexOptionsResult(
        simulationStart,
        emNode4Uuid,
        emSupUuid,
        0.002200000413468004.asMegaWatt,
        0.002200000413468004.asMegaWatt,
        0.002200000413468004.asMegaWatt
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

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(0)

      // we expect the total flex options of the grid from the superior em agent
      val totalFlexOptions = connection.receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      totalFlexOptions shouldBe Map(
        emSupUuid -> new ExtendedFlexOptionsResult(
          simulationStart,
          emSupUuid,
          emSupUuid,
          0.002200000413468004.asMegaWatt,
          0.002200000413468004.asMegaWatt,
          0.0062000004134680035.asMegaWatt
        )
      )

      // after we received all options we will send a message, to keep the current set point
      connection.sendSetPoints(
        0,
        Map(emSupUuid -> new PValue(2.200000413468004.asKiloWatt)).asJava,
        Optional.of(900),
        log
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(0)

      // we expect a new set point for each inferior em agent
      val inferiorSetPoints = connection.receiveWithType(classOf[EmSetPointDataResponse])
        .emData().asScala
        .flatMap(_._2.getReceiverToSetPoint.asScala)

      if (inferiorSetPoints.size == 1) {
        inferiorSetPoints.addAll(
          connection.receiveWithType(classOf[EmSetPointDataResponse])
            .emData().asScala
            .flatMap(_._2.getReceiverToSetPoint.asScala)
        )
      }

      inferiorSetPoints(emNode3Uuid).getP.toScala.value should equalWithTolerance(0.asKiloWatt)
      inferiorSetPoints(emNode4Uuid).getP.toScala.value should equalWithTolerance(2.200000413468004.asKiloWatt)

      // we send the new set points to the inferior em agents
      connection.sendSetPoints(
        0,
        Map(
          emNode3Uuid -> new PValue(0.0.asMegaWatt),
          emNode4Uuid -> new PValue(0.002200000413468004.asMegaWatt),
        ).asJava,
        Optional.of(900),
        log
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(0)

      // we expect a finish message
      connection.receiveWithType(classOf[EmCompletion])


    }

  }
}
