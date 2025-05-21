/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.agent.participant.ParticipantAgentInit
import edu.ie3.simona.agent.participant.ParticipantAgentInit.ParticipantRefs
import edu.ie3.simona.api.data.em.model.EmSetPoint
import edu.ie3.simona.api.data.em.ontology.{
  EmCompletion,
  FlexOptionsResponse,
  RequestEmFlexResults,
}
import edu.ie3.simona.api.data.em.{EmMode, ExtEmDataConnection}
import edu.ie3.simona.api.data.ontology.{
  DataMessageFromExt,
  ScheduleDataServiceMessage,
}
import edu.ie3.simona.api.simulation.ontology.ControlResponseMessageFromExt
import edu.ie3.simona.config.RuntimeConfig.{
  LoadRuntimeConfig,
  PvRuntimeConfig,
  StorageRuntimeConfig,
}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.model.InputModelContainer.SimpleInputContainer
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  Create,
  PrimaryServiceRegistrationMessage,
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  RegisterForWeatherMessage,
  WeatherData,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.service.em.ExtEmDataService.InitExtEmData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.EmCommunicationTestData
import edu.ie3.simona.test.matchers.QuantityMatchers
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.util.quantities.QuantityUtils.*
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.util.{Optional, UUID}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.jdk.CollectionConverters.{
  MapHasAsJava,
  MapHasAsScala,
  SeqHasAsJava,
}

class ExtEmBaseIT
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with EmCommunicationTestData
    with QuantityMatchers
    with TestSpawnerTyped {

  protected val messageTimeout: FiniteDuration = 30.seconds

  protected val log: Logger = LoggerFactory.getLogger("ExtEmCommunicationIT")

  private val emSupUuid =
    UUID.fromString("858f3d3d-4189-49cd-9fe5-3cd49b88dc70")
  private val emNode3Uuid =
    UUID.fromString("fd1a8de9-722a-4304-8799-e1e976d9979c")
  private val emNode4Uuid =
    UUID.fromString("ff0b995a-86ff-4f4d-987e-e475a64f2180")

  private val gridAgent = TestProbe[GridAgent.Request]("GridAgent")
  private val resultListener = TestProbe[ResultEvent]("ResultListener")
  private val primaryServiceProxy =
    TestProbe[ServiceMessage]("PrimaryServiceProxy")
  private val weatherService = TestProbe[ServiceMessage]("WeatherService")

  private val participantRefs = ParticipantRefs(
    gridAgent = gridAgent.ref,
    primaryServiceProxy = primaryServiceProxy.ref,
    services = Map(ServiceType.WeatherService -> weatherService.ref),
    resultListener = Iterable(resultListener.ref),
  )

  "An ExtEmDataService in base mode" should {
    val scheduler = TestProbe[SchedulerMessage]("scheduler")
    val extSimAdapter =
      TestProbe[ControlResponseMessageFromExt]("extSimAdapter")

    val service = spawn(ExtEmDataService(scheduler.ref))
    val serviceRef = service.ref
    implicit val adapter: ActorRef[DataMessageFromExt] =
      spawn(ExtEmDataService.adapter(service))

    val connection = new ExtEmDataConnection(
      List(emSupUuid, emNode3Uuid, emNode4Uuid).asJava,
      EmMode.BASE,
    )
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
        keys.next(),
      )
    )

    val storageAgentNode3 = spawn(
      ParticipantAgentInit(
        SimpleInputContainer(storageInput),
        StorageRuntimeConfig(),
        outputConfig,
        participantRefs,
        simulationParams,
        Right(emAgentNode3),
        keys.next(),
      )
    )

    val pvAgentNode4 = spawn(
      ParticipantAgentInit(
        SimpleInputContainer(pvNode4),
        PvRuntimeConfig(),
        outputConfig,
        participantRefs,
        simulationParams,
        Right(emAgentNode4),
        keys.next(),
      )
    )

    val loadAgentNode4 = spawn(
      ParticipantAgentInit(
        SimpleInputContainer(loadInput),
        LoadRuntimeConfig(),
        outputConfig,
        participantRefs,
        simulationParams,
        Right(emAgentNode4),
        keys.next(),
      )
    )

    /* PRE_INIT */
    val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, PRE_INIT_TICK)
    scheduler
      .expectMessageType[ScheduleActivation] // lock activation scheduled

    service ! Create(
      InitExtEmData(connection, simulationStart),
      key,
    )

    val activationMsg = scheduler.expectMessageType[ScheduleActivation]
    activationMsg.tick shouldBe INIT_SIM_TICK
    activationMsg.unlockKey shouldBe Some(key)
    implicit val serviceActivation: ActorRef[Activation] = activationMsg.actor

    // we expect a completion for the participant locks
    scheduler.expectMessage(Completion(lockActivation))

    /* INIT */

    // activate the service for init tick
    serviceActivation ! Activation(INIT_SIM_TICK)
    scheduler.expectMessage(Completion(serviceActivation))

    primaryServiceProxy.receiveMessages(
      4,
      messageTimeout,
    ) should contain allOf (
      PrimaryServiceRegistrationMessage(
        pvAgentNode3,
        pvNode3.getUuid,
      ),
      PrimaryServiceRegistrationMessage(
        storageAgentNode3,
        storageInput.getUuid,
      ),
      PrimaryServiceRegistrationMessage(
        pvAgentNode4,
        pvNode4.getUuid,
      ),
      PrimaryServiceRegistrationMessage(
        loadAgentNode4,
        loadInput.getUuid,
      )
    )

    // pv agent 3
    pvAgentNode3 ! RegistrationFailedMessage(primaryServiceProxy.ref)

    // deal with weather service registration
    weatherService.expectMessage(
      RegisterForWeatherMessage(
        pvAgentNode3,
        pvNode3.getNode.getGeoPosition.getY,
        pvNode3.getNode.getGeoPosition.getX,
      )
    )

    pvAgentNode3 ! RegistrationSuccessfulMessage(weatherService.ref, 0L)

    // pv agent 4
    pvAgentNode4 ! RegistrationFailedMessage(primaryServiceProxy.ref)

    weatherService.expectMessage(
      RegisterForWeatherMessage(
        pvAgentNode4,
        pvNode4.getNode.getGeoPosition.getY,
        pvNode4.getNode.getGeoPosition.getX,
      )
    )
    pvAgentNode4 ! RegistrationSuccessfulMessage(weatherService.ref, 0L)

    // storage
    storageAgentNode3 ! RegistrationFailedMessage(primaryServiceProxy.ref)

    // load
    loadAgentNode4 ! RegistrationFailedMessage(primaryServiceProxy.ref)

    "work correctly when requesting flex options" in {
      /* TICK: 0 */

      val weatherData0 = DataProvision(
        0L,
        weatherService.ref,
        WeatherData(
          WattsPerSquareMeter(0),
          WattsPerSquareMeter(0),
          Celsius(0d),
          MetersPerSecond(0d),
        ),
        Some(900L),
      )

      pvAgentNode3 ! weatherData0
      pvAgentNode4 ! weatherData0

      // we request the em option for the superior em agent
      connection.sendExtMsg(
        new RequestEmFlexResults(0L, List(emSupUuid).asJava, false)
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(0L)

      val receivedFlexOptions0 = connection
        .receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      receivedFlexOptions0.size shouldBe 1
      val flexOptions0 = receivedFlexOptions0(emSupUuid)
      flexOptions0.getSender shouldBe emSupUuid
      flexOptions0.getReceiver shouldBe emSupUuid
      flexOptions0.getpMin should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      flexOptions0.getpRef should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      flexOptions0.getpMax should equalWithTolerance(
        0.006200000413468004.asMegaWatt
      )

      // we return a new set point
      val setPoints0 = Map(emSupUuid -> new EmSetPoint(emSupUuid))

      connection.sendSetPoints(0L, setPoints0.asJava, Optional.of(900L), log)

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(0L)

      connection.receiveWithType(classOf[EmCompletion])

      /* TICK: 900 */

      val weatherData900 = DataProvision(
        900L,
        weatherService.ref,
        WeatherData(
          WattsPerSquareMeter(0),
          WattsPerSquareMeter(0),
          Celsius(0d),
          MetersPerSecond(0d),
        ),
        Some(1800L),
      )

      pvAgentNode3 ! weatherData900
      pvAgentNode4 ! weatherData900

      // we request the em option for the superior em agent
      connection.sendExtMsg(
        new RequestEmFlexResults(900L, List(emSupUuid).asJava, false)
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(900L)

      val receivedFlexOptions900 = connection
        .receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      receivedFlexOptions900.size shouldBe 1
      val flexOptions900 = receivedFlexOptions900(emSupUuid)
      flexOptions900.getSender shouldBe emSupUuid
      flexOptions900.getReceiver shouldBe emSupUuid
      flexOptions900.getpMin should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      flexOptions900.getpRef should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      flexOptions900.getpMax should equalWithTolerance(
        0.006200000413468004.asMegaWatt
      )

      // we return a new set point
      val setPoints900 = Map(
        emSupUuid -> new EmSetPoint(emSupUuid, 0.006200000413468004.asMegaWatt)
      )

      connection.sendSetPoints(
        900L,
        setPoints900.asJava,
        Optional.of(1800L),
        log,
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(900L)

      connection.receiveWithType(classOf[EmCompletion])
    }

    "work correctly when requesting disaggregated flex options" in {
      /* TICK: 1800 */

      val weatherData1800 = DataProvision(
        1800L,
        weatherService.ref,
        WeatherData(
          WattsPerSquareMeter(0),
          WattsPerSquareMeter(0),
          Celsius(0d),
          MetersPerSecond(0d),
        ),
        Some(2700L),
      )

      pvAgentNode3 ! weatherData1800
      pvAgentNode4 ! weatherData1800

      // we request the em option for the superior em agent
      connection.sendExtMsg(
        new RequestEmFlexResults(1800L, List(emSupUuid).asJava, true)
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(1800L)

      val receivedFlexOptions1800 = connection
        .receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      receivedFlexOptions1800.size shouldBe 1
      val flexOptions1800 = receivedFlexOptions1800(emSupUuid)
      flexOptions1800.getSender shouldBe emSupUuid
      flexOptions1800.getReceiver shouldBe emSupUuid
      flexOptions1800.getpMin should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      flexOptions1800.getpRef should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      flexOptions1800.getpMax should equalWithTolerance(
        0.006200000413468004.asMegaWatt
      )

      flexOptions1800.hasDisaggregated shouldBe true
      val disaggregated1800 = flexOptions1800.getDisaggregated.asScala

      val node3Flex1800 = disaggregated1800(emNode3Uuid)
      node3Flex1800.getpMin should equalWithTolerance(
        0.asMegaWatt
      )
      node3Flex1800.getpRef should equalWithTolerance(
        0.asMegaWatt
      )
      node3Flex1800.getpMax should equalWithTolerance(
        0.004.asMegaWatt
      )

      val node4Flex1800 = disaggregated1800(emNode4Uuid)
      node4Flex1800.getpMin should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      node4Flex1800.getpRef should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      node4Flex1800.getpMax should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )

      // we return a new set point
      val setPoints1800 = Map(emSupUuid -> new EmSetPoint(emSupUuid))

      connection.sendSetPoints(
        1800L,
        setPoints1800.asJava,
        Optional.of(2700L),
        log,
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(1800L)

      connection.receiveWithType(classOf[EmCompletion])

      /* TICK: 2700 */

      val weatherData2700 = DataProvision(
        2700L,
        weatherService.ref,
        WeatherData(
          WattsPerSquareMeter(0),
          WattsPerSquareMeter(0),
          Celsius(0d),
          MetersPerSecond(0d),
        ),
        Some(3600L),
      )

      pvAgentNode3 ! weatherData2700
      pvAgentNode4 ! weatherData2700

      // we request the em option for the superior em agent
      connection.sendExtMsg(
        new RequestEmFlexResults(2700L, List(emSupUuid).asJava, true)
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(2700L)

      val receivedFlexOptions2700 = connection
        .receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      receivedFlexOptions2700.size shouldBe 1
      val flexOptions2700 = receivedFlexOptions2700(emSupUuid)
      flexOptions2700.getSender shouldBe emSupUuid
      flexOptions2700.getReceiver shouldBe emSupUuid
      flexOptions2700.getpMin should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      flexOptions2700.getpRef should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      flexOptions2700.getpMax should equalWithTolerance(
        0.006200000413468004.asMegaWatt
      )

      flexOptions2700.hasDisaggregated shouldBe true
      val disaggregated2700 = flexOptions2700.getDisaggregated.asScala

      val node3Flex2700 = disaggregated2700(emNode3Uuid)
      node3Flex2700.getpMin should equalWithTolerance(
        0.asMegaWatt
      )
      node3Flex2700.getpRef should equalWithTolerance(
        0.asMegaWatt
      )
      node3Flex2700.getpMax should equalWithTolerance(
        0.004.asMegaWatt
      )

      val node4Flex2700 = disaggregated2700(emNode4Uuid)
      node4Flex2700.getpMin should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      node4Flex2700.getpRef should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )
      node4Flex2700.getpMax should equalWithTolerance(
        0.002200000413468004.asMegaWatt
      )

      // we return a new set point
      val setPoints2700 = Map(
        emSupUuid -> new EmSetPoint(emSupUuid, 0.006200000413468004.asMegaWatt)
      )

      connection.sendSetPoints(
        2700L,
        setPoints2700.asJava,
        Optional.of(3600L),
        log,
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(2700L)

      connection.receiveWithType(classOf[EmCompletion])
    }

    "work correctly without requesting flex options" in {
      /* TICK: 3600 */

      val weatherData3600 = DataProvision(
        3600L,
        weatherService.ref,
        WeatherData(
          WattsPerSquareMeter(0),
          WattsPerSquareMeter(0),
          Celsius(0d),
          MetersPerSecond(0d),
        ),
        Some(4500L),
      )

      pvAgentNode3 ! weatherData3600
      pvAgentNode4 ! weatherData3600

      // we send a new set point
      val setPoints3600 = Map(
        emSupUuid -> new EmSetPoint(emSupUuid, 0.002200000413468004.asMegaWatt)
      )

      connection.sendSetPoints(
        3600L,
        setPoints3600.asJava,
        Optional.of(4500L),
        log,
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(3600L)

      connection.receiveWithType(classOf[EmCompletion])

      /* TICK: 4500 */

      val weatherData4500 = DataProvision(
        4500L,
        weatherService.ref,
        WeatherData(
          WattsPerSquareMeter(0),
          WattsPerSquareMeter(0),
          Celsius(0d),
          MetersPerSecond(0d),
        ),
        Some(5400L),
      )

      pvAgentNode3 ! weatherData4500
      pvAgentNode4 ! weatherData4500

      // we send a new set point
      val setPoints4500 = Map(
        emSupUuid -> new EmSetPoint(emSupUuid, 0.006200000413468004.asMegaWatt)
      )

      connection.sendSetPoints(
        4500L,
        setPoints4500.asJava,
        Optional.of(5400L),
        log,
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(4500L)

      connection.receiveWithType(classOf[EmCompletion])

    }
  }
}
