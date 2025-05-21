/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.agent.participant.ParticipantAgentInit.ParticipantRefs
import edu.ie3.simona.agent.participant.{ParticipantAgent, ParticipantAgentInit}
import edu.ie3.simona.api.data.em.model.{
  EmSetPoint,
  FlexOptionRequest,
  FlexOptions,
  FlexRequestResult,
}
import edu.ie3.simona.api.data.em.ontology.{
  EmCompletion,
  EmSetPointDataResponse,
  FlexOptionsResponse,
  FlexRequestResponse,
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
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}
import squants.motion.MetersPerSecond
import squants.thermal.Celsius
import tech.units.indriya.ComparableQuantity

import java.util.{Optional, UUID}
import javax.measure.quantity.Power
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.jdk.CollectionConverters.{
  MapHasAsJava,
  MapHasAsScala,
  SeqHasAsJava,
}
import scala.jdk.OptionConverters.RichOptional

class ExtEmCommunicationIT
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

  private val connection = new ExtEmDataConnection(
    List(emSupUuid, emNode3Uuid, emNode4Uuid).asJava,
    EmMode.EM_COMMUNICATION,
  )

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")
  private val extSimAdapter =
    TestProbe[ControlResponseMessageFromExt]("extSimAdapter")
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

  "An ExtEmDataService in communication mode" should {
    val service = spawn(ExtEmDataService(scheduler.ref))
    val serviceRef = service.ref
    given adapter: ActorRef[DataMessageFromExt] =
      spawn(ExtEmDataService.adapter(service))
    connection.setActorRefs(adapter, extSimAdapter.ref)

    "with participant agents work correctly" in {
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
        ),
        "PvAgentNode3",
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
        ),
        "storageAgentNode3",
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
        ),
        "PvAgentNode4",
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
        ),
        "LoadAgentNode4",
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
      given serviceActivation: ActorRef[Activation] = activationMsg.actor

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

      given pvAgents: Seq[ActorRef[ParticipantAgent.Request]] =
        Seq(pvAgentNode3, pvAgentNode4)

      /* TICK: 0 */

      val weatherData0 = WeatherData(
        WattsPerSquareMeter(0),
        WattsPerSquareMeter(0),
        Celsius(0d),
        MetersPerSecond(0d),
      )

      communicate(
        0,
        900,
        weatherData0,
        Map(
          emSupUuid -> new FlexOptions(
            emSupUuid,
            emSupUuid,
            0.002200000413468004.asMegaWatt,
            0.002200000413468004.asMegaWatt,
            0.006200000413468004.asMegaWatt,
            Optional.empty,
          ),
          emNode3Uuid -> new FlexOptions(
            emSupUuid,
            emNode3Uuid,
            0.asMegaWatt,
            0.asMegaWatt,
            0.004.asMegaWatt,
            Optional.empty,
          ),
          emNode4Uuid -> new FlexOptions(
            emSupUuid,
            emNode4Uuid,
            0.002200000413468004.asMegaWatt,
            0.002200000413468004.asMegaWatt,
            0.002200000413468004.asMegaWatt,
            Optional.empty,
          ),
        ),
        Map(
          emSupUuid -> 2.200000413468004.asKiloWatt,
          emNode3Uuid -> 0.asKiloWatt,
          emNode4Uuid -> 2.200000413468004.asKiloWatt,
        ),
      )

      /* TICK: 900 */

      val weatherData900 = WeatherData(
        WattsPerSquareMeter(0),
        WattsPerSquareMeter(0),
        Celsius(0d),
        MetersPerSecond(0d),
      )

      communicate(
        900,
        1800,
        weatherData900,
        Map(
          emSupUuid -> new FlexOptions(
            emSupUuid,
            emSupUuid,
            0.002200000413468004.asMegaWatt,
            0.002200000413468004.asMegaWatt,
            0.006200000413468004.asMegaWatt,
            Optional.empty,
          ),
          emNode3Uuid -> new FlexOptions(
            emSupUuid,
            emNode3Uuid,
            0.asMegaWatt,
            0.asMegaWatt,
            0.004.asMegaWatt,
            Optional.empty,
          ),
          emNode4Uuid -> new FlexOptions(
            emSupUuid,
            emNode4Uuid,
            0.002200000413468004.asMegaWatt,
            0.002200000413468004.asMegaWatt,
            0.002200000413468004.asMegaWatt,
            Optional.empty,
          ),
        ),
        Map(
          emSupUuid -> 2.200000413468004.asKiloWatt,
          emNode3Uuid -> 0.asKiloWatt,
          emNode4Uuid -> 2.200000413468004.asKiloWatt,
        ),
      )

    }

    // helper methods

    def communicate(
        tick: Long,
        nextTick: Long,
        weatherData: WeatherData,
        flexOptions: Map[UUID, FlexOptions],
        setPoints: Map[UUID, ComparableQuantity[Power]],
    )(using
        serviceActivation: ActorRef[Activation],
        adapter: ActorRef[DataMessageFromExt],
        pvAgents: Seq[ActorRef[ParticipantAgent.Request]],
    ): Unit = {

      /* start communication */
      val inferiorEms = Set(emNode3Uuid, emNode4Uuid)

      // we first send a flex option request to the superior em agent
      connection.sendFlexRequests(
        tick,
        Map(
          emSupUuid -> new FlexOptionRequest(
            emSupUuid,
            Optional.empty,
            Optional.empty,
          )
        ).asJava,
        Optional.of(nextTick),
        log,
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(tick)

      // we expect to receive a request per inferior em agent
      val requestsToInferior = connection
        .receiveWithType(classOf[FlexRequestResponse])
        .flexRequests()
        .asScala

      requestsToInferior.size shouldBe 1
      requestsToInferior(emSupUuid) shouldBe new FlexRequestResult(
        simulationStart.plusSeconds(tick),
        emSupUuid,
        List(emNode3Uuid, emNode4Uuid).asJava,
      )

      // we send a request to each inferior em agent
      connection.sendFlexRequests(
        tick,
        Map(
          emNode3Uuid -> new FlexOptionRequest(
            emNode3Uuid,
            Optional.of(emSupUuid),
            Optional.empty,
          ),
          emNode4Uuid -> new FlexOptionRequest(
            emNode4Uuid,
            Optional.of(emSupUuid),
            Optional.empty,
          ),
        ).asJava,
        Optional.of(nextTick),
        log,
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(tick)

      val data = DataProvision(
        tick,
        weatherService.ref,
        weatherData,
        Some(nextTick),
      )
      pvAgents.foreach(_ ! data)

      // we expect to receive flex options from the inferior em agents
      val flexOptionResponseInferior = connection
        .receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      if (flexOptionResponseInferior.size != 2) {
        flexOptionResponseInferior.addAll(
          connection
            .receiveWithType(classOf[FlexOptionsResponse])
            .receiverToFlexOptions()
            .asScala
        )
      }

      flexOptionResponseInferior.keySet shouldBe inferiorEms

      flexOptionResponseInferior.foreach { case (receiver, results) =>
        val expectedOptions = flexOptions(receiver)

        results.getReceiver shouldBe expectedOptions.receiver
        results.getSender shouldBe expectedOptions.sender
        results.getpMin() should equalWithTolerance(expectedOptions.pMin)
        results.getpRef() should equalWithTolerance(expectedOptions.pRef)
        results.getpMax() should equalWithTolerance(expectedOptions.pMax)
      }

      // we send the flex options to the superior em agent
      connection.sendFlexOptions(
        tick,
        Map(emSupUuid -> inferiorEms.map(flexOptions).toList.asJava).asJava,
        Optional.of(nextTick),
        log,
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(tick)

      // we expect the total flex options of the grid from the superior em agent
      val totalFlexOptions = connection
        .receiveWithType(classOf[FlexOptionsResponse])
        .receiverToFlexOptions()
        .asScala

      totalFlexOptions.keySet shouldBe Set(emSupUuid)

      totalFlexOptions.foreach { case (receiver, result) =>
        val expectedOptions = flexOptions(receiver)

        result.getReceiver shouldBe expectedOptions.receiver
        result.getSender shouldBe expectedOptions.sender
        result.getpMin() should equalWithTolerance(expectedOptions.pMin)
        result.getpRef() should equalWithTolerance(expectedOptions.pRef)
        result.getpMax() should equalWithTolerance(expectedOptions.pMax)
      }

      // after we received all options we will send a message, to keep the current set point
      connection.sendSetPoints(
        tick,
        Map(
          emSupUuid -> new EmSetPoint(emSupUuid, setPoints(emSupUuid))
        ).asJava,
        Optional.of(nextTick),
        log,
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(tick)

      // we expect a new set point for each inferior em agent
      val inferiorSetPoints = connection
        .receiveWithType(classOf[EmSetPointDataResponse])
        .emData()
        .asScala
        .flatMap(_._2.getReceiverToSetPoint.asScala)

      if (inferiorSetPoints.size != 2) {
        inferiorSetPoints.addAll(
          connection
            .receiveWithType(classOf[EmSetPointDataResponse])
            .emData()
            .asScala
            .flatMap(_._2.getReceiverToSetPoint.asScala)
        )
      }

      inferiorSetPoints.keySet shouldBe inferiorEms

      inferiorSetPoints.foreach { case (receiver, results) =>
        results.getP.toScala.value should equalWithTolerance(
          setPoints(receiver)
        )
      }

      def toSetPoint(uuid: UUID): (UUID, EmSetPoint) =
        uuid -> new EmSetPoint(uuid, setPoints(uuid))

      // we send the new set points to the inferior em agents
      connection.sendSetPoints(
        tick,
        inferiorEms.map(toSetPoint).toMap.asJava,
        Optional.of(nextTick),
        log,
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(adapter))
      serviceActivation ! Activation(tick)

      // we expect a finish message
      connection.receiveWithType(classOf[EmCompletion])
    }

  }

}
