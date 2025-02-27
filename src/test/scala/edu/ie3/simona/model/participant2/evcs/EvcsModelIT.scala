/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import edu.ie3.datamodel.models.result.system.{EvResult, EvcsResult}
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.RegistrationFailedMessage
import edu.ie3.simona.agent.participant2.ParticipantAgentInit
import edu.ie3.simona.agent.participant2.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology.{
  ProvideEvcsFreeLots,
  RequestEvcsFreeLots,
}
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.config.RuntimeConfig.EvcsRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.service.SimonaService.Create
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.common.{TestSpawnerTyped, UnitSpec}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.testkit.TestActorRef
import squants.Each

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

class EvcsModelIT
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with EvcsInputTestData
    with TestSpawnerTyped {

  private implicit val classicSystem: ActorSystem = system.toClassic

  private implicit val simulationStartDate: ZonedDateTime =
    defaultSimulationStart

  private val simulationParams = SimulationParameters(
    24 * 3600,
    Each(1e-14),
    defaultSimulationStart,
    defaultSimulationStart.plus(2, ChronoUnit.DAYS),
  )

  private val notifierConfig = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false,
    flexResult = true,
  )

  "An EVCS model with ExtEvDataService" should {

    "handle a few requests and arrivals as expected" in {

      val gridAgent = TestProbe[GridAgent.Request]("GridAgent")
      val resultListener = TestProbe[ResultEvent]("ResultListener")
      val primaryServiceProxy =
        TestProbe[ServiceMessage]("PrimaryServiceProxy")
      val scheduler = TestProbe[SchedulerMessage]("Scheduler")
      val extSimAdapter = TestProbe[Any]("ExtSimAdapter")

      /* Create ExtEvDataService */
      val evService = TestActorRef(
        ExtEvDataService.props(scheduler.ref.toClassic),
        "ExtEvDataService",
      )

      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(evService, extSimAdapter.ref.toClassic)
      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      evService ! Create(
        InitExtEvData(extEvData),
        key,
      )
      scheduler.expectMessage(
        ScheduleActivation(evService, INIT_SIM_TICK, Some(key))
      )

      /* Create ParticipantAgent with EvcsModel */
      val participantRefs = ParticipantRefs(
        gridAgent = gridAgent.ref,
        primaryServiceProxy = primaryServiceProxy.ref.toClassic,
        services = Map(ServiceType.EvMovementService -> evService.ref),
        resultListener = Iterable(resultListener.ref),
      )

      val evcsAgent = spawn(
        ParticipantAgentInit(
          evcsInputModel,
          EvcsRuntimeConfig(),
          notifierConfig,
          participantRefs,
          simulationParams,
          Left(scheduler.ref),
        )
      )

      val scheduleMsg = scheduler.expectMessageType[ScheduleActivation]
      scheduleMsg.tick shouldBe INIT_SIM_TICK
      val evcsActivation = scheduleMsg.actor

      /* INIT */

      evService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(evService.toTyped))

      evcsActivation ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(
          evcsAgent.toClassic,
          evcsInputModel.getUuid,
        )
      )
      evcsAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

      // providing the first data tick
      extEvData.provideArrivingEvs(
        Map.empty[UUID, java.util.List[EvModel]].asJava,
        Some(long2Long(0L)).toJava,
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      evService ! Activation(INIT_SIM_TICK)

      scheduler.receiveMessages(2) should contain allOf (
        Completion(evcsActivation, Some(0)),
        Completion(evService, None)
      )

      /* TICK 0 */

      // Request free lots
      extEvData.sendExtMsg(
        new RequestEvcsFreeLots()
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(0)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        Map(evcsInputModel.getUuid -> int2Integer(2)).asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      // Send arrivals
      val arrivals = Map(
        evcsInputModel.getUuid -> List[EvModel](ev1, ev2).asJava
      ).asJava

      extEvData.provideArrivingEvs(
        arrivals,
        Some(long2Long(9000)).toJava,
      )
      evService ! Activation(0)

      scheduler.expectMessage(Completion(evService, None))

      evcsActivation ! Activation(0)

      resultListener
        .receiveMessages(3)
        .map { case ParticipantResultEvent(result) =>
          result
        }
        .foreach {
          case evResult: EvResult if evResult.getInputModel == ev1.getUuid =>
            evResult.getTime shouldBe 0.toDateTime
            evResult.getP should beEquivalentTo(5.0.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(50.0.asPercent)
          case evResult: EvResult if evResult.getInputModel == ev2.getUuid =>
            evResult.getTime shouldBe 0.toDateTime
            evResult.getP should beEquivalentTo(5.0.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(75.0.asPercent)
          case evcsResult: EvcsResult
              if evcsResult.getInputModel == evcsInputModel.getUuid =>
            evcsResult.getTime shouldBe 0.toDateTime
            evcsResult.getP should beEquivalentTo(10.0.asKiloWatt)
            evcsResult.getQ should beEquivalentTo(0.0.asKiloVar)
          case unexpected =>
            fail(s"Unexpected result $unexpected was found.")
        }

      // ev2 is full at 1800
      scheduler.expectMessage(Completion(evcsActivation, Some(1800)))

      /* TICK 1800 */

      evcsActivation ! Activation(1800)

      resultListener
        .receiveMessages(2)
        .map { case ParticipantResultEvent(result) =>
          result
        }
        .foreach {
          case evResult: EvResult if evResult.getInputModel == ev2.getUuid =>
            evResult.getTime shouldBe 1800.toDateTime
            evResult.getP should beEquivalentTo(0.0.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(100.0.asPercent)
          case evcsResult: EvcsResult
              if evcsResult.getInputModel == evcsInputModel.getUuid =>
            evcsResult.getTime shouldBe 1800.toDateTime
            evcsResult.getP should beEquivalentTo(5.0.asKiloWatt)
            evcsResult.getQ should beEquivalentTo(0.0.asKiloVar)
          case unexpected =>
            fail(s"Unexpected result $unexpected was found.")
        }

      // ev1 is full at 3600
      scheduler.expectMessage(Completion(evcsActivation, Some(3600)))

      /* TICK 3600 */

      evcsActivation ! Activation(3600)

      // TODO Save non-charging EVs with 0 kW in operating point
      resultListener
        .receiveMessages(2)
        .map { case ParticipantResultEvent(result) =>
          result
        }
        .foreach {
          case evResult: EvResult if evResult.getInputModel == ev1.getUuid =>
            evResult.getTime shouldBe 3600.toDateTime
            evResult.getP should beEquivalentTo(0.0.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(100.0.asPercent)
          case evcsResult: EvcsResult
              if evcsResult.getInputModel == evcsInputModel.getUuid =>
            evcsResult.getTime shouldBe 3600.toDateTime
            evcsResult.getP should beEquivalentTo(0.0.asKiloWatt)
            evcsResult.getQ should beEquivalentTo(0.0.asKiloVar)
          case unexpected =>
            fail(s"Unexpected result $unexpected was found.")
        }

      // Next data at 18000
      scheduler.expectMessage(Completion(evcsActivation, Some(9000)))

    }

  }

}
