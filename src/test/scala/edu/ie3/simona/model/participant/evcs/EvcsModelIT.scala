/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.datamodel.models.result.system.{EvResult, EvcsResult}
import edu.ie3.simona.agent.participant.ParticipantAgentInit
import edu.ie3.simona.agent.participant.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.api.data.connection.ExtEvDataConnection
import edu.ie3.simona.api.data.model.ev.EvModel
import edu.ie3.simona.api.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.ontology.ev.*
import edu.ie3.simona.config.RuntimeConfig.EvcsRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  PrimaryServiceRegistrationMessage,
  RegistrationFailedMessage,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy.{
  ExpectResult,
  NoResult,
}
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.common.{TestSpawnerTyped, UnitSpec}
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.simona.util.TickUtil.toDateTime
import edu.ie3.util.quantities.QuantityUtils.*
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import squants.Each

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import java.util.OptionalLong
import scala.jdk.CollectionConverters.*

/** Tests the combined functionality of
  * [[edu.ie3.simona.agent.participant.ParticipantAgent]] with an [[EvcsModel]]
  * and [[ExtEvDataService]].
  */
class EvcsModelIT
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with EvcsInputTestData
    with TestSpawnerTyped {

  private implicit val simulationStartDate: ZonedDateTime =
    defaultSimulationStart

  given simulationParams: SimulationParameters = SimulationParameters(
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

    val evA = ev1.copyWithDeparture(9000)
    val evB = ev2.copyWithDeparture(18000)
    val evC = ev3.copyWithDeparture(14400)

    "handle a few requests and arrivals as expected" in {

      val resultProxy =
        TestProbe[ResultEvent | ExpectResult | NoResult]("ResultServiceProxy")
      val primaryServiceProxy =
        TestProbe[PrimaryServiceProxy.Message]("PrimaryServiceProxy")
      val scheduler = TestProbe[SchedulerMessage]("Scheduler")
      val extSimAdapter = TestProbe[Any]("ExtSimAdapter")

      val extEvData = new ExtEvDataConnection()

      /* Create ExtEvDataService */
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val evService = spawn(
        ExtEvDataService
          .apply(scheduler.ref, InitExtEvData(extEvData), serviceKey),
        "ExtEvDataService",
      )

      extEvData.setActorRefs(
        evService,
        extSimAdapter.ref,
      )

      // no message for scheduling first service activation expected
      scheduler.expectNoMessage()

      /* Create ParticipantAgent with EvcsModel */
      given ParticipantRefs = ParticipantRefs(
        primaryServiceProxy = primaryServiceProxy.ref,
        resultServiceProxy = resultProxy.ref,
        services = Map(ServiceType.EvMovementService -> evService),
      )

      val evcsKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, PRE_INIT_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      val evcsAgent = spawn(
        ParticipantAgentInit(
          evcsInputContainer,
          EvcsRuntimeConfig(
            departureTargetSoc = 1.0
          ),
          notifierConfig,
          Left(scheduler.ref),
          evcsKey,
        )
      )

      scheduler.expectMessage(
        ScheduleActivation(evcsAgent, INIT_SIM_TICK, Some(evcsKey))
      )

      /* INIT */

      evcsAgent ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(
          evcsAgent,
          evcsInputModel.getUuid,
        )
      )
      evcsAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      // providing the first data tick
      extEvData.provideArrivingEvs(
        Map.empty[UUID, java.util.List[EvModel]].asJava,
        OptionalLong.of(0),
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      evService ! Activation(INIT_SIM_TICK)

      scheduler.receiveMessages(2) should contain allOf (
        Completion(evcsAgent, Some(0)),
        Completion(evService, None)
      )

      /* TICK 0 */

      // Request prices (dummy implementation)
      extEvData.sendExtMsg(new RequestCurrentPrices())
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(0)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideCurrentPrices(
        Map(evcsInputModel.getUuid -> double2Double(0.0)).asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(0)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // No EV connected
        Map(evcsInputModel.getUuid -> int2Integer(2)).asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      resultProxy.expectNoMessage()

      // Send arrivals
      extEvData.provideArrivingEvs(
        Map(
          evcsInputModel.getUuid -> List[EvModel](evA, evB).asJava
        ).asJava,
        OptionalLong.of(9000),
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      evService ! Activation(0)

      scheduler.expectMessage(Completion(evService, None))

      evcsAgent ! Activation(0)

      // the result proxy is informed that a result will be provided
      resultProxy.expectMessage(ExpectResult(evcsInputModel.getUuid, 0))

      resultProxy
        .receiveMessages(3)
        .map { case ParticipantResultEvent(result) =>
          result
        }
        .foreach {
          case evResult: EvResult if evResult.getInputModel == evA.getUuid =>
            evResult.getTime shouldBe 0.toDateTime
            evResult.getP should beEquivalentTo(5.0.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(50.0.asPercent)
          case evResult: EvResult if evResult.getInputModel == evB.getUuid =>
            evResult.getTime shouldBe 0.toDateTime
            evResult.getP should beEquivalentTo(5.0.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(75.0.asPercent)
          case evcsResult: EvcsResult =>
            evcsResult.getInputModel shouldBe evcsInputModel.getUuid
            evcsResult.getTime shouldBe 0.toDateTime
            evcsResult.getP should beEquivalentTo(10.0.asKiloWatt)
            evcsResult.getQ should beEquivalentTo(0.0.asKiloVar)
          case unexpected =>
            fail(s"Unexpected result $unexpected was found.")
        }

      // evB is full at 1800
      scheduler.expectMessage(Completion(evcsAgent, Some(1800)))

      /* TICK 1800 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(1800)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // evA and evB connected
        // Fully occupied EVCS are not included
        Map.empty[UUID, java.lang.Integer].asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      resultProxy.expectNoMessage()

      // EVCS activation without arrivals
      evcsAgent ! Activation(1800)

      // the result proxy is informed that a result will be provided
      resultProxy.expectMessage(ExpectResult(evcsInputModel.getUuid, 1800))

      resultProxy
        .receiveMessages(2)
        .map { case ParticipantResultEvent(result) =>
          result
        }
        .foreach {
          case evResult: EvResult if evResult.getInputModel == evB.getUuid =>
            evResult.getTime shouldBe 1800.toDateTime
            evResult.getP should beEquivalentTo(0.0.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(100.0.asPercent)
          case evcsResult: EvcsResult =>
            evcsResult.getInputModel shouldBe evcsInputModel.getUuid
            evcsResult.getTime shouldBe 1800.toDateTime
            evcsResult.getP should beEquivalentTo(5.0.asKiloWatt)
            evcsResult.getQ should beEquivalentTo(0.0.asKiloVar)
          case unexpected =>
            fail(s"Unexpected result $unexpected was found.")
        }

      // evA is full at 3600
      scheduler.expectMessage(Completion(evcsAgent, Some(3600)))

      /* TICK 3600 */

      evcsAgent ! Activation(3600)

      // the result proxy is informed that a result will be provided
      resultProxy.expectMessage(ExpectResult(evcsInputModel.getUuid, 3600))

      resultProxy
        .receiveMessages(2)
        .map { case ParticipantResultEvent(result) =>
          result
        }
        .foreach {
          case evResult: EvResult if evResult.getInputModel == evA.getUuid =>
            evResult.getTime shouldBe 3600.toDateTime
            evResult.getP should beEquivalentTo(0.0.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(100.0.asPercent)
          case evcsResult: EvcsResult =>
            evcsResult.getInputModel shouldBe evcsInputModel.getUuid
            evcsResult.getTime shouldBe 3600.toDateTime
            evcsResult.getP should beEquivalentTo(0.0.asKiloWatt)
            evcsResult.getQ should beEquivalentTo(0.0.asKiloVar)
          case unexpected =>
            fail(s"Unexpected result $unexpected was found.")
        }

      // evA is departing at 9000
      scheduler.expectMessage(Completion(evcsAgent, Some(9000)))

      /* TICK 9000 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(9000)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // evB connected
        // evA (departing at this tick) is not included
        Map(evcsInputModel.getUuid -> int2Integer(1)).asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      // Request departing EVs
      extEvData.sendExtMsg(
        new RequestDepartingEvs(
          Map(evcsInputModel.getUuid -> List(evA.getUuid).asJava).asJava
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(9000)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideDepartingEvs(
        List[EvModel](evA.copyWith(10.0.asKiloWattHour)).asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      // Send (empty) arrivals in order to update next tick
      extEvData.provideArrivingEvs(
        Map.empty[UUID, java.util.List[EvModel]].asJava,
        OptionalLong.of(10800),
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      evService ! Activation(9000)

      scheduler.expectMessage(Completion(evService, None))

      evcsAgent ! Activation(9000)

      // the result proxy is informed that a result will be provided
      resultProxy.expectMessage(ExpectResult(evcsInputModel.getUuid, 9000))

      // Next data at 10800
      scheduler.expectMessage(Completion(evcsAgent, Some(10800)))

      /* TICK 10800 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(10800)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // evB connected
        Map(evcsInputModel.getUuid -> int2Integer(1)).asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      // Send arrivals
      extEvData.provideArrivingEvs(
        Map(
          evcsInputModel.getUuid -> List[EvModel](evC).asJava
        ).asJava,
        OptionalLong.of(14400),
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      evService ! Activation(10800)

      scheduler.expectMessage(Completion(evService, None))

      evcsAgent ! Activation(10800)

      // the result proxy is informed that a result will be provided
      resultProxy.expectMessage(ExpectResult(evcsInputModel.getUuid, 10800))

      resultProxy
        .receiveMessages(2)
        .map { case ParticipantResultEvent(result) =>
          result
        }
        .foreach {
          case evResult: EvResult if evResult.getInputModel == evC.getUuid =>
            evResult.getTime shouldBe 10800.toDateTime
            evResult.getP should beEquivalentTo(10.0.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(75.0.asPercent)
          case evcsResult: EvcsResult =>
            evcsResult.getInputModel shouldBe evcsInputModel.getUuid
            evcsResult.getTime shouldBe 10800.toDateTime
            evcsResult.getP should beEquivalentTo(10.0.asKiloWatt)
            evcsResult.getQ should beEquivalentTo(0.0.asKiloVar)
          case unexpected =>
            fail(s"Unexpected result $unexpected was found.")
        }

      // evC is full at 12600
      scheduler.expectMessage(Completion(evcsAgent, Some(12600)))

      /* TICK 12600 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(12600)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // evB and evC connected
        // Fully occupied EVCS are not included
        Map.empty[UUID, java.lang.Integer].asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      // EVCS activation
      evcsAgent ! Activation(12600)

      // the result proxy is informed that a result will be provided
      resultProxy.expectMessage(ExpectResult(evcsInputModel.getUuid, 12600))

      resultProxy
        .receiveMessages(2)
        .map { case ParticipantResultEvent(result) =>
          result
        }
        .foreach {
          case evResult: EvResult if evResult.getInputModel == evC.getUuid =>
            evResult.getTime shouldBe 12600.toDateTime
            evResult.getP should beEquivalentTo(0.0.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(100.0.asPercent)
          case evcsResult: EvcsResult =>
            evcsResult.getInputModel shouldBe evcsInputModel.getUuid
            evcsResult.getTime shouldBe 12600.toDateTime
            evcsResult.getP should beEquivalentTo(0.0.asKiloWatt)
            evcsResult.getQ should beEquivalentTo(0.0.asKiloVar)
          case unexpected =>
            fail(s"Unexpected result $unexpected was found.")
        }

      // evC is departing at 14400
      scheduler.expectMessage(Completion(evcsAgent, Some(14400)))

      /* TICK 14400 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(14400)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // evB connected
        // evC (departing at this tick) is not included
        Map(evcsInputModel.getUuid -> int2Integer(1)).asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      // Request departing EVs
      extEvData.sendExtMsg(
        new RequestDepartingEvs(
          Map(evcsInputModel.getUuid -> List(evC.getUuid).asJava).asJava
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(14400)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideDepartingEvs(
        List[EvModel](evC.copyWith(20.0.asKiloWattHour)).asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      // Send (empty) arrivals in order to update next tick
      extEvData.provideArrivingEvs(
        Map.empty[UUID, java.util.List[EvModel]].asJava,
        OptionalLong.of(18000),
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      evService ! Activation(14400)

      scheduler.expectMessage(Completion(evService, None))

      evcsAgent ! Activation(14400)

      // the result proxy is informed that a result will be provided
      resultProxy.expectMessage(ExpectResult(evcsInputModel.getUuid, 14400))

      // evB is departing at 18000
      scheduler.expectMessage(Completion(evcsAgent, Some(18000)))

      /* TICK 18000 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(18000)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // No EVs connected
        // evB (departing at this tick) is not included
        Map(evcsInputModel.getUuid -> int2Integer(2)).asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      // Request departing EVs
      extEvData.sendExtMsg(
        new RequestDepartingEvs(
          Map(evcsInputModel.getUuid -> List(evB.getUuid).asJava).asJava
        )
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      evService ! Activation(18000)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideDepartingEvs(
        List[EvModel](evB.copyWith(10.0.asKiloWattHour)).asJava
      )

      scheduler.expectMessage(Completion(evService, None))

      // Send (empty) arrivals in order to update next tick
      extEvData.provideArrivingEvs(
        Map.empty[UUID, java.util.List[EvModel]].asJava,
        OptionalLong.empty(),
      )
      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(evService))

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      evService ! Activation(18000)

      scheduler.expectMessage(Completion(evService, None))

      evcsAgent ! Activation(18000)

      // the result proxy is informed that a result will be provided
      resultProxy.expectMessage(ExpectResult(evcsInputModel.getUuid, 18000))

      // No future arrivals planned, next activation: end of simulation
      scheduler.expectMessage(Completion(evcsAgent, Some(48 * 3600)))

    }

  }

}
