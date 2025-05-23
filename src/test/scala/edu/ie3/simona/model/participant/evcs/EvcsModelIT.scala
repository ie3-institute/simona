/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.datamodel.models.result.system.{EvResult, EvcsResult}
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.RegistrationFailedMessage
import edu.ie3.simona.agent.participant.ParticipantAgentInit
import edu.ie3.simona.agent.participant.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology._
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
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  Create,
  PrimaryServiceRegistrationMessage,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.common.{TestSpawnerTyped, UnitSpec}
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils._
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter._
import squants.Each

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

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

    val evA = ev1.copyWithDeparture(9000)
    val evB = ev2.copyWithDeparture(18000)
    val evC = ev3.copyWithDeparture(14400)

    "handle a few requests and arrivals as expected" in {

      val gridAgent = TestProbe[GridAgent.Request]("GridAgent")
      val resultListener = TestProbe[ResultEvent]("ResultListener")
      val primaryServiceProxy =
        TestProbe[ServiceMessage]("PrimaryServiceProxy")
      val scheduler = TestProbe[SchedulerMessage]("Scheduler")
      val extSimAdapter = TestProbe[Any]("ExtSimAdapter")

      /* Create ExtEvDataService */
      val evService = spawn(
        ExtEvDataService.apply(scheduler.ref),
        "ExtEvDataService",
      )

      val adapterToExt = spawn(
        ExtEvDataService.adapter(evService),
        "ExtEvDataService-external-adapter",
      )

      val extEvData = new ExtEvDataConnection()
      extEvData.setActorRefs(
        adapterToExt.toClassic,
        extSimAdapter.ref.toClassic,
      )
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, PRE_INIT_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      evService ! Create(
        InitExtEvData(extEvData),
        serviceKey,
      )

      val scheduleServiceMsg = scheduler.expectMessageType[ScheduleActivation]
      scheduleServiceMsg.tick shouldBe INIT_SIM_TICK
      scheduleServiceMsg.unlockKey shouldBe Some(serviceKey)
      val serviceActivation = scheduleServiceMsg.actor

      /* Create ParticipantAgent with EvcsModel */
      val participantRefs = ParticipantRefs(
        gridAgent = gridAgent.ref,
        primaryServiceProxy = primaryServiceProxy.ref,
        services = Map(ServiceType.EvMovementService -> evService),
        resultListener = Iterable(resultListener.ref),
      )

      val evcsKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, PRE_INIT_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      val evcsAgent = spawn(
        ParticipantAgentInit(
          evcsInputContainer,
          EvcsRuntimeConfig(),
          notifierConfig,
          participantRefs,
          simulationParams,
          Left(scheduler.ref),
          evcsKey,
        )
      )

      val scheduleEvcsMsg = scheduler.expectMessageType[ScheduleActivation]
      scheduleEvcsMsg.tick shouldBe INIT_SIM_TICK
      scheduleEvcsMsg.unlockKey shouldBe Some(evcsKey)
      val evcsActivation = scheduleEvcsMsg.actor

      /* INIT */

      serviceActivation ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(serviceActivation))

      evcsActivation ! Activation(INIT_SIM_TICK)

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
        Some(long2Long(0L)).toJava,
      )
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      serviceActivation ! Activation(INIT_SIM_TICK)

      scheduler.receiveMessages(2) should contain allOf (
        Completion(evcsActivation, Some(0)),
        Completion(serviceActivation, None)
      )

      /* TICK 0 */

      // Request prices (dummy implementation)
      extEvData.sendExtMsg(new RequestCurrentPrices())
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      serviceActivation ! Activation(0)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideCurrentPrices(
        Map(evcsInputModel.getUuid -> double2Double(0.0)).asJava
      )

      scheduler.expectMessage(Completion(serviceActivation, None))

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      serviceActivation ! Activation(0)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // No EV connected
        Map(evcsInputModel.getUuid -> int2Integer(2)).asJava
      )

      scheduler.expectMessage(Completion(serviceActivation, None))

      resultListener.expectNoMessage()

      // Send arrivals
      extEvData.provideArrivingEvs(
        Map(
          evcsInputModel.getUuid -> List[EvModel](evA, evB).asJava
        ).asJava,
        Some(long2Long(9000)).toJava,
      )
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      serviceActivation ! Activation(0)

      scheduler.expectMessage(Completion(serviceActivation, None))

      evcsActivation ! Activation(0)

      resultListener
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
      scheduler.expectMessage(Completion(evcsActivation, Some(1800)))

      /* TICK 1800 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      serviceActivation ! Activation(1800)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // evA and evB connected
        // Fully occupied EVCS are not included
        Map.empty[UUID, java.lang.Integer].asJava
      )

      scheduler.expectMessage(Completion(serviceActivation, None))

      resultListener.expectNoMessage()

      // EVCS activation without arrivals
      evcsActivation ! Activation(1800)

      resultListener
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
      scheduler.expectMessage(Completion(evcsActivation, Some(3600)))

      /* TICK 3600 */

      evcsActivation ! Activation(3600)

      resultListener
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
      scheduler.expectMessage(Completion(evcsActivation, Some(9000)))

      /* TICK 9000 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      serviceActivation ! Activation(9000)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // evB connected
        // evA (departing at this tick) is not included
        Map(evcsInputModel.getUuid -> int2Integer(1)).asJava
      )

      scheduler.expectMessage(Completion(serviceActivation, None))

      // Request departing EVs
      extEvData.sendExtMsg(
        new RequestDepartingEvs(
          Map(evcsInputModel.getUuid -> List(evA.getUuid).asJava).asJava
        )
      )
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      serviceActivation ! Activation(9000)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideDepartingEvs(
        List[EvModel](evA.copyWith(10.0.asKiloWattHour)).asJava
      )

      scheduler.expectMessage(Completion(serviceActivation, None))

      // Send (empty) arrivals in order to update next tick
      extEvData.provideArrivingEvs(
        Map.empty[UUID, java.util.List[EvModel]].asJava,
        Some(long2Long(10800)).toJava,
      )
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      serviceActivation ! Activation(9000)

      scheduler.expectMessage(Completion(serviceActivation, None))

      evcsActivation ! Activation(9000)

      resultListener.expectNoMessage()

      // Next data at 10800
      scheduler.expectMessage(Completion(evcsActivation, Some(10800)))

      /* TICK 10800 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      serviceActivation ! Activation(10800)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // evB connected
        Map(evcsInputModel.getUuid -> int2Integer(1)).asJava
      )

      scheduler.expectMessage(Completion(serviceActivation, None))

      resultListener.expectNoMessage()

      // Send arrivals
      extEvData.provideArrivingEvs(
        Map(
          evcsInputModel.getUuid -> List[EvModel](evC).asJava
        ).asJava,
        Some(long2Long(14400)).toJava,
      )
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      serviceActivation ! Activation(10800)

      scheduler.expectMessage(Completion(serviceActivation, None))

      evcsActivation ! Activation(10800)

      resultListener
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
      scheduler.expectMessage(Completion(evcsActivation, Some(12600)))

      /* TICK 12600 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      serviceActivation ! Activation(12600)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // evB and evC connected
        // Fully occupied EVCS are not included
        Map.empty[UUID, java.lang.Integer].asJava
      )

      scheduler.expectMessage(Completion(serviceActivation, None))

      // EVCS activation
      evcsActivation ! Activation(12600)

      resultListener
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
      scheduler.expectMessage(Completion(evcsActivation, Some(14400)))

      /* TICK 14400 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      serviceActivation ! Activation(14400)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // evB connected
        // evC (departing at this tick) is not included
        Map(evcsInputModel.getUuid -> int2Integer(1)).asJava
      )

      scheduler.expectMessage(Completion(serviceActivation, None))

      // Request departing EVs
      extEvData.sendExtMsg(
        new RequestDepartingEvs(
          Map(evcsInputModel.getUuid -> List(evC.getUuid).asJava).asJava
        )
      )
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      serviceActivation ! Activation(14400)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideDepartingEvs(
        List[EvModel](evC.copyWith(20.0.asKiloWattHour)).asJava
      )

      scheduler.expectMessage(Completion(serviceActivation, None))

      // Send (empty) arrivals in order to update next tick
      extEvData.provideArrivingEvs(
        Map.empty[UUID, java.util.List[EvModel]].asJava,
        Some(long2Long(18000)).toJava,
      )
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      serviceActivation ! Activation(14400)

      scheduler.expectMessage(Completion(serviceActivation, None))

      evcsActivation ! Activation(14400)

      resultListener.expectNoMessage()

      // evB is departing at 18000
      scheduler.expectMessage(Completion(evcsActivation, Some(18000)))

      /* TICK 18000 */

      // Request free lots
      extEvData.sendExtMsg(new RequestEvcsFreeLots())
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      serviceActivation ! Activation(18000)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideEvcsFreeLots(
        // No EVs connected
        // evB (departing at this tick) is not included
        Map(evcsInputModel.getUuid -> int2Integer(2)).asJava
      )

      scheduler.expectMessage(Completion(serviceActivation, None))

      // Request departing EVs
      extEvData.sendExtMsg(
        new RequestDepartingEvs(
          Map(evcsInputModel.getUuid -> List(evB.getUuid).asJava).asJava
        )
      )
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      serviceActivation ! Activation(18000)

      extEvData.receiveTriggerQueue.take() shouldBe new ProvideDepartingEvs(
        List[EvModel](evB.copyWith(10.0.asKiloWattHour)).asJava
      )

      scheduler.expectMessage(Completion(serviceActivation, None))

      // Send (empty) arrivals in order to update next tick
      extEvData.provideArrivingEvs(
        Map.empty[UUID, java.util.List[EvModel]].asJava,
        None.toJava,
      )
      extSimAdapter.expectMessage(
        new ScheduleDataServiceMessage(adapterToExt.toClassic)
      )

      // waiting for ExtEvDataService
      scheduler.expectNoMessage()

      serviceActivation ! Activation(18000)

      scheduler.expectMessage(Completion(serviceActivation, None))

      evcsActivation ! Activation(18000)

      resultListener.expectNoMessage()

      // No future arrivals planned, next activation: end of simulation
      scheduler.expectMessage(Completion(evcsActivation, Some(48 * 3600)))

    }

  }

}
