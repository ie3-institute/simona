/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ActivePower,
  ActivePowerExtra,
}
import edu.ie3.simona.agent.participant2.MockParticipantModel.{
  MockRequestMessage,
  MockResponseMessage,
  MockResult,
  MockSecondaryData,
}
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  GridSimulationFinished,
  NoDataProvision,
  RequestAssetPowerMessage,
}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.model.participant2.{
  ParticipantModelInit,
  ParticipantModelShell,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{Kilovars, ReactivePower}
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter._
import squants.energy.Kilowatts
import squants.{Each, Power}

import java.time.ZonedDateTime

/** Test for [[ParticipantAgent]] and [[ParticipantModelShell]] using a mock
  * participant [[MockParticipantModel]]. Primary and secondary data handling is
  * tested here.
  */
class ParticipantAgentSpec extends ScalaTestWithActorTestKit with UnitSpec {

  private implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")

  private implicit val activePowerTolerance: Power = Kilowatts(1e-10)
  private implicit val reactivePowerTolerance: ReactivePower = Kilovars(1e-10)

  "A ParticipantAgent that is not controlled by EM" when {

    "not depending on external services" should {

      "calculate operating point and results correctly with no additional model activations" in {

        val scheduler = createTestProbe[SchedulerMessage]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()
        val responseReceiver = createTestProbe[MockResponseMessage.type]()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[Activation]]()

        // no additional activation ticks
        val model = new MockParticipantModel()
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map.empty
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              expectedRequestTick = 12 * 3600,
              requestVoltageDeviationTolerance = Each(1e-14),
            ),
            Iterable(resultListener.ref),
            Left(scheduler.ref, receiveAdapter.ref),
          )
        )
        val activationRef =
          receiveAdapter.expectMessageType[ActorRef[Activation]]

        // TICK 8 * 3600: Start of operation interval

        participantAgent ! MockRequestMessage(0, responseReceiver.ref)
        responseReceiver.expectMessage(MockResponseMessage)

        activationRef ! Activation(operationInterval.start)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.006.asMegaWatt)
            result.getQ should equalWithTolerance(0.00290593262.asMegaVar)
        }

        scheduler.expectMessage(
          Completion(activationRef, Some(operationInterval.end))
        )

        // TICK 12 * 3600: GridAgent requests power

        participantAgent ! MockRequestMessage(0, responseReceiver.ref)
        responseReceiver.expectMessage(MockResponseMessage)

        // first request
        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        // 8 hours of 0 kW, 4 hours of 6 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(2))
            q should approximate(Kilovars(0.968644209676))
        }

        // second request with same voltage
        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerUnchangedMessage] match {
          case AssetPowerUnchangedMessage(p, q) =>
            p should approximate(Kilowatts(2))
            q should approximate(Kilovars(0.968644209676))
        }

        // third request with different voltage
        participantAgent ! RequestAssetPowerMessage(
          12 * 3600,
          Each(0.98),
          Each(0),
        )

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(2))
            // not voltage dependent
            q should approximate(Kilovars(0.968644209676))
        }

        participantAgent ! GridSimulationFinished(12 * 3600, 24 * 3600)

        // TICK 20 * 3600: Outside of operation interval (last tick)

        participantAgent ! MockRequestMessage(0, responseReceiver.ref)
        responseReceiver.expectMessage(MockResponseMessage)

        activationRef ! Activation(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        scheduler.expectMessage(Completion(activationRef))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! MockRequestMessage(0, responseReceiver.ref)
        responseReceiver.expectMessage(MockResponseMessage)

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(4))
            q should approximate(Kilovars(1.93728841935))
        }

        participantAgent ! GridSimulationFinished(24 * 3600, 36 * 3600)

      }

      "calculate operating point and results correctly with additional model activations" in {

        val scheduler = createTestProbe[SchedulerMessage]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[Activation]]()

        // with additional activation ticks
        val model = new MockParticipantModel(mockActivationTicks =
          Map(
            0 * 3600L -> 4 * 3600L, // still before operation, is ignored
            8 * 3600L -> 12 * 3600L, // middle of operation
            12 * 3600L -> 22 * 3600L, // after operation, is ignored
          )
        )
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map.empty
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              expectedRequestTick = 12 * 3600,
              requestVoltageDeviationTolerance = Each(1e-14),
            ),
            Iterable(resultListener.ref),
            Left(scheduler.ref, receiveAdapter.ref),
          )
        )
        val activationRef =
          receiveAdapter.expectMessageType[ActorRef[Activation]]

        // TICK 8 * 3600: Start of operation interval

        activationRef ! Activation(operationInterval.start)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.006.asMegaWatt)
            result.getQ should equalWithTolerance(0.00290593262.asMegaVar)
        }

        scheduler.expectMessage(
          Completion(activationRef, Some(12 * 3600))
        )

        // TICK 12 * 3600: Inside of operation interval and GridAgent requests power

        activationRef ! Activation(12 * 3600)

        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        // 8 hours of 0 kW, 4 hours of 6 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(2))
            q should approximate(Kilovars(0.968644209676))
        }

        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

        participantAgent ! GridSimulationFinished(12 * 3600, 24 * 3600)

        // calculation should start now
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (12 * 3600).toDateTime
            result.getP should equalWithTolerance(0.006.asMegaWatt)
            result.getQ should equalWithTolerance(0.00290593262.asMegaVar)
        }

        scheduler.expectMessage(
          Completion(activationRef, Some(operationInterval.end))
        )

        // TICK 20 * 3600: Outside of operation interval (last tick)

        activationRef ! Activation(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        scheduler.expectMessage(Completion(activationRef))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(4))
            q should approximate(Kilovars(1.93728841935))
        }

        participantAgent ! GridSimulationFinished(24 * 3600, 36 * 3600)

      }

    }

    "depending on secondary data" should {

      "calculate operating point and results correctly with additional model activations" in {

        val scheduler = createTestProbe[SchedulerMessage]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()
        val service = createTestProbe()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[Activation]]()

        // with additional activation ticks
        val model = new MockParticipantModel(mockActivationTicks =
          Map(
            0 * 3600L -> 4 * 3600L, // still before operation, is ignored
            8 * 3600L -> 12 * 3600L, // middle of operation
            12 * 3600L -> 22 * 3600L, // after operation, is ignored
          )
        )
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map(service.ref.toClassic -> 0)
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              expectedRequestTick = 12 * 3600,
              requestVoltageDeviationTolerance = Each(1e-14),
            ),
            Iterable(resultListener.ref),
            Left(scheduler.ref, receiveAdapter.ref),
          )
        )
        val activationRef =
          receiveAdapter.expectMessageType[ActorRef[Activation]]

        // TICK 0: Outside of operation interval

        activationRef ! Activation(0)

        // nothing should happen, still waiting for secondary data...
        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

        participantAgent ! DataProvision(
          0,
          service.ref.toClassic,
          MockSecondaryData(Kilowatts(1)),
          Some(6 * 3600),
        )

        // outside of operation interval, 0 MW
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe simulationStartDate
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        // next model tick and next data tick are ignored,
        // because we are outside of operation interval
        scheduler.expectMessage(
          Completion(activationRef, Some(operationInterval.start))
        )

        // TICK 6 * 3600: Outside of operation interval, only data expected, no activation

        participantAgent ! DataProvision(
          6 * 3600,
          service.ref.toClassic,
          MockSecondaryData(Kilowatts(3)),
          Some(12 * 3600),
        )

        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

        // TICK 8 * 3600: Start of operation interval

        activationRef ! Activation(operationInterval.start)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.009.asMegaWatt)
            result.getQ should equalWithTolerance(0.00435889893.asMegaVar)
        }

        // next model tick and next data tick are both hour 12
        scheduler.expectMessage(
          Completion(activationRef, Some(12 * 3600))
        )

        // TICK 12 * 3600: Inside of operation interval, secondary data and GridAgent requests power

        activationRef ! Activation(12 * 3600)

        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        // 8 hours of 0 kW, 4 hours of 6+3=9 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(3))
            q should approximate(Kilovars(1.452966314514))
        }

        participantAgent ! GridSimulationFinished(12 * 3600, 24 * 3600)

        // nothing should happen, still waiting for secondary data...
        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

        participantAgent ! DataProvision(
          12 * 3600,
          service.ref.toClassic,
          MockSecondaryData(Kilowatts(6)),
          Some(15 * 3600),
        )

        // calculation should start now
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (12 * 3600).toDateTime
            result.getP should equalWithTolerance(0.012.asMegaWatt)
            result.getQ should equalWithTolerance(0.005811865258.asMegaVar)
        }

        // new data is expected at 18 hours
        scheduler.expectMessage(
          Completion(activationRef, Some(15 * 3600))
        )

        // TICK 15 * 3600: Inside of operation interval, but empty input data received

        activationRef ! Activation(15 * 3600)

        // nothing should happen, still waiting for secondary data...
        scheduler.expectNoMessage()

        participantAgent ! NoDataProvision(
          15 * 3600,
          service.ref.toClassic,
          Some(18 * 3600),
        )

        // no-op activation, thus no result expected
        resultListener.expectNoMessage()

        // new data is expected at 18 hours
        scheduler.expectMessage(
          Completion(activationRef, Some(18 * 3600))
        )

        // TICK 18 * 3600: Inside of operation interval because of expected secondary data

        activationRef ! Activation(18 * 3600)

        // nothing should happen, still waiting for secondary data...
        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

        participantAgent ! DataProvision(
          18 * 3600,
          service.ref.toClassic,
          MockSecondaryData(Kilowatts(9)),
          Some(24 * 3600),
        )

        // calculation should start now
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (18 * 3600).toDateTime
            result.getP should equalWithTolerance(0.015.asMegaWatt)
            result.getQ should equalWithTolerance(0.00726483157257.asMegaVar)
        }

        scheduler.expectMessage(
          Completion(activationRef, Some(operationInterval.end))
        )

        // TICK 20 * 3600: Outside of operation interval (last tick)

        activationRef ! Activation(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        // Since we left the operation interval, there are no more ticks to activate
        scheduler.expectMessage(Completion(activationRef))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        // 6 hours of 6+6=12 kW, 2 hours of 6+9=15 kW, 4 hours of 0 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(8.5))
            q should approximate(Kilovars(4.116737891123))
        }

        participantAgent ! GridSimulationFinished(24 * 3600, 36 * 3600)

        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

      }

    }

    "depending on primary data" should {

      "calculate operating point and results correctly" in {

        val scheduler = createTestProbe[SchedulerMessage]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()
        val service = createTestProbe()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[Activation]]()

        // no additional activation ticks
        val physicalModel = new MockParticipantModel()

        val model = ParticipantModelInit.createPrimaryModel(
          physicalModel,
          ActivePowerExtra,
        )
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map(service.ref.toClassic -> 0)
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              expectedRequestTick = 12 * 3600,
              requestVoltageDeviationTolerance = Each(1e-14),
            ),
            Iterable(resultListener.ref),
            Left(scheduler.ref, receiveAdapter.ref),
          )
        )
        val activationRef =
          receiveAdapter.expectMessageType[ActorRef[Activation]]

        // TICK 0: Outside of operation interval

        activationRef ! Activation(0)

        // nothing should happen, still waiting for primary data...
        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

        participantAgent ! DataProvision(
          0,
          service.ref.toClassic,
          ActivePower(Kilowatts(1)),
          Some(6 * 3600),
        )

        // outside of operation interval, 0 MW
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe simulationStartDate
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        // next model tick and next data tick are ignored,
        // because we are outside of operation interval
        scheduler.expectMessage(
          Completion(activationRef, Some(operationInterval.start))
        )

        // TICK 6 * 3600: Outside of operation interval, only data expected, no activation

        participantAgent ! DataProvision(
          6 * 3600,
          service.ref.toClassic,
          ActivePower(Kilowatts(3)),
          Some(12 * 3600),
        )

        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

        // TICK 8 * 3600: Start of operation interval

        activationRef ! Activation(operationInterval.start)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.003.asMegaWatt)
            result.getQ should equalWithTolerance(0.00145296631.asMegaVar)
        }

        // next data tick is hour 12
        scheduler.expectMessage(
          Completion(activationRef, Some(12 * 3600))
        )

        // TICK 12 * 3600: Inside of operation interval, GridAgent requests power

        activationRef ! Activation(12 * 3600)

        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        // 8 hours of 0 kW, 4 hours of 3 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(1))
            q should approximate(Kilovars(0.48432210484))
        }

        participantAgent ! GridSimulationFinished(12 * 3600, 24 * 3600)

        // nothing should happen, still waiting for primary data...
        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

        participantAgent ! DataProvision(
          12 * 3600,
          service.ref.toClassic,
          ActivePower(Kilowatts(6)),
          Some(18 * 3600),
        )

        // calculation should start now
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (12 * 3600).toDateTime
            result.getP should equalWithTolerance(0.006.asMegaWatt)
            result.getQ should equalWithTolerance(0.00290593263.asMegaVar)
        }

        // new data is expected at 18 hours
        scheduler.expectMessage(
          Completion(activationRef, Some(18 * 3600))
        )

        // TICK 18 * 3600: Inside of operation interval because of expected primary data

        activationRef ! Activation(18 * 3600)

        // nothing should happen, still waiting for primary data...
        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

        participantAgent ! DataProvision(
          18 * 3600,
          service.ref.toClassic,
          ActivePower(Kilowatts(3)),
          Some(24 * 3600),
        )

        // calculation should start now
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (18 * 3600).toDateTime
            result.getP should equalWithTolerance(0.003.asMegaWatt)
            result.getQ should equalWithTolerance(0.00145296631.asMegaVar)
        }

        scheduler.expectMessage(
          Completion(activationRef, Some(operationInterval.end))
        )

        // TICK 20 * 3600: Outside of operation interval (last tick)

        activationRef ! Activation(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        // Since we left the operation interval, there are no more ticks to activate
        scheduler.expectMessage(Completion(activationRef))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        // 6 hours of 6 kW, 2 hours of 3 kW, 4 hours of 0 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(3.5))
            q should approximate(Kilovars(1.695127366932))
        }

        participantAgent ! GridSimulationFinished(24 * 3600, 36 * 3600)

        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

      }

    }

  }

  "A ParticipantAgent that is controlled by EM" when {

    "not depending on external services" should {

      "calculate operating point and results correctly with no additional model activations" in {

        val em = createTestProbe[FlexResponse]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[FlexRequest]]()

        // no additional activation ticks
        val model = new MockParticipantModel()
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map.empty
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              expectedRequestTick = 12 * 3600,
              requestVoltageDeviationTolerance = Each(1e-14),
            ),
            Iterable(resultListener.ref),
            Right(em.ref, receiveAdapter.ref),
          )
        )
        val flexRef = receiveAdapter.expectMessageType[ActorRef[FlexRequest]]

        // TICK 8 * 3600: Start of operation interval

        flexRef ! FlexActivation(operationInterval.start)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(1))
            min should approximate(Kilowatts(-1))
            max should approximate(Kilowatts(3))
        }

        flexRef ! IssuePowerControl(operationInterval.start, Kilowatts(3))

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.003.asMegaWatt)
            result.getQ should equalWithTolerance(0.00145296631.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(operationInterval.end),
          )
        )

        // TICK 12 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        // 8 hours of 0 kW, 4 hours of 3 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(1))
            q should approximate(Kilovars(0.48432210483))
        }

        participantAgent ! GridSimulationFinished(12 * 3600, 24 * 3600)

        // TICK 20 * 3600: Outside of operation interval (last tick)

        flexRef ! FlexActivation(operationInterval.end)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(0))
            min should approximate(Kilowatts(0))
            max should approximate(Kilowatts(0))
        }

        flexRef ! IssueNoControl(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        em.expectMessage(FlexCompletion(model.uuid))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        // 8 hours of 3 kW, 4 hours of 0 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(2))
            q should approximate(Kilovars(0.96864420966))
        }

        participantAgent ! GridSimulationFinished(24 * 3600, 36 * 3600)

      }

      "calculate operating point and results correctly with additional model activations" in {

        val em = createTestProbe[FlexResponse]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[FlexRequest]]()

        // with additional activation ticks
        val model = new MockParticipantModel(
          mockActivationTicks = Map(
            0 * 3600L -> 4 * 3600L, // out of operation, is ignored
            8 * 3600L -> 12 * 3600L, // in operation
            12 * 3600L -> 22 * 3600L, // out of operation, is ignored
          ),
          mockChangeAtNext = Set(
            0, // out of operation, is ignored
            12 * 3600, // in operation
            20 * 3600, // out of operation, is ignored
          ),
        )
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map.empty
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              expectedRequestTick = 12 * 3600,
              requestVoltageDeviationTolerance = Each(1e-14),
            ),
            Iterable(resultListener.ref),
            Right(em.ref, receiveAdapter.ref),
          )
        )
        val flexRef = receiveAdapter.expectMessageType[ActorRef[FlexRequest]]

        // TICK 8 * 3600: Start of operation interval

        flexRef ! FlexActivation(operationInterval.start)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(1))
            min should approximate(Kilowatts(-1))
            max should approximate(Kilowatts(3))
        }

        flexRef ! IssuePowerControl(operationInterval.start, Kilowatts(3))

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.003.asMegaWatt)
            result.getQ should equalWithTolerance(0.00145296631.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(12 * 3600),
          )
        )

        // TICK 12 * 3600: Inside of operation interval and GridAgent requests power

        flexRef ! FlexActivation(12 * 3600)

        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(1))
            q should approximate(Kilovars(0.48432210483))
        }

        resultListener.expectNoMessage()
        em.expectNoMessage()

        participantAgent ! GridSimulationFinished(12 * 3600, 24 * 3600)

        // calculation should start now
        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(1))
            min should approximate(Kilowatts(-1))
            max should approximate(Kilowatts(3))
        }

        flexRef ! IssueNoControl(12 * 3600)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (12 * 3600).toDateTime
            result.getP should equalWithTolerance(0.001.asMegaWatt)
            result.getQ should equalWithTolerance(0.0004843221.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtNextActivation = true,
            requestAtTick = Some(operationInterval.end),
          )
        )

        // TICK 20 * 3600: Outside of operation interval (last tick)

        flexRef ! FlexActivation(operationInterval.end)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(0))
            min should approximate(Kilowatts(0))
            max should approximate(Kilowatts(0))
        }

        flexRef ! IssueNoControl(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        em.expectMessage(FlexCompletion(model.uuid))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        // 8 hours of 1 kW, 4 hours of 0 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(0.6666666667))
            q should approximate(Kilovars(0.32288140322))
        }

        participantAgent ! GridSimulationFinished(24 * 3600, 36 * 3600)

      }

    }

    "depending on secondary data" should {

      "calculate operating point and results correctly with additional model activations" in {

        val em = createTestProbe[FlexResponse]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()
        val service = createTestProbe()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[FlexRequest]]()

        // with additional activation ticks
        val model = new MockParticipantModel(
          mockActivationTicks = Map(
            0 * 3600L -> 4 * 3600L, // out of operation, is ignored
            8 * 3600L -> 12 * 3600L, // in operation
            12 * 3600L -> 22 * 3600L, // out of operation, is ignored
          ),
          mockChangeAtNext = Set(
            0, // out of operation, is ignored
            18 * 3600, // in operation
            20 * 3600, // out of operation, is ignored
          ),
        )
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map(service.ref.toClassic -> 0)
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              expectedRequestTick = 12 * 3600,
              requestVoltageDeviationTolerance = Each(1e-14),
            ),
            Iterable(resultListener.ref),
            Right(em.ref, receiveAdapter.ref),
          )
        )
        val flexRef = receiveAdapter.expectMessageType[ActorRef[FlexRequest]]

        // TICK 0: Outside of operation interval

        flexRef ! FlexActivation(0)

        // nothing should happen, still waiting for secondary data...
        resultListener.expectNoMessage()
        em.expectNoMessage()

        participantAgent ! DataProvision(
          0,
          service.ref.toClassic,
          MockSecondaryData(Kilowatts(1)),
          Some(6 * 3600),
        )

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(0))
            min should approximate(Kilowatts(0))
            max should approximate(Kilowatts(0))
        }

        flexRef ! IssueNoControl(0)

        // outside of operation interval, 0 MW
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe simulationStartDate
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        // next model tick and next data tick are ignored,
        // because we are outside of operation interval
        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(operationInterval.start),
          )
        )

        // TICK 6 * 3600: Outside of operation interval, only data expected, no activation

        participantAgent ! DataProvision(
          6 * 3600,
          service.ref.toClassic,
          MockSecondaryData(Kilowatts(1)),
          Some(12 * 3600),
        )

        resultListener.expectNoMessage()
        em.expectNoMessage()

        // TICK 8 * 3600: Start of operation interval

        flexRef ! FlexActivation(operationInterval.start)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(2))
            min should approximate(Kilowatts(0))
            max should approximate(Kilowatts(4))
        }

        flexRef ! IssuePowerControl(operationInterval.start, Kilowatts(3))

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.003.asMegaWatt)
            result.getQ should equalWithTolerance(0.00145296631.asMegaVar)
        }

        // next model tick and next data tick are both hour 12
        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(12 * 3600),
          )
        )

        // TICK 12 * 3600: Inside of operation interval, GridAgent requests power

        flexRef ! FlexActivation(12 * 3600)

        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        // 8 hours of 0 kW, 4 hours of 3 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(1))
            q should approximate(Kilovars(0.48432210483))
        }

        participantAgent ! GridSimulationFinished(12 * 3600, 24 * 3600)

        // nothing should happen, still waiting for secondary data...
        resultListener.expectNoMessage()
        em.expectNoMessage()

        participantAgent ! DataProvision(
          12 * 3600,
          service.ref.toClassic,
          MockSecondaryData(Kilowatts(2)),
          Some(18 * 3600),
        )

        // calculation should start now
        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(3))
            min should approximate(Kilowatts(1))
            max should approximate(Kilowatts(5))
        }

        flexRef ! IssueNoControl(12 * 3600)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (12 * 3600).toDateTime
            result.getP should equalWithTolerance(0.003.asMegaWatt)
            result.getQ should equalWithTolerance(0.00145296631.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(18 * 3600),
          )
        )

        // TICK 18 * 3600: Inside of operation interval because of expected secondary data

        flexRef ! FlexActivation(18 * 3600)

        // nothing should happen, still waiting for secondary data...
        resultListener.expectNoMessage()
        em.expectNoMessage()

        participantAgent ! DataProvision(
          18 * 3600,
          service.ref.toClassic,
          MockSecondaryData(Kilowatts(5)),
          Some(24 * 3600),
        )

        // calculation should start now
        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(6))
            min should approximate(Kilowatts(4))
            max should approximate(Kilowatts(8))
        }

        flexRef ! IssueNoControl(18 * 3600)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (18 * 3600).toDateTime
            result.getP should equalWithTolerance(0.006.asMegaWatt)
            result.getQ should equalWithTolerance(0.002905932629.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtNextActivation = true,
            requestAtTick = Some(operationInterval.end),
          )
        )

        // TICK 20 * 3600: Outside of operation interval (last tick)

        flexRef ! FlexActivation(operationInterval.end)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(0))
            min should approximate(Kilowatts(0))
            max should approximate(Kilowatts(0))
        }

        flexRef ! IssueNoControl(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        // Since we left the operation interval, there are no more ticks to activate
        em.expectMessage(FlexCompletion(model.uuid))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        // 6 hours of 3 kW, 2 hours of 6 kW, 4 hours of 0 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(2.5))
            q should approximate(Kilovars(1.210805262))
        }

        participantAgent ! GridSimulationFinished(24 * 3600, 36 * 3600)

        resultListener.expectNoMessage()
        em.expectNoMessage()

      }

    }

    "depending on primary data" should {

      "calculate operating point and results correctly" in {

        val em = createTestProbe[FlexResponse]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()
        val service = createTestProbe()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[FlexRequest]]()

        // no additional activation ticks
        val physicalModel = new MockParticipantModel()

        val model = ParticipantModelInit.createPrimaryModel(
          physicalModel,
          ActivePowerExtra,
        )
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map(service.ref.toClassic -> 0)
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              expectedRequestTick = 12 * 3600,
              requestVoltageDeviationTolerance = Each(1e-14),
            ),
            Iterable(resultListener.ref),
            Right(em.ref, receiveAdapter.ref),
          )
        )
        val flexRef = receiveAdapter.expectMessageType[ActorRef[FlexRequest]]

        // TICK 0: Outside of operation interval

        flexRef ! FlexActivation(0)

        // nothing should happen, still waiting for primary data...
        resultListener.expectNoMessage()
        em.expectNoMessage()

        participantAgent ! DataProvision(
          0,
          service.ref.toClassic,
          ActivePower(Kilowatts(1)),
          Some(6 * 3600),
        )

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(0))
            min should approximate(Kilowatts(0))
            max should approximate(Kilowatts(0))
        }

        flexRef ! IssueNoControl(0)

        // outside of operation interval, 0 MW
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe simulationStartDate
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        // next model tick and next data tick are ignored,
        // because we are outside of operation interval
        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(operationInterval.start),
          )
        )

        // TICK 6 * 3600: Outside of operation interval, only data expected, no activation

        participantAgent ! DataProvision(
          6 * 3600,
          service.ref.toClassic,
          ActivePower(Kilowatts(3)),
          Some(12 * 3600),
        )

        resultListener.expectNoMessage()
        em.expectNoMessage()

        // TICK 8 * 3600: Start of operation interval

        flexRef ! FlexActivation(operationInterval.start)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(3))
            min should approximate(Kilowatts(3))
            max should approximate(Kilowatts(3))
        }

        flexRef ! IssuePowerControl(operationInterval.start, Kilowatts(3))

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.003.asMegaWatt)
            result.getQ should equalWithTolerance(0.00145296631.asMegaVar)
        }

        // next data tick is hour 12
        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(12 * 3600),
          )
        )

        // TICK 12 * 3600: Inside of operation interval, GridAgent requests power

        flexRef ! FlexActivation(12 * 3600)

        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        // 8 hours of 0 kW, 4 hours of 3 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(1))
            q should approximate(Kilovars(0.48432210483))
        }

        participantAgent ! GridSimulationFinished(12 * 3600, 24 * 3600)

        // nothing should happen, still waiting for primary data...
        resultListener.expectNoMessage()
        em.expectNoMessage()

        participantAgent ! DataProvision(
          12 * 3600,
          service.ref.toClassic,
          ActivePower(Kilowatts(6)),
          Some(18 * 3600),
        )

        // calculation should start now
        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(6))
            min should approximate(Kilowatts(6))
            max should approximate(Kilowatts(6))
        }

        flexRef ! IssueNoControl(12 * 3600)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (12 * 3600).toDateTime
            result.getP should equalWithTolerance(0.006.asMegaWatt)
            result.getQ should equalWithTolerance(0.00290593263.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(18 * 3600),
          )
        )

        // TICK 18 * 3600: Inside of operation interval because of expected primary data

        flexRef ! FlexActivation(18 * 3600)

        // nothing should happen, still waiting for primary data...
        resultListener.expectNoMessage()
        em.expectNoMessage()

        participantAgent ! DataProvision(
          18 * 3600,
          service.ref.toClassic,
          ActivePower(Kilowatts(3)),
          Some(24 * 3600),
        )

        // calculation should start now
        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(3))
            min should approximate(Kilowatts(3))
            max should approximate(Kilowatts(3))
        }

        flexRef ! IssueNoControl(18 * 3600)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (18 * 3600).toDateTime
            result.getP should equalWithTolerance(0.003.asMegaWatt)
            result.getQ should equalWithTolerance(0.00145296631.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(operationInterval.end),
          )
        )

        // TICK 20 * 3600: Outside of operation interval (last tick)

        flexRef ! FlexActivation(operationInterval.end)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(0))
            min should approximate(Kilowatts(0))
            max should approximate(Kilowatts(0))
        }

        flexRef ! IssueNoControl(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        // Since we left the operation interval, there are no more ticks to activate
        em.expectMessage(FlexCompletion(model.uuid))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        // 6 hours of 6 kW, 2 hours of 3 kW, 4 hours of 0 kW
        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(3.5))
            q should approximate(Kilovars(1.695127366932))
        }

        participantAgent ! GridSimulationFinished(24 * 3600, 36 * 3600)

        resultListener.expectNoMessage()
        em.expectNoMessage()

      }

    }

  }

}
