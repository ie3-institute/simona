/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.ActorRef
import akka.testkit.TestProbe
import edu.ie3.simona.ontology.messages.PowerMessage.ProvideGridPowerMessage.ExchangePower
import edu.ie3.simona.ontology.messages.PowerMessage.{
  ProvideGridPowerMessage,
  RequestGridPowerMessage
}
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage.ExchangeVoltage
import edu.ie3.simona.ontology.messages.VoltageMessage.{
  ProvideSlackVoltageMessage,
  RequestSlackVoltageMessage
}
import edu.ie3.simona.test.common.UnitSpec

import java.util.UUID
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

/** Provide mock grid agents for testing the DBFSAlgorithm. These agents are an
  * agent for inferior grids and an agent for superior grids. Each grid agent
  * consists of a TestProbe and a sequence of grid nodes.
  */
trait DBFSMockGridAgents extends UnitSpec {
  private val floatPrecision: Double = 0.00000000001

  sealed trait GAActorAndModel {
    val gaProbe: TestProbe
    val nodeUuids: Seq[UUID]

    def ref: ActorRef = gaProbe.ref
  }

  final case class InferiorGA(
      override val gaProbe: TestProbe,
      override val nodeUuids: Seq[UUID]
  ) extends GAActorAndModel {

    def expectGridPowerRequest(): ActorRef = {
      gaProbe
        .expectMsgType[RequestGridPowerMessage]
        .nodeUuids should contain allElementsOf nodeUuids

      gaProbe.lastSender
    }

    def expectSlackVoltageProvision(
        expectedSweepNo: Int,
        expectedExchangedVoltages: Seq[ExchangeVoltage]
    ): Unit = {
      inside(gaProbe.expectMsgType[ProvideSlackVoltageMessage]) {
        case ProvideSlackVoltageMessage(sweepNo, exchangedVoltages) =>
          sweepNo shouldBe expectedSweepNo

          exchangedVoltages.size shouldBe expectedExchangedVoltages.size
          expectedExchangedVoltages.foreach { expectedVoltage =>
            exchangedVoltages.find(
              _.nodeUuid == expectedVoltage.nodeUuid
            ) match {
              case Some(ExchangeVoltage(_, actualE, actualF)) =>
                actualE should equalWithTolerance(
                  expectedVoltage.e,
                  floatPrecision
                )
                actualF should equalWithTolerance(
                  expectedVoltage.f,
                  floatPrecision
                )
              case None =>
                fail(
                  s"Expected ExchangeVoltage with node UUID ${expectedVoltage.nodeUuid} " +
                    s"was not included in ProvideSlackVoltageMessage."
                )
            }
          }
      }
    }

    def requestSlackVoltage(receiver: ActorRef, sweepNo: Int): Unit =
      gaProbe.send(
        receiver,
        RequestSlackVoltageMessage(sweepNo, nodeUuids)
      )
  }

  final case class SuperiorGA(
      override val gaProbe: TestProbe,
      override val nodeUuids: Seq[UUID]
  ) extends GAActorAndModel {

    def expectSlackVoltageRequest(expectedSweepNo: Int): ActorRef = {
      inside(
        gaProbe
          .expectMsgType[RequestSlackVoltageMessage]
      ) {
        case RequestSlackVoltageMessage(msgSweepNo: Int, msgUuids: Seq[UUID]) =>
          msgSweepNo shouldBe expectedSweepNo
          msgUuids should have size nodeUuids.size
          msgUuids should contain allElementsOf nodeUuids
      }

      gaProbe.lastSender
    }

    def expectGridPowerProvision(
        expectedExchangedPowers: Seq[ExchangePower],
        maxDuration: FiniteDuration = 30 seconds
    ): Unit = {
      inside(gaProbe.expectMsgType[ProvideGridPowerMessage](maxDuration)) {
        case ProvideGridPowerMessage(exchangedPower) =>
          exchangedPower should have size expectedExchangedPowers.size

          expectedExchangedPowers.foreach { expectedPower =>
            exchangedPower.find(_.nodeUuid == expectedPower.nodeUuid) match {
              case Some(ExchangePower(_, actualP, actualQ)) =>
                actualP should equalWithTolerance(
                  expectedPower.p,
                  floatPrecision
                )
                actualQ should equalWithTolerance(
                  expectedPower.q,
                  floatPrecision
                )
              case None =>
                fail(
                  s"Expected ExchangePower with node UUID ${expectedPower.nodeUuid} " +
                    s"was not included in ProvideGridPowerMessage."
                )
            }
          }

      }
    }

    def requestGridPower(receiver: ActorRef, sweepNo: Int): Unit = {
      gaProbe.send(
        receiver,
        RequestGridPowerMessage(
          sweepNo,
          nodeUuids
        )
      )
    }
  }
}
