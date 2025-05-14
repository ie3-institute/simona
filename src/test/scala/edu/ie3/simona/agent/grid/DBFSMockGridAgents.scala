/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.{
  ExchangePower,
  ExchangeVoltage,
}
import edu.ie3.simona.agent.grid.GridAgentMessages.*
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower}
import org.apache.pekko.actor.testkit.typed.scaladsl.TestProbe
import org.apache.pekko.actor.typed.ActorRef
import squants.Power
import squants.electro.Volts
import squants.energy.Megawatts

import java.util.UUID
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

/** Provide mock grid agents for testing the DBFSAlgorithm. These agents are an
  * agent for inferior grids and an agent for superior grids. Each grid agent
  * consists of a TestProbe and a sequence of grid nodes.
  */
trait DBFSMockGridAgents extends UnitSpec {
  private implicit val powerTolerance: Power = Megawatts(1e-10)
  private implicit val reactivePowerTolerance: ReactivePower = Megavars(1e-10)
  private implicit val electricPotentialTolerance
      : squants.electro.ElectricPotential = Volts(1e-6)

  sealed trait GAActorAndModel {
    val gaProbe: TestProbe[GridAgent.Request]
    val nodeUuids: Seq[UUID]

    def ref: ActorRef[GridAgent.Request] = gaProbe.ref
  }

  final case class InferiorGA(
      override val gaProbe: TestProbe[GridAgent.Request],
      override val nodeUuids: Seq[UUID],
  ) extends GAActorAndModel {

    def expectGridPowerRequest(): ActorRef[GridAgent.Request] =
      gaProbe.expectMessageType[GridAgent.Request] match {
        case requestGridPowerMessage: RequestGridPower =>
          requestGridPowerMessage.nodeUuids should contain allElementsOf nodeUuids

          requestGridPowerMessage.sender

        case unknown =>
          fail(
            s"Invalid message $unknown received when expecting a request for grid power values!"
          )
      }

    def expectSlackVoltageProvision(
        expectedSweepNo: Int,
        expectedExchangedVoltages: Seq[ExchangeVoltage],
    ): Unit =
      gaProbe.expectMessageType[GridAgent.Request] match {
        case msg: SlackVoltageResponse =>
          msg.currentSweepNo shouldBe expectedSweepNo

          msg.nodalSlackVoltages.size shouldBe expectedExchangedVoltages.size
          expectedExchangedVoltages.foreach { expectedVoltage =>
            msg.nodalSlackVoltages.find(
              _.nodeUuid == expectedVoltage.nodeUuid
            ) match {
              case Some(ExchangeVoltage(_, actualE, actualF)) =>
                actualE should approximate(expectedVoltage.e)
                actualF should approximate(expectedVoltage.f)
              case None =>
                fail(
                  s"Expected ExchangeVoltage with node UUID ${expectedVoltage.nodeUuid} " +
                    s"was not included in ProvideSlackVoltageMessage."
                )
            }
          }

        case unknown =>
          fail(
            s"Invalid message $unknown received when expecting a provision of slack voltage values!"
          )
      }

    def requestSlackVoltage(
        receiver: ActorRef[GridAgent.Request],
        sweepNo: Int,
    ): Unit =
      receiver ! SlackVoltageRequest(sweepNo, nodeUuids, gaProbe.ref)
  }

  final case class SuperiorGA(
      override val gaProbe: TestProbe[GridAgent.Request],
      override val nodeUuids: Seq[UUID],
  ) extends GAActorAndModel {

    def expectSlackVoltageRequest(
        expectedSweepNo: Int
    ): ActorRef[GridAgent.Request] =
      gaProbe.expectMessageType[GridAgent.Request](10.seconds) match {
        case requestSlackVoltageMessage: SlackVoltageRequest =>
          requestSlackVoltageMessage.currentSweepNo shouldBe expectedSweepNo
          requestSlackVoltageMessage.nodeUuids should have size nodeUuids.size
          requestSlackVoltageMessage.nodeUuids should contain allElementsOf nodeUuids

          requestSlackVoltageMessage.sender

        case unknown =>
          fail(
            s"Invalid message $unknown received when expecting a request for slack voltage values!"
          )
      }

    def expectGridPowerProvision(
        expectedExchangedPowers: Seq[ExchangePower],
        maxDuration: FiniteDuration = 30 seconds,
    ): Unit =
      gaProbe.expectMessageType[GridAgent.Request](maxDuration) match {
        case msg: GridPowerResponse =>
          msg.nodalResidualPower should have size expectedExchangedPowers.size

          expectedExchangedPowers.foreach { expectedPower =>
            msg.nodalResidualPower.find(
              _.nodeUuid == expectedPower.nodeUuid
            ) match {
              case Some(ExchangePower(_, actualP, actualQ)) =>
                actualP should approximate(expectedPower.p)
                actualQ should approximate(expectedPower.q)
              case None =>
                fail(
                  s"Expected ExchangePower with node UUID ${expectedPower.nodeUuid} " +
                    s"was not included in ProvideGridPowerMessage."
                )
            }
          }
        case unknown =>
          fail(
            s"Invalid message $unknown received when expecting a provision of grid power values!"
          )
      }

    def requestGridPower(
        receiver: ActorRef[GridAgent.Request],
        sweepNo: Int,
    ): Unit = {
      receiver ! RequestGridPower(sweepNo, nodeUuids, gaProbe.ref)
    }
  }
}
