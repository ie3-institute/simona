/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.ActorRef.noSender
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestFSMRef
import akka.util.Timeout
import breeze.numerics.pow
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals.RelevantResultValues
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InconsistentStateException
}
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.SystemParticipant
import edu.ie3.simona.model.participant.control.QControl.CosPhiFixed
import edu.ie3.simona.model.participant.load.{FixedLoadModel, LoadReference}
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleTriggerMessage
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.test.common.AgentSpec
import edu.ie3.simona.test.common.model.participant.LoadTestData
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.OperationInterval
import org.mockito.Mockito.when
import org.scalatest.PrivateMethodTester
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor3, TableFor5}
import org.scalatestplus.mockito.MockitoSugar
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import java.util.concurrent.TimeUnit
import javax.measure.quantity.{Energy, Power}

class ParticipantAgentFundamentalsSpec
    extends AgentSpec(
      ActorSystem(
        "ParticipantAgentSpec",
        ConfigFactory
          .parseString("""
            |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
            |akka.loglevel="DEBUG"
        """.stripMargin)
      )
    )
    with LoadTestData
    with PrivateMethodTester
    with TableDrivenPropertyChecks {
  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  private val outputConfig: ParticipantNotifierConfig =
    ParticipantNotifierConfig(
      simulationResultInfo = false,
      powerRequestReply = false
    )

  /* Get one instance of the mock for participant agent */
  val mockAgentTestRef: TestFSMRef[AgentState, ParticipantStateData[
    ApparentPower
  ], ParticipantAgentMock] =
    TestFSMRef(
      new ParticipantAgentMock(
        scheduler = self
      )
    )
  val mockAgent: ParticipantAgentMock = mockAgentTestRef.underlyingActor

  val powerValues =
    Map(
      0L -> ApparentPower(
        Quantities.getQuantity(1d, MEGAWATT),
        Quantities
          .getQuantity(0d, MEGAVAR)
      ),
      1L -> ApparentPower(
        Quantities.getQuantity(2d, MEGAWATT),
        Quantities
          .getQuantity(1d, MEGAVAR)
      ),
      3L -> ApparentPower(
        Quantities.getQuantity(3d, MEGAWATT),
        Quantities
          .getQuantity(2d, MEGAVAR)
      ),
      4L -> ApparentPower(
        Quantities.getQuantity(5d, MEGAWATT),
        Quantities
          .getQuantity(4d, MEGAVAR)
      ),
      7L -> ApparentPower(
        Quantities.getQuantity(3d, MEGAWATT),
        Quantities
          .getQuantity(2d, MEGAVAR)
      ),
      8L -> ApparentPower(
        Quantities.getQuantity(6d, MEGAWATT),
        Quantities
          .getQuantity(5d, MEGAVAR)
      ),
      9L -> ApparentPower(
        Quantities.getQuantity(6d, MEGAWATT),
        Quantities
          .getQuantity(5d, MEGAVAR)
      ),
      10L -> ApparentPower(
        Quantities.getQuantity(4d, MEGAWATT),
        Quantities
          .getQuantity(3d, MEGAVAR)
      )
    )

  /* Calculates the reactive power as the square of the active power */
  val activeToReactivePowerFuncOpt: Option[
    PartialFunction[ComparableQuantity[Power], ComparableQuantity[Power]]
  ] =
    Some(
      new PartialFunction[ComparableQuantity[Power], ComparableQuantity[
        Power
      ]] {
        override def isDefinedAt(
            activePower: ComparableQuantity[Power]
        ): Boolean = true

        override def apply(
            activePower: ComparableQuantity[Power]
        ): ComparableQuantity[Power] =
          Quantities.getQuantity(
            pow(activePower.to(MEGAWATT).getValue.doubleValue(), 2),
            MEGAVAR
          )
      }
    )

  "Determining the activation ticks within operation time" should {
    "throw an exception, if an integer multiple of the resolution does not meet an hour" in {
      val simulationStart =
        TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")
      val exception = intercept[AgentInitializationException] {
        mockAgent.firstFullResolutionInSimulation(simulationStart, 47L)
      }
      exception.getMessage shouldBe "The data resolution has to be adjusted, so that an integer multiple of it fits a full hour."
    }

    "base on correct first information tick in simulation" in {
      val testData: TableFor3[String, Long, Long] = Table(
        ("simulationStartString", "resolution", "expectedFirstTick"),
        ("2020-01-01 00:00:00", 900L, 0L),
        ("2020-01-01 00:15:00", 900L, 0L),
        ("2020-01-01 00:15:10", 900L, 890L),
        ("2020-01-01 00:15:00", 1800L, 900L),
        ("2020-01-01 00:14:10", 1800L, 950L)
      )

      forAll(testData) {
        (
            simulationStartString: String,
            resolution: Long,
            expectedFirstTick: Long
        ) =>
          {
            val simulationStart =
              TimeUtil.withDefaults.toZonedDateTime(simulationStartString)
            val firstTick = mockAgent.firstFullResolutionInSimulation(
              simulationStart,
              resolution
            )

            firstTick shouldBe expectedFirstTick
          }
      }
    }

    "bring up correct activation ticks" in {
      val testData: TableFor5[String, Long, Long, Long, List[Long]] = Table(
        (
          "simulationStartString",
          "resolution",
          "operationStart",
          "operationEnd",
          "expectedTicks"
        ),
        ("2020-01-01 00:00:00", 900L, 0L, 2700L, List(0L, 900L, 1800L, 2700L)),
        ("2020-01-01 00:15:00", 900L, 0L, 2700L, List(0L, 900L, 1800L, 2700L)),
        ("2020-01-01 00:15:00", 900L, 0L, 2699L, List(0L, 900L, 1800L)),
        ("2020-01-01 00:12:00", 900L, 0L, 2700L, List(180L, 1080L, 1980L)),
        (
          "2020-01-01 00:12:00",
          900L,
          0L,
          2880L,
          List(180L, 1080L, 1980L, 2880L)
        )
      )

      forAll(testData) {
        (
            simulationStartString: String,
            resolution: Long,
            operationStart: Long,
            operationEnd: Long,
            expectedTicks: List[Long]
        ) =>
          {
            val simulationStart =
              TimeUtil.withDefaults.toZonedDateTime(simulationStartString)
            val additionalActivationTicks =
              mockAgent.activationTicksInOperationTime(
                simulationStart,
                resolution,
                operationStart,
                operationEnd
              )

            additionalActivationTicks.corresponds(expectedTicks.toArray)(
              _ == _
            ) shouldBe true
          }
      }
    }
  }

  "Determining the next activation tick" should {
    "bring up no activation trigger" in {
      val baseStateData = ParticipantAgentFundamentalsSpec.mockBaseStateData(
        Array.emptyLongArray,
        Map.empty
      )

      mockAgent.popNextActivationTrigger(baseStateData) match {
        case (None, actualBaseStateData) =>
          /* There is no activation trigger and base state data haven't changed */
          actualBaseStateData shouldBe baseStateData
        case (Some(_), _) =>
          fail("Did not get the right activation triggers and state data")
      }
    }

    "bring up the next foreseen data tick, if this is the closest one" in {
      val baseStateData = ParticipantAgentFundamentalsSpec.mockBaseStateData(
        Array(100L, 200L, 300L),
        Map(
          self -> Some(10L),
          noSender -> Some(0L)
        )
      )

      mockAgent.popNextActivationTrigger(baseStateData) match {
        case (Some(activationSeq), actualBaseStateData) =>
          /* There is exactly one activation trigger for tick 0 */
          activationSeq.size shouldBe 1
          activationSeq.headOption match {
            case Some(
                  ScheduleTriggerMessage(
                    ActivityStartTrigger(tick),
                    actorToBeScheduled
                  )
                ) =>
              tick shouldBe 0L
              actorToBeScheduled shouldBe mockAgentTestRef
            case _ => fail("Sequence of activation triggers has wrong content.")
          }
          /* Base state data haven't changed */
          actualBaseStateData shouldBe baseStateData
        case _ =>
          fail("Did not get the right activation triggers and state data")
      }
    }

    "bring up the next additional activation tick, if this is the closest one and pop it from base state data" in {
      val baseStateData = ParticipantAgentFundamentalsSpec.mockBaseStateData(
        Array(0L, 10L, 20L),
        Map(
          self -> Some(200L),
          noSender -> Some(100L)
        )
      )

      mockAgent.popNextActivationTrigger(baseStateData) match {
        case (Some(activationSeq), actualBaseStateData) =>
          /* There is exactly one activation trigger for tick 1 */
          activationSeq.size shouldBe 1
          activationSeq.headOption match {
            case Some(
                  ScheduleTriggerMessage(
                    ActivityStartTrigger(tick),
                    actorToBeScheduled
                  )
                ) =>
              tick shouldBe 0L
              actorToBeScheduled shouldBe mockAgentTestRef
            case _ => fail("Sequence of activation triggers has wrong content.")
          }
          /* Additional activation tick has been popped from base state data */
          actualBaseStateData.additionalActivationTicks.corresponds(
            Array(10L, 20L)
          )(_ == _) shouldBe true
          actualBaseStateData.foreseenDataTicks shouldBe baseStateData.foreseenDataTicks
        case _ =>
          fail("Did not get the right activation triggers and state data")
      }
    }

    "bring up the next additional activation tick, if it and a data tick at the same time are the closest ones and pop it from base state data" in {
      val baseStateData = ParticipantAgentFundamentalsSpec.mockBaseStateData(
        Array(0L, 10L, 20L),
        Map(
          self -> Some(20L),
          noSender -> Some(0L)
        )
      )

      mockAgent.popNextActivationTrigger(baseStateData) match {
        case (Some(activationSeq), actualBaseStateData) =>
          /* There is exactly one activation trigger for tick 1 */
          activationSeq.size shouldBe 1
          activationSeq.headOption match {
            case Some(
                  ScheduleTriggerMessage(
                    ActivityStartTrigger(tick),
                    actorToBeScheduled
                  )
                ) =>
              tick shouldBe 0L
              actorToBeScheduled shouldBe mockAgentTestRef
            case _ => fail("Sequence of activation triggers has wrong content.")
          }
          /* Additional activation tick has been popped from base state data */
          actualBaseStateData.additionalActivationTicks.corresponds(
            Array(10L, 20L)
          )(_ == _) shouldBe true
          actualBaseStateData.foreseenDataTicks shouldBe baseStateData.foreseenDataTicks
        case _ =>
          fail("Did not get the right activation triggers and state data")
      }
    }
  }

  "Calculating the average power from simulation results" should {
    "lead to correct average power, if the window start is before the first data" in {
      val apparentPower = mockAgent.averageResults(
        powerValues,
        -10L,
        5L,
        None
      )
      apparentPower match {
        case ApparentPower(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0.8666666666666667, MEGAWATT)
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0.5333333333333334, MEGAVAR)
          )
      }
    }

    "lead to correct average power, if the window end is after the most recent data" in {
      val apparentPower =
        mockAgent.averageResults(
          powerValues,
          8L,
          15L,
          None
        )
      apparentPower match {
        case ApparentPower(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(4.571428571428573, MEGAWATT)
          )
          q should equalWithTolerance(
            Quantities.getQuantity(3.571428571428571, MEGAVAR)
          )
      }
    }

    "lead to correct average power in between" in {
      val apparentPower =
        mockAgent.averageResults(
          powerValues,
          8L,
          15L,
          None
        )
      apparentPower match {
        case ApparentPower(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(4.571428571428573, MEGAWATT)
          )
          q should equalWithTolerance(
            Quantities.getQuantity(3.571428571428571, MEGAVAR)
          )
      }
    }

    "lead to correct average power, if the window start is before the first data - with active to reactive power function" in {
      val apparentPower =
        mockAgent.averageResults(
          powerValues,
          -10L,
          5L,
          activeToReactivePowerFuncOpt
        )
      apparentPower match {
        case ApparentPower(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0.8666666666666667, MEGAWATT)
          )
          q should equalWithTolerance(
            Quantities.getQuantity(2.8666666666666667, MEGAVAR)
          )
      }
    }

    "lead to correct average power, if the window end is after the most recent data - with active to reactive power function" in {
      val apparentPower =
        mockAgent.averageResults(
          powerValues,
          8L,
          15L,
          activeToReactivePowerFuncOpt
        )
      apparentPower match {
        case ApparentPower(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(4.571428571428573, MEGAWATT)
          )
          q should equalWithTolerance(
            Quantities.getQuantity(21.71428571428571, MEGAVAR)
          )
      }
    }

    "lead to correct average power in between - with active to reactive power function" in {
      val apparentPower =
        mockAgent.averageResults(
          powerValues,
          8L,
          15L,
          activeToReactivePowerFuncOpt
        )
      apparentPower match {
        case ApparentPower(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(4.571428571428573, MEGAWATT)
          )
          q should equalWithTolerance(
            Quantities.getQuantity(21.71428571428571, MEGAVAR)
          )
      }
    }
  }

  "Determining the relevant result data" should {
    "returns the correct data, if already something has been answered and new data is awaited" in {
      val requestTick = 1800L
      val resultValueStore = ValueStore(
        900,
        Map(
          800L -> ApparentPower(
            Quantities.getQuantity(0d, MEGAWATT),
            Quantities.getQuantity(0d, MEGAVAR)
          ),
          1000L -> ApparentPower(
            Quantities.getQuantity(0d, MEGAWATT),
            Quantities.getQuantity(0d, MEGAVAR)
          ),
          1200L -> ApparentPower(
            Quantities.getQuantity(0d, MEGAWATT),
            Quantities.getQuantity(0d, MEGAVAR)
          ),
          1400L -> ApparentPower(
            Quantities.getQuantity(0d, MEGAWATT),
            Quantities.getQuantity(0d, MEGAVAR)
          ),
          1600L -> ApparentPower(
            Quantities.getQuantity(0d, MEGAWATT),
            Quantities.getQuantity(0d, MEGAVAR)
          ),
          1800L -> ApparentPower(
            Quantities.getQuantity(0d, MEGAWATT),
            Quantities.getQuantity(0d, MEGAVAR)
          )
        )
      )
      val requestValueStore = ValueStore(
        900,
        Map(
          900L -> ApparentPower(
            Quantities.getQuantity(0d, MEGAWATT),
            Quantities.getQuantity(0d, MEGAVAR)
          )
        )
      )

      mockAgent.getRelevantResultData(
        requestTick,
        resultValueStore,
        requestValueStore
      ) shouldBe Some(
        RelevantResultValues(
          900L,
          1800L,
          Map(
            800L -> ApparentPower(
              Quantities.getQuantity(0d, MEGAWATT),
              Quantities.getQuantity(0d, MEGAVAR)
            ),
            1000L -> ApparentPower(
              Quantities.getQuantity(0d, MEGAWATT),
              Quantities.getQuantity(0d, MEGAVAR)
            ),
            1200L -> ApparentPower(
              Quantities.getQuantity(0d, MEGAWATT),
              Quantities.getQuantity(0d, MEGAVAR)
            ),
            1400L -> ApparentPower(
              Quantities.getQuantity(0d, MEGAWATT),
              Quantities.getQuantity(0d, MEGAVAR)
            ),
            1600L -> ApparentPower(
              Quantities.getQuantity(0d, MEGAWATT),
              Quantities.getQuantity(0d, MEGAVAR)
            ),
            1800L -> ApparentPower(
              Quantities.getQuantity(0d, MEGAWATT),
              Quantities.getQuantity(0d, MEGAVAR)
            )
          )
        )
      )
    }

    "returns the correct data, if already something has been answered and NO new data is awaited" in {
      val requestTick = 1800L
      val resultValueStore = ValueStore(
        900,
        Map(
          800L -> ApparentPower(
            Quantities.getQuantity(0d, MEGAWATT),
            Quantities.getQuantity(0d, MEGAVAR)
          )
        )
      )
      val requestValueStore = ValueStore(
        900,
        Map(
          900L -> ApparentPower(
            Quantities.getQuantity(0d, MEGAWATT),
            Quantities.getQuantity(0d, MEGAVAR)
          )
        )
      )

      mockAgent.getRelevantResultData(
        requestTick,
        resultValueStore,
        requestValueStore
      ) shouldBe Some(
        RelevantResultValues(
          900L,
          1800L,
          Map(
            800L -> ApparentPower(
              Quantities.getQuantity(0d, MEGAWATT),
              Quantities.getQuantity(0d, MEGAVAR)
            )
          )
        )
      )
    }
  }

  "Determining the applicable nodal voltage" should {
    "deliver the correct voltage" in {
      val baseStateData = ParticipantModelBaseStateData(
        simulationStartDate,
        simulationEndDate,
        FixedLoadModel(
          UUID.randomUUID(),
          "test_load",
          OperationInterval(0L, 1800L),
          1.0,
          CosPhiFixed(0.95),
          Quantities.getQuantity(100d, KILOVOLTAMPERE),
          0.95,
          LoadReference.ActivePower(Quantities.getQuantity(95d, KILOWATT))
        ),
        None,
        outputConfig,
        Array(0L, 900L, 1800L),
        Map.empty,
        1e-12,
        ValueStore
          .forVoltage(901L, Quantities.getQuantity(1d, PowerSystemUnits.PU)),
        ValueStore(901L),
        ValueStore(901L),
        ValueStore(901L)
      )

      ParticipantAgent.getAndCheckNodalVoltage(
        baseStateData,
        1000L
      ) shouldBe Quantities
        .getQuantity(1d, PU)
    }

    "throw an error, if no nodal voltage is available" in {
      val baseStateData = ParticipantModelBaseStateData(
        simulationStartDate,
        simulationEndDate,
        FixedLoadModel(
          UUID.randomUUID(),
          "test_load",
          OperationInterval(0L, 1800L),
          1.0,
          CosPhiFixed(0.95),
          Quantities.getQuantity(100d, KILOVOLTAMPERE),
          0.95,
          LoadReference.ActivePower(Quantities.getQuantity(95d, KILOWATT))
        ),
        None,
        outputConfig,
        Array(0L, 900L, 1800L),
        Map.empty,
        1e-12,
        ValueStore(901L),
        ValueStore(901L),
        ValueStore(901L),
        ValueStore(901L)
      )

      intercept[InconsistentStateException] {
        ParticipantAgent.getAndCheckNodalVoltage(baseStateData, 1000L)
      }.getMessage shouldBe "Not knowing any nodal voltage is not supposed to happen."
    }
  }
}

case object ParticipantAgentFundamentalsSpec extends MockitoSugar {

  /** Mock [[ParticipantModelBaseStateData]] in order to only test the handling
    * of additional activation ticks as well as foreseen data ticks
    *
    * @param additionalActivationTicks
    *   Array of additional activation ticks
    * @param foreseenDataTicks
    *   Mapping of data providers to foreseen activation ticks
    * @return
    *   mocked base state data
    */
  def mockBaseStateData(
      additionalActivationTicks: Array[Long],
      foreseenDataTicks: Map[ActorRef, Option[Long]]
  ): ParticipantModelBaseStateData[
    ApparentPower,
    FixedRelevantData.type,
    SystemParticipant[FixedRelevantData.type]
  ] = {
    val modelMock = mock[SystemParticipant[FixedRelevantData.type]]
    when(modelMock.getUuid).thenReturn(UUID.randomUUID())

    ParticipantModelBaseStateData(
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00"),
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01 23:59:00"),
      modelMock,
      None,
      ParticipantNotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false
      ),
      additionalActivationTicks,
      foreseenDataTicks,
      0d,
      ValueStore(0L),
      ValueStore(0L),
      ValueStore(0L),
      ValueStore(0L)
    )
  }
}
