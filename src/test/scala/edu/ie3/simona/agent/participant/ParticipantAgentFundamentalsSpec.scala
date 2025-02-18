/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import breeze.numerics.pow
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals.RelevantResultValues
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.config.RuntimeConfig.BaseRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InconsistentStateException,
}
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.SystemParticipant
import edu.ie3.simona.model.participant.control.QControl.CosPhiFixed
import edu.ie3.simona.model.participant.load.FixedLoadModel.FixedLoadRelevantData
import edu.ie3.simona.model.participant.load.{FixedLoadModel, LoadReference}
import edu.ie3.simona.test.common.AgentSpec
import edu.ie3.simona.test.common.model.participant.LoadTestData
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{
  Kilovoltamperes,
  Megavars,
  ReactivePower,
  Vars,
}
import org.apache.pekko.actor.ActorRef.noSender
import org.apache.pekko.actor.{ActorRef, ActorSystem}
import org.apache.pekko.testkit.TestFSMRef
import org.apache.pekko.util.Timeout
import org.mockito.Mockito.when
import org.scalatest.PrivateMethodTester
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor3, TableFor5}
import org.scalatestplus.mockito.MockitoSugar
import squants.energy.{Kilowatts, Megawatts, Watts}
import squants.{Each, Power}

import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.collection.{SortedMap, SortedSet}

class ParticipantAgentFundamentalsSpec
    extends AgentSpec(
      ActorSystem(
        "ParticipantAgentSpec",
        ConfigFactory
          .parseString("""
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="DEBUG"
        """.stripMargin),
      )
    )
    with LoadTestData
    with PrivateMethodTester
    with TableDrivenPropertyChecks
    with MockitoSugar {
  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)
  private implicit val pTolerance: Power = Watts(0.1)
  private implicit val qTolerance: ReactivePower = Vars(0.1)

  private val outputConfig: NotifierConfig =
    NotifierConfig(
      simulationResultInfo = false,
      powerRequestReply = false,
      flexResult = false,
    )

  /* Get one instance of the mock for participant agent */
  private val mockAgentTestRef: TestFSMRef[AgentState, ParticipantStateData[
    ComplexPower
  ], ParticipantAgentMock] =
    TestFSMRef(
      new ParticipantAgentMock(
        scheduler = self,
        initStateData = mock[ParticipantInitializeStateData[
          SystemParticipantInput,
          BaseRuntimeConfig,
          ComplexPower,
        ]],
      )
    )
  val mockAgent: ParticipantAgentMock = mockAgentTestRef.underlyingActor

  private val powerValues =
    Map(
      0L -> ComplexPower(
        Megawatts(1.0),
        Megavars(0.0),
      ),
      1L -> ComplexPower(
        Megawatts(2.0),
        Megavars(1.0),
      ),
      3L -> ComplexPower(
        Megawatts(3.0),
        Megavars(2.0),
      ),
      4L -> ComplexPower(
        Megawatts(5.0),
        Megavars(4.0),
      ),
      7L -> ComplexPower(
        Megawatts(3.0),
        Megavars(2.0),
      ),
      8L -> ComplexPower(
        Megawatts(6.0),
        Megavars(5.0),
      ),
      9L -> ComplexPower(
        Megawatts(6.0),
        Megavars(5.0),
      ),
      10L -> ComplexPower(
        Megawatts(4.0),
        Megavars(3.0),
      ),
    )

  /* Calculates the reactive power as the square of the active power */
  private val activeToReactivePowerFuncOpt: Option[
    PartialFunction[squants.Power, ReactivePower]
  ] =
    Some(
      new PartialFunction[squants.Power, ReactivePower] {
        override def isDefinedAt(
            activePower: squants.Power
        ): Boolean = true

        override def apply(
            activePower: squants.Power
        ): ReactivePower =
          Megavars(pow(activePower.toMegawatts, 2))
      }
    )

  "Determining the activation ticks within operation time" should {
    "throw an exception, if an integer multiple of the resolution does not meet an hour" in {
      val simulationStart =
        TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
      val exception = intercept[AgentInitializationException] {
        mockAgent.firstFullResolutionInSimulation(simulationStart, 47L)
      }
      exception.getMessage shouldBe "The data resolution has to be adjusted, so that an integer multiple of it fits a full hour."
    }

    "base on correct first information tick in simulation" in {
      val testData: TableFor3[String, Long, Long] = Table(
        ("simulationStartString", "resolution", "expectedFirstTick"),
        ("2020-01-01T00:00:00Z", 900L, 0L),
        ("2020-01-01T00:15:00Z", 900L, 0L),
        ("2020-01-01T00:15:10Z", 900L, 890L),
        ("2020-01-01T00:15:00Z", 1800L, 900L),
        ("2020-01-01T00:14:10Z", 1800L, 950L),
      )

      forAll(testData) {
        (
            simulationStartString: String,
            resolution: Long,
            expectedFirstTick: Long,
        ) =>
          {
            val simulationStart =
              TimeUtil.withDefaults.toZonedDateTime(simulationStartString)
            val firstTick = mockAgent.firstFullResolutionInSimulation(
              simulationStart,
              resolution,
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
          "expectedTicks",
        ),
        ("2020-01-01T00:00:00Z", 900L, 0L, 2700L, List(0L, 900L, 1800L, 2700L)),
        ("2020-01-01T00:15:00Z", 900L, 0L, 2700L, List(0L, 900L, 1800L, 2700L)),
        ("2020-01-01T00:15:00Z", 900L, 0L, 2699L, List(0L, 900L, 1800L)),
        ("2020-01-01T00:12:00Z", 900L, 0L, 2700L, List(180L, 1080L, 1980L)),
        (
          "2020-01-01T00:12:00Z",
          900L,
          0L,
          2880L,
          List(180L, 1080L, 1980L, 2880L),
        ),
      )

      forAll(testData) {
        (
            simulationStartString: String,
            resolution: Long,
            operationStart: Long,
            operationEnd: Long,
            expectedTicks: List[Long],
        ) =>
          {
            val simulationStart =
              TimeUtil.withDefaults.toZonedDateTime(simulationStartString)
            val additionalActivationTicks =
              mockAgent.activationTicksInOperationTime(
                simulationStart,
                resolution,
                operationStart,
                operationEnd,
              )

            additionalActivationTicks.corresponds(expectedTicks)(
              _ == _
            ) shouldBe true
          }
      }
    }
  }

  "Determining the next activation tick" should {
    "bring up no activation trigger" in {
      val baseStateData = ParticipantAgentFundamentalsSpec.mockBaseStateData(
        SortedSet.empty,
        Map.empty,
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
        SortedSet(100L, 200L, 300L),
        Map(
          self -> Some(10L),
          noSender -> Some(0L),
        ),
      )

      mockAgent.popNextActivationTrigger(baseStateData) match {
        case (Some(activation), actualBaseStateData) =>
          activation shouldBe 0L
          /* Base state data haven't changed */
          actualBaseStateData shouldBe baseStateData
        case _ =>
          fail("Did not get the right activation triggers and state data")
      }
    }

    "bring up the next additional activation tick, if this is the closest one and pop it from base state data" in {
      val baseStateData = ParticipantAgentFundamentalsSpec.mockBaseStateData(
        SortedSet(0L, 10L, 20L),
        Map(
          self -> Some(200L),
          noSender -> Some(100L),
        ),
      )

      mockAgent.popNextActivationTrigger(baseStateData) match {
        case (Some(activation), actualBaseStateData) =>
          activation shouldBe 0L
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
        SortedSet(0L, 10L, 20L),
        Map(
          self -> Some(20L),
          noSender -> Some(0L),
        ),
      )

      mockAgent.popNextActivationTrigger(baseStateData) match {
        case (Some(activation), actualBaseStateData) =>
          activation shouldBe 0L
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
        None,
      )
      apparentPower match {
        case ComplexPower(p, q) =>
          p should approximate(Megawatts(0.8666666666666667))
          q should approximate(Megavars(0.5333333333333334))
      }
    }

    "lead to correct average power, if the window end is after the most recent data" in {
      val apparentPower =
        mockAgent.averageResults(
          powerValues,
          8L,
          15L,
          None,
        )
      apparentPower match {
        case ComplexPower(p, q) =>
          p should approximate(Megawatts(4.571428571428573))
          q should approximate(Megavars(3.571428571428571))
      }
    }

    "lead to correct average power in between" in {
      val apparentPower =
        mockAgent.averageResults(
          powerValues,
          8L,
          15L,
          None,
        )
      apparentPower match {
        case ComplexPower(p, q) =>
          p should approximate(Megawatts(4.571428571428573))
          q should approximate(Megavars(3.571428571428571))
      }
    }

    "lead to correct average power, if the window start is before the first data - with active to reactive power function" in {
      val apparentPower =
        mockAgent.averageResults(
          powerValues,
          -10L,
          5L,
          activeToReactivePowerFuncOpt,
        )
      apparentPower match {
        case ComplexPower(p, q) =>
          p should approximate(Megawatts(0.8666666666666667))
          q should approximate(Megavars(2.8666666666666667))
      }
    }

    "lead to correct average power, if the window end is after the most recent data - with active to reactive power function" in {
      val apparentPower =
        mockAgent.averageResults(
          powerValues,
          8L,
          15L,
          activeToReactivePowerFuncOpt,
        )
      apparentPower match {
        case ComplexPower(p, q) =>
          p should approximate(Megawatts(4.571428571428573))
          q should approximate(Megavars(21.71428571428571))
      }
    }

    "lead to correct average power in between - with active to reactive power function" in {
      val apparentPower =
        mockAgent.averageResults(
          powerValues,
          8L,
          15L,
          activeToReactivePowerFuncOpt,
        )
      apparentPower match {
        case ComplexPower(p, q) =>
          p should approximate(Megawatts(4.571428571428573))
          q should approximate(Megavars(21.71428571428571))
      }
    }
  }

  "Determining the relevant result data" should {
    "returns the correct data, if already something has been answered and new data is awaited" in {
      val requestTick = 1800L
      val resultValueStore = ValueStore(
        900,
        SortedMap(
          800L -> ComplexPower(
            Megawatts(0.0),
            Megavars(0.0),
          ),
          1000L -> ComplexPower(
            Megawatts(0.0),
            Megavars(0.0),
          ),
          1200L -> ComplexPower(
            Megawatts(0.0),
            Megavars(0.0),
          ),
          1400L -> ComplexPower(
            Megawatts(0.0),
            Megavars(0.0),
          ),
          1600L -> ComplexPower(
            Megawatts(0.0),
            Megavars(0.0),
          ),
          1800L -> ComplexPower(
            Megawatts(0.0),
            Megavars(0.0),
          ),
        ),
      )
      val requestValueStore = ValueStore(
        900,
        SortedMap(
          900L -> ComplexPower(
            Megawatts(0.0),
            Megavars(0.0),
          )
        ),
      )

      mockAgent.getRelevantResultData(
        requestTick,
        resultValueStore,
        requestValueStore,
      ) shouldBe Some(
        RelevantResultValues(
          900L,
          1800L,
          Map(
            800L -> ComplexPower(
              Megawatts(0.0),
              Megavars(0.0),
            ),
            1000L -> ComplexPower(
              Megawatts(0.0),
              Megavars(0.0),
            ),
            1200L -> ComplexPower(
              Megawatts(0.0),
              Megavars(0.0),
            ),
            1400L -> ComplexPower(
              Megawatts(0.0),
              Megavars(0.0),
            ),
            1600L -> ComplexPower(
              Megawatts(0.0),
              Megavars(0.0),
            ),
            1800L -> ComplexPower(
              Megawatts(0.0),
              Megavars(0.0),
            ),
          ),
        )
      )
    }

    "returns the correct data, if already something has been answered and NO new data is awaited" in {
      val requestTick = 1800L
      val resultValueStore = ValueStore(
        900,
        SortedMap(
          800L -> ComplexPower(
            Megawatts(0.0),
            Megavars(0.0),
          )
        ),
      )
      val requestValueStore = ValueStore(
        900,
        SortedMap(
          900L -> ComplexPower(
            Megawatts(0.0),
            Megavars(0.0),
          )
        ),
      )

      mockAgent.getRelevantResultData(
        requestTick,
        resultValueStore,
        requestValueStore,
      ) shouldBe Some(
        RelevantResultValues(
          900L,
          1800L,
          Map(
            800L -> ComplexPower(
              Megawatts(0.0),
              Megavars(0.0),
            )
          ),
        )
      )
    }
  }

  "Determining the applicable nodal voltage" should {
    "deliver the correct voltage" in {
      val baseStateData = ParticipantModelBaseStateData[
        ComplexPower,
        FixedLoadRelevantData.type,
        ConstantState.type,
        FixedLoadModel,
      ](
        simulationStartDate,
        simulationEndDate,
        FixedLoadModel(
          UUID.randomUUID(),
          "test_load",
          OperationInterval(0L, 1800L),
          CosPhiFixed(0.95),
          Kilovoltamperes(100.0),
          0.95,
          LoadReference.ActivePower(Kilowatts(95.0)),
        ),
        None,
        outputConfig,
        SortedSet(0L, 900L, 1800L),
        Map.empty,
        1e-12,
        ValueStore.forVoltage(901L, Each(1.0)),
        ValueStore(901L),
        ValueStore(901L),
        ValueStore(901L),
        ValueStore(901L),
        None,
      )

      ParticipantAgent.getAndCheckNodalVoltage(
        baseStateData,
        1000L,
      ) shouldBe Each(1.0)
    }

    "throw an error, if no nodal voltage is available" in {
      val baseStateData = ParticipantModelBaseStateData[
        ComplexPower,
        FixedLoadRelevantData.type,
        ConstantState.type,
        FixedLoadModel,
      ](
        simulationStartDate,
        simulationEndDate,
        FixedLoadModel(
          UUID.randomUUID(),
          "test_load",
          OperationInterval(0L, 1800L),
          CosPhiFixed(0.95),
          Kilovoltamperes(100.0),
          0.95,
          LoadReference.ActivePower(Kilowatts(95.0)),
        ),
        None,
        outputConfig,
        SortedSet(0L, 900L, 1800L),
        Map.empty,
        1e-12,
        ValueStore(901L),
        ValueStore(901L),
        ValueStore(901L),
        ValueStore(901L),
        ValueStore(901L),
        None,
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
      additionalActivationTicks: SortedSet[Long],
      foreseenDataTicks: Map[ActorRef, Option[Long]],
  ): ParticipantModelBaseStateData[
    ComplexPower,
    FixedRelevantData.type,
    ConstantState.type,
    SystemParticipant[FixedRelevantData.type, ComplexPower, ConstantState.type],
  ] = {
    val modelMock = mock[SystemParticipant[
      FixedRelevantData.type,
      ComplexPower,
      ConstantState.type,
    ]]
    when(modelMock.getUuid).thenReturn(UUID.randomUUID())

    ParticipantModelBaseStateData[
      ComplexPower,
      FixedRelevantData.type,
      ConstantState.type,
      SystemParticipant[
        FixedRelevantData.type,
        ComplexPower,
        ConstantState.type,
      ],
    ](
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z"),
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01T23:59:00Z"),
      modelMock,
      None,
      NotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false,
        flexResult = false,
      ),
      additionalActivationTicks,
      foreseenDataTicks,
      0d,
      ValueStore(0L),
      ValueStore(0L),
      ValueStore(0L),
      ValueStore(0L),
      ValueStore(0L),
      None,
    )
  }
}
