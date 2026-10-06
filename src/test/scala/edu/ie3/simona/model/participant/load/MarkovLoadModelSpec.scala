/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.profile.PowerProfileKey
import edu.ie3.simona.config.ConfigParams.BaseCsvParams
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.ActivePowerOperatingPoint
import edu.ie3.simona.model.participant.load.MarkovLoadModel.{
  MarkovLoadFactoryData,
  MarkovLoadModelState,
}
import edu.ie3.simona.service.Data.SecondaryData.MarkovDataFunction
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.service.load.LoadProfileStore
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.LoadInputTestData
import edu.ie3.simona.test.helper.TestResourceHelper
import edu.ie3.simona.test.matchers.DoubleMatchers
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKW}
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.Power
import squants.energy.{KilowattHours, Kilowatts, Watts}

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.mutable.ListBuffer

class MarkovLoadModelSpec
    extends UnitSpec
    with DoubleMatchers
    with LoadInputTestData
    with TestResourceHelper {

  private val simulationStartDate =
    TimeUtil.withDefaults.toZonedDateTime("2022-01-01T00:00:00Z")

  // testing tolerances
  private given ApparentPower = Kilovoltamperes(1e-6)
  private given Power = Watts(1e-6)
  private given Double = 1e-6

  /** Step function cycling through two states. All calls are recorded.
    */
  private def recordingStepFunction(
      calls: ListBuffer[(ZonedDateTime, Int, Long)]
  ): (ZonedDateTime, Int, Long) => (Power, Int) =
    (time, previousState, seed) => {
      calls.append((time, previousState, seed))
      (Kilowatts(previousState + 1d), (previousState + 1) % 2)
    }

  private def factoryData(
      energyScaling: Option[squants.Energy] = None,
      resolution: Long = 900L,
      calls: ListBuffer[(ZonedDateTime, Int, Long)] = ListBuffer.empty,
  ): MarkovLoadFactoryData = MarkovLoadFactoryData(
    Some(Kilowatts(4d)),
    energyScaling,
    resolution,
    recordingStepFunction(calls),
  )

  "A Markov load model" should {

    "be built for the Markov load model behaviour" in {
      val factory = LoadModel.getFactory(
        loadInput,
        LoadRuntimeConfig(modelBehaviour = "markov"),
        primary = false,
      )

      factory match {
        case markovFactory: MarkovLoadModel.Factory =>
          markovFactory.getRequiredSecondaryServices shouldBe Iterable(
            ServiceType.MarkovLoadProfileService
          )
        case unexpected =>
          fail(s"Received unexpected factory $unexpected")
      }
    }

    "derive a reproducible seed from the uuid of the load" in {
      val config = LoadRuntimeConfig(modelBehaviour = "markov")
      val otherLoad = loadInput
        .copy()
        .uuid(UUID.fromString("d1d9f3a6-2f2c-4e5f-9c5a-2b7a3a1f8e01"))
        .build()

      val seed = MarkovLoadModel.Factory(loadInput, config).seed

      MarkovLoadModel.Factory(loadInput, config).seed shouldBe seed
      MarkovLoadModel.Factory(otherLoad, config).seed should not be seed
    }

    "warm up the Markov chain before the simulation start" in {
      val cases = Table(
        ("resolution", "expectedSteps"),
        (900L, 96),
        (3600L, 24),
      )

      forAll(cases) { (resolution, expectedSteps) =>
        val calls = ListBuffer.empty[(ZonedDateTime, Int, Long)]
        val factory = MarkovLoadModel
          .Factory(loadInput, LoadRuntimeConfig(modelBehaviour = "markov"))
          .update(factoryData(resolution = resolution, calls = calls))

        val state = factory.getInitialState(0L, simulationStartDate)

        // runs through the day before the simulation start
        calls.size shouldBe expectedSteps
        calls.map(_._1) shouldBe Range
          .Long(expectedSteps.toLong, 0L, -1L)
          .map(step => simulationStartDate.minusSeconds(step * resolution))

        // starts with state 0, then uses the returned next state
        calls.map(_._2) shouldBe Range(0, expectedSteps).map(_ % 2)
        calls.map(_._3).toSet shouldBe Set(factory.seed)

        // the initial state results from the last warm-up step
        val (lastTime, lastState, lastSeed) = calls.last
        lastTime shouldBe simulationStartDate.minusSeconds(resolution)
        state shouldBe MarkovLoadModelState(
          0L,
          Kilowatts(lastState + 1d),
          (lastState + 1) % 2,
        )
        lastSeed shouldBe factory.seed
      }
    }

    "be instantiated without scaling for power reference" in {
      val model = MarkovLoadModel
        .Factory(loadInput, LoadRuntimeConfig(modelBehaviour = "markov"))
        .update(factoryData())
        .create()

      model.referenceScalingFactor should approximate(1d)
      // max power of the model / cos phi
      model.sRated should approximate(Kilovoltamperes(4d / 0.95))
    }

    "apply the configured scaling for power reference" in {
      val model = MarkovLoadModel
        .Factory(
          loadInput,
          LoadRuntimeConfig(modelBehaviour = "markov", scaling = 2d),
        )
        .update(factoryData())
        .create()

      model.referenceScalingFactor should approximate(2d)
      model.sRated should approximate(Kilovoltamperes(2d * 4d / 0.95))
    }

    "not apply the configured scaling again for energy reference" in {
      // the scaling is applied to the input by ParticipantModelInit
      val model = MarkovLoadModel
        .Factory(
          loadInput,
          LoadRuntimeConfig(
            modelBehaviour = "markov",
            reference = "energy",
            scaling = 2d,
          ),
        )
        .update(factoryData(energyScaling = Some(KilowattHours(1500d))))
        .create()

      model.referenceScalingFactor should approximate(2d)
    }

    "warm up the Markov chain before the start of operation" in {
      val operationStart = simulationStartDate.plusHours(18).plusMinutes(7)
      val load = loadInput
        .copy()
        .operationTime(
          OperationTime.builder().withStart(operationStart).build()
        )
        .build()

      val calls = ListBuffer.empty[(ZonedDateTime, Int, Long)]
      MarkovLoadModel
        .Factory(load, LoadRuntimeConfig(modelBehaviour = "markov"))
        .update(factoryData(calls = calls))
        .getInitialState(0L, simulationStartDate)

      // operation start 18:07 is rounded down to 18:00
      val firstStepTime = simulationStartDate.plusHours(18)

      calls.size shouldBe 96
      calls.head._1 shouldBe firstStepTime.minusDays(1)
      calls.last._1 shouldBe firstStepTime.minusMinutes(15)
    }

    "be instantiated correctly with energy reference" in {
      val model = MarkovLoadModel
        .Factory(
          loadInput,
          LoadRuntimeConfig(modelBehaviour = "markov", reference = "energy"),
        )
        .update(factoryData(energyScaling = Some(KilowattHours(1500d))))
        .create()

      // eConsAnnual of the load is 3000 kWh
      model.referenceScalingFactor should approximate(2d)
      model.sRated should approximate(Kilovoltamperes(2d * 4d / 0.95))
    }

    "fail for energy reference without energy scaling" in {
      val factory = MarkovLoadModel
        .Factory(
          loadInput,
          LoadRuntimeConfig(modelBehaviour = "markov", reference = "energy"),
        )
        .update(factoryData())

      intercept[CriticalFailureException] {
        factory.create()
      }
    }

    "fail without factory data" in {
      val factory = MarkovLoadModel.Factory(
        loadInput,
        LoadRuntimeConfig(modelBehaviour = "markov"),
      )

      intercept[CriticalFailureException] {
        factory.create()
      }
      intercept[CriticalFailureException] {
        factory.getInitialState(0L, simulationStartDate)
      }
    }

    "advance the Markov chain with received data" in {
      val model = MarkovLoadModel
        .Factory(
          loadInput,
          LoadRuntimeConfig(modelBehaviour = "markov", reference = "energy"),
        )
        .update(factoryData(energyScaling = Some(KilowattHours(1500d))))
        .create()

      val calls = ListBuffer.empty[(ZonedDateTime, Int, Long)]
      val stepFunction = recordingStepFunction(calls)

      val state = MarkovLoadModelState(900L, Kilowatts(1d), 1)

      val updatedState = model.handleInput(
        state,
        Seq(
          MarkovDataFunction((previousState, seed) =>
            stepFunction(simulationStartDate, previousState, seed)
          )
        ),
        onePU,
      )

      calls.map(call => (call._2, call._3)) shouldBe Seq((1, model.seed))
      updatedState shouldBe MarkovLoadModelState(900L, Kilowatts(2d), 0)

      // scaled power
      model.determineOperatingPoint(updatedState) match {
        case (ActivePowerOperatingPoint(power), nextTick) =>
          power should approximate(Kilowatts(4d))
          nextTick shouldBe None
      }

      // without new data, the state does not change
      model.handleInput(updatedState, Seq.empty, onePU) shouldBe updatedState

      // the chain state is kept for the new tick
      model.determineState(
        updatedState,
        ActivePowerOperatingPoint(zeroKW),
        1800L,
        simulationStartDate.plusSeconds(1800L),
      ) shouldBe updatedState.copy(tick = 1800L)
    }

    "provide reproducible initial states with a Markov load profile" in {
      val store = LoadProfileStore(
        Datasource(csvParams =
          Some(
            BaseCsvParams(
              ",",
              getResourcePath("/edu/ie3/simona/service/load/_it").toString,
              isHierarchic = false,
            )
          )
        )
      )

      val data = store
        .getMarkovLoadFactoryData(
          new PowerProfileKey("test", PowerProfileKey.Type.MARKOV)
        )
        .getOrElse(fail("We expect factory data here!"))

      val config = LoadRuntimeConfig(modelBehaviour = "markov")

      def initialState: MarkovLoadModelState = MarkovLoadModel
        .Factory(loadInput, config)
        .update(data)
        .getInitialState(0L, simulationStartDate)

      val state = initialState

      state shouldBe initialState
      Set(0, 1) should contain(state.markovState)
      // the model is normalized between 0 kW and 4 kW
      state.power.toKilowatts should (be >= 0d and be <= 4d)
    }
  }
}
